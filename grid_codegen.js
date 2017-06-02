/*
Copyright (c) 2014, Intel Corporation

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Intel Corporation nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//----------------------------------------------------------------------------
// Purpose: Grid Language Fortran Code Generation
// Author : Konstantinos Krommydas
// Date   : May 19, 2014
//----------------------------------------------------------------------------


// Used for declaring functions as variables (needed in Fortran). Scope is
// per function. This is initialized to blank at the start of EACH function.
var Func_decl;

// Used for declaring row, col, etc. Scope is per function. This is 
// initialized to blank at the start of EACH function.
// Need separate than step code, because declarations cannot go into
// execution block in Fortran (i.e., mix declarations with other code).
var Row_col_decl;

// Used for declaring endX. Scope is per function. This is 
// initialized to blank at the start of EACH function.
var Index_end_decl;

// Used for declaring _d3Tab1, etc. (i.e., title names). Scope is per function
// In the form: INTEGER :: <gridName>_<titleName> = <value>, ...
// This is initialized to blank at the start of EACH function.
var TitleDefs_decl;

// Used for declaring grids declared in various steps of a function. 
// Scope is per function. This is initialized to blank at the start 
// of EACH function.
var Grids_new_decl;

// Used for saving all the free() commands for dynamically allocated grids.
// Used in C, Fortran and OpenCL alike. This is initialized to blank at the 
// start of EACH function.
var AllocFreePerFunc;

// Used to save types of all functions in program. Array index is assigned 
// in order of calling in the program.
var TypesAllFuncs;

// Used to save names of all functions in program. Array IDs are assigned 
// in order of calling in the duration of the program.
var NamesAllFuncs;

// Used to record id numbers of each function as it is parsed (used for 
// finding type by associating the two variables above).
var GID_function;

// Used to record current step in a function (for naming the loop variables
// in consecutive steps).
// TODO: This is not necessary: function object has a curStepNum parameter.
var CurStep;

// Used to store grid IDs of grids that have been declared in current function
// (so as to avoid redeclaration in subsequent steps of the same function).
var GridsInFunc;

// Used to store, as an array, the ft_row, ft_col, etc., variables for all 
// dims found and declared so far, so we do not redundantly redeclare them for
// different steps, as in JS (where it doesn't matter).
// Similarly for endv index per dimension.
var Loop_var_per_dim;
var Index_end_per_dim;

// Used to select between generating (to save/show) parallel or non-parallel
// code.
var ShowParallel;

// Used to store the generated code for derived types, i.e., Fortran TYPE 
// structures (to be stored in a module). Initialized once per program.
var TypeStr;

// Used to store code used in library functions that need to be generated
// at runtime (e.g., because we need to know a grid's datatype/dimensions).
// LibFunctionsCode will enclose all library function code in a MODULE and 
// that module will be USEs by every function it needs to be called from. 
// MODULE will include code for any global variables needed.
var LibFunctionsCode;

// Structures of Arrays (SoA) is 1, Arrays of Structures (AoS) is 0.
var Soa;

//Used to store names of declared modules, so we can call USE on them. 
//We create "types_module" that contains all derived types, and "lib_module",
//which contains code for library functions (global vars and subroutines).
//Now, we call the former from the latter (if the former is NOT empty), and 
//the latter from EVERY function in the program.
//TODO: Only call USE for the modules needed (if) in a given function/module.
//	Even better, use ONLY clause of USE, to specify exactly what to use.


// TODO: If we do not initialize this (or anything like this, contents will be
// "duplicated" with subsequent program runs if not reloading the page/script)
// Hardcoded code for file input: reading csv:
// __TYPE__ is replaced in code generation with the data type of the grid 
// which will be loaded with the values from the CSV.
// __DIMENSION__ will contain COMMA and the DIMENSION(X,X,...) for the grid OR
// nothing if a table with derived type.
var FileInput_loadCSV_Fortran =
    "SUBROUTINE CSV_FILE___SUBNAME__(testgrid, fileName)\n" +
    "IMPLICIT NONE\n" +
    "CHARACTER (LEN=128) :: fileName\n" +
    "CHARACTER(LEN=128) :: tmp_char\n" +
    "__TYPE__ __DIMENSION__ testgrid\n" +
    "INTEGER :: I,J\n" +
    "OPEN (22, FILE=fileName)\n" +
    "__CSVREADWRITE__" +
    "CLOSE (22)\n" +
    "RETURN\n" +
    "END SUBROUTINE\n";



//DC99:
//TODO: Add support for the LARGE test file.
var LibLoadCSR_F =
"subroutine readCSR(neq,nb,iam,jam,a_off,a_diag_lu,dq,dqc,res,color_indices)\n" +
"    implicit none\n" +
"    integer, dimension(40)  :: color_indices\n" +
"    integer,dimension(:),allocatable:: iam, jam\n" +
"    real, dimension(:,:,:), allocatable :: a_off\n" +
"    real, dimension(:,:,:), allocatable :: a_diag_lu\n" +
"    real, dimension(:,:), allocatable :: dq,dqc,res\n" +
"    integer :: nb, neq, i,j,nnz, k, row, ierr\n" +
"    !use sample distribution for random generated matrix\n" +
"    allocate(iam(neq+3), STAT=ierr ) ! little extra to keep read simple\n" +
"    if  (ierr/=0) print *, \"Error Allocating iam\"\n" +
"    ! read iam file\n" +
"    if (neq == 6309) then\n" +
"       open(11,file='iamS.txt')\n" +
"       DO row = 1,1052\n" +
"          k  = (row-1)*6+1\n" +
"          READ(11,FMT=*) iam(k),iam(k+1),iam(k+2),iam(k+3),iam(k+4),iam(k+5)\n" +
"       END DO\n" +
"       close(11)\n" +
"    end if\n" +
"    ! allocating memory based on number of non zeros\n" +
"    nnz = iam(neq+1)-1\n" +
"    allocate(jam(nnz),  STAT=ierr)\n" +
"    if  (ierr/=0) print *, \"Error Allocating jam\"\n" +
"       allocate(a_off(nb, nb, nnz),  STAT=ierr)\n" +
"    if  (ierr/=0) print *, \"Error Allocating _off\"\n" +
"       allocate(a_diag_lu(nb, nb, neq),  STAT=ierr)\n" +
"    if  (ierr/=0) print *, \"Error Allocating a_diag_lu\"\n" +
"       allocate(dq(nb,neq),  STAT=ierr)\n" +
"    if  (ierr/=0) print *, \"Error Allocating dq\"\n" +
"       allocate(dqc(nb,neq),  STAT=ierr)\n" +
"    if  (ierr/=0) print *, \"Error Allocating dqc\"\n" +
"       allocate(res(nb,neq),  STAT=ierr)\n" +
"    if  (ierr/=0) print *, \"Error Allocating res\"\n" +
"    ! read and generate rest of the data \n" +
"    if (neq == 6309) then\n" +
"       open(11,file='jamS.txt')\n" +
"       DO row = 1,14295\n" +
"          k = (row-1)*6+1\n" +
"          READ(11,FMT=*) jam(k),jam(k+1),jam(k+2),jam(k+3),jam(k+4),jam(k+5)\n" +
"       END DO\n" +
"       close(11)\n" +
"    end if\n" +
"    if (neq == 6309) then\n" +
"       open(11,file='color_indicesS.txt')\n" +
"       DO row = 1,4\n" +
"          k = (row-1)*5+1\n" +
"          READ(11,FMT=*) color_indices(k),color_indices(k+1),color_indices(k+2), &\n" +
"          color_indices(k+3), color_indices(k+4)\n" +
"       END DO\n" +
"       close(11)\n" +
"    end if\n" +
"    ! generate a_off, a_diag_lu, dq, res\n" +
"    call random_number(a_off)\n" +
"    call random_number(a_diag_lu)\n" +
"    call random_number(dq)\n" +
"    dqc = dq\n" +
"    call random_number(res)\n" +
"    a_off = 0.1 * a_off\n" +
"    a_diag_lu = 0.1*a_diag_lu\n" +
"    dq = 0.1 * dq\n" +
"    dqc = 0.1 * dqc\n" +
"    res = 0.1 * res\n" +
"    return\n" +
"  end subroutine readCSR\n";

   

function showAuTuMenu() {

    sO = CurStepObj;

    if ((sO.stageInStep > StageInStep.New) &&
        (sO.stageInStep < StageInStep.AllDone)) {
        alert("Please complete current step before generating code");
        return;
    }
	
    initHtmlGridIds();

    drawOutAutotuneMenu();

}


//----------------------------------------------------------------------------
// Draw code-generation and auto-tune menu page.
//----------------------------------------------------------------------------
function drawOutAutotuneMenu() {

    var menuId = OutHtmlId;
    var menu1 = document.getElementById(menuId);

    var str = "<body><h1>Code generation options</h1>";
    str += "<p>Please, select your target platform, target language(s), and " +
	"data layout optimization below." +
	/* TODO: Enable later if we add more buttons for on-line/serv-side ver
	"and the desired auto-tuning options.<br>" + 
    	"Then, click on the button that corresponds to the desired action " +
	"(hover mouse for a brief explanations)." + 
	*/
	"<br><br></p>";

    str += "<form name='targetForm'>" +
	"<label for='target'><b>Target Platform:</b></label><br>" +
        "<input type='radio' name='targSel' value='CPU' > CPU" +
	"<br>" +
        "<input type='radio' name='targSel' value='MIC' > MIC" +
	/* TODO: Enable later + appropriately adapt gen.files naming conventions
	"<br>" +
	"<input type='radio' name='targSel' value='GPU'" +
	" disabled='disabled' > Gen Graphics" +
	*/
	"<br><br>";

    str += "<label for='target'><b>Target Languages:</b></label><br>" +
	"<input type='checkbox' name='langSel' value='Fortran' > Fortran" +
	"<br>" +
        "<input type='checkbox' name='langSel' value='C' > C" +
	/* TODO: Enable later + appropriately adapt gen.files naming conventions
	"<br>" +
	"<input type='checkbox' name='langSel' value='OpenCL'" +
	" disabled='disabled' > OpenCL" +
	*/
	"<br><br>";

    str += "<label for='target'><b>Basic Auto-Tuning Options:</b></label><br>" +
	"<input type='checkbox' name='autoTuneSel' value='ser'>Serial version" +
	"<br>" +
	"<input type='checkbox' name='autoTuneSel' value='parTool'>" + 
	"Parallel version (tool-generated)" +
	"<br>" +
	"<input type='checkbox' name='autoTuneSel' value='parComp' >" +
	"Parallel version (compiler-generated)" +
	"<br><br>";

    str += "<label for='target'><b>Extra Auto-Tuning Options:</b></label><br>" +
	"<input type='checkbox' name='autoTune2Sel' value='dataLayoutDef'" +
	" hidden='hidden' checked='checked'>" +
	"<input type='checkbox' name='autoTune2Sel' value='dataLayout' >" +
	"Data layout transformations (SoA/AoS)" +
	/* TODO: Enable later + appropriately adapt gen.files naming conventions 
	"<br>" +
	"<input type='checkbox' name='autoTune2Sel' value='loopCollapse' >" +
	"Loop collapse transformations" +
	"<br>" +
	"<input type='checkbox' name='autoTune2Sel' value='loopInterch'" +
	" disabled='disabled' >" +
	"Loop interchange transformations" +
	*/
	"<br><br>";

    /* TODO: Enable later if we want to provide on-line/server-side version
    str += "<label for='target'><b>Working mode:</b></label><br>" +
        "<input type='radio' onclick='buttonsEnableDisable(0);'" +
	" name='w_mode' value='online' > On-line" +
	"<br>" +
        "<input type='radio' onclick='buttonsEnableDisable(1);' name='w_mode'" +
	" value='offline' > Off-line" +
	"<br><br>";
    */

    str += 
	/* TODO: Enable later if we want to provide on-line/server-side version
	"<input type='button' name='submitButton' value='Create Source'" +
	" disabled='disabled' " + 
	"title='Create source files for the selected options. Source" +
	"files will be available in the output folder.'" +
	"onclick='handleBut(0);'/>" +
	"<br><br>" + 
	"<input type='button' name='submitButton' value='Create Binaries'" +
	" disabled='disabled' " +
	"title='Create binary files for the selected options. Binary" +
	"files will be available in the output folder.'" +
	"onclick='handleBut(1);'/>" +
	"<br><br>"+
	"<input type='button' name='submitButton' value='Generate auto-tune script'" +
	" disabled='disabled' " +
	"script' title='Generate an auto-tune script that compiles, " +
	"executes, and times the implementations resulting from the " +
	"selected options. Auto-tune script will be available in the " +
	"output folder.' onclick='handleBut(2);'/>" +
	"<br><br>" +
	"<input type='button' name='submitButton' value='Auto-tune and time'" +
	" disabled='disabled' " +
	"title='Create binary files for the selected options and " +
	"run an auto-generated auto-tune script that compiles, executes, " +
	"and times the implementations resulting from the selected" +
	"options. Files will be available in the output folder.'" +
	" onclick='handleBut(3);'/>" +
	"<br><br>" +
	*/
        "<input type='button' name='submitButton' value='Generate .glf file'" +
	/* TODO: Enable later if we want to provide on-line/server-side version
	" disabled='disabled' " +
	*/
	"title='Generate source files and auto-tune script and download " +
	"on your computer.'" +
	" onclick='handleBut(4);'/>" +
	"<p>&nbsp</p><hr><p><b>INSTRUCTIONS</b></p>" +
	"<ol><li>Click the \"Generate .glf file\" button above to " +
	"download the <i>sourceCodes.glf</i> file.</li>" +
	"<li><a href=\"http://glaf.cs.vt.edu/glaf_online/splitfiles.pl\"> " +
	"Download the required PERL " +
	"script</a> in the same directory where you downloaded the .glf file." +
	"</li><li>Run the PERL script with the command: " + 
	"<i>perl splitfiles.pl</i></li>" +
	"<li>The following are generated under the \"prog\" sub-directory:</li>" +
	"<ul><li>All code implementations in an appropriate folder structure." +
	"</li><li>A <i>Makefile</i> that can be used to compile all code " +
	"implementations.</li>" +
	"<li>A script (<i>runScript.sh</i>) to execute and measure execution " +
	"time of all code implementations.</li></ul>" +
	"<li>Run the make command from within the \"prog\" sub-directory: " +
	"<i>make</i></li>" +
	"<li>Run the execution and timing script from within the \"prog\"" +
	" sub-directory: <i>sh runScript.sh</i></li>" +
	"<li>View the execution time results by clicking the results.html " +
	"file created in the \"prog\" sub-directory.</li></ol>"
	;

    str += "<tr>" + "<td class='caption'>" +
        "<input type='button' value='Back'" +
	"onclick='changeModule(CurProgObj.curModNum)'>" +
        "</td>";

    menu1.innerHTML = str;
    menu1.className = 'menuTable';

}


//-----------------------------------------------------------------------------
// Responsible for enabling/disabling the action buttons,
// depending on current working mode selected:
// a = 0 (on-line mode/client-server)
// a = 1 (off-line/files downloaded locally)
//-----------------------------------------------------------------------------
function buttonsEnableDisable(a) {
    
    var onlineItems = document.getElementsByName('submitButton');
    if(a==0) {

        for (var i = 0; i < onlineItems.length-1; i++)
            onlineItems[i].disabled = false;
	onlineItems[onlineItems.length-1].disabled = true;

    } else {
	
	for (var i = 0; i < onlineItems.length-1; i++)
	    onlineItems[i].disabled = true;
	onlineItems[onlineItems.length-1].disabled = false;

    }

}


//-----------------------------------------------------------------------------
// Performs validation of selected options in auto-tune menu screen
// Need to select at least: one target platform, language, basic
// auto-tuning option.
//-----------------------------------------------------------------------------
function validateSelections() {

    // Counter for conditions being satisfied (total of 3).
    var satisfied = 0;

    // Target platform selection.
    var targChoice = document.getElementsByName('targSel');
    for (var i = 0; i < targChoice.length; i++) {

        if (targChoice[i].checked) {

	    satisfied++;
	    break;

        }

    }

    // Language selection.
    var langChoice = document.getElementsByName('langSel');
    for (var i = 0; i < langChoice.length; i++) {

        if (langChoice[i].checked) {

	    satisfied++;
	    break;

        }

    }

    // Basic auto-tuning selection.
    var auTuChoice = document.getElementsByName('autoTuneSel');	    
    for (var i = 0; i < auTuChoice.length; i++) {

        if (auTuChoice[i].checked) {

	    satisfied++;
	    break;

        }
    
    }


    if (satisfied == 3)
        return 1;
    else
        return -1;

}


//AT:
function handleBut(option) {

    // Need to check/validate option combinations.
    // e.g., For GPU we only allow OpenCL.
    // e.g., Need to select ONE target platform.
    // e.g., Need to select AT LEAST one target language.
    // e.g., Need to select AT LEAST one basic auto-tuning option.


    // Find the target platform choice and save: CPU:0, MIC:1, GPU:2
    // Only one choice possible (radio button).
    var targChoice = document.getElementsByName('targSel');
    var targChoiceVal;
    for (var i = 0; i < targChoice.length; i++) {

        if (targChoice[i].checked) {
		    
	    switch(targChoice[i].value) {

	        case 'CPU':
		    targChoiceVal = 0;
		    break;
		case 'MIC':
		    targChoiceVal = 1;
		    break;
		case 'GPU':
		    targChoiceVal = 2;

	    }

	    break;

	}

    }

    // Find the language choice(s) and save in array:
    // Fortran:0, C:1, OpenCL:2
    // User may select one or more target languages.
    var langChoice = document.getElementsByName('langSel');
    var langChoiceArr = new Array();
    for (var i = 0; i < langChoice.length; i++) {

        if (langChoice[i].checked) {
		    
	    switch(langChoice[i].value) {

	        case 'Fortran':
		    langChoiceArr.push(0);
		    break;
		case 'C':
		    langChoiceArr.push(1);
		    break;
		case 'OpenCL':
		    langChoiceArr.push(2);

	    }

	}

    }

    // Find the auto-tuning options and save in array:
    // Serial version:0
    // Parallel version (tool generated):1
    // Parallel version (compiler generated):2
    // User may select one or more auto-tuning options.
    var auTuChoice = document.getElementsByName('autoTuneSel');	    
    var auTuChoiceArr = new Array();
    for (var i = 0; i < auTuChoice.length; i++) {

        if (auTuChoice[i].checked) {
		    
	    switch(auTuChoice[i].value) {

	        case 'ser':
		    auTuChoiceArr.push(0);
		    break;
		case 'parTool':
		    auTuChoiceArr.push(1);
		    break;
		case 'parComp':
		    auTuChoiceArr.push(2);
		    
	    }

	}

    }

    // Find the extra auto-tuning options and save in array:
    // Data layout transformations (SoA/AoS):0
    // Loop collapse transformations:1
    // Loop interchange transformations:2
    // User may select one or more auto-tuning options.
    // TODO: Add data validation based on previous choices.
    var auTu2Choice = document.getElementsByName('autoTune2Sel');	    
    var auTu2ChoiceArr = new Array();
    for (var i = 0; i < auTu2Choice.length; i++) {

        if (auTu2Choice[i].checked) {
		    
	    switch(auTu2Choice[i].value) {

		case 'dataLayoutDef':
		    auTu2ChoiceArr.push(-1);
		    break;
		case 'dataLayout':
		    auTu2ChoiceArr.pop();
		    auTu2ChoiceArr.push(0);
		    break;
		case 'loopCollapse':
		    auTu2ChoiceArr.push(1);
		    break;
		case 'loopInterch':
		    auTu2ChoiceArr.push(2);
		    
	    }

	}

    }

    // Debugging:
    //alert("Option (button pressed): " + option +
    //	"\nTarget platform option: " + targChoiceVal + 
    //	"\nLanguage choice options: " + langChoiceArr + 
    //	"\nAuto-tune options: " + auTuChoiceArr +
    //	"\nAuto-tune extra options: " + auTu2ChoiceArr);



    // Proceed to the appropriate actions, depending on the choices:
    // For all button options code-generation will be performed.
    // We are using by convention the following naming scheme:
    // prog_<CPU/MIC/GPU>_<FORTRAN/C/OPENCL>_<SER/PARTOOL/PARCOMP>_<SoA/AoS>
    // plus the appropriate file extension.
    // TODO: Loop collapse transformations and loop interchange transformations
    // are features to add.

    // Code generation needs to be done for all 4 available choices/buttons.
    // TODO: CAUTION: OpenCL needs to be handled differently. Need to
    // revisit the code below!

    var targ = auTuOpts2string(0, targChoiceVal);
    var fileNames = new Array(); // Array to save file names.
    var sourceCodes = new Array(); // Array to save source codes.
    
    for (var i = 0; i < langChoiceArr.length; i++) {

	for (var j = 0; j < auTuChoiceArr.length; j++) {

	    for (var k = 0; k < auTu2ChoiceArr.length; k++) {

	        var lang = auTuOpts2string(1, langChoiceArr[i]);
	        var auTu = auTuOpts2string(2, auTuChoiceArr[j]);
	        var auTu2 = auTuOpts2string(3, auTu2ChoiceArr[k]);

	        fileNames.push("prog_" + targ + "_" + lang + "_" + auTu);
	   
	        if (auTu2ChoiceArr[k] == 0) {
		
		    // Make a copy of the last element.
		    fileNames.push(fileNames.slice(fileNames.length-1));
		    // Create SoA and AoS versions.
		    fileNames[fileNames.length-2] += "_AoS";

	        }

                fileNames[fileNames.length-1] += "_SoA";
		
 
	        // Add appropriate file extensions.
	        if (langChoiceArr[i] == 0) { 

		    fileNames[fileNames.length-1]+= ".f90";

		    // Create source (serial or parallel-tool) for SoA (def.)
		    // else (if parallel-compiler), generate serial only and
		    // compiler will then use this to auto-parallelize.

		    // If user has selected SoA/AoS transformations:
		    if (auTu2ChoiceArr[k] == 0) {

		        fileNames[fileNames.length-2] += ".f90";
			
			if (auTuChoiceArr[j] == 0 || auTuChoiceArr[j] == 1) {
			 
			    var str = showFortranStr(0,auTuChoiceArr[j],0);
   			    sourceCodes.push(str);

			} else {

			    var str = showFortranStr(0,0,0);
			    sourceCodes.push(str);

			}
		    }

		    if (auTuChoiceArr[j] == 0 || auTuChoiceArr[j] == 1) {
			
			var str = showFortranStr(1,auTuChoiceArr[j],0);
		        sourceCodes.push(str);

		    } else {

			var str = showFortranStr(1,0,0);
			sourceCodes.push(str);

		    }

	        } else if (langChoiceArr[i] == 1) {
		    
		    fileNames[fileNames.length-1] += ".c";

		    // Create source (serial or parallel-tool) for SoA (def.)
		    // else (if parallel-compiler), generate serial only and
		    // compiler will then use this to auto-parallelize.

		    if (auTu2ChoiceArr[k] == 0) {

		        fileNames[fileNames.length-2] += ".c";

			if (auTuChoiceArr[j] == 0 || auTuChoiceArr[j] == 1) {

			    var str = showCstr(0,auTuChoiceArr[j],0);
			    sourceCodes.push(str);

			} else {

			    var str = showCstr(0,0,0);
			    sourceCodes.push(str);

			}

		    }

		    if (auTuChoiceArr[j] == 0 || auTuChoiceArr[j] == 1) {

			var str = showCstr(1,auTuChoiceArr[j],0);
		        sourceCodes.push(str);

		    } else {

			var str = showCstr(1,0,0);
			sourceCodes.push(str);

		    }

	        } else if (langChoiceArr[i] == 2)
		    alert("NOT SUPPORTED IN THIS VERSION");
		    // TODO: Add OpenCL file creation/names.

	    }

	}

    }

    //alert(fileNames);


    // TODO: Options 0 to 3 are not used if on-line/server-side version is
    //       disabled (currently this mode is disabled/commented-out).

    // TODO:
    // *) If possible skip the PHP and call CGI only. 

    // 1) Create auto-tuning script, based on ALL choices (static+greedy).
    //    Should TIME results and export results in an xml/txt.
    // 2) Create PHP file for (5) and test.
    // 3) Create a way to import into html the results written from autotuning.

    if(option == 0 || option == 1 || option == 2 || option == 3) {
        // 1)
        // Convert fileNames and sourceCodes arrays to pass them to PHP and POST
        // Called PHP file will create the source files using the corresponding
        // file names. Files will be in folders having the SAME name as the files
        // but WITHOUT the .f90/.c/etc. extension.
        var fileNamesJSON = JSON.stringify(fileNames);
        var sourceCodesJSON = encodeURIComponent(JSON.stringify(sourceCodes));
        var params = "fileNames=" + fileNamesJSON + "&sourceCodes=" + 
                     sourceCodesJSON; 
        var req = false;
        req = new XMLHttpRequest();
        req.open("POST", "saveSources.php", true);
        req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        req.send(params); //Gets executed.
    }

    var pap = new Array(); 

    if(option == 1 || option == 2 || option == 3) {
        // 2)
        // Convert compile script to pass to PHP and POST.
        // Called PHP file will create the compile script using the name under
        // convention and then call a CGI-script that will run it to create the
        // binaries in their appropriate folders, according to the convention.
        pap = createCompileScript(fileNames, langChoiceArr, auTuChoiceArr, auTu2ChoiceArr);
        var compileScript = pap[0];
        alert(compileScript); 
        var compileScriptJSON = JSON.stringify(compileScript);
        params = "compScript=" + compileScriptJSON; 
        req = false;
        req = new XMLHttpRequest();
        req.open("POST", "compileBinaries.php", true);    
        req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        req.send(params); //Gets executed.
    }

/*
    if (option == 2 || option == 3) {
        // 3)
        // Convert auto-tuning script to pass to PHP and POST.
        // Called PHP file will create the auto-tune script using the name under
        // convention.
        var auTuScriptJSON = JSON.stringify();
        params = "auTuScript=" + auTuScriptJSON; 
        req = false;
        req = new XMLHttpRequest();
        req.open("POST", "genAuTuScript.php?auTuScript=" + auTuScriptJSON, true);   
        req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        req.send(params); //Gets executed
}
*/

    if (option == 3) {
        // 4)
        // Will run the generated auto-tune script.
        // Called PHP file will call a CGI-script to run the auto-tune script
    	// created in (3).
    	//alert(pap[1]);
    	var runExScriptJSON = JSON.stringify(pap[1]);
    	params = "runExScript=" + runExScriptJSON;
    	req = false;
    	req = new XMLHttpRequest();
    	req.open("POST", "runAuTuScript.php", false);    
    	// Receive response from php and display it in HTML page.
    	req.onreadystatechange = function() {
	if (req.readyState == 4)
	    if (req.status == 200) {
	        refreshWithResults(req.responseText);
	    }
        }
        req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        req.send(params); //Gets executed.

    }


    if (option == 4) {

	// Filenames and source-codes are available at this point.
	// Binaries are not (and should not since done locally).
	// TODO: Auto-tune script needs to be available.
	// TODO: Auto-tune and time script needs to be available (but not run).

	var savedSingleFile = "";

        var proceed = validateSelections();

        if (proceed == -1) {

	    var msg_str = "You need to select at least: one " +
		      "Target Platform, one Target Language, " +
		      "one Basic Auto-Tuning Option."		

            alert(msg_str);

        } else {


	    for (var i = 0; i < fileNames.length; i++) {

		    savedSingleFile += "FILENAME:" + fileNames[i] + "\n";
		    savedSingleFile += sourceCodes[i];	

	    } 

	    alert(savedSingleFile);

   	    savedSingleFile = encodeURIComponent(savedSingleFile);
	    download2SaveFortran(savedSingleFile, "sourceCodes.glf");
        
        }

    }

}


//-----------------------------------------------------------------------------
// Refreshes the content of the webpage with the results obtained and a back
// button.
// TODO: Extend: Place on side frame, next to the options, not below.
//-----------------------------------------------------------------------------
function refreshWithResults(responseText) {

    var str = "<h2>Results:</h2><br>";
    str += responseText;
    document.getElementById(OutHtmlId).innerHTML += str;

}


//-----------------------------------------------------------------------------
// Given the filenames array create a string
// that will compile the above filenames. Since, they may be parallel or serial
// or serial versions but which need to be compiled with -parallel flag, we
// need the rest information, as well, and use it to identify the above.
//-----------------------------------------------------------------------------
function createCompileScript(fileNames, langChoiceArr, auTuChoiceArr, 
auTu2ChoiceArr) {

    var returnedValue = new Array(); // Contains comp.string + run ex. script
    var compileScript = ""; // Will contain the string for the compile script.
    var compiler = ""; // Will contain the compiler (ICC/IFORT/...)
    var outp_inp = ""; // Wil contain "-o <executable> <input_source>" string.
    var flags = ""; // Wil lcontain the needed flags (for omp, math, etc.)
    var source = ""; // For running "source ..." for compilers.
    var glob_ctr = 0; // Counting all possible combinations (=fileNames.length).
    var exec_filename = ""; // Executable filename.
    var folder_name = ""; // Folder where to store binary/find source.
    var preamble = "source /opt/intel/composer_xe_2013/bin/compilervars.sh" +
		   " intel64\n";

    var runExScript = new Array(); // Contains the run commands for each exec.

    for (var i = 0; i < langChoiceArr.length; i++) {

 	flags = "-O2 "; // Default.

	if (langChoiceArr[i] == 0) {

    	    compiler = "ifort";

	} else if (langChoiceArr[i] == 1) {

   	    compiler = "icc";

	} else if (langChoiceArr[i] == 2) {
		
	    //compiler = ""; //TODO: Add OpenCL.
		
	}

	for (var j = 0; j < auTuChoiceArr.length; j++) {

	    if (auTuChoiceArr[j] == 0) {

		// Nothing.

	    } else if (auTuChoiceArr[j] == 1) {

		flags += "-openmp"; // For openmp (tool-parallelized).

	    } else if (auTuChoiceArr[j] == 2) {

		flags += "-parallel"; // For auto-par (compiler-parallelized).

	    }

	    // TODO: CAUTION: These options are NESTED, i.e., if 0 is selected
	    // then for EACH of SoA/AoS, we need to provide all combinations
	    // with 1 (i.e., loop collapse transformations), and so on.
	    for (var k = 0; k < auTu2ChoiceArr.length; k++) {

		if (auTu2ChoiceArr[0] == 0) {

		    folder_name = fileNames[glob_ctr].replace(/\..*$/, "");
		    exec_filename = folder_name + "_exec";

		    outp_inp = "-o " + exec_filename + " " + 
			       fileNames[glob_ctr];

        	    compileScript += "cd " + folder_name + "\n" + compiler + 
				     " " + flags + " " + outp_inp + "\n" + 
				     "cd ..\n";

		    runExScript.push("./" + folder_name + "/" + exec_filename);


		    glob_ctr++;

		    folder_name = fileNames[glob_ctr].replace(/\..*$/, "");
		    exec_filename = folder_name +  "_exec";

		    outp_inp = "-o " + exec_filename + " " +
			       fileNames[glob_ctr];

        	    compileScript += "cd " + folder_name + "\n" + compiler + 
				     " " + flags + " " + outp_inp + "\n" +
				     "cd ..\n";
		    
		    runExScript.push("./" + folder_name + "/" + exec_filename);

		    glob_ctr++;

		} else {

		    folder_name = fileNames[glob_ctr].replace(/\..*$/, "");
		    exec_filename = folder_name + "_exec";

		    outp_inp = "-o " + exec_filename + " " +
			       fileNames[glob_ctr];

        	    compileScript += "cd " + folder_name + "\n" + compiler + 
				     " " + flags + " " + outp_inp + "\n" +
				     "cd ..\n";

		    runExScript.push("./" + folder_name + "/" + exec_filename);

		    glob_ctr++;

		}

		flags = "-O2 ";

	    } // End of k.

	} // End of j.

    } // End of i. 

    compileScript = preamble + compileScript;

    returnedValue[0] = compileScript;
    returnedValue[1] = runExScript;

    return returnedValue;

}


//-----------------------------------------------------------------------------
// Given the option type (target:0, language:1, auto-tune option:2) and the
// specific option number as passed on mouse-click on button, this function
// returns the corresponding string to be used in the file-name generation
// according to the convention described above.
//-----------------------------------------------------------------------------
function auTuOpts2string(optType, opt) {

    var str;

    if (optType == 0) {

	switch(opt) {

	    case(0):
		str = "CPU";
		break;
	    case(1):
		str = "MIC";
		break;
	    case(2):
		str = "GPU";

	}


    } else if (optType == 1) {

	switch(opt) {

            case(0):
                str = "FORTRAN";
                break;
            case(1):
                str = "C";
                break;
            case(2):
                str = "OPENCL";

        }

    } else if (optType == 2) {

        switch(opt) {

            case(0):
                str = "SER";
                break;
            case(1):
                str = "PARTOOL";
                break;
            case(2):
                str = "PARCOMP";
		break;
	    case(3):
		str = "DATALAYOUT";
		break;
	    case(4):
		str = "COLLAPSE";
		break;
	    case(5):
		str = "LOOPINTCHG";


        }

    }

    return str;

}


//----------------------------------------------------------------------------
// Grid Language identifiers may not always be acceptable to Fortran -- e.g.,
// reserved keywords. One strategy is to prefix every identifier with "ft_" 
// FORTRAN variable names must start with a letter.
//----------------------------------------------------------------------------
function var2Fortran(str) {

    return "ft_" + str;

}


//----------------------------------------------------------------------------
// Method called to get Fortran code for an expression.
// Note: For Fortran77 only, use the commented version for comparators.
//----------------------------------------------------------------------------
function expr2FortranString(e) {

    var ret = "";
    var pre = "";
    var post = "";
    var sep = "";

    if (!e) { // Empty string.

    } else if (e.str == "!=") {

        //ret = ".NE.";
        ret = "/=";

    } else if (e.str == "<") {

        //ret = ".LT.";
        ret = "<";

    } else if (e.str == "<=") {

        //ret = ".LE.";
        ret = "<=";

    } else if (e.str == "==") {

        //ret = ".EQ.";
        ret = "==";

    } else if (e.str == ">=") {

        //ret = ".GE.";
        ret = ">=";

    } else if (e.str == ">") {

        //ret = ".GT.";
        ret = ">";
        
	// TODO: Take care of TWO consecutive such literals (e.g., .NOT., .GT.
        // should be .NOT.GE. and not .NOT..GE.)
	// TODO: Implement NOT/AND/OR.
    } else if (e.str == ".TRUE." || e.str == ".FALSE.") {

	//DC4:
	ret = e.str;

    } else if (e.str == "AND" || e.str == "OR" || e.str == "NOT") {

	//DC10: Fixing AND, OR, NOT (adding '.')
	ret = "." + e.str + ".";

    } else if (e.type == ExprType.Break) {

	ret = "EXIT";

    } else if ((e.isOperator() || e.isNumber() || e.isFormulaKeyword()) &&
        !e.isLetName()) {
        // Covering all other operators, except LT,LE,EQ,NE,GT,GE.

        ret = e.str;

    } else if (e.isString()) { // For taking care of abc string literals.

        ret = "\"" + e.str + "\"";

    } else if (e.isLeaf()) { // Other type of leaf

        // If it is pointing to a grid, append <gridName>_ 
        // to the title name (since this is how we declare them
        // as variables in Fortran program to discern use of
        // same title name in different grids).
        if (e.type == ExprType.Title) {

            ret = e.gO.caption + "_" + e.str;

        } else if (e.isGridRef() && e.gO.typesInDim != -1) {

            // TODO: CAUTION: When does this case occur? Need to take care of 
	    // AoS case!
	    ret = "typvar_" + e.gO.caption;

        } else if (e.gO != null && e.gO.structCaption != null) { //DC3:

	    ret = e.gO.structCaption + "%" + e.gO.caption;

	} else {

	     ret = var2Fortran(e.str);
            
        }

    } else if (e.isGridCell()) { // Any non-scalar grid

        // If the grid has multiple data types we will need to generate
	// special code than for "typical" grids with a global (single) type.
        if (e.gO.typesInDim != -1 /*&& e.gO.numDims > 1*/) {

	   
	    pre = "typvar_" + e.gO.caption;

            var lastd = e.exprArr.length - 1;

            // Need to find title in the dimension where we have titles AND
	    // the dimensions. We don't care about the other titles (we just 
	    // substitute their values in Fortran since declared).
            // We want to find which dimX (as declared by convention in our 
	    // derived Fortran type).
            // CAUTION: e.exprArr[e.gO.typesInDim].str contains 
	    // e.g., d3Tab3, not <gridName_>d3Tab3!
            var titleSrch = e.exprArr[e.gO.typesInDim].exprArr[0].str;
	    var dimChosen = e.gO.dimTitles[e.gO.typesInDim].indexOf(titleSrch);

            if (Soa) {

		pre += "%" + titleSrch + "("; //DC:
                //pre += "%dim" + dimChosen + "("; //DC:

	    } else {
            
		    pre += "(";
	    
	    }

            // Now, reverse row and col index and iterate over them and add 
	    // those remaining,
            // i.e., excluding the dim that was "transformed" into the struct 
	    // containing different data types for each one of its 
	    // "dimensions".

            var tmp_expr_indices = new Array();

            for (var i = 0; i <= lastd; i++) {

                //e.g., row, col, ind3, ...
                tmp_expr_indices[i] = expr2FortranString(e.exprArr[i]) +
                    " + 1";

            }

            var tmp_switch = tmp_expr_indices[ColDimId];
            tmp_expr_indices[ColDimId] = tmp_expr_indices[RowDimId];
            tmp_expr_indices[RowDimId] = tmp_switch;

            // Since we have changed the indices, we have to update 
            // the typesInDim temp variable we'll search against to
            // exclude index from expression.
	    
            var typesInDimAlt;

            if (e.gO.typesInDim == 0) {

                typesInDimAlt = 1;

	    } else if (e.gO.typesInDim == 1) {

                typesInDimAlt = 0;

	    } else {

                typesInDimAlt = e.gO.typesInDim;

	    }

	    var i;
            for (var i = lastd; i >= 1; i--) {

                if (i != typesInDimAlt) {

                    ret += tmp_expr_indices[i];

                    // Do not print comma if types in last dimension.
                    if (!(typesInDimAlt == 0 && i == 1)) {

                        ret += ", ";

		    }

                }

            }

	    // TODO: CAUTION: Stray 'i' (see declaration above
	    // & use in for loop).
            if (i != typesInDimAlt) {

                ret += tmp_expr_indices[0]; 
		// Last dimension in series with no comma 
                // to follow.

            }

            if (Soa) {

                ret += ")";

	    } else {
		    
		ret += ")" + "%" + titleSrch; //DC:
                //ret += ")" + "%dim" + dimChosen; //DC:

	    }

        } else {

	    //DC3:
	    if (e.gO.structCaption == null) {

                pre = var2Fortran(e.gO.caption);
	    
	    } else {

		pre = e.gO.structCaption + "%" + e.gO.caption;

	    }

            // Last dimension. For a scalar, lastd = -1 so none of the 
            // statements below get executed -- i.e., ret = "";
            var lastd = e.exprArr.length - 1;

            if (e.gO.inArgNum >= 0 && e.gO.numDims == 0) { // i.e., scalar arg.

                pre = "fun_" + e.gO.caption; 
		// Replace in formulas with this (newly) declared,so that we 
		// achieve passing by value.

            }


            if (lastd != -1) pre += "("; 

            // TODO: In below, are the names always indX, row, col for the 
            // dimensions? That's what we assumed.
            // NO! IT MAY BE A PARAMETER, e.g. fun_param0 (or constant).
            // TODO: Use dimNameExprs! This is where it is actualy stored. 
            // CAUTION
            // FOR NOW, I just convert whatever the expression and +1.
            // so that fun_param becomes fun_param + 1. Otherwise, if
            // fun_param is found in function will use its value WITHOUT +1.
            // i.e., is there any reason this is not correct and I had 
            // changed it to var orig = ...?
            for (var i = lastd; i >= 2; i--) { // Print higher dims.

                //var orig = expr2FortranString(e.exprArr[i]);
                //ret += orig.replace(var2Fortran("ind"+i), "(" + 
                //var2Fortran("ind"+i) + " + 1), ");
                ret += expr2FortranString(e.exprArr[i]) + " + 1, ";

            }

            // Print row expression because Fortran syntax requires [row][col]
            if (lastd >= 0) { // Print row expr

                //var orig = expr2FortranString(e.exprArr[RowDimId]);
                //ret += orig.replace(var2Fortran("row"), "(" + 
                //		var2Fortran("row") + " + 1)");
                ret += expr2FortranString(e.exprArr[RowDimId]) + " + 1";
            }

            // Print col expression last because Fortran syntax requires 
	    // [][][col]
            if (lastd >= 1) { // print col expr

                //var orig = expr2FortranString(e.exprArr[ColDimId]);
                //ret += ", " + orig.replace(var2Fortran("col"), "(" + 
                //		var2Fortran("col") + " + 1)");
                ret += ", " + expr2FortranString(e.exprArr[ColDimId]) +
                    " + 1";
            
	    }

            if (lastd != -1) ret += ")";
             
        }

    } else { // Compound statement or function.

        if (e.isLibFuncCall()) {

            // TODO: Make a better job organizing/going through libs/functions
            // By using something like "searchLibFunctions(string)" that 
            // searches on libraries that have been loaded or else.
            // Make strings constants and work with replacing codes (__XXX__).
            if (e.str == "FileInput.loadCSVFile" || e.str ==
                "FileOutput.saveCSVFile") {

                var load_or_save = (e.str == "FileInput.loadCSVFile") ? 1 :
                    0;

		// Searching if code for LOAD/SAVE has been previously 
		// generated for this grid.
                // Note: Adding the "(" in search to avoid true for Out1 when 
		// e.g., we only have Out (the indexOf which would return 
		// TRUE).
                var existenceSearch = "";
                if (load_or_save) {

                    existenceSearch = "LOAD_" + e.exprArr[0].gO.caption +
                        "(";

                } else {

                    existenceSearch = "SAVE_" + e.exprArr[0].gO.caption +
                        "(";

                }

                pre = "fileName = \"" + e.exprArr[1].str + "\"\n";
                
                if (load_or_save) {

                    pre += "CALL CSV_FILE_LOAD_" + e.exprArr[0].gO.caption;
                
		} else {

                    pre += "CALL CSV_FILE_SAVE_" + e.exprArr[0].gO.caption;
                
		}

                pre += "(";
                post = ")";
                sep = ",";

                ret = expr2FortranString(e.exprArr[0]) + ", fileName";

		// If code has been previously generated for this grid,
		// no need to re-generate duplicate code. CALLs (as done
		// above in pre, are all is needed).
                if (LibFunctionsCode.indexOf(existenceSearch) == -1) {
                    
		    LibFunctionsCode = LibFunctionsCode.replace(
                        "__COMMONVARS__",
                        "CHARACTER (LEN=128) :: fileName\n");

                    // Generate code for function and place it before any
		    // other code in generated Fortran code.
                    var tmpLibFunctionsCode = FileInput_loadCSV_Fortran;

		    // Create code corresponding to loading or saving, by 
		    // replacing in the generic code template for LOAD/SAVE.
                    if (load_or_save) {

                        tmpLibFunctionsCode = tmpLibFunctionsCode.replace(
                            "__SUBNAME__", "LOAD_" + e.exprArr[0].gO.caption
                        );
                    
		    } else {

                        tmpLibFunctionsCode = tmpLibFunctionsCode.replace(
                            "__SUBNAME__", "SAVE_" + e.exprArr[0].gO.caption
                        );
                    
		    }

                    var tmp = "";

                    // 'If' corresponds to single-type (global) grid
		    // 'else' corresponds to derived-type (struct) with 
		    // multiple data types for a dimension (i.e., the struct):
                    if (e.exprArr[0].gO.typesInDim == -1) {

                        if (load_or_save) {

                            tmp = "READ(22, *) ((testgrid(I,J), J=1," + e
                                .exprArr[0].gO.dimActSize[RowDimId] +
                                "), I=1," + e.exprArr[0].gO.dimActSize[
                                    ColDimId] + ")\n";


                        } else {

                            tmp = "DO I=1," + e.exprArr[0].gO.dimActSize[
                                    RowDimId] +
                                "\nDO J=1," + (e.exprArr[0].gO.dimActSize[
                                    ColDimId] - 1) +
                                "\nWRITE(tmp_char, *) testgrid(I,J)\n" +
                                "WRITE(22, '(A, A)', advance='no') " +
				"TRIM(tmp_char), ','\n" +
                                "END DO\n" +
                                "WRITE(22,*) testgrid(I," + e.exprArr[0].gO
                                .dimActSize[ColDimId] +
                                ")\nEND DO\n";

                        }

                        LibFunctionsCode += tmpLibFunctionsCode.replace(
                            "__TYPE__", getDataTypeString(e.exprArr[0]
                                .gO, e.exprArr[0]));

                        LibFunctionsCode = LibFunctionsCode.replace(
                            "__DIMENSION__", ",DIMENSION(" +
                            getDimensionString(e.exprArr[0].gO) +
                            ") ::");

                        LibFunctionsCode = LibFunctionsCode.replace(
                            "__CSVREADWRITE__", tmp);

                    } else {

                        LibFunctionsCode += tmpLibFunctionsCode.replace(
                            "__TYPE__", "TYPE (TYP_" + e.exprArr[0].gO
                            .caption + ")");
                        
			if (Soa) {

                            LibFunctionsCode = LibFunctionsCode.replace(
                                "__DIMENSION__", "");
                        
			} else {

                            LibFunctionsCode = LibFunctionsCode.replace(
                                "__DIMENSION__", ",DIMENSION(" +
                                getDimensionString(e.exprArr[0].gO) +
                                ") ::");
                        
			}

                        // Two sub-cases: typesInDim == 0 or 1:
                        var allocationstring = "";

                        if (e.exprArr[0].gO.typesInDim == 0) { 
			// If horizontal (row) types.

                            if (load_or_save) {

                                for (var dimX = 0; dimX < e.exprArr[0].gO
                                    .dimActSize[RowDimId]; dimX++) {

                                    if (Soa) {

                                        allocationstring =
                                            "READ (22, *) (testgrid%dim__ID__(J), J=1,__MAXDIM__)\n";
                                    
				    } else {

                                        allocationstring =
                                            "READ (22, *) (testgrid(J)%dim__ID__, J=1,__MAXDIM__)\n";
                                    
				    }

                                    tmp += allocationstring.replace(
                                        "__ID__", dimX);

                                    tmp = tmp.replace("__MAXDIM__", e.exprArr[
                                        0].gO.dimActSize[ColDimId]);

                                }

                            } else {

                                for (var dimX = 0; dimX < e.exprArr[0].gO
                                    .dimActSize[RowDimId]; dimX++) {

                                    if (Soa) {

                                        allocationstring = "DO J=1," + (e
                                                .exprArr[0].gO.dimActSize[
                                                    ColDimId] - 1) +
                                            "\nWRITE(tmp_char, *) testgrid%dim__ID__(J)\n" +
                                            "WRITE(22, '(A, A)', advance='no') TRIM(tmp_char), ','\n" +
                                            "END DO\n" +
                                            "WRITE(22,*) testgrid%dim__ID__(__MAXDIM__)";
                                    
				    } else {
					    
                                        allocationstring = "DO J=1," + (e
                                                .exprArr[0].gO.dimActSize[
                                                    ColDimId] - 1) +
                                            "\nWRITE(tmp_char, *) testgrid(J)%dim__ID__\n" +
                                            "WRITE(22, '(A, A)', advance='no') TRIM(tmp_char), ','\n" +
                                            "END DO\n" +
                                            "WRITE(22,*) testgrid(__MAXDIM__)%dim__ID__";

                                    }

                                    tmp += allocationstring.replace(
                                        /__ID__/g, dimX);

                                    tmp = tmp.replace("__MAXDIM__", e.exprArr[
                                            0].gO.dimActSize[ColDimId]) +
                                        "\n";

                                }

                            }

                        } else if (e.exprArr[0].gO.typesInDim == 1) { 
			// If vertical (col) types.

                            allocationstring = "DO I=1," + e.exprArr[0].gO
                                .dimActSize[RowDimId] + "\n";

                            var separator = "";

                            if (load_or_save) {

                                allocationstring += "READ (22, *) ";

                            } else {

                                allocationstring += "WRITE (22, *) ";
                                separator = "',', "
                            }

                            var I = 0;
                            for (var dimX = 0; dimX < e.exprArr[0].gO.dimActSize[
                                    ColDimId] - 1; dimX++) {

                                if (Soa) {

                                    allocationstring += "testgrid%dim" +
                                    I + "(I), " + separator;

				} else {

                                    allocationstring += "testgrid(I)%dim" +
                                    I + ", " + separator;
				
				}

                                I += 1;

                            }

                            if (Soa){

                                allocationstring += "testgrid%dim" + I +
                                "(I)\n";

			    } else {

                                allocationstring += "testgrid(I)%dim" + I +
                                "\n";
			    
			    }

                            tmp = allocationstring + "END DO\n";

                        }

                        LibFunctionsCode = LibFunctionsCode.replace(
                            "__CSVREADWRITE__", tmp);

                    }

                }

            } else {

                ret = processLibFunctions(e);

            }
            // TODO: Here, put code for handling other LIBRARY function calls.

        } else if (e.isUserFuncCall()) {

            pre = var2Fortran(e.str);
            pre += "(";
            post = ")";
            sep = ",";

            for (var i = 0; i < e.exprArr.length; i++) {

                if (i > 0) ret += sep;
                ret += expr2FortranString(e.exprArr[i]);

            }

        } else {

            sep = " ";

            for (var i = 0; i < e.exprArr.length; i++) {

                if (i > 0) ret += sep;
                ret += expr2FortranString(e.exprArr[i]);

            }
        }

    }

    return pre + ret + post;

}


//----------------------------------------------------------------------------
// Returns the appropriate code for library functions, in Fortran.
// TODO: No type or similar checking performed.
// TODO: CAUTION: ROUND is not supported.
//----------------------------------------------------------------------------
function processLibFunctions(e) {

    var pre = "";
    var ret = "";
    var post = "";
    var sep = ", "

    for (var i = 0; i < e.exprArr.length; i++) {

        if (i > 0) ret += sep;
        ret += expr2FortranString(e.exprArr[i]);

    }
    post = ")";

    if (e.str == "Math.mod") {

        pre = "MOD(";

    } else if (e.str == "Math.cos") {

        pre = "COS(";

    } else if (e.str == "Math.sin") {

        pre = "SIN(";

    } else if (e.str == "Math.tan") {

        pre = "TAN(";

    } else if (e.str == "Math.max") {

        pre = "MAX(";

    } else if (e.str == "Math.min") {

        pre = "MIN(";

    } else if (e.str == "Math.exp") {

        pre = "EXP(";

    } else if (e.str == "Math.log") {

        pre = "LOG(";

    } else if (e.str == "Math.sqrt") {

        pre = "SQRT(";

    } else if (e.str == "Math.abs") {

        pre = "ABS(";

    } else if (e.str == "Math.ceil") {

        pre = "CEILING(";

    } else if (e.str == "Math.floor") {

        pre = "FLOOR(";

    } else if (e.str == "Math.pow") {

        pre = "(";
        ret = expr2FortranString(e.exprArr[0]) + "**" +
            expr2FortranString(e.exprArr[1])
        post = ")";

    } else if (e.str == "System.runCommand") {

        pre = "CALL SYSTEM(";
        ret = ("\'" + e.exprArr[0].str + "\'");

    } else if (e.str == "Math.random") {

        // TODO: Must be a REAL grid cell or a REAL grid name.
        pre = "CALL RANDOM_SEED()\nCALL RANDOM_NUMBER("

    } else if (e.str == "Math.sum") { //DC4:

	pre = "SUM(";
	ret = expr2FortranString(e.exprArr[0]) + "(" + 
	      expr2FortranString(e.exprArr[1]) + ":" +
	       expr2FortranString(e.exprArr[2]) + ")";

    } else if (e.str == "FileInput.loadCSRFile") {
	
	//DC99:
	// Build appropriate function call (call + arguments).
	pre = "CALL readCSR(";
	ret = expr2FortranString(e.exprArr[0]) + ", " +
	      expr2FortranString(e.exprArr[1]) + ", " +
	      expr2FortranString(e.exprArr[2]) + ", " +
	      expr2FortranString(e.exprArr[3]) + ", " +
	      expr2FortranString(e.exprArr[4]) + ", " +
	      expr2FortranString(e.exprArr[5]) + ", " +
	      expr2FortranString(e.exprArr[6]) + ", " +
	      expr2FortranString(e.exprArr[7]) + ", " +
	      expr2FortranString(e.exprArr[8]) + ", " +
	      expr2FortranString(e.exprArr[9]);
	post = ")";

	if (LibFunctionsCode.indexOf("readCSR") == -1) {
                    
	    LibFunctionsCode = LibFunctionsCode.replace(
             	               "__COMMONVARS__","");

	    LibFunctionsCode += LibLoadCSR_F;

	}

    }

    return pre + ret + post;

}


//----------------------------------------------------------------------------
// Save the Fortran string generated for the program.
// parallel: save parallel version if 1. Serial if 0.
// show: To be passed in showFortranStr to alert(code) if 1.
// 	 Else, just to return the code string to caller.
//----------------------------------------------------------------------------
function saveFortranStr(parallel, show) {

    // Get the JSON string.
    // Regenerate code every time, otherwise may save old one if updated.
    TypesAllFuncs = new Array();
    NamesAllFuncs = new Array();
    var str = encodeURIComponent(showFortranStr(1, parallel, show)); 
    createAPI(1); //API:
    // TODO: Is SoA by default (hence '1' as first argument).
    download2SaveFortran(str, CurProgObj.fileName);

}


//----------------------------------------------------------------------------
// Download a file to save as Fortran
// TODO: Save as .f90 file.
//----------------------------------------------------------------------------
function download2SaveFortran(str, filename) {

    // Get the anchor element reserved for saving and change its attributs.   
    var elem = document.getElementById('saveLink');
    elem.setAttribute('href', 'data:application/octet-stream,' + str);
    elem.download = filename; //setAttribute('download', 'myfile.txt');
    // Clic the link so that it will start downloading (saving).
    elem.click();

}


//----------------------------------------------------------------------------
// Show the Fortran string generated for the program.
// strOfArr: Use structures of arrays (SoA) if 1. Arrays of structures (AoS)
// 	     if 0.
// show: Show code in JS (using alert) if 1. Just return code string if 0.
//----------------------------------------------------------------------------
function showFortranStr(strOfArr, parallel, show) {

    Soa = strOfArr;
    ShowParallel = parallel;

    // If parallel code generation selected first we need to analyze.
    if (ShowParallel) {

        var mO = CurModObj; // TODO: Generalize for multiple modules.
        var fO = mO.allFuncs[getFuncIdByName(mO, "Main")];
        analyzeParallelismAll(fO, 0);
    
    }

    TypesAllFuncs = new Array();
    NamesAllFuncs = new Array();

    var code = getFortranStr();

    //DC4: Removing "ft_" from resulting code (done only when we are in code
    //     integration mode).
    //DC6: Updated
    if (CodeIntegrationMode) {

        code = code.replace( /( |\t|\n|\(|,|:)ft_/mg, "$1");

    }


    if (show) {

        alert(code);

    } else {

        return code;
    }

}


//----------------------------------------------------------------------------
// Returns Fortran for the current step in current funtion that the
// user is currently working on.
//----------------------------------------------------------------------------
function getFortranStr() {

    var mO = CurModObj;

    // First, generate code for all functions, including 'main()'
    // Will be elements of the func_code array.
    var func_code = new Array(); // Used to store code for EACH function
    
    TypeStr = ""; // Used to store TYPEs (i.e., structures).
    
    // All library code (e.g., readCSV) will be contained in a single 
    // library functions' module.
    // In this module all TYPEs and common (global) variables will
    // be USEd (if any).
    LibFunctionsCode = "MODULE lib_module\n__USETYPES__" +
        "__COMMONVARS__" +
        "CONTAINS\n";

    // A single string that contains all function code.
    var func_code_all = "";

    // TODO: Be careful with global variables. If not initialized every time, 
    // they'll hold the value, until exiting program!
    GID_function = 0;

    for (var f = mO.FuncStartID; f < mO.allFuncs.length; f++) {

	if (ShowParallel && !funcContainsParStep(mO, f) && !FuncHasSerVer[f])
    	    CalledFromSer = 1;

        func_code[f] = getFortranStr4Func(mO, f);
        
	if (ShowParallel)
 	    CalledFromSer = 0;

	//DC5: Type (if a function) is included in func_code[f].
	func_code_all += func_code[f];

	// If we are in ShowParallel, we may need a serial only version (in
	// case of a function that contains (even a single) parallel step(s).
	//
	if (ShowParallel && FuncHasSerVer[f]) {
 
	    // We need to change: 
	    // (a) Header - append "_ser".
	    // (b) Remove all OpenMP pragma directives (single lines in C).
	    
	    CalledFromSer = 1;
	    var altSerFunc = getFortranStr4Func(mO, f);
	    //altSerFunc = altSerFunc.replace("(", "_ser(");
	    // Regex: From the start of a line match as less as possible 
	    // before finding "#pragma omp" (this includes tabs), then match
	    // as less as possible until finding a newline. Do this for EACH
	    // line of the string (m specifier).
	    var funname = var2Fortran(mO.allFuncs[f].funcCallExpr.str);
	    // In previous versions there was a bug when a called function had
	    // the caller name as a prefix (manually named via the GUI).
	    var funname2 = funname + '\\(';
	    var funnameRegx = new RegExp(funname2,"g");
	    altSerFunc = altSerFunc.replace(funnameRegx, funname + "_ser(");
	    altSerFunc = altSerFunc.replace(/^.*?!\$OMP.*\n?/mg, "");

	    //DC5: Type (if a function) is included in func_code[f]
	    func_code_all += altSerFunc;
	
	    CalledFromSer = 0;

	}

    }

    // Generate code for main method.
    var main_call = "\nPROGRAM prog_main\n";

    main_call += addIndentation(0) + "USE funcs_module\n";
    main_call += addIndentation(0) + "IMPLICIT NONE\n";
    main_call += addIndentation(0) + "INTEGER, DIMENSION(4,1) :: " +
        CurModObj.allFuncs[DefMainFuncInd].allGrids[1].caption + "\n";

    // TODO: We assume main returns int as a return code (like C).
    main_call += addIndentation(0) + "INTEGER :: " +
        var2Fortran(CurModObj.allFuncs[DefMainFuncInd].allGrids[0].caption) +
        "\n";

    // If generating parallel implementation, setting nested parallelism off
    // by default.
    // TODO: This may be an option for the auto-tuner.
    if (ShowParallel) {
	    
        main_call += addIndentation(0) + "CALL OMP_SET_NESTED(.FALSE.)\n";

    }

    main_call += addIndentation(0) +
        var2Fortran(CurModObj.allFuncs[DefMainFuncInd].allGrids[0].caption) +
        " = " + var2Fortran(DefMainFuncName) +
        "(" + CurModObj.allFuncs[DefMainFuncInd].allGrids[1].caption +
        ")\n" + addIndentation(0) + "STOP\n";

    main_call += "END\n";


    // If library code (e.g., TYPES) used, finalize module USEing it, 
    // else use empty string.
    if (LibFunctionsCode.indexOf("__COMMONVARS__") == -1) {

        LibFunctionsCode += "END MODULE\n\n";

        //TODO: May be the case that the grid read is NOT of struct type. 
	//	DOwn the road use ONLY clause of USE, to only use types 
	//	that are ACTUALLY used in the library module
        //	Or even better nest USE in EACH subroutine of the library 
	//	functions module.
        LibFunctionsCode = (TypeStr == "") ? LibFunctionsCode.replace(
            "__USETYPES__", "") : LibFunctionsCode.replace(
            "__USETYPES__", "USE types_module\n");

    } else {

        LibFunctionsCode = "";

    }

    // Create code for module to include TYPEs (if any).
    TypeStr = (TypeStr == "") ? TypeStr : "MODULE types_module\n" +
        TypeStr + "END MODULE\n\n";

    // Create module that will contain ALL functions used in the program
    // (as stored in func_code_all), and including Main function.
    var funcs_module = "MODULE funcs_module\n" + (ShowParallel ?
            "USE OMP_LIB\n" : "") + "__USEMODULES__" + "IMPLICIT NONE\n" +
        "CONTAINS\n\n";
    funcs_module += func_code_all + "END MODULE\n";

    // Final generated code will contain the TYPEs code, the library functions
    // code (e.g., for read/write CSV), the functions' module (that contains
    // all functions, including Main), and the PROGRAM MAIN that calls
    // Main function and subsequently any other functions.
    var returnedCode = TypeStr + LibFunctionsCode + funcs_module +
        main_call;

    // Replace in ALL function code (that contains by default the 
    // __USEMODULES__ string), "USE lib_module" and/or "USE types_module"
    // depending on whether either or both are empty or not.
    // TODO: This is not optimal. Will eventually need to USE ONLY the modules
    // that are used and IF they are used.
    returnedCode = (TypeStr == "") ? returnedCode : returnedCode.replace(
        /__USEMODULES__/g, "USE types_module\n__USEMODULES__");
    
    returnedCode = (LibFunctionsCode != "") ? returnedCode.replace(
        /__USEMODULES__/g, "USE lib_module\n") : returnedCode.replace(
        /__USEMODULES__/g, "");

    returnedCode = returnedCode.replace(/UNIQUEIND/g, "INTEGER"); //TT
    
    return returnedCode;

}


//----------------------------------------------------------------------------
// Returns integer code for string of data type.
// TODO: When supporting all Fortran types, revise this with new types.
//----------------------------------------------------------------------------
function dataTypeStrToInt(dt_string) {

    if (dt_string === "INTEGER")
        return 0;
    else if (dt_string === "REAL*8")
        return 1;
    else if (dt_string === "STRING")
        return 2;
    else if (dt_string === "LOGICAL")
        return 3;
    else if (dt_string === "CHARACTER")
        return 4;
    else if (dt_string === "REAL")
        return 5;
    else if (dt_string === "VOID") //DC5:
	return 8;
    else
        alert("TYPE NOT YET SUPPORTED");

}


//----------------------------------------------------------------------------
// Returns data type string for given integer code.
// TODO: When supporting all Fortran types, revise this with new types.
//----------------------------------------------------------------------------
function dataTypeIntToStr(typecode) {

    switch (typecode) {

        case 0:
            return "INTEGER";
            break;
        case 1:
            return "REAL*8";
            break;
        case 2:
            return "STRING (LEN=128)"; 
	    //TODO: No such type in Fortran. Use alternative.
            break;
        case 3:
            return "LOGICAL";
            break;
        case 4:
            return "CHARACTER";
            break;
        case 5:
            //alert("Data type: real4 not supported in Fortran");
	    return "REAL";
            //TODO: Support it.
            break;
        case 6:
            //alert("Data type: const not supported in Fortran"); //TODO:
	    return "INTEGER"; //TT TODO:CAUTION (how it interacts w/ rest).
            break;
        case 7:
            alert("Data type: func not supported in Fortran"); //TODO:	
            break;
	case 8: //DC5:
	    return "VOID";
	    break;
        default:
            alert("Invalid input as data type");

    }

}


//----------------------------------------------------------------------------
// Returns Fortran code for a single function.
//----------------------------------------------------------------------------
function getFortranStr4Func(mO, f) {

    var fO = mO.allFuncs[f];
    
    
    // Initialize current step numbering for current function to zero.
    CurStep = 0;
    
    // Initialize GridsInFunc.
    GridsInFunc = new Array();
    Loop_var_per_dim = new Array();
    Index_end_per_dim = new Array();

    if (var2Fortran(fO.funcCallExpr.str) == "ft_Main") {
	    
        // Add an INTEGER as main's return value (TypesAllFuncs[0]).
        TypesAllFuncs.push("INTEGER");
        GID_function = 1; //NEXT one

    } else {
        
        for (var i = 0; i < TypesAllFuncs.length; i++) {

            if (NamesAllFuncs[i] == var2Fortran(fO.funcCallExpr.str)) {
                
		//alert("FOUND in i=" + i + "Callee="+NamesAllFuncs[i]  + 
                //"type="+TypesAllFuncs[i]);
                //TODO: (check) Add a dummy type entry for function 
                //return value. Will be updated later 
                //(TypesAllFuncs[gID]).
                TypesAllFuncs.push(TypesAllFuncs[i]);
            
	    }

        }

    }

    // Used for declaring functions as variables (needed in Fortran). 
    // Initialize to blank at the start of each new function.
    Func_decl = "";

    // See their declaration (global scope) for details on below:
    Row_col_decl = "";
    Index_end_decl = "";
    Grids_new_decl = "";
    TitleDefs_decl = "";
    AllocFreePerFunc = "";

    //DC5: Get returnValue data type. If void ==> subroutine, else function
    var funcType = getDataTypeString(fO.allGrids[0], null); 
    var func_head;
    if (fO.allGrids[0].dataTypes[0] == DataTypes.Void) {

	func_head = "SUBROUTINE " +  var2Fortran(fO.funcCallExpr.str) + "(";

    } else {

	func_head = funcType + " FUNCTION " + 
		    var2Fortran(fO.funcCallExpr.str) + "(";

    }

    // Used to store all the arguments to the function, which in Fortran 
    // have to be declared explicitly.
    var func_args_decl = "";
	
	// In func_vars we declare the type and name of any grids that were
    // passed as parameters in the current function for which we are 
    // generating code.
    var func_vars = "";

    var func_val_init = "";

    // Add argument list to function header. To do that, go through ALL
    // grids in the function and add thos who are incoming args.
    for (var g = 0; g < fO.allGrids.length; g++) {

        var gO = fO.allGrids[g]; 

        // TODO: If a grid with specific indices e.g. array[3][1] treat as 
        // passed by value!
        if (gO.inArgNum >= 0) { // This grid is an incoming arg.

            if (gO.inArgNum > 0) func_head += ","; // arg separator.

            if (gO.numDims > 1 && gO.typesInDim != -1) {

                //TODO: CAUTION: This is for the case of TYPE variable
                //	passed using its name (i.e., no specific element).

	        func_head += "typvar_" + gO.caption;


            } else {

                func_head += var2Fortran(gO.caption); 
		// Grid caption as arg name.

            }

	    // For non-scalar grids. (also0 applies to 1D arrays).
            if (gO.numDims > 0) { 

                if (var2Fortran(fO.funcCallExpr.str) != "ft_Main") {

                    var datatype = getDataTypeString(gO, null);

                    if (datatype.indexOf("TYPE") != -1) {

			// TODO: CAUTION: Do I need to discern SoA/AoS?
			var tmp1 = findTypeVarType(gO,0);
			var tmp2 = tmp1.replace(/typvar_(.*)\(.*\)/, "typvar_$1");
                        func_args_decl += addIndentation(0) + tmp2;
                    
		    } else {

                        // We use assumed array sizes, so that we cover the
                        // general case including the allocatable grids.
                        // In this case we use ':' for each dimension, in
			// DIMENSIONS().
			var assumed_size = "";
                        for (var t = 0; t < getDimensionString(gO).split(
                                ",").length - 1; t++) {

                            assumed_size += ":,";
                        
			}

                        assumed_size += ":";

                        func_args_decl += addIndentation(0) + datatype +
                            ", DIMENSION(" +
                            assumed_size + 
                            ") :: " +
                            var2Fortran(gO.caption) + "\n";

                    }

                } else {
		// If this is the Main function, it has a default argument.

		    // TODO: Change if needed. Right now, for main's arguments
		    // assumption is that the startup args are integers, of 
                    // dims(4,1).
                    func_args_decl += addIndentation(0) +
                    "INTEGER, DIMENSION(4,1) :: " +
                    var2Fortran(gO.caption) + "\n";
		}

            } else {

                // TODO: Here getDataTypeString(gO) for scalars returns
                // undefined
                func_args_decl += addIndentation(0) + getDataTypeString(
                        gO, null) + " :: " +
                    var2Fortran(gO.caption) + "\n";
                // TODO: Use commas to separate.
                // For scalars no need to display DIMENSIONS().
				
		// For non-scalar grids, we do nothing (passed by reference).
            	// For scalar-grids, we have to copy the src value into a temp
	   	// variable called fun_<src_var_name>.
				
		// Initialization occurs as a separate step, to avoid SAVE 
		// semasiology across function calls (when declaring and 
		// init'ing at the same time 
    		// in Fortran - SEE Fortran language SAVE semantics).
    		// TODO: Can fuse with earlier similar loop for function 
		// header.
		func_vars += addIndentation(0) + getDataTypeString(gO,
                        null) + " :: " + "fun_" +
                    gO.caption + "\n";

                func_val_init += addIndentation(0) + "fun_" + gO.caption +
                    " = " + var2Fortran(gO.caption) + "\n";

            }

        }
    }
    
    func_head += ")\n";

    // At this point we have completed in func_head the function header
    // that contains the type of function, the function name, and its
    // arguments contained in parentheses.


    // Temp return value, will be assigned to func name before exiting 
    // function.
    //
    // Used for declaration of the ret value declaration within the func.
    // This is always in position 0 in fO.allGrids[].
    //DC5: No need to declare return value in subroutines (void).
    if (fO.allGrids[0].dataTypes[0] != DataTypes.Void) {

        func_vars += addIndentation(0) + getDataTypeString(fO.allGrids[0],
            null) + " :: " +
            var2Fortran(fO.allGrids[0].caption) + "\n";

    }

    // STEP: Code for each step in the function.
    //
    var step_code = "";
    //
    var stepStart = 1; // Note: Function prototype at step 0. 
    //
    for (var s = stepStart; s < fO.allSteps.length; s++) {

        step_code += getFortranStr4Step(fO, fO.allSteps[s], mO);

    }

    // The calculated value needs to be returned via the function name in 
    // Fortran.
    // TODO: This will be printed always, even if there may be another
    // return statement immediately preceding it. But, will not be
    // semanticaly or syntactically wrong. This is to capture the case
    // that the user does NOT use ANY return statement. UNLESS, we OR
    // leave to the compiler to warn the user that he HAS to use at
    // least one RETURN (or say that there is no need for RETURN as 
    // last_if_indent statement/step if not within branch, etc.)
    //DC5: Do NOT print if VOID, i.e., subroutine.
    var ret_val_assignment = ""; 
    if (fO.allGrids[0].dataTypes[0] != DataTypes.Void) {

        ret_val_assignment = addIndentation(0) + 
	    var2Fortran(fO.funcCallExpr.str) +
            " = " + var2Fortran(fO.allGrids[0].caption) + "\n";

    }

    // Construct the final function string that contains:
    // a) the function header (type + name + arguments).
    // b) arguments' declaration (type + name).
    // c) temp scalar variables (to keep 'pass by value' semantics for
    //    scalar arguments).
    // d) row, col, indX (loop indices) - no redefinition across steps.   
    // e) end0, end1, etc. (loop end variables) - no redefinition across steps
    // f) title definitions.
    // g) new grid declarations (i.e., not passed as arguments).
    // h) Initialization of scalar variables that store scalar function
    //    arguments (fun_<scalar_arg_name>).
    // i) computation code for all steps of this function.
    // j) return value assignment to function name.
    // k) end function statement.
    var function_string = func_head;
    function_string += addUSEmodules(stepStart, fO, mO); //NS:
    function_string += addCommonBlocks(stepStart, fO, mO); //NS2:
    function_string += func_args_decl + func_vars + 
        Row_col_decl + Index_end_decl + TitleDefs_decl + Grids_new_decl +
        func_val_init + step_code + AllocFreePerFunc;
    function_string += ret_val_assignment; //DC5: Removed rest from here
    function_string += addIndentation(0) + "RETURN\n" +
        "END\n\n"; //DC5: Removed FUNCTION (to support subroutines too).


    return function_string;

}


//NS:
function addUSEmodules(stepStart, fO, mO) {

    var usedModulesString = "";

    // Create array to hold all USEd modules
    var usedModules = new Array();

    for (var s = stepStart; s < fO.allSteps.length; s++) {

        var sO = fO.allSteps[s];

	for (var g = 0; g < sO.allGridIds.length; g++) {

	    var gId = sO.allGridIds[g];

	    var gO = fO.allGrids[gId];

	    if (gO.globalRefId != -1 && gO.inExternalMod) {

	        // If not yet added, add it to array
		if (usedModules.indexOf(gO.nameExternalMod) == -1)
		    usedModules.push(gO.nameExternalMod);

	    }

	}

    }

    // Go through the USEd modules array and create string USE...
    for (var i = 0; i < usedModules.length; i++) {

	usedModulesString += addIndentation(0) + "USE " +
			     usedModules[i] + "\n";

    }

    return usedModulesString;

}


//NS2:
// TODO: Derived TYPEs not supported in common blocks yet.
function addCommonBlocks(stepStart, fO, mO) {

    var declCOMMONstring1 = "";
    var declCOMMONstring2 = "";

    // Create array to hold all declared COMMON blocks.
    // This is to avoid duplicate declarations, as once a var in a common 
    // block is found ALL of them in a named block need to be declared 
    // appropriately.
    var declaredCommons = new Array();

    for (var s = stepStart; s < fO.allSteps.length; s++) {

        var sO = fO.allSteps[s];

	for (var g = 0; g < sO.allGridIds.length; g++) {

	    var gId = sO.allGridIds[g];

	    var gO = fO.allGrids[gId];

	    // Only if it is a grid belonging to COMMON block, that has NOT 
	    // been declared already.
	    if (gO.globalRefId != -1 && gO.isCommon && 
		declaredCommons.indexOf(gO.nameCommon) == -1) {

		var varsInCurrentCommon = "";

		declaredCommons.push(gO.nameCommon);

		// Add ALL grids in global scope that share the same common 
		// block name.

		// Loop through all grids in global scope
		for (var gg=0; gg < mO.globalFunc.allGrids.length; gg++) {
		
		    var ggO = mO.globalFunc.allGrids[gg];

		    // Take into account only those who are isCommon
		    if (ggO.isCommon && ggO.nameCommon == gO.nameCommon) {

	                declCOMMONstring1 += addIndentation(0) + 
					     getDataTypeString(gO, null) + 
					     " :: " + var2Fortran(ggO.caption) + "\n";

		
			var ddims = "";
			if (ggO.numDims > 0) ddims = "(" + getDimensionString(ggO) +")";
			
			varsInCurrentCommon += var2Fortran(ggO.caption) + ddims + ", ";

		    }

		}

		// Remove last occur. of ',' from varsInCurrentCommon!
		varsInCurrentCommon = varsInCurrentCommon.substring(0, varsInCurrentCommon.length -2);

		declCOMMONstring2 += addIndentation(0) + "COMMON /" + 
				gO.nameCommon + "/ " +
				varsInCurrentCommon + "\n";

	    }

	}

    }

    return declCOMMONstring1 + declCOMMONstring2;

}


//----------------------------------------------------------------------------
// Returns data type from TypesArray[] as string for a given grid.
// TODO: Special care is needed for multiple data types when titles
// are present (when multiple data types for rows/columns).
// TODO: CAUTION: wrong for multidimensional, but correct. Only called from
// 				  where gO.typesInDIm == -1? If so, fix.
// TODO: Added parameter 'e' for when we have an expression and want to find
// 	the data type from the appropriate dimension for multidimensional
//	grids with multiple data types for the dimension which has types.
//	If e = null AND gO.typesInDim == -1 then it is a scalar grid (e.g., 
//	return value) OR non-scalar grid reference.
//	Otherwise, it is EITHER a gridcell OR a typevar reference.
//----------------------------------------------------------------------------
function getDataTypeString(gO, e) {

    // Where to search for dataType (if global, search in position 0).
    var typeDim;
    if (gO.typesInDim == -1) {

        typeDim = 0;

    } else {

        if (e != null) {

            // Which index's value should we get the value from (e.g., d3Tab2):
            var indx = expr2FortranString(e.exprArr[gO.typesInDim]);
            
	    // Find what integer value this corresponds to (by looking at 
	    // TitleDefs_decl, where the values for each title are defined).
            // First, return assignment (e.g., d3Tab2=2) in numberString:
            var regex = new RegExp(indx + "=[0-9]*");
            var numberString = TitleDefs_decl.match(regex);
            // Then get the part of string after the = (i.e., the number):
            var numberss = (numberString.toString()).substring(
                numberString.toString().indexOf("=") + 1);

            // Make integer number from string:
            typeDim = parseInt(numberss);

        } else { 
		
	    // In this case typesInDim != -1 and e == null (numDims > 1 will
	    // be in the only valid cases).
            // And only occurs when a TYPE grid is passed to a function (using
	    // only its name).

            //alert("Passing a TYPE grid");

            return findTypeVarType(gO, 1);

        }
    }

    if (gO.dataTypes[typeDim] === undefined) {

        alert("THIS SHOULDN'T OCCUR");

    } else {

        switch (gO.dataTypes[typeDim]) {

            case 0:
                return "INTEGER";
                break;
            case 1:
                return "REAL*8";
                break;
            case 2:
                return "STRING"; 
		//TODO: CAUTION: No such type in Fortran. Use 
                //alternative. [STRING]
                break;
            case 3:
                return "LOGICAL";
                break;
            case 4:
                return "CHARACTER";
                break;
            case 5:
                //alert("Data type: real4 not supported in Fortran"); //TODO:
                return "REAL";
		break;
            case 6:
                //alert("Data type: const not supported in Fortran"); //TODO:
	        return "UNIQUEIND"; //TT	
                break;
            case 7:
                alert("Data type: func not supported in Fortran"); //TODO: 
                break;
	    case 8: //DC5:
		return "VOID";
		break;
            default:
                alert("Invalid input as data type");

        }

    }

}


//----------------------------------------------------------------------------
// Returns dimensions of a grid as a string.
// This information is used in declaring variables' DIMENSION().
// Takes care of multiple data type grids (to construct TYPE).
// TODO: Scalars are declared without the DIMENSION() keyword, so no scalar
// gO should be called with getDimensionString().
//----------------------------------------------------------------------------
function getDimensionString(gO) {

    //TODO: Be careful of dimDynSize.
    var modarr = gO.dimActSize.slice(0); 

    modarr[0] = gO.dimDynSize[ColDimId] ? var2Fortran(gO.dimDynSize[ColDimId]) : gO.dimActSize[
        ColDimId];
    modarr[1] = gO.dimDynSize[RowDimId] ? var2Fortran(gO.dimDynSize[RowDimId]) : gO.dimActSize[
        RowDimId];

    for (var i = modarr.length - 1; i >= 2; i--) {

        modarr[i] = gO.dimDynSize[i] ? var2Fortran(gO.dimDynSize[i]) : gO.dimActSize[i];

    }

    var dimensions_string = "";

    if (gO.numDims == 0) alert("SCALAR: SHOULD NOT OCCUR");

    // Since we have changed the indices, we have to update 
    // the typesInDim temp variable we'll search against to
    // exclude index from declared dimensions.
    var typesInDimAlt;
    if (gO.typesInDim == 0) {

        typesInDimAlt = 1;

    } else if (gO.typesInDim == 1) {

        typesInDimAlt = 0;

    } else {

        typesInDimAlt = gO.typesInDim;

    }

    var i;

    for (i = modarr.length - 1; i >= 1; i--) {

        if (i != typesInDimAlt) {

            dimensions_string += modarr[i];

            // Do not print comma if types in last dimension or if last 
	    // dimension is 1 (1D).
            if (!(typesInDimAlt == 0 && i == 1) && modarr[0] != 1)
                dimensions_string += ", ";
        }

    }

    if (i != typesInDimAlt) {

        if (modarr[0] != 1) // If 1D (last dimension is 1) do not print it.
            dimensions_string += modarr[0]; 
	// Last dimension in series with no comma to follow.

    }

    return dimensions_string;

}


//----------------------------------------------------------------------------
// Returns 1 if grid has been declared in current function.
// Since parsing occurs sequentially, we do not store info 
// for all functions, we reset the list at the start of every function.
// Info stored in global array: GridsInFunc.
//----------------------------------------------------------------------------
function gridDeclaredInFunc(gId) {

    for (var i = 0; i < GridsInFunc.length; i++) {

        if (GridsInFunc[i] == gId)
            return 1;

    }

    return 0;

}


//----------------------------------------------------------------------------
// Returns the string to create the appropriate TYPEs in Fortran for grids
// that need one (i.e., the ones that have multiple types per dimension).
//----------------------------------------------------------------------------
function createTypeString(gO) {

    var gridDefinition = "";

    if (gO.numDims == 0) {

        gridDefinition = getDataTypeString(gO, null) + " :: " +
            var2Fortran(gO.caption) + "\n";
        // For scalars no need to display DIMENSIONS()

    } else if (gO.numDims == 1 && gO.typesInDim == -1) {
	// TODO	
        // 1D cannot have different data types, UNLESS a derived TYPE var.
        // This also applies to 1D (vectors),in which case will be: 
	// DIMENSIONS(1,X).
        // This is a grid AND is NOT an incoming arg. 
        gridDefinition = getDataTypeString(gO, null) + ", DIMENSION(" +
            getDimensionString(gO) + ") :: " +
            var2Fortran(gO.caption) + "\n";

    } else { // gO.numDims > 1

	// 'If' refers to the case where we have a TYPE.    
        if (gO.typesInDim != -1) {

            gridDefinition = findTypeVarType(gO, 0);

        } else {
	// 'Else' case refers to a "typical" grid.

            gridDefinition = getDataTypeString(gO, null) + ", DIMENSION(" +
                getDimensionString(gO) + ") :: " +
                var2Fortran(gO.caption) + "\n";

        }

    }

    return gridDefinition;

}


//----------------------------------------------------------------------------
// Returns Fortran code for a given step in a given function.
//----------------------------------------------------------------------------
function getFortranStr4Step(fO, sO, mO) {

    var grids = "";
    var titleDefs = "";
    Struct_el_decl = "";
    
    // Used to keep track of which DO loops are parallel, so we
    // can close them appropriately in the reverse order.
    var stepOmpDoStack = new Array();

    // Increase step ID (within a function- across functions this is 
    // re-initialized to zero).
    CurStep++;

    var allocatablesOfStep = "";

    var funcID = getFuncIdByName(mO, fO.funcCallExpr.str)

    // Go through all grids in the step and declare as needed fields.
    for (var g = 0; g < sO.allGridIds.length; g++) {

        var gId = sO.allGridIds[g];
        var gO = fO.allGrids[gId];

        if ((gO.inArgNum < 0) && (!gO.isRetVal) && (gO.globalRefId == -1)) { //NS: last check

            // If grid has been already declared within THIS function, 
            // do not re-declare.
            if (!gridDeclaredInFunc(gId)) {

                var newgrid = createTypeString(gO); 
		// Get the declaration of current grid.
                var isAllocGrid = 0;

                // Get what is inside the parentheses (i.e., the dimensions).
		// Can be null if it is a scalar grid.
		var dimensions;
		if (newgrid.indexOf("TYP_") != -1) {

		    dimensions = newgrid.match(/typvar_.+?\((.+)\)/);
		

		} else {
                   
		    dimensions = newgrid.match(/\((.+)\)/);

		}

                if (dimensions != null) {

		    dimensions[0] = dimensions[0].replace(/typvar_.+\(/, "(");
		    newgrid = newgrid.replace(dimensions[0], "");

                    var split_dims = dimensions[1].replace(/ /g, "");
                    split_dims = split_dims.split(",");

                    var colons = ""; 
		    // The colons to replace the dimensions in non-scalar grid
		    // declaration (we use assumed sizes in declarations,
		    // where instead of the dimension, we use ':' for each
		    // dimension).

                    for (var k = 0; k < split_dims.length; k++) {

                        colons += ":,"; // Increase by one for each dimension

			// If dimension is not a number, this means we may be 
			// using
			// a variable as a dimension (i.e., ALLOCATABLE).
                        if (isNaN(split_dims[k])) {

                            // Only exception if we have a TYPE declared which
			    // is NOT allocatable.
                            // TODO: If we decide to allow tables of structs 
			    // later this will need to be reconsidered.
                            if (split_dims[k].indexOf("TYP_") == -1 &&
                                split_dims[k].indexOf("DIMENSION") == -1) {

                                isAllocGrid = 1;
                                //split_dims[k] = var2Fortran(split_dims[k]);

                            }

                        }

                    }


		    var tmp_dim_alloc = "";
		    for(var i = 0; i < split_dims.length - 1; i++)
		        tmp_dim_alloc += split_dims[i] + ",";
			
		    tmp_dim_alloc += split_dims[split_dims.length - 1];
		
		    var gridNam;
		    if(gO.typesInDim == -1)
		        gridNam = var2Fortran(gO.caption);
		    else 
	                gridNam = "typvar_" + gO.caption;


                    // Construct allocatable grids declaration string.
		    if (newgrid.indexOf("TYP_") == -1) {

                        newgrid = newgrid.replace(" ", " ALLOCATABLE, ");
			newgrid = newgrid.replace("DIMENSION", "DIMENSION(" + 
				colons.slice(0, colons.length - 1) + ")");

		    } else {
			 // Corresponds to AoS (TYPE (TYP_<NAME>), DIMENSION(:)
			 // :: typvar_<NAME>(<DIMS>)
			 newgrid = newgrid.replace("),", "), ALLOCATABLE,");

		    }

                    allocatablesOfStep += addIndentation(0) +
                          "ALLOCATE(" + gridNam + "(" + tmp_dim_alloc + "))\n";
                            

                }

		// Commenting.
		if(gO.comment != null) 
		    grids += addIndentation(0) + "! " + gO.comment + "\n";

                grids += addIndentation(0) + newgrid;

                // Push into list of grids that have been declared (in the 
                // context of the current function being parsed).
                GridsInFunc.push(gId);

            }
        } 


        // STEP: Get var defs for titles.
        
        // Get titleDefs (e.g., var _d3Tab1,_d3Tab2...).
        var titleDefTmp = getTitleDefsOfGrid(gO);

        if (titleDefTmp != "") {

            // Remove "var " and trailing ';' (e.g., _d3Tab1,_d3Tab2...)
            // and prepend the grid name before the title, to discern
	    // for the cases when a (same) title is used for different
	    // grids that corresponds to different values.
	    titleDefTmp = titleDefTmp.replace("var ", "");
            titleDefTmp = titleDefTmp.replace(";", "");
            titleDefTmp = titleDefTmp.replace(/_/g, gO.caption + "_");
            titleDefTmp = "INTEGER :: " + titleDefTmp;

	    // If it has been declared already, do not re-declare.
            if (TitleDefs_decl.indexOf(titleDefTmp) == -1) {

                TitleDefs_decl += addIndentation(0) + titleDefTmp;

            }

        }

    }


    // STEP: Create Fortran do loops (a loop for each index var).
    // Note: rangeExpr.exprArr[] contains root range expressions.
    //
    var rangeExpr = sO.boxExprs[CodeBoxId.Range];
    var loop_close = "";

    var forstr = ""; 
    // In non-parallel the above variable stores all loops, 
    // in parallel code generation it only stores paral/ble ones.
    
    var forstr2 = ""; 
    // Used in parallel code generation to store non-parallelizable loops.

    var collapsed_loop_vars = ""; 
    // Used in parallel code generation, to store all loop variables, 
    // so that no endv assignments between collapsed DO loops.

    var private_vars = ""; 
    // Used in parallel code generation to store loop-private variables 
    // (e.g., LET variables).
    
    // Used for indentation purposes (adding extra, when needed - when we have
    // foreach loop over one or more dimensions, otherwise it is zero).
    var index_extra_indent = 0;


    // if there are index variables.
    //
    if (rangeExpr && rangeExpr.exprArr && rangeExpr.isForeach()) {

        var num_index_vars = rangeExpr.exprArr.length;
        index_extra_indent = num_index_vars;
        
	var collapse = 0; 
	// Defines whether we are using collapse in current step's loop.
	
        var collapse_int; 
	// Used for altering collapse value within the loop, but we keep 
	// the original, too, in collapse_int.
	
        if (ShowParallel) {

            // If there are more than one par/ble dimensions in Pragma_str we 
	    // will use COLLAPSE(X), where X is the number of dimensions 
	    // (parallel DO LOOPS).
	    // TODO: Later as part of auto-tuning, we will test all possible
	    // combinations of collapsing loops.
            collapse = Pragma_str[funcID][CurStep].split(' ').length - 1;

        }
        
	collapse_int = collapse;

	var forStrArr = new Array(); // REORDER

        for (var iv = 0; iv < num_index_vars; iv++) {

            // STEP: get code for foreach loop 
            //
            var rexpr = rangeExpr.exprArr[iv];

            assert(rexpr.gO, "Range expr must have a gO");

            // TODO: FIX FIX FIX
            var ivar = var2Fortran(rexpr.labelExpr.str);

            // Define the value of literal 'end'.
            var endv = var2Fortran(DefEndName + rexpr.selDim);

            //TODO: Why did I change the below to the above?			
            //var endv = expr2FortranString(rexpr.exprArr[RangeFields.End]);

            // IMPORTANT NOTE: semasiology is SAVE if assignment 
            // is at the time of declaration. Saved between function calls. 
            // That's why here we separate declaration and initialization.

	    // We do not allow re-declaration of the same end variable across 
	    // steps.
            if (Index_end_per_dim.indexOf(endv) == -1) {

                Index_end_decl += addIndentation(0) + "INTEGER :: " +
                    endv + "\n";
                Index_end_per_dim.push(endv);

            }


            // Pick the end value based on whether the size of the dim is
            // variable (dynamically allocated) or not.
            //
            var actendval = rexpr.gO.dimActSize[rexpr.selDim];
            var dynendval = rexpr.gO.dimDynSize[rexpr.selDim];
	    var endval;
	    // If regular grid (i.e., not TYPE)
	    //DC10: or a scalar grid that is part of an (existing) struct
	    if (rexpr.gO.typesInDim == -1) {

		//DC10:
		var tmpp;
	    	if (rexpr.gO.structCaption == null) {

                	tmpp = var2Fortran(rexpr.gO.caption);
	    
	    	} else {

			tmpp = rexpr.gO.structCaption + "%" + rexpr.gO.caption;

	    	}


	        endval = (dynendval) ? "SIZE(" + tmpp +
                ", " + (rexpr.selDim + 1) + ")" : actendval;

	    } else {

		if (Soa) {

		    // E.g., if typesInDim = 0, then end0 not used, 
		    //       end1 = SIZE(...%dim0, 2) and end2 = SIZE(..., 1)
		    //       if typesInDim = 1, then end1 not used,
		    //       end0 = SIZE(...%dim0, 2) and end2 = SIZE(..., 1)
		    //var dimSel = (rexpr.selDim > rexpr.gO.typesInDim) ? 
		    //		 rexpr.gO.numDims - rexpr.selDim : 
		    //		 rexpr.gO.numDims - rexpr.selDim - 1;
		
		    var dimSel = (rexpr.selDim > 0) ?
				 rexpr.gO.numDims - rexpr.selDim :
				 rexpr.gO.numDims - rexpr.gO.typesInDim; 

		    // If SoA, we need to get the size of any of the elements
		    // Remember: all will have the same dimensions by design.
		    endval = (dynendval) ? "SIZE(" + "typvar_" + 
		    	     rexpr.gO.caption + "%" +
			     rexpr.gO.dimTitles[gO.typesInDim][0] +
                             ", " + dimSel + ")" : actendval; //DC:

		    //endval = (dynendval) ? "SIZE(" + "typvar_" + 
		    //	     rexpr.gO.caption + "%dim0" +
                    //       ", " + dimSel + ")" : actendval; //DC:
		
		} else {

		    // If AoS, always 1D, so 1st dimension in SIZE().
		    endval = (dynendval) ? "SIZE(" + "typvar_" + 
			     rexpr.gO.caption +
                             ", " + "1" + ")" : actendval;


		}

	    }


	    // In parallel code generation, if loop is parallel, we append the
	    // endval to the collapsed loop vars, else we declare it normally
	    // at its normal position (whereas collapsed loop vars appear 
	    // BEFORE the OMP PARALLEL DO COLLAPSED(X) directive).
            if (ShowParallel) {

                if (Pragma_str[funcID][CurStep].indexOf(ivar) != -1) {

                    collapsed_loop_vars += addIndentation(0) + endv +
                        " = " + endval +
                        "-1\n";

                } else {
		    //DC9:
                    forstr2 += addIndentation(0) + endv + " = " + endval +
                        "-1\n";

                }

            } else {
		//DC9:
                forstr += addIndentation(0) + endv + " = " + endval +
                    "-1\n";

            }

            if (Loop_var_per_dim.indexOf(ivar) == -1) {

                // Adding the names of index variables to the declarations 
                // string if it has NOT been declared for this dimension in 
                // THIS function and push it in the array denoting it has 
                // been now declared.
                Row_col_decl += addIndentation(0) + "INTEGER :: " + ivar +
                    "\n";
                Loop_var_per_dim.push(ivar);

            }


            // Step: Start/End/Step expressions

            var start = expr2FortranString(rexpr.exprArr[RangeFields.Start]);
            var end = expr2FortranString(rexpr.exprArr[RangeFields.End]);
            var step = expr2FortranString(rexpr.exprArr[RangeFields.Step]);

            if (ShowParallel) { 
	        // ShowParallel check is implied 0 if collapse is > 0 
		// (safely delete check).

		// If loop is parallel over ivar:    
                if (Pragma_str[funcID][CurStep].indexOf(ivar) != -1) {

                    if (collapse_int != 0) {

                        forstr += addIndentation(0) +
                            "!$OMP PARALLEL DO COLLAPSE(" + collapse +
                            ")\n";

                        collapse_int = 0; 
			// So that only first parallelizable dimension is 
			// written (with collapse thereby incorporating all 
			// parallelizable dimensions).

                    }

		    // TODO: This will be needed when we allow multiple 
		    // combinations in auto-tuner.
                    // Here maintain a stack of OMP DO loops, so we can 
		    // close accordingly.
                    //stepOmpDoStack.push("!$OMP END PARALLEL DO\n");

                    //forstr += addIndentation(iv) + "DO " + ivar + " = " +
                    //    start + ", " + end + ", " +
                    //    step + "\n";
                    // In Fortran, second value in start, end, step: 
		    // means <= end

		    forStrArr.push(new Array(ivar, start, end, step));

                } else {
		    // Normal (non-parallel) DO loop:

		    // TODO: Will be needed in allowing multiple combinations
		    // of collapsing/non-collapsing in auto-tuner.	
                    //stepOmpDoStack.push("");
                    forstr2 += addIndentation(iv) + "DO " + ivar + " = " +
                        start + ", " + end + ", " +
                        step + "\n";
                  
                }

            } else { // Non-parallelized version: all DO loops, normal code:

                //forstr += addIndentation(iv) + "DO " + ivar + " = " +
                //    start + ", " + end + ", " +
                //    step + "\n";
		    
		forStrArr.push(new Array(ivar, start, end, step));

            }

        }


	// TODO: CAUTION: For (ShowParallel == 1, parallelizable step) and
	// (ShowParallel == 0), we do it in forstr variable.
	// For both, the assumption is that an index variable CANNOT
	// be allowed to be used as a boundary variable in ANOTHER loop, or step
	// or start condition!
	// TODO: CAUTION: When we allow this in the GUI, will need to take care.	
	// TODO: CAUTION: Need ordering for forst2, too, when exists. 
	// VERIFY SAFETY IS ENFORCED (for serial, need to check if independent?)

	// Loop re-ordering to be suitable to language's array storage format.
	// indX, row, col: for C.
	// indX, col, row: for Fortran.
	

	var higherDims = new Array();
		
	// Find if there are higher dimensions first (i.e., indX):
	for (var i = 0; i < forStrArr.length; i++) {

	    if (forStrArr[i][0].indexOf(DefDimIndNameRoot) != -1) {

	        // Place those including indX in a separate array:
	        higherDims.push(forStrArr[i]);
	        // And delete from original array:
	        forStrArr.splice(i,1);
	        i--;

	    } 

	}
	
	// Sort both arrays: 
	// (a) the first in descending order (e.g., ind4,ind2)
	// (b) the second in [c]olumn, [r]ow (for Fortran col-major-friendly
	// format).
	// TODO: WARNING: Order may affect results (even in paral/ble loops)?
	higherDims.sort();
	higherDims.reverse();
	forStrArr.sort();

	// Join the appropriately sorted arrays.
	forStrArr = higherDims.concat(forStrArr);

	// Create the FOR loops' string in this order:
	for (var i = 0; i < forStrArr.length; i++) {
	    //DC9:
	    forstr += addIndentation(i) + "DO " + forStrArr[i][0] + " = " +
                        forStrArr[i][1] + ", " + forStrArr[i][2] + ", " +
                        forStrArr[i][3] + "\n";

	}

	

        // Now that we have scanned all DO loops and know the appropriate
        // closing pairs (which ones correspond to OMP DO loops):
        for (var iv = 0; iv < num_index_vars; iv++) {

            if (ShowParallel) {

                loop_close += addIndentation(num_index_vars - iv - 1) +
                    "END DO\n";

                if (collapse && iv == num_index_vars - 1) {

                    // Only for last one and only if collapse.
                    loop_close += addIndentation(0) +
                        "!$OMP END PARALLEL DO\n";

                }

		// TODO: May be needed in autotuning multiple combinations
		// of collapsing/non-collapsing loops. 
                // Here need to close the appropriate OMP DO loop.
                // loop_close += stepOmpDoStack.pop();

            } else {

                loop_close += addIndentation(num_index_vars - iv - 1) +
                    "END DO\n";

            }

        }

    } else if (rangeExpr && rangeExpr.isForever()) {

        forstr += addIndentation(0) + "DO WHILE (.TRUE.)\n";
        loop_close = addIndentation(0) + "END DO\n";

    }


    // STEP: Go through all the boxes and create mask/formula.
    //

    var stmt = "";
    var prev_indent = 1; // First statement always has 1 tab indentation.
    var mask_unmatched = 0; // Set to n when thereare n unmatched if stmt.
    var last_if_indent = 0; 
    // Used to store the indent value of last unmatched if stmt.

    var indent = 1;

    // When there is NO foreach loop decrease by one,
    // because default one takes into account its existence always.
    if (index_extra_indent == 0) index_extra_indent = -1;
    else index_extra_indent--;


    for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

        // Step: close braces -- if there is a reduction in indent level
        //       from previous, close braces.
        //
        prev_indent = indent;
	if (mask_unmatched == 0) {
	    indent = 1; // This only occurs when we have an empty IF statement.
	} else {
            indent = sO.boxAttribs[box].indent;
	}

        var boxexpr = sO.boxExprs[box];

        // Step: handle mask statment if/else/elseif/breakif.
        //
        if (sO.boxAttribs[box].isMask()) {

            if (boxexpr.isElse()) {

                // 'else' has no expression between ().

                var tmp2 = last_if_indent;
                for (var i = indent; i < tmp2; i++) {

                    stmt += addIndentation(last_if_indent +
                        index_extra_indent) + "ENDIF\n";
                    mask_unmatched--;
                    last_if_indent--;
                    
                }

                stmt += addIndentation(last_if_indent +
                    index_extra_indent) + "ELSE\n";

		// Commenting.
		if (sO.boxAttribs[box].comment != null) 
		    stmt += addIndentation(last_if_indent +
                    	    index_extra_indent) + "! " + 
			    sO.boxAttribs[box].comment + "\n";	



            } else if (boxexpr && boxexpr.exprArr && boxexpr.exprArr.length) {

                // condition with child expression  -- if/elseif/breakif.
                //

                if (boxexpr.isIf()) {

                    mask_unmatched++; // Increase unmatched if stmts.
                    
                    var tmp2 = last_if_indent;

                    for (var i = indent; i <= tmp2; i++) {

                        stmt += addIndentation(last_if_indent +
                            index_extra_indent) + "ENDIF\n";
                        mask_unmatched--;
                        last_if_indent--;

                    }

                    stmt += addIndentation(indent + index_extra_indent) +
                        "IF" + "(" + expr2FortranString(boxexpr) +
                        ") THEN\n";

		    // Commenting.
		    if (sO.boxAttribs[box].comment != null) 
		        stmt += addIndentation(indent + index_extra_indent) + 
				"! " + sO.boxAttribs[box].comment + "\n";

                    last_if_indent = indent; // So that we can close it later.
                    
                } else if (boxexpr.isElseIf()) {

                    var tmp2 = last_if_indent;
                    for (var i = indent; i < tmp2; i++) {

                        stmt += addIndentation(last_if_indent) +
                            "ENDIF\n";
                        mask_unmatched--;
                        last_if_indent--;

                    }

                    stmt += addIndentation(indent + index_extra_indent) +
                        "ELSEIF" + "(" + expr2FortranString(boxexpr) +
                        ") THEN\n";
                    
		    // Commenting.
		    if (sO.boxAttribs[box].comment != null) 
			stmt += addIndentation(indent + index_extra_indent) + 
				"! " + sO.boxAttribs[box].comment + "\n";

                } else {
                    // TODO: What is this case doing? (breakif).
                    stmt += addIndentation(indent + index_extra_indent) +
                        boxexpr.str + "(" +
                        expr2FortranString(boxexpr) + ") {\n";
                    alert(boxexpr.str);
                }

            } else {

                stmt += "";
                //TODO: This is not correct for Fortran? Delete else 
                //altogether?

            }

        } else {

            // Step: Process a formula statement.
	    
	    // Commenting.
	    if (sO.boxAttribs[box].comment != null) 
	        stmt += addIndentation(indent + index_extra_indent) + 
                        "! " + sO.boxAttribs[box].comment + "\n";

            // TODO: Do some more checking on the following. May be trickier 
            // than that.
            if (sO.boxAttribs[box].indent <= last_if_indent) {

                for (var i = 0; i <= (last_if_indent - sO.boxAttribs[box]
                        .indent); i++) {

                    stmt += addIndentation(last_if_indent +
                        index_extra_indent) + "ENDIF\n"
                    mask_unmatched--;
                    last_if_indent--;

                }

            }

            if (sO.boxAttribs[box].isFormula() && boxexpr.exprArr.length >
                2 && (boxexpr.exprArr[1].isAssignOp() ||
                    boxexpr.exprArr[1].isXAssignOp())) {

                // Should cover all cases: simple equality OR
                // the case of +=, -=, *=, /= (assertion of valid 
                // exprArr[1] is guarranteed by previous steps in GUI).

                // TODO: Here what if name changes (or is set by LET). 
                // Need to take special care.

		var currFormLine;
	
		// 'If' refers to +=, etc. cases, 'else' to normal assignments
                if (boxexpr.exprArr[1].isXAssignOp()) {
                    
		    // HERE IF isXAssignOp() then find the two parts BEFORE
                    // and AFTER the XAssignOp
                    // and output RHS = RHS + LHS (use indexOf() and 
                    // substring() JS methods).
                    // Because ifort does not support the +=, etc. notation.

                    //TODO: May it have any side effects?
                    var wholestr = expr2FortranString(boxexpr);
                    var pre_str = wholestr.substring(0, wholestr.indexOf(
                        boxexpr.exprArr[1].str) - 1);
                    var post_str = wholestr.substring(wholestr.indexOf(
                        "+=") + 3);

                    currFormLine = addIndentation(indent + index_extra_indent) +
                        pre_str + " = " + pre_str + " " +
                        boxexpr.exprArr[1].str.substring(0, 1) + " " +
                        post_str + "\n";

                } else {

		    //DC10: Adding scalar variables in a (potentially) parallel
		    //      loop in the PRIVATE() list.
		    if (ShowParallel) {
			    
			var gO = boxexpr.exprArr[0].gO;
			
			// TODO: WARNING: If we directly enter other than row/col as
			//       index, and we DON'T do it via CLICKING on the intersection
			//       on the grid, then we may get null (because of how the
			//       expression is created).
			if (gO == null) {

				alert("Error: NULL gO");

			}
			// For scalar variables only (on the LHS, i.e., written).
			// TODO: Also, before should be "(", or ", "
			if (gO.numDims == 0 && 
			private_vars.indexOf(boxexpr.exprArr[0].str + ", ") == -1) {

			    private_vars += var2Fortran(boxexpr
			    .exprArr[0].str) + ", ";

			}

		    }

                    currFormLine = addIndentation(indent + index_extra_indent) +
                        expr2FortranString(boxexpr) + "\n";

                }

                // In current formula box: Loop through the exprArr[] 
                // array and detect if there are functions (isFuncCall()).
                for (var lp = 2; lp < boxexpr.exprArr.length; lp++) {
                    // TODO: If +=, -=, etc. then checking 2 is redundant?


                    // If it is a function, save its type to pass when 
                    // building the function code
                    // Note: For library function calls we don't need 
		    // explicit declaration.
                    if (boxexpr.exprArr[lp].isUserFuncCall()) {

                        // Get global id of function within module
                        var f = getFuncIdByName(mO, boxexpr.exprArr[lp].str);

                        // Only add to declarations if NOT already declared 
			// (i.e., 1st time called).
                        if (Func_decl.indexOf(var2Fortran(boxexpr.exprArr[
                                lp].str) + "\n") == -1) {

                            // Used for data type declaration of function in 
			    // caller.
                            Func_decl += addIndentation(0) +
                                getDataTypeString(mO.allFuncs[f].allGrids[
                                    0], null) + " :: " +
                                var2Fortran(boxexpr.exprArr[lp].str) +
                                "\n";

                            // TODO: This means that
                            // the function will have a limited scope (within 
                            // current function-caller, declared there). Is 
			    // that OK? 

                            TypesAllFuncs[GID_function] =
                                getDataTypeString(
                                    mO.allFuncs[f].allGrids[0], null);

                            NamesAllFuncs[GID_function] = var2Fortran(
                                boxexpr.exprArr[lp].str);

                            GID_function++; 
			    //Increase ID to represent next called function.

                        }


			// Called function "version": Serial or parallel, 
			// depending on whether CURRENT step of CURRENT 
			// function is parallel or not (to avoid nested 
			// parallelism).
			
			var funcCallName = expr2FortranString(
					boxexpr.exprArr[lp]);
			if (ShowParallel) {
		
			    var funcC = boxexpr.exprArr[lp].str;
		            var calleeFuncId = getFuncIdByName(mO, funcC);
		            if (funcContainsParStep(mO, calleeFuncId)) {

			        if (Pragma_str[funcID][CurStep] != "" || 
				    CalledFromSer) {

				    var wholeCall = expr2FortranString(
				    		    boxexpr.exprArr[lp]);
				    // Replace in stmt for current formula.
				    funcCallName = wholeCall.replace(
						    "(", "_ser(");
				    currFormLine = currFormLine.replace(
						    wholeCall, funcCallName);
				    FuncHasSerVer[calleeFuncId] = 1;

				}

			    }

			}

                    }

                }

		stmt += currFormLine;

            } else if (boxexpr.exprArr[0] != null &&
                boxexpr.exprArr[0].type == ExprType.Return) {

		// TODO:
                // 1) Check if return statement in position 0 (can't be 
                // anywhere else).
		// 2) If in subroutine, disallow 'return <returnValue>'

                var return_expression = "";
                var ret_val_assignment = "";

                // Parse the expression to be returned and save in order to
                // assign to the function's name (i.e., value to be returned).
                for (var i = 1; i < boxexpr.exprArr.length; i++) {

                    if (i > 0) return_expression += " ";

                    return_expression += expr2FortranString(
                        boxexpr.exprArr[i]);

                }

		// If subroutine, then no assignment of return value
		if (fO.allGrids[0].dataTypes[0] != DataTypes.Void) {

                    if (boxexpr.exprArr.length != 1) {

                        ret_val_assignment = var2Fortran(fO.funcCallExpr.str) +
                            " = " + return_expression;

                    } else {

                        ret_val_assignment = var2Fortran(fO.funcCallExpr.str) +
                            " = " + var2Fortran(fO.allGrids[0].caption);

                    }

		    stmt += addIndentation(indent + index_extra_indent) +
                    	ret_val_assignment + "\n" 
		}

                stmt += addIndentation(indent +
                        index_extra_indent) + "RETURN\n";

            } else {

                //alert("DEAD CODE?/incomplete statement?");

		// LET statement:    
                if (boxexpr !== undefined && boxexpr.exprArr[0] !==
                    undefined && boxexpr.exprArr[0].isLet()) {

		    //DC10: Avoid duplicate declaration of same-named LET stmts
		    // If grid has been already declared within THIS function, 
 	            // do not re-declare.
		    // Note: For LET stmts we don't use gId, but their name.
		    var gId = boxexpr.exprArr[0].exprArr[0].str;
         	    if (!gridDeclaredInFunc(gId)) {

                        var let_data_type = findLetTypeCont(boxexpr, 0, mO); 

		        //DC10: Added "::"
                        grids += addIndentation(0) + let_data_type + " :: " +
                            var2Fortran(boxexpr.exprArr[0].exprArr[0].str) +
                            "\n";
		    
			
			// Push into list of grids that have been declared (in the 
	                // context of the current function being parsed).
        	        GridsInFunc.push(gId);

		    }

                    if (ShowParallel) {
			    
			private_vars += var2Fortran(boxexpr
                        .exprArr[0].exprArr[0].str) + ", ";

		    }

                    var let_RHS = "";

                    for (var let_ass = 1; let_ass < boxexpr.exprArr.length; let_ass++) {

			if (let_ass > 1) let_RHS += " ";
                        let_RHS += expr2FortranString(boxexpr.exprArr[
                            let_ass]);

		    }

                    stmt += addIndentation(indent + index_extra_indent) +
                        var2Fortran(boxexpr.exprArr[0].exprArr[0].str) +
                        " = " + let_RHS + "\n";
                    // Now, only takes care for the part AFTER the LET name.

                } else if (boxexpr !== undefined && boxexpr.exprArr[0] !==
                    undefined && !boxexpr.exprArr[0].isLibFuncCall() &&
		    !boxexpr.exprArr[0].isFormulaKeyword()) { 
		    
		    // Regular function call without assignment or anything 
		    // else.

                    // TODO: CAUTION: Is there any other case except for 
		    // standalone function call left?
                    // Later, may convert these to subroutines instead of 
		    // assigning to dummy variable.
                    // Need to check: if(boxexpr !== undefined && 
		    // boxexpr.exprArr[0] !== undefined)!


                    //********************************************************
                    //**************Code in starred section is copied from****
		    //**************earlier part******************************
                    // TODO: See notes in the earlier part.

                    // Get global id of function within module.
                    var f = getFuncIdByName(mO, boxexpr.exprArr[0].str);

                    // Only add to declarations if NOT already declared 
		    // (i.e., 1st time called).
                    if (Func_decl.indexOf(var2Fortran(boxexpr.exprArr[0].str) +
                            "\n") == -1) {

                        // Used for data type declaration of function in 
			// caller.
                        Func_decl += addIndentation(0) +
                            getDataTypeString(mO.allFuncs[f].allGrids[0],
                                null) + " :: " +
                            var2Fortran(boxexpr.exprArr[0].str) + "\n";


                        // Used for declaration of type in header of function.
                        TypesAllFuncs[GID_function] = getDataTypeString(
                            mO.allFuncs[f].allGrids[0], null);

                        NamesAllFuncs[GID_function] = var2Fortran(
                            boxexpr.exprArr[0].str);

                        GID_function++; 
			//Increase ID to represent next called function.

                        //****************************************************

                        // TODO: If I rewrite this as subroutines, then this 
			// won't be needed.
                        // Nor the "res_<funcName>" part in stmt assignment 
			// below.
                        // This is a dummy variable, doesn't need to be 
			// checked for parallelism (does it affect it at all?
			// wrt function checks).
                        // No, since code generation does not affect 
			// parallelism procedures, they still see only a 
			// standalone function at this box.

			//DC5: Not needed in the case of subroutines (i.e., void)
			//     return value.
			if (mO.allFuncs[f].allGrids[0].dataTypes[0] != DataTypes.Void) {

                            grids += addIndentation(0) + getDataTypeString(mO
                                .allFuncs[f].allGrids[0], null) + " :: " +
                                "res_" + boxexpr.exprArr[0].str + "\n";
			}

                    }

		    // Called function "version": Serial or parallel, depending
		    // on whether CURRENT step of CURRENT function is parallel
		    // or not (to avoid nested parallelism).
		   
		    var funcCallName = expr2FortranString(boxexpr);
		    if (ShowParallel) {
		
			var funcC = boxexpr.exprArr[0].str;
		        var calleeFuncId = getFuncIdByName(mO, funcC);
		        if (funcContainsParStep(mO, calleeFuncId)) {
		            
			    // TODO: CAUTION: HERE I NEED TO CHECK IF I AM THE
			    // SERIAL VERSION OF A 2-VERSION function. IF SO
			    // THEN WILL NEED TO CALL SERIAL VERSION 
			    // *IRRESPECTIVE OF WHETHER MY STEP IS PARALLEL OR
			    // NOT* - can be through a simple parameter in 
			    // calling 4Step when called from Cstr? or a GLOBAL
			    // variable (ensure correctness...)
			    if (Pragma_str[funcID][CurStep] != "" || 
				CalledFromSer) { 
			        // Parallel

				// Finally add the rest of box's call including
				// the parentheses and arguments in call.
				// Just replace where leftmost left paren is.
				var wholeCall = expr2FortranString(boxexpr);
				funcCallName = wholeCall.replace("(", 
						"_ser(");
			        // Mark that callee needs a SERIAL version.
			        FuncHasSerVer[calleeFuncId] = 1;

		            }

		        }

		    }

		    //DC5: Not needed in case of subroutines (i.e., void ret. value).
		    if (mO.allFuncs[f].allGrids[0].dataTypes[0] != DataTypes.Void) {

                        stmt += addIndentation(indent + index_extra_indent) +
                            "res_" + boxexpr.exprArr[0].str + " = " +
                            funcCallName + "\n";

		    } else {

			stmt += addIndentation(indent + index_extra_indent) +
			    "CALL " + funcCallName + "\n";

		    }

                } else { 
		
		    // Anything else (including library functions - taken care
		    // within expr2FortranString).
		    //DC9:
                    var tmp_stmt = expr2FortranString(boxexpr);

		    if (tmp_stmt != "") 
		        stmt += addIndentation(indent + index_extra_indent) +
			        tmp_stmt + "\n";


                }

            }

        }

    }

    // Step: close end braces -- if there is a reduction in indent level
    //       from previous, close braces. Note: Indentation must end at 
    //       with just 1 tab. 
    //
    // Only do below if there are if stmts that have not been matched 
    // (closed).
    // TODO: NOT START FROM 1 IF EMPTY!!!!!

    // last_if_indent might be different than mask_unmatched
    // e.g., if first mask blank we'll close an if that does not exist 
    // (otherwise we'd have to keep last open position to start instead
    // of var i=1, which is effectively similar)
    // TODO: Could as well use only mask_unmatched in for loop,should be ok
    for (var i = 1; i <= last_if_indent; i++) {
        if (i <= mask_unmatched) {
            stmt += addIndentation(last_if_indent + index_extra_indent) +
                "ENDIF\n";
        }
    }

    // Add grids declared in current step to Grids_new_decl to be added
    // in function's declarations' code section.
    Grids_new_decl += grids;

    // Add OMP PRIVATE clause, for parallel version, if we have a
    // OMP PARALLEL DO directive AND private variables.
    if (ShowParallel) {

	// DC10: Remove private variables that are also reduction variables
	if (Pragma_reduction[funcID][CurStep] != "") {

	    var regex = new RegExp(/: (.*)\)/);
	    var toFind = Pragma_reduction[funcID][CurStep].match(regex);	
	    if (private_vars.indexOf(toFind[1]) != -1) {

	 	//alert("FOUND!");
		private_vars = private_vars.replace(toFind[1] + ", ", "");	

	    }

	}

        // If we don't have a parallel loop, then do not use private.
        if (forstr.length != 0 && private_vars != "") {
            private_vars = private_vars.replace(/, +$/, "");
            private_vars = "!$OMP PRIVATE(" + private_vars + ")\n"
            forstr = forstr.replace(/\n/, " &\n" + private_vars);
            //TODO: Add indentation for private_vars.
        }

        // If we don't have a parallel loop, then do not use private.
        if (forstr.length != 0 && Pragma_reduction[funcID][CurStep] != "") {
           
	    var tmp_red = Pragma_reduction[funcID][CurStep];	
            tmp_red = tmp_red.replace("\n", "");
	    if (private_vars != "")
                forstr = forstr.replace(/\n/, " \n" + tmp_red + " &\n");
	    else
                forstr = forstr.replace(/\n/, " &\n" + tmp_red + "\n");

            //TODO: Add indentation for private_vars.
            
        }

    }



    // In order to obtain the dynamic variables we parse the allocatablesOfStep
    // variable and Struct_el_decl and process accordingly.
    var allocatablesFree = allocatablesOfStep.replace(/^\tALLOCATE\(ft_(.+)\(.+$/mg, "\tDEALLOCATE(ft_$1)");
    // This takes care of AoS.
    allocatablesFree = allocatablesFree.replace(/^\tALLOCATE\(typvar_(.+)\(.+$/mg, 
		       "\tDEALLOCATE(typvar_$1)");
    // This takes care of elements of SoA.
    allocatablesFree += Struct_el_decl.replace(/^\tALLOCATE\(typvar_(.+)\(.+$/mg, 
		        "\tDEALLOCATE(typvar_$1)");
    // TODO: Also, make sure AoS/SoA frees function correctly.
    AllocFreePerFunc += allocatablesFree;


    // STEP: Combine all the above components together:
    // a) Loop variables' initialization from collapsed loops.
    // b) Allocatable variables of step.
    // c) DO/OMP DO loops (their combinations depending on the case).
    // d) Actual step computation code.
    // e) Loop closing clauses (serial and/or parallel).

    // Commenting.
    var code = (sO.title == null) ? "" : addIndentation(0) + "!" + 
	       sO.title + "\n"; 
    code += (sO.stepComment == "") ? "" : addIndentation(0) + "!" + 
	    sO.stepComment + "\n";
    code += allocatablesOfStep + Struct_el_decl + collapsed_loop_vars + 
	    forstr + forstr2 + stmt + loop_close;

    return (code);

}


// --------------------------------------------------------------------------
// Searches current step for all boxes that define a LET and returns its 
// type. 
// --------------------------------------------------------------------------
function findLetType(argexpr, mO) {

    var sO = CurStepObj; 
    // Will always need to search in same screen, i.e., CurStepObj

    for (var b = CodeBoxId.BoxStart; b < sO.boxExprs.length; b++) {

        if (sO.boxExprs[b]) {

            // First expression in the expression statement.
            //
            var expr0 = sO.boxExprs[b].exprArr[0];

            if (expr0 && expr0.isLet()) {

                // Note: Let name is present in exprArr[0] position.
                //
                if (expr0.exprArr[0].str == argexpr.str) {

                    return findLetTypeCont(sO.boxExprs[b], 1, mO);

                }
            }
        }
    }


}


// --------------------------------------------------------------------------
// Searches current step for all boxes that define a LET and returns its 
// type.
// --------------------------------------------------------------------------
function findLetTypeCont(boxexpr, asDataTypes, mO) {

    // We need to search all exprArr[1] to end,0 get the most general type 
    // (consider only gridcells and functions and constants) and generate code
    // to assign it to a var named with the let name (i.e., exprArr[0].str).
    // If any of it is a gridCell, function parallelism code should parse it
    // as needed.
    // GUI takes care of typechecking (i.e., no strings and numbers mixed). If
    // a number exists it means we only have numbers (be it integers, reals, 
    // etc.).
    // CAUTION: What about "generic" LETs like those in a foreach like 
    // Out[row,col]?

    var ret = "";
    var let_data_type = DataTypes.Integer; //TT 
    // Default. If found REAL, will stay REAL. If we find anything else
    // e.g., STRING, LOGICAL etc in a grid cell or function, it will take
    // this value (and keep it, since all will be the same-enforced by GUI).

    for (var i = 1; i < boxexpr.exprArr.length; i++) {
        
	if (boxexpr.exprArr[i].isNumber()) {

            if (boxexpr.exprArr[i].str.indexOf(".") != -1) {

                // Contains '.', so it is a decimal (i.e., REAL). Update type.
                let_data_type = DataTypes.Real4; //TT

            }

        } else if (boxexpr.exprArr[i].isGridCell()) {

            // Two cases: is a "regular" grid with ONE data type.
            // Or, a grid that contains many datatypes across one dimension.

            //If found REAL earlier, disregard any further INTEGERs found.
            if (let_data_type != DataTypes.Real4) {

                let_data_type = getDataTypeString(boxexpr.exprArr[i].gO,
                    boxexpr.exprArr[i]);
		if(let_data_type == "UNIQUEIND") //TT
                	let_data_type = DataTypes.UniqInd; //TT
               	else //TT
                	let_data_type = dataTypeStrToInt(let_data_type); //TT

            }

        } else if (boxexpr.exprArr[i].isUserFuncCall()) { 
	// TODO: CAUTION: Need for library funcs?


            //****************************************************************
            //************Code in starred section is copied from earlier part*
            // TODO: See notes in the earlier part.

	    /*
            if (!asDataTypes) { 
		
		// This only needs to be done when parsing for code generation
                // NOT when adding a LET parameter in a function and doing 
		// on-the-fly parsing to get its type to assign to the 
		// parameter, when LET's type depends on a function.

                // Get global id of function within module
                var f = getFuncIdByName(mO, boxexpr.exprArr[i].str);

                // Only add to declarations if NOT already declared (i.e., 1st
		// time called)
                if (Func_decl.indexOf(var2Fortran(boxexpr.exprArr[i].str) +
                        "\n") == -1) {

                    // Used for data type declaration of function in caller
                    Func_decl += addIndentation(0) + getDataTypeString(mO
                            .allFuncs[f].allGrids[0], null) + " :: " +
                        var2Fortran(boxexpr.exprArr[i].str) + "\n";


                    // Used for declaration of type in header of function
                    TypesAllFuncs[GID_function] = getDataTypeString(
                        mO.allFuncs[f].allGrids[0], null);
                    
		    NamesAllFuncs[GID_function] = var2Fortran(
                        boxexpr.exprArr[i].str);
                    
		    GID_function++; 
		    // Increase ID to represent next called function

                }

            }
	    */

            // Get data type of the ReturnValue.
            // First get name of function, find id, find it in 
	    // module.allFuncs[id].allGrids[0]
            // TODO: What about library functions? Same? What module? 
	    // How to get it?
            if (let_data_type != DataTypes.Real4) {

                let_data_type = getDataTypeString(mO.allFuncs[
                    getFuncIdByName(mO, boxexpr.exprArr[i].str)].allGrids[
                    0]);

		if(let_data_type == "UNIQUEIND") //TT
                	let_data_type = DataTypes.UniqInd; //TT
               	else //TT
                	let_data_type = dataTypeStrToInt(let_data_type); //TT

            }

        } else if (boxexpr.exprArr[i].isLet()) {

            if (let_data_type != DataTypes.Real4) {
		  
		var typeCode = findLetType(boxexpr.exprArr[i], mO);    
                let_data_type = typeCode;

	    }

        }

    }

    if (asDataTypes) {

        //Return using their numeric equivalent in DataTypes.
        return let_data_type;

    } else {

        return dataTypeIntToStr(let_data_type);

    }

}


// --------------------------------------------------------------------------
// Function used to add indentation to each line of code.
// Default is plus 1.
// --------------------------------------------------------------------------
function addIndentation(indent) {

    indent += 1;
    var str = "";

    for (var i = 0; i < indent; i++)
        str += "\t";

    return str;

}


// --------------------------------------------------------------------------
// Function used to return declaration of typevars.
// If declType is 1, this means this is the first time we are declaring this
// type, so we need to append to TypeStr the declaration of the TYPE and its 
// body.
// --------------------------------------------------------------------------
function findTypeVarType(gO, onlyDataType) {

    var gridDefinition = "";
    var typename = "";

    // Convention: variable named as <typvar_><grid name> 
    // (e.g., Out -> typvar_Out).
    typeVariable = "typvar_" + gO.caption;

    var tmp_type_body = "";

    var tmp_length = gO.dimDynSize[gO.typesInDim];
    tmp_length = (tmp_length) ? tmp_length : gO.dimActSize[gO.typesInDim];
    // TODO: This should NOT be allowed by GUI.
    if(isNaN(tmp_length)) 
        alert("Number of elements in a struct cannot be dynamic.");

    // 'If' case refers to structure of arrays, 'else' to array of structures.
    // In either case, we have a different way of declaring the TYPE's body,
    // as the difference of SoA vs. AoS is.
    if (Soa) {

        for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

            // We use assumed array sizes, so that we cover the
            // general case including the allocatable grids.
            // In this case we use ':' for each dimension, in
	    // DIMENSIONS().
	    var assumed_size = "";
            for (var t = 0; t < getDimensionString(gO).split(
                 ",").length - 1; t++) {

                assumed_size += ":,";
                        
	    }

            assumed_size += ":";


	    // For SoA, elements are dynamically allocated (since
	    // stack may not be big enough, and to allow passing
	    // of pointers when passign a struct as a parameter).

	    tmp_type_body += dataTypeIntToStr(gO.dataTypes[i]) +
                ", DIMENSION(" + assumed_size + "), ALLOCATABLE :: " + 
	    	gO.dimTitles[gO.typesInDim][i] + "\n"; //DC:

            //tmp_type_body += dataTypeIntToStr(gO.dataTypes[i]) +
            //    ", DIMENSION(" + assumed_size + "), ALLOCATABLE :: " + 
	    //	"dim" + i + "\n"; //DC:

	     // Get dimensions of struct elements, so as to allocate in function.
	    // NOTE: We're using the getDimensionString_C version (which is 
	    //       fine since we are consistent with the method.
	    var tmp_alloc_str = getDimensionString_C(gO);
	    tmp_alloc_str = tmp_alloc_str.replace("][", ","); 
	    tmp_alloc_str = tmp_alloc_str.replace(/^\[|\]$/g, "");
	    if (tmp_alloc_str == "") tmp_alloc_str = 1; 
	    
	    Struct_el_decl += addIndentation(0) + "ALLOCATE(" + 
		typeVariable + "%" + gO.dimTitles[gO.typesInDim][i] + 
		"(" +  tmp_alloc_str   + "));\n" ; //DC:


	    //Struct_el_decl += addIndentation(0) + "ALLOCATE(" + 
	    //	typeVariable + "%dim" + i + "(" +  tmp_alloc_str   + 
	    //	"));\n" ; //DC:

        }

    } else {

        for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

	    tmp_type_body += dataTypeIntToStr(gO.dataTypes[i]) + " :: " +
                gO.dimTitles[gO.typesInDim][i] + "\n"; //DC:

            //tmp_type_body += dataTypeIntToStr(gO.dataTypes[i]) + " :: " +
            //    "dim" + i + "\n"; //DC:

        }

    }

    // We need to change all '('  to '\\(' (same for right parentheses), in 
    // case we have DIMENSIONS(X).
    // TODO: Are there any other cases we need to escape for using in regular 
    // expressions?

    // What we are doing below is search for the body of the TYPE that 
    // corresponds to this variable, and find the name of that corresponding 
    // TYPE. Since the convention in constructing TYPES is to start from dim0 
    // and increase (dimX) and follow the last dimX with END TYPE TYP_<name>,
    // we can parse <name> and use this.
    // This is to avoid redeclaring the same TYPE with the only difference
    // being the grid name.
    //
    // We make sure that each type has a unique body, otherwise the new TYPES 
    // will reuse the old one (similarly search and use on creation on 
    // createTypeString).

    var toSearch = tmp_type_body.replace(/\(/g, '\\(');
   
    toSearch = toSearch.replace(/\)/g, '\\)');
    
    var toSearch2 = "END TYPE TYP_\(.+\)";

    var match_expr = new RegExp(toSearch + toSearch2);
    
    var res = TypeStr.match(match_expr);

    if (res != null) {
       
        typename = res[1];

    } else {
        
        // TODO: We may want to follow a more general convention (another 
	// naming scheme based on IDs).
        typename = gO.caption;

    }

    gridDefinition = "TYPE (TYP_" + typename;

    if (!onlyDataType) {

        if (Soa) {

            gridDefinition += ") " + typeVariable + "\n";

        } else {

	    if (gO.numDims > 1) {

                gridDefinition += "), DIMENSION(:) :: " +
                    typeVariable + "(" + getDimensionString(gO) + ")\n";
	
	    } else {

		gridDefinition += ") " + typeVariable + "("
			getDimensionString(gO) + ");\n";

	    }

        }
   
    }

    if (res == null) {

        TypeStr += "TYPE TYP_" + typename + "\n" + tmp_type_body +
            "END TYPE TYP_" + typename + "\n";

    }

    return gridDefinition;

}
