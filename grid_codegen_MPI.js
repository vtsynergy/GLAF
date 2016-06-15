// At the end see how/if we can parameterize existing code generation
// to only add MPI related constructs when/if needed, otherwise
// fall back to simple code generation.

//TODO: Add free() functionality from grid_codegen_C.js

//MPI: MPI-related global variables
var MPI_Vars;
//MPI: Includes information about if there is at least one global
//     grid in a step of a function
//     Format FuncHasMPIstep[function_id][step_id]
//     CAUTION: main function is in FuncHasMPIstep[2]
var FuncHasMPIstep;

//MPI:
// Used for declaring startX. Scope is per function. This is 
// initialized to blank at the start of EACH function.
var Index_start_decl;

//MPI:
var Index_end_per_dim;

//MPI: Returns 0 if current function includes a global grid in one
//     of its steps. 
function checkFuncHasMPIstep(m, f) {

    var found = 0;
    var fO = m.allFuncs[f];

    for (var s = 0; s < fO.allSteps.length; s++) {

        if (FuncHasMPIstep[f][s] == 1) {
            found = 1;
	    break;
	}

    }

    return found;

}


//----------------------------------------------------------------------------
//MPI:
// Detects if there are any Global or Global and Distributed
// grids in any of the functions of the program.
// If there are, then we need to initiate MPI and declare the
// appropriate helping variables.
// Also, initializes FuncHasMPIstep (see declaration for details).
// Returns 1 or 0 if MPI is needed or not, respectively.
//----------------------------------------------------------------------------
function generateMPIinitCode() {

    var doMPIinit = 0;
    var mO = CurModObj;

    for (var f = mO.FuncStartID; f < mO.allFuncs.length; f++) {

	var fO = mO.allFuncs[f];
	FuncHasMPIstep[f] = new Array();

	for (var s = 0; s < fO.allSteps.length; s++) {

	    for (var g = 0; g < fO.allSteps[s].allGridIds.length; g++) {

	        var gID = fO.allSteps[s].allGridIds[g];
		var gO = fO.allGrids[gID];

		if (gO.isGlobal == true) {

		    doMPIinit = 1;
		    FuncHasMPIstep[f][s] = 1;
		    break; // Once a global grid found, break for cur step.

		} else {

		    FuncHasMPIstep[f][s] = 0;
		    // Continue checking until all grids are finished
		    // or a global grid is found.
		}

	    }

	}


    }

    if (doMPIinit) {
	    MPI_Vars = "int Tag = 1;\n" +
		       "MPI_Status Stat;\n" +
		       "int Num_ranks;\n\n";
    }// else
    //	    alert("MPI needed = " + doMPIinit);
    return doMPIinit;

}


//----------------------------------------------------------------------------
//MPI:
// Save the MPI string generated for the program
// parallel: save parallel version if 1. Serial if 0.
// show: To be passed in showCstr to alert(code) if 1.
// 	 Else, just to return the code string to caller.
//----------------------------------------------------------------------------
function saveMPICstr(parallel, show) {

    // Get the JSON string
    //Regenerate code every time, otherwise may save old one if updated
    TypesAllFuncs = new Array();
    NamesAllFuncs = new Array();
    var str = encodeURIComponent(showMPICstr(1, parallel, show));
    //createAPI(0); //API: 
    //TODO: Is SoA by default (hence '1' as first argument)
    download2SaveFortran(str, CurProgObj.fileName);

}


//----------------------------------------------------------------------------
// Show the MPI C string generated for the program
// strOfArr: Use structures of arrays (SoA) if 1. Arrays of structures (AoS)
// 	     if 0.
// show: Show code in JS (using alert) if 1. Just return code string if 0.
//----------------------------------------------------------------------------
function showMPICstr(strOfArr, parallel, show) {

    Soa = strOfArr;
    ShowParallel = parallel;

    /*
    // If parallel code generation selected first we need to analyze.
    if (ShowParallel) {

        var mO = CurModObj; // TODO: Generalize for multiple modules.
        var fO = mO.allFuncs[getFuncIdByName(mO, "Main")];
        analyzeParallelismAll(fO, 0);
    
    }
    */
    
    TypesAllFuncs = new Array();
    NamesAllFuncs = new Array();
    FuncHasMPIstep = new Array(); //MPI:

    var code = getMPIstr();
    if (show) {

        alert(code);

    } else {

        return code;
    }

}




//----------------------------------------------------------------------------
// Returns C for the current step in current funtion that the
// user is currently working on
//----------------------------------------------------------------------------
function getMPIstr() {

    var mO = CurModObj;

    // First, generate code for all functions, including 'main()'
    // Will be elements of the func_code array.
    var func_code = new Array(); // Used to store code for EACH function
    
    var initMPI = generateMPIinitCode(); //MPI

    // Variable to store all include statements needed.
    var inclStmts = "#include <stdio.h>\n" +
	    	    "#include <stdlib.h>\n";
    
    //MPI:
    if (initMPI) inclStmts += "#include <mpi.h>\n";

    /*
    if (ShowParallel) {

        inclStmts += "#include <omp.h>\n";

    }
    */

    TypeStr = ""; // Used to store TYPEs (i.e., structures).
    Func_prototypes = new Array();   
 
    // A single string that contains all function code.
    var func_code_all = "";

    // TODO: Be careful with global variables. If not initialized every time, 
    // they'll hold the value, until exiting program!
    GID_function = 0;


    for (var f = mO.FuncStartID; f < mO.allFuncs.length; f++) {

	/*
        if (ShowParallel && !funcContainsParStep(mO, f) && !FuncHasSerVer[f])
    		CalledFromSer = 1;
	*/

        func_code[f] = getMPIstr4Func(mO, f);

	/*
	if (ShowParallel)
 	    CalledFromSer = 0;
	*/

	//Note: Be careful where TypesAllFuncs starts != mO.allFuncs[start]
	func_code_all += TypesAllFuncs[f - mO.FuncStartID] + " " +
            func_code[f];

	/*
	// If we are in ShowParallel, we may need a serial only version (in
	// case of a function that contains (even a single) parallel step(s).
	//
	if (ShowParallel && FuncHasSerVer[f]) {
 
	    // We need to change: 
	    // (a) Header - append "_ser".
	    // (b) Remove all OpenMP pragma directives (single lines in C).
	    
	    CalledFromSer = 1;
	    var altSerFunc = func_code[f]; //getCstr4Func(mO, f);
	    altSerFunc = altSerFunc.replace("(", "_ser(");
	    // Regex: From the start of a line match as less as possible 
	    // before finding "#pragma omp" (this includes tabs), then match
	    // as less as possible until finding a newline. Do this for EACH
	    // line of the string (m specifier).
	    altSerFunc = altSerFunc.replace(/^.*?#pragma omp.*\n?/mg, "");
	    func_code_all += TypesAllFuncs[f-mO.FuncStartID] + " " + altSerFunc;
	
	    CalledFromSer = 0;
	}
	*/

    }

    // Generate code for main method.
    var main_call = "int main(int argc, char *argv[]) {\n";

    //TODO:C: Use startup arguments grid as *argv[]. Add more flexibility.
    main_call += addIndentation(0) + "char *" +
        var2C(CurModObj.allFuncs[DefMainFuncInd].allGrids[1].caption)+"[4];\n";

     main_call += addIndentation(0) + "int " +
        var2C(CurModObj.allFuncs[DefMainFuncInd].allGrids[0].caption) + ";\n";

    // If generating parallel implementation, setting nested parallelism off
    // by default.
    // TODO: This may be an option for the auto-tuner.
    /*
    if (ShowParallel) {
	    
        main_call += addIndentation(0) + "omp_set_nested(0);\n";

    }
    */


    //MPI: Init MPI if MPI program
    //     Can be called at most one per MPI program.
    if (initMPI) {

        main_call += addIndentation(0) + "MPI_Init(&argc,&argv);\n" +
		     addIndentation(0) +
		     "MPI_Comm_size(MPI_COMM_WORLD, &Num_ranks);\n"

    }


    main_call += addIndentation(0) +
        var2C(CurModObj.allFuncs[DefMainFuncInd].allGrids[0].caption) +
        " = " + var2C(DefMainFuncName) +
        "(" + var2C(CurModObj.allFuncs[DefMainFuncInd].allGrids[1].caption) +
        ");\n";

    //MPI: Finalize MPI if MPI program
    if (initMPI) {

        main_call += addIndentation(0) + "MPI_Finalize();\n";

    }

    main_call += "}\n";

    // Check if any math functions have been called in the program, so as to 
    // add the math library in the include statements.
    if (InclMath) {
  
        inclStmts += "#include <math.h>\n\n";

    } else {
    
    	inclStmts += "\n";

    }

    // Construct function prototypes
    var func_protos = "";
    for (var i = mO.FuncStartID; i < mO.allFuncs.length; i++) {

        func_protos += TypesAllFuncs[i - mO.FuncStartID] + " " + 
		       Func_prototypes[i - mO.FuncStartID];

	/*
	// If we are in ShowParallel, and we have a serial only version (in
	// case of a function that contains (even a single) parallel step(s).
	// we will need its prototype, too.
	if (ShowParallel && FuncHasSerVer[i]) {
 
            func_protos += TypesAllFuncs[i - mO.FuncStartID] + " " + 
		       Func_prototypes[i - mO.FuncStartID].replace("(",
				       "_ser(");
	    
	    
	}
	*/

    }
    func_protos += "\n\n";


    // Final generated code will contain the TYPEs code, the library functions
    // code (e.g., for read/write CSV), the functions' code (that contains
    // all functions, including ft_Main), and the PROGRAM "int main" that calls
    // Main function and subsequently any other functions.
    // MPI: Added MPI_Vars
    var returnedCode = inclStmts + MPI_Vars + TypeStr + func_protos + 
		       func_code_all + "\n" + main_call;

    returnedCode = returnedCode.replace(/UNIQUEIND/g, "int"); //TT

    return returnedCode;

}


function getMPIstr4Func(mO, f) {

    var fO = mO.allFuncs[f];
    
    // This is the GID_function id of current function corresponding to 
    // TypesAllFuncs and NamesAllFuncs.
    var gID_f = 0;
    
    // Initialize current step numbering for current function to zero.
    CurStep = 0;
    
    // Initialize GridsInFunc.
    GridsInFunc = new Array();
    Loop_var_per_dim = new Array();
    Index_start_per_dim = new Array(); //MPI:
    Index_end_per_dim = new Array();

    if (var2C(fO.funcCallExpr.str) == "ft_Main") {
	    
        // Add an INTEGER as main's return value (TypesAllFuncs[0])
        TypesAllFuncs.push("int");
        //gID_f = 1;	//NEXT one
        GID_function = 1; //NEXT one

    } else {
        
        for (var i = 0; i < TypesAllFuncs.length; i++) {

            if (NamesAllFuncs[i] == var2C(fO.funcCallExpr.str)) {
                
		//alert("FOUND in i=" + i + "Callee="+NamesAllFuncs[i]  + 
                //"type="+TypesAllFuncs[i]);
                //TODO: (check) Add a dummy type entry for function 
                //return value. Will be updated later 
                //(TypesAllFuncs[gID])
                TypesAllFuncs.push(TypesAllFuncs[i]);
                gID_f = i; //Set, so it can be used to assign the 
                //required type to temp return value.
            
	    }

        }

    }

    // Used for recording functions once (and then recording
    // their types and names to be used in other places of
    // code generation. 
    // Initialize to blank at the start of each new function
    Func_decl = "";

    // See their declaration (global scope) for details on below:
    Row_col_decl = "";
    Index_start_decl = ""; //MPI:
    Index_end_decl = "";
    Grids_new_decl = "";
    TitleDefs_decl = "";

    // Create function header (function type plus name). Arguments to be
    // added in subsequent steps.
    // TODO:C: Commented out.
    var func_head =  /*getDataTypeString_C(fO.allGrids[0],null) + " " +*/ 
	    var2C(fO.funcCallExpr.str) + "(";
	
    // In func_vars we declare the type and name of any grids that were
    // passed as parameters in the current function for which we are 
    // generating code.
    var func_vars = "";

    var func_val_init = "";

    var arr_dynValues = new Array();
	
    // Add argument list to function header. To do that, go through ALL
    // grids in the function and add thos who are incoming args.
    for (var g = 0; g < fO.allGrids.length; g++) {

        var gO = fO.allGrids[g]; 

        // TODO: If a grid with specific indices e.g. array[3][1] treat as 
        // passed by value!
        if (gO.inArgNum >= 0) { // This grid is an incoming arg

            if (gO.numDims > 1 && gO.typesInDim != -1) {

		if (gO.inArgNum > 0) func_head += ", "; // arg separator

	    	func_head += getDataTypeString_C(gO) + " ";

		if (!Soa) func_head += " *";
                //TODO: CAUTION: This is for the case of TYPE variable
                //	passed using its name (i.e., no specific element).
                func_head += "typvar_" + gO.caption;

            } else {

		if (gO.numDims >= 1) { 

		    if (gO.inArgNum > 0) func_head += ", "; // arg separator

	    	    func_head += getDataTypeString_C(gO) + " ";

                    func_head += "*" + var2C(gO.caption); 
		    // Grid caption as arg name 

	    	} else {

		    // Ensure has not been implicitly declared via a
		    // dynamically sized non-scalar grid (see below).
		    if (func_head.indexOf(var2C(gO.caption))==-1){ 
		        
			if (gO.inArgNum > 0) func_head += ", "; // arg separator

	    		func_head += getDataTypeString_C(gO) + " ";
			func_head += var2C(gO.caption);
		    
		    }
		    // else do not declare at all.

		}

            }

	    if (gO.numDims >=1) {
         	
		// Given we dynamically allocate all non-scalar grids, we
		// need to pass dimensions that are variables (the constant
		// dimensions are auto-generated as numbers in resulting
		// code). They are also needed to be used in loops (in 
		// Fortran we could get this info using SIZE(), but in C
		// there is nothing similar).
		var dynVals = ", ";
		for (var i = 0; i < gO.dimActSize.length; i++) {

		    if (gO.dimDynSize[i] != null) {
			// The second check takes care of scalars that may have
			// been passed as parameters (i.e., explicitly), while
			// the first is for implicit passing (scalar grids used
			// for dynamic size of non-scalar grids).
		        if(arr_dynValues.indexOf(var2C(gO.dimDynSize[i]))==-1 &&
			   func_head.indexOf(var2C(gO.dimDynSize[i]))==-1) {
			    
			    arr_dynValues.push(var2C(gO.dimDynSize[i]));
			    dynVals += "int "+var2C(gO.dimDynSize[i]) + ",";
		            
			}
			    
		    }

		}

		if (dynVals != ", ") {

		    func_head += dynVals.replace(/,+$/, "");;
		    
		}
	    } else { //KK:FUN
		
		// For scalar-grids, we have to copy the src value into a temp
	   	// variable called fun_<src_var_name>  
		// TODO: Can fuse with earlier similar loop for function 
		// header.  
		func_vars += addIndentation(0) + getDataTypeString_C(gO,
                        null) + " fun_" + gO.caption + ";\n";

                func_val_init += addIndentation(0) + "fun_" + gO.caption +
                    " = " + var2C(gO.caption) + ";\n";

	    }

        }

    }
    
    func_head += ")";

    Func_prototypes.push(func_head + ";\n");

    func_head += " {\n";

    // At this point we have completed in func_head the function header
    // that contains the type of function, the function name, and its
    // arguments contained in parentheses.


    //MPI: If current function has been found to include at least an
    //     MPI step, then we need to define a variable indicating
    //     each rank's id.
    var funcID = getFuncIdByName(mO, fO.funcCallExpr.str);
    if (checkFuncHasMPIstep(mO, funcID)) {

	    func_vars += addIndentation(0) + "int myRank;\n" +
		         addIndentation(0) + 
			 "MPI_Comm_rank(MPI_COMM_WORLD,&myRank);\n";

    }



    // Used for declaration of the ret value declaration within the func.
    // This is always in position 0 in fO.allGrids[].
    func_vars += addIndentation(0) + getDataTypeString_C(fO.allGrids[0],
            null) + " " + var2C(fO.allGrids[0].caption) + ";\n";

    // STEP: Code for each step in the function
    //
    var step_code = "";
    //
    var stepStart = 1; // Note: Function prototype at step 0. 
    //
    for (var s = stepStart; s < fO.allSteps.length; s++) {

        step_code += getMPIstr4Step(fO, fO.allSteps[s], mO); //MPI

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
    // j) return value assignment to function name
    // k) end function statement
    var function_string = func_head;
    function_string += func_vars + //KK:FUN //MPI: Added Index_start_decl
        Row_col_decl + Index_start_decl + Index_end_decl + TitleDefs_decl + 
	Grids_new_decl +
        func_val_init + step_code;
    
    // If no return statement added (e.g., when "void"), we return 1 (since 
    // ALL Grid Language functions need a return type).
    //if (function_string.indexOf("return") == -1) {

    //	function_string += addIndentation(0) + "return 1;\n";

    //}

    function_string += "}\n\n";

    return function_string;


}



function getMPIstr4Step(fO, sO, mO) {

    var grids = "";
    var titleDefs = "";
    Struct_el_decl = ""; 

    // Used to keep track of which DO loops are parallel, so we
    // can close them appropriately in the reverse order.
    var stepOmpDoStack = new Array();

    // Increase step ID (within a function- across functions this is 
    // re-initialized to zero)
    CurStep++;

    var allocatablesOfStep = "";

    var funcID = getFuncIdByName(mO, fO.funcCallExpr.str);

    // Go through all grids in the step and declare as needed fields.
    for (var g = 0; g < sO.allGridIds.length; g++) {

        var gId = sO.allGridIds[g];
        var gO = fO.allGrids[gId];

        if ((gO.inArgNum < 0) && (!gO.isRetVal)) {

            // If grid has been already declared within THIS function, 
            // do not re-declare
            if (!gridDeclaredInFunc(gId)) {

                var newgrid = createTypeString_C(gO); 
		// Get the declaration of current grid
                var isAllocGrid = 0;

                // Get what is inside the parentheses (i.e., the dimensions).
		// Can be null if it is a scalar grid.
                var dimensions = newgrid.match(/\[(.+)\]/);

                if (dimensions != null) {

        	    // Check all dims. If EVEN one dynamic is found, we need
	    	    // to address the array as a 1D dynamically allocated.
	    	    // dimActSize length always has length equal to the 
	    	    // dimensions, even if their value is not the one
	    	    // used (but rather a dynamic size if applicable).
	    	    // TODO:C: Doesn't cover STRUCTS with dynamic num
	    	    // of elements.

		    newgrid = newgrid.replace(dimensions[0], "");
	   
                    var split_dims = dimensions[1].replace(/ /g, "");
                    split_dims = split_dims.split("][");

                    for (var k = 0; k < split_dims.length; k++) {

			// If dimension is not a number, this means we may be 
			// using
			// a variable as a dimension.
                        if (isNaN(split_dims[k])) {

                            // Only exception if we have a TYPE declared which
			    // is NOT allocatable.
                            // TODO: If we decide to allow tables of structs 
			    // later this will need to be reconsidered.
                            if (split_dims[k].indexOf("TYP_") == -1 &&
                                split_dims[k].indexOf("DIMENSION") == -1) {

                                isAllocGrid = 1;
                                //split_dims[k] = var2C(split_dims[k]);

                            }

                        }

                    }

		    // C pointer to be malloc'd
		    // ...and do actual allocation by calling "malloc".

		    var tmp_type;
		    if(gO.numDims > 1 && gO.typesInDim != -1)
		        tmp_type = findTypeVarType_C(gO,1);
		    else
			tmp_type = getDataTypeString_C(gO, null);

		    var tmp_dim_alloc = "";
		    for(var i = 0; i < split_dims.length - 1; i++)
		        tmp_dim_alloc += split_dims[i] + "*";
			
		    tmp_dim_alloc += split_dims[split_dims.length - 1];
		
		    var gridNam;
		    if(gO.typesInDim == -1)
		        gridNam = var2C(gO.caption);
		    else
		        gridNam = "typvar_" + gO.caption;

		    //MPI:
		    //if (!checkFuncHasMPIstep(mO, funcID)) {
		    if (!(gO.isGlobal && gO.isDistributed)) {
                        allocatablesOfStep += addIndentation(0) +
                            gridNam + " = " +
                            "(" +  tmp_type + " *)malloc(sizeof(" +
                            tmp_type + ")*" + tmp_dim_alloc + ");\n";

		    } else {

		        allocatablesOfStep += addIndentation(0) +
                            gridNam + " = " +
                            "(" +  tmp_type + " *)malloc(sizeof(" +
                            tmp_type + ")*(" + tmp_dim_alloc + 
			    "/numRanks)" +
			    ");\n";

		    }

                }

		// Commenting
		if(gO.comment != null) 
		    grids += addIndentation(0) + "// " + gO.comment + "\n";
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
            //titleDefTmp = titleDefTmp.replace(";", "");
            titleDefTmp = titleDefTmp.replace(/_/g, gO.caption + "_");
            titleDefTmp = "int " + titleDefTmp;

	    // If it has been declared already, do not re-declare.
            if (TitleDefs_decl.indexOf(titleDefTmp) == -1) {

                TitleDefs_decl += addIndentation(0) + titleDefTmp;

            }

        }

    }


    // STEP: Create C for loops (a loop for each index var)
    // Note: rangeExpr.exprArr[] contains root range expressions
    //
    var rangeExpr = sO.boxExprs[CodeBoxId.Range];
    var loop_close = "";

    var forstr = ""; 
    // In non-parallel the above variable stores all loops, 
    // in parallel code generation it only stores paral/ble ones.
    
    var forstr2 = ""; 
    // Used in parallel code generation to store non-parallelizable loops

    var collapsed_loop_vars = ""; 
    // Used in parallel code generation, to store all loop variables, 
    // so that no endv assignments between collapsed DO loops.

    var private_vars = ""; 
    // Used in parallel code generation to store loop-private variables 
    // (e.g., LET variables).
    
    // Used for indentation purposes (adding extra, when needed - when we have
    // foreach loop over one or more dimensions, otherwise it is zero).
    var index_extra_indent = 0;


    // if there are index variables
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
            var ivar = var2C(rexpr.labelExpr.str);

            //MPI: Will only be used later if grid Global + Distr. + Ext. dim
	    var startv = var2C(rexpr.exprArr[RangeFields.Start].str);
	    
	    // Define the value of literal 'end'
            // var endv = var2C(DefEndName + rexpr.selDim);
	    //MPI: Bug fix (for generality for MPI)
	    var endv = var2C(rexpr.exprArr[RangeFields.End].str);


            //TODO: Why did I change the below to the above?			
            //var endv = expr2Cstring(rexpr.exprArr[RangeFields.End]);

            // IMPORTANT NOTE: semasiology is SAVE if assignment 
            // is at the time of declaration. Saved between function calls. 
            // That's why here we separate declaration and initialization.

	    // We do not allow re-declaration of the same start/end variable 
	    // across steps. 
	    if (Index_start_per_dim.indexOf(endv) == -1) {

                Index_start_decl += addIndentation(0) + "int " +
                    startv + ";\n";
                Index_start_per_dim.push(startv);

            }	    
	    if (Index_end_per_dim.indexOf(endv) == -1) {

                Index_end_decl += addIndentation(0) + "int " +
                    endv + ";\n";
                Index_end_per_dim.push(endv);

            }


            // Pick the end value based on whether the size of the dim is
            // variable (dynamically allocated) or not.
            //
            var actendval = rexpr.gO.dimActSize[rexpr.selDim];
            var dynendval = rexpr.gO.dimDynSize[rexpr.selDim];

	    //MPI:
	    var endval;
	    if (dynendval == null) {

	        if (rexpr.gO.isGlobal && rexpr.gO.isDistributed && 
				rexpr.gO.dimIsExtended[iv]) {

		    endval = actendval + "/numRanks";		    

		} else {

		    endval = actendval;

		}

	    } else {

	        endval = var2C(dynendval); 

	    }
            //var endval = (dynendval) ? var2C(dynendval)
            //     : actendval;


	    // In parallel code generation, if loop is parallel, we append the
	    // endval to the collapsed loop vars, else we declare it normally
	    // at its normal position (whereas collapsed loop vars appear 
	    // BEFORE the OMP PARALLEL DO COLLAPSED(X) directive).
            if (ShowParallel) {

                if (Pragma_str[funcID][CurStep].indexOf(ivar) != -1) {

                    collapsed_loop_vars += addIndentation(0) + endv +
                        " = " + endval +
                        "-1;\n";

                } else {

                    forstr2 += addIndentation(iv) + endv + " = " + endval +
                        "-1;\n";

                }

            } else {

		//MPI:
		if (rexpr.gO.isGlobal && rexpr.gO.isDistributed && 
				rexpr.gO.dimIsExtended[iv])
		    forstr += addIndentation(iv) + startv + " = 0;\n"; 

                forstr += addIndentation(iv) + endv + " = " + endval +
                    "-1;\n";

            }

            if (Loop_var_per_dim.indexOf(ivar) == -1) {

                // Adding the names of index variables to the declarations 
                // string if it has NOT been declared for this dimension in 
                // THIS function and push it in the array denoting it has 
                // been now declared.
                Row_col_decl += addIndentation(0) + "int " + ivar +
                    ";\n";
                Loop_var_per_dim.push(ivar);

            }


            // Step: Start/End/Step expressions
            var start = expr2Cstring(rexpr.exprArr[RangeFields.Start]);
            var end = expr2Cstring(rexpr.exprArr[RangeFields.End]);
            var step = expr2Cstring(rexpr.exprArr[RangeFields.Step]);
	
            if (ShowParallel) { 
	        // ShowParallel check is implied 0 if collapse is > 0 
		// (safely delete check).

		// If loop is parallel over ivar:    
                if (Pragma_str[funcID][CurStep].indexOf(ivar) != -1) {

                    if (collapse_int != 0) {

                        forstr += addIndentation(0) +
                            "#pragma omp parallel for collapse(" + collapse +
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

                    //forstr +="for (" + ivar + " = " +
                    //    start + "; " + ivar + " <= " + end + "; " +
                    //    ivar + " += " + step + ") {\n";

		    forStrArr.push(new Array(ivar, start, end, step));

                } else {
		    // Normal (non-parallel) DO loop:

		    // TODO: Will be needed in allowing multiple combinations
		    // of collapsing/non-collapsing in auto-tuner.	
                    //stepOmpDoStack.push("");
                    forstr2 += addIndentation(iv) + "for (" + ivar + " = " +
                        start + "; " + ivar + " <= " + end + "; " +
                        ivar + " += " + step + ") {\n";
                  
                }

            } else { // Non-parallelized version: all DO loops, normal code:

		//forstr += addIndentation(iv) + "for (" + ivar + " = " +
                //    start + "; " + ivar + " <= " + end + "; " +
                //    ivar + " += " + step + ") {\n";

		forStrArr.push(new Array(ivar, start, end, step));

            }

        }


	// TODO: CAUTION: For (ShowParallel == 1, parallelizable step) and
	// (ShowParallel == 0), we do it in forstr variable.
	// For both, the assumption is that an index variable CANNOT
	// be allowed to be used as a boundary variable in ANOTHER loop, or step
	// or start condition!
	// TODO: CAUTION: When we allow this in the GUI, will need to take care.	// TODO: CAUTION: Need ordering for forst2, too, when exists. 
	// VERIFY SAFETY IS ENFORCED (for serial, need to check if independent?)

	// Loop re-ordering to be suitable to language's array storage format.
	// indX, row, col: for C
	// indX, col, row: for Fortran
	
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
	// (b) the second in [r]ow, [c]olumn (for C row-major-friendly
	// format).
	higherDims.sort();
	higherDims.reverse();
	forStrArr.sort();
	forStrArr.reverse();

	// Join the appropriately sorted arrays.
	forStrArr = higherDims.concat(forStrArr);

	// Create the FOR loops' string in this order:
	for (var i = 0; i < forStrArr.length; i++) {

	    forstr += addIndentation(i) + "for (" + forStrArr[i][0] + 
	        	" = " +
	    		forStrArr[i][1] + "; " + forStrArr[i][0] + 
			" <= " + forStrArr[i][2] + "; " +
                        forStrArr[i][0] + " += " + forStrArr[i][3] + ") {\n";

	}

	

        // Now that we have scanned all DO loops and know the appropriate
        // closing pairs (which ones correspond to OMP DO loops):
        for (var iv = 0; iv < num_index_vars; iv++) {

            if (ShowParallel) {

                loop_close += addIndentation(num_index_vars - iv - 1) +
                    "}\n";

                if (collapse && iv == num_index_vars - 1) {

                    //Only for last one and only if collapse
                    loop_close += addIndentation(0) +
                        "\n";

                }

		// TODO: May be needed in autotuning multiple combinations
		// of collapsing/non-collapsing loops. 
                //Here need to close the appropriate OMP DO loop.
                //loop_close += stepOmpDoStack.pop();

            } else {

                loop_close += addIndentation(num_index_vars - iv - 1) +
                    "}\n";

            }

        }

    } else if (rangeExpr && rangeExpr.isForever()) {

        forstr += addIndentation(0) + "while(1) {\n";
        loop_close = addIndentation(0) + "}\n";

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
        //       from previous, close braces
        //
        prev_indent = indent;
	if (mask_unmatched == 0) {
	    indent = 1; // This is only in the case where we have an empty IF
	} else {
            indent = sO.boxAttribs[box].indent;
	}

        var boxexpr = sO.boxExprs[box];

        // Step: handle mask statment if/else/elseif/breakif
        //
        if (sO.boxAttribs[box].isMask()) {

            if (boxexpr.isElse()) {

                // 'else' has no expression between ()

                var tmp2 = last_if_indent;
                for (var i = indent; i < tmp2; i++) {

                    stmt += addIndentation(last_if_indent +
                        index_extra_indent) + "}\n";
                    mask_unmatched--;
                    last_if_indent--;
                    
                }

                stmt += addIndentation(last_if_indent +
                    index_extra_indent) + "} else {\n";

		// Commenting
		if (sO.boxAttribs[box].comment != null) 
		    stmt += addIndentation(last_if_indent +
                    	    index_extra_indent) + "// " + 
			    sO.boxAttribs[box].comment + "\n";

            } else if (boxexpr && boxexpr.exprArr && boxexpr.exprArr.length) {

                // condition with child expression  -- if/elseif/breakif
                //

                if (boxexpr.isIf()) {

                    mask_unmatched++; // Increase unmatched if stmts
                    
                    var tmp2 = last_if_indent;

                    for (var i = indent; i <= tmp2; i++) {

                        stmt += addIndentation(last_if_indent +
                            index_extra_indent) + "}\n";
                        mask_unmatched--;
                        last_if_indent--;

                    }

                    stmt += addIndentation(indent + index_extra_indent) +
                        "if" + "(" + expr2Cstring(boxexpr) +
                        ") {\n";

		    // Commenting
		    if (sO.boxAttribs[box].comment != null) 
		        stmt += addIndentation(indent + index_extra_indent) + 
				"// " + sO.boxAttribs[box].comment + "\n";   

                    last_if_indent = indent; // So that we can close it later
                    
                } else if (boxexpr.isElseIf()) {

                    var tmp2 = last_if_indent;
                    for (var i = indent; i < tmp2; i++) {

                        stmt += addIndentation(last_if_indent) +
                            "}\n";
                        mask_unmatched--;
                        last_if_indent--;

                    }

                    stmt += addIndentation(indent + index_extra_indent) +
                        "} else if" + "(" + expr2Cstring(boxexpr) +
                        ") {\n";
                    
		    // Commenting
		    if (sO.boxAttribs[box].comment != null) 
			stmt += addIndentation(indent + index_extra_indent) + 
				"// " + sO.boxAttribs[box].comment + "\n";

                } else {
                    // TODO: What is this case doing? (breakif)
                    stmt += addIndentation(indent + index_extra_indent) +
                        boxexpr.str + "(" +
                        expr2Cstring(boxexpr) + ") {\n";
                    alert(boxexpr.str);
                }

            } else {

                stmt += "";
                //TODO: This is not correct? Delete else 
                //altogether?

            }

        } else {

            // Step: Process a formula statement

	    // Commenting
	    if (sO.boxAttribs[box].comment != null) 
	        stmt += addIndentation(indent + index_extra_indent) + 
                        "// " + sO.boxAttribs[box].comment + "\n";  

            // TODO: Do some more checking on the following. May be trickier 
            // than that
            if (sO.boxAttribs[box].indent <= last_if_indent) {

                for (var i = 0; i <= (last_if_indent - sO.boxAttribs[box]
                        .indent); i++) {

                    stmt += addIndentation(last_if_indent +
                        index_extra_indent) + "}\n"
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
                    var wholestr = expr2Cstring(boxexpr);
                    var pre_str = wholestr.substring(0, wholestr.indexOf(
                        boxexpr.exprArr[1].str) - 1);
                    var post_str = wholestr.substring(wholestr.indexOf(
                        "+=") + 3);

                    currFormLine = addIndentation(indent + index_extra_indent) +
                        pre_str + " = " + pre_str + " " +
                        boxexpr.exprArr[1].str.substring(0, 1) + " " +
                        post_str + ";\n";

                } else {

                    currFormLine = addIndentation(indent + index_extra_indent) +
                        expr2Cstring(boxexpr) + ";\n";

                }

                // In current formula box: Loop through the exprArr[] 
                // array and detect if there are functions (isFuncCall())
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
                        if (Func_decl.indexOf(var2C(boxexpr.exprArr[
                                lp].str) + ";\n") == -1) {

                            // Used for data type declaration of function in 
			    // caller.
                            Func_decl += addIndentation(0) +
                                getDataTypeString_C(mO.allFuncs[f].allGrids[0],
                                null) + " " +
                                var2C(boxexpr.exprArr[0].str) + ";\n"; 

                            // TODO: This means that
                            // the function will have a limited scope (within 
                            // current function-caller, declared there). Is 
			    // that OK? 

                            TypesAllFuncs[GID_function] =
                                getDataTypeString_C(
                                    mO.allFuncs[f].allGrids[0], null);

                            NamesAllFuncs[GID_function] = var2C(
                                boxexpr.exprArr[lp].str);

                            GID_function++; 
			    //Increase ID to represent next called function.

                        }


			// Called function "version": Serial or parallel, 
			// depending on whether CURRENT step of CURRENT 
			// function is parallel or not (to avoid nested 
			// parallelism).
			
			var funcCallName = expr2Cstring(boxexpr.exprArr[lp]);
			if (ShowParallel) {
		
			    var funcC = boxexpr.exprArr[lp].str;
		            var calleeFuncId = getFuncIdByName(mO, funcC);
		            if (funcContainsParStep(mO, calleeFuncId)) {

			        if (Pragma_str[funcID][CurStep] != "" || 
				    CalledFromSer) {

				    var wholeCall = expr2Cstring(
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

                // Check if return statement in position 0 (can't be 
                // anywhere else)

                var return_expression = "";
                var ret_val_assignment = "";

                // Parse the expression to be returned and save in order to
                // assign to the function's name (i.e., value to be returned)
                for (var i = 1; i < boxexpr.exprArr.length; i++) {

                    if (i > 0) return_expression += " ";

                    return_expression += expr2Cstring(boxexpr.exprArr[i]);

                }

                if (boxexpr.exprArr.length != 1) {

                    ret_val_assignment = "return " + return_expression;

                } else {

                    ret_val_assignment = "return " + 
			    var2C(fO.allGrids[0].caption);

                }

                stmt += addIndentation(indent + index_extra_indent) +
                    ret_val_assignment + ";\n";

            } else {

                //alert("DEAD CODE?/incomplete statement?");

		// LET statement:    
                if (boxexpr !== undefined && boxexpr.exprArr[0] !==
                    undefined && boxexpr.exprArr[0].isLet()) {

                    var let_data_type = findLetTypeCont(boxexpr, 0, mO); 
		    // findLetTypeCont returns FORTRAN-style data types, so we 
		    // need to "pipe" through dataTypeIntToStr_C after we get 
		    // the (language-independent) data type integer 
		    // representation of the framework.
		    let_data_type = dataTypeStrToInt(let_data_type);
		    let_data_type = dataTypeIntToStr_C(let_data_type);

                    grids += addIndentation(0) + let_data_type + " " +
                        var2C(boxexpr.exprArr[0].exprArr[0].str) +
                        ";\n";
		    
                    if (ShowParallel) {
			    
			private_vars += var2C(boxexpr
                        .exprArr[0].exprArr[0].str) + ", ";

		    }

                    var let_RHS = "";

                    for (var let_ass = 1; let_ass < boxexpr.exprArr.length; let_ass++) {

                        let_RHS += expr2Cstring(boxexpr.exprArr[
                            let_ass]);

		    }

                    stmt += addIndentation(indent + index_extra_indent) +
                        var2C(boxexpr.exprArr[0].exprArr[0].str) +
                        " = " + let_RHS + ";\n";
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
                    if (Func_decl.indexOf(var2C(boxexpr.exprArr[0].str) +
                            ";\n") == -1) { //TODO: C: CAUTION: Check 

                        // Used for data type declaration of function in 
			// caller.
                        Func_decl += addIndentation(0) +
                            getDataTypeString_C(mO.allFuncs[f].allGrids[0],
                                null) + " " +
                            var2C(boxexpr.exprArr[0].str) + ";\n";


                        // Used for declaration of type in header of function.
                        TypesAllFuncs[GID_function] = getDataTypeString_C(
                            mO.allFuncs[f].allGrids[0], null);

                        NamesAllFuncs[GID_function] = var2C(
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

                        grids += addIndentation(0) + getDataTypeString_C(mO
                                .allFuncs[f].allGrids[0], null) + " " +
                            "res_" + boxexpr.exprArr[0].str + ";\n";

                    }

		    // Called function "version": Serial or parallel, depending
		    // on whether CURRENT step of CURRENT function is parallel
		    // or not (to avoid nested parallelism).
		   
		    var funcCallName = expr2Cstring(boxexpr);
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
				var wholeCall = expr2Cstring(boxexpr);
				funcCallName = wholeCall.replace("(", 
						"_ser(");
			        // Mark that callee needs a SERIAL version.
			        FuncHasSerVer[calleeFuncId] = 1;

		            }

		        }

		    }
		  
                    stmt += addIndentation(indent + index_extra_indent) +
                        "res_" + boxexpr.exprArr[0].str + " = " +
                        funcCallName + ";\n";

                } else { 
		
		    // Anything else (including library functions - taken care
		    // within expr2Cstring).

                    stmt += addIndentation(indent + index_extra_indent) +
                        expr2Cstring(boxexpr) + ";\n";

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
                "}\n";
        }
    }

    // Add grids declared in current step to Grids_new_decl to be added
    // in function's declarations' code section.
    Grids_new_decl += grids;

    var assignment_reduction = "";
    var assignment_red_rvs = "";
    var red_name = ""; 
    var res = "";

    // Add OMP PRIVATE clause, for parallel version, if we have a
    // OMP PARALLEL DO directive AND private variables.
    if (ShowParallel) {

        // If we don't have a parallel loop, then do not use private.
        if (forstr.length != 0 && private_vars != "") {
            
            //TODO: Add indentation for private_vars.
            private_vars = private_vars.replace(/, +$/, "");
            private_vars = "private(" + private_vars + ")"
            forstr = forstr.replace(/\n/, " " + private_vars + "\n");

        }
	
	// If we don't have a parallel loop, then do not use private.
        if (forstr.length != 0 && Pragma_reduction[funcID][CurStep] != "") {
            
            // Save pragma_reduction global to local, make small leters, 
            // remove preceding "!$ omp " and trailing \n.
            var tmp_reduction_pr = Pragma_reduction[funcID][CurStep];
            
            var match_expr = new RegExp(": \(.+\)");
            res = tmp_reduction_pr.match(match_expr);
            res = res[1].replace(")", ""); 
	    // TODO: CAUTION: Need to take care of multiple vars/ops case.
            var res2 = res.replace("ft_", ""); // To search for this one.
            for (var g = 0; g < sO.allGridIds.length; g++) {

       	        var gId = sO.allGridIds[g];
       		var gO = fO.allGrids[gId];
       		if(gO.caption == res2) {

		    //alert("FOUND:" + gO.caption);
		    if (gO.numDims > 0) {

		        red_name = "tmp_" + gO.caption;
			assignment_reduction = addIndentation(0) + 
				getDataTypeString_C(gO,null) + " " + 
				red_name + "=";
			//alert("IT IS NON_SCALAR " + assignment_reduction);

		    }

       		}

            }
            tmp_reduction_pr = tmp_reduction_pr.replace("!$OMP REDUCTION", 
			       "reduction");
            tmp_reduction_pr = tmp_reduction_pr.replace("\n", "");
            if(assignment_reduction != "")
            	tmp_reduction_pr = tmp_reduction_pr.replace(res, red_name);
            forstr = forstr.replace(/\n/,  
		     " " + tmp_reduction_pr + "\n");
            //TODO: Add indentation for private_vars.
            
        }

    }

    if(assignment_reduction != "") {
        var match_expr = new RegExp(res + "\\[.+?\\]", "g");
	var res1 = stmt.match(match_expr);
	assignment_reduction += res1[0] + ";\n";
	assignment_red_rvs = addIndentation(0) + res1[0] + "=" + 
				red_name + ";\n";
	stmt = stmt.replace(match_expr, red_name); 
	// Replace with temporary scalar variable for reduction.
    }

    // STEP: Combine all the above components together:
    // a) Loop variables' initialization from collapsed loops.
    // b) Allocatable variables of step.
    // c) DO/OMP DO loops (their combinations depending on the case).
    // d) Actual step computation code.
    // e) Loop closing clauses (serial and/or parallel).

    // Commenting
    var code = (sO.title == null) ? "" : addIndentation(0) + "// " + 
	    sO.title + "\n"; 
    code += (sO.stepComment == "") ? "" : addIndentation(0) + "// " + 
	    sO.stepComment + "\n";
    code += allocatablesOfStep + Struct_el_decl + collapsed_loop_vars + 
	    assignment_reduction + forstr + forstr2 + stmt + loop_close + 
	    assignment_red_rvs;

    return (code);
}


