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
//Purpose: Grid Language C Code Generation
//Author : Konstantinos Krommydas
//Date   : November 11, 2014
//----------------------------------------------------------------------------

// Saves the allocation string for the elements of all structs of a step. It is
// reinitialized per step.
var Struct_el_decl;

// Variable to indicate if any math libarary's function has been used, so as
// to include the library and appropriate flag for compilation.
var InclMath;

// Variable to save the strings for all function prototypes (needed in C, since
// we keep everything into a single C file - i.e., no H/library file).
var Func_prototypes;

//DC99:
// Holds the code for used library functions for C code generation
var LibFunctionsCode_C;

//DC99:
//TODO: Add support for the LARGE test file.
var LibLoadCSR_C =
    "void readCSR(int neq, int nb, int* iam, int* jam, double* a_off, " + 
    		 "double* a_diag_lu, double* dq, double* dqc, double* res, " +  
		 "int* color_indices) {\n" +
    "	int k;\n" +
    "	int nnz, row;\n" +
    "	// Use sample distribution for random generated matrix\n" +
    "	iam = (int *)malloc(sizeof(int)*(neq+3));\n" +
    "	if (iam == NULL)\n" +
    "	    printf(\"Error Allocating iam\\n\");\n" +
    "	FILE* file_ptr;\n" +
    "	// Read iam file\n" +
    "	if (neq == 6309) {\n" +
    "		file_ptr = fopen(\"iamS.txt\", \"r\");\n" +	
    "		for (row = 0; row < 1052; row++) {\n" +		
    "			k = row * 6;\n" +
    "			// Reads 6 values from current line into iam[]\n" +
    "			fscanf(file_ptr, \"%d\", &iam[k]);\n" +
    "			fscanf(file_ptr, \"%d\", &iam[k+1]);\n" +
    "			fscanf(file_ptr, \"%d\", &iam[k+2]);\n" +
    "			fscanf(file_ptr, \"%d\", &iam[k+3]);\n" +
    "			fscanf(file_ptr, \"%d\", &iam[k+4]);\n" +
    "			fscanf(file_ptr, \"%d\", &iam[k+5]);\n" +
    "		}\n" +	
    "		fclose(file_ptr);\n" +	
    "	}\n" +
    "	// Allocating memory based on number of non-zeros\n" +
    "	nnz = iam[neq] - 1; //TODO:verify +1 !\n" +
    "	jam = (int *)malloc(sizeof(int)*nnz);\n" +
    "	if (jam == NULL)\n" +
    " 		printf(\"Error Allocating jam\\n\");\n" +
    "	a_off = (double *)malloc(sizeof(double)*nb*nb*nnz);\n" +
    "	if (a_off == NULL)\n" +
    "		printf(\"Error Allocating a_off\\n\");\n" +
    "	a_diag_lu = (double *)malloc(sizeof(double)*nb*nb*neq);\n" +
    "	if (a_diag_lu == NULL)\n" +
    "		printf(\"Error Allocating a_diag_lu\\n\");\n" +
    "	dq = (double *)malloc(sizeof(double)*nb*neq);\n" +
    "	if (dq == NULL)\n" +
    "		printf(\"Error Allocating dq\\n\");\n" +
    "	dqc = (double *)malloc(sizeof(double)*nb*neq);\n" +
    "	if (dqc == NULL)\n" +
    "		printf(\"Error Allocating dqc\\n\");\n" +
    "	res = (double *)malloc(sizeof(double)*nb*neq);\n" +
    "	if (res == NULL)\n" +
    "		printf(\"Error Allocating res\\n\");\n" +
    "	// Read and generate rest of data\n" +
    "	if (neq == 6309) {\n" +
    "		file_ptr = fopen(\"jamS.txt\", \"r\");\n" +
    "		for (row = 0; row < 14295; row++) {\n" +
    "			k = row * 6;\n" +
    "			// Reads 6 values from current line into jam[]\n" +
    "			fscanf(file_ptr, \"%d\", &jam[k]);\n" +
    "			fscanf(file_ptr, \"%d\", &jam[k+1]);\n" +
    "			fscanf(file_ptr, \"%d\", &jam[k+2]);\n" +
    "			fscanf(file_ptr, \"%d\", &jam[k+3]);\n" +
    "			fscanf(file_ptr, \"%d\", &jam[k+4]);\n" +
    "			fscanf(file_ptr, \"%d\", &jam[k+5]);\n" +
    "		}\n" +	
    "		fclose(file_ptr);\n" +	
    "	}\n" +
    "	if (neq == 6309) {\n" +	
    "		file_ptr = fopen(\"color_indicesS.txt\", \"r\");\n" +	
    "		for (row = 0; row < 4; row++) {\n" +
    "			k = row * 5;\n" +
    "			// Reads 5 values from current line into color_indices[]\n" +
    "			fscanf(file_ptr, \"%d\", &color_indices[k]);\n" +
    "			fscanf(file_ptr, \"%d\", &color_indices[k+1]);\n" +
    "			fscanf(file_ptr, \"%d\", &color_indices[k+2]);\n" +
    "			fscanf(file_ptr, \"%d\", &color_indices[k+3]);\n" +
    "			fscanf(file_ptr, \"%d\", &iam[k+4]);\n" +
    "		}\n" +	
    "		fclose(file_ptr);\n" +
    "	}\n" +
    "	// Generate a_off, a_diag_lu, dq, res\n" +
    "	//TODO: Generate random numbers as needed equivalent to fortran\n" +
    "	//CAUTION: we do not apply the exact analogous to Fortran\n" + 
    "	//random_number()\n" +
    "	for (row = 0; row < nb * nb * nnz; row++)\n" +
    "		a_off[row] = 0.1 * (rand()/(double)RAND_MAX);\n" +
    "	for (row = 0; row < nb * nb * neq; row++)\n" +
    "		a_diag_lu[row] = 0.1 * (rand()/(double)RAND_MAX);\n" +
    "	for (row = 0; row < nb * neq; row++)\n" +
    "		dq[row] = 0.1 * (rand()/(double)RAND_MAX);\n" +
    "	for (row = 0; row < nb * neq; row++)\n" +
    "		res[row] = 0.1 * (rand()/(double)RAND_MAX);\n" +
    "	for (row = 0; row < nb * neq; row++)\n" +
    "		dqc[row] = dq[row];\n" +
    "}\n\n";


//----------------------------------------------------------------------------
// Returns the appropriate code for library functions, in C.
// TODO: No type or similar checking performed.
// TODO: CAUTION: ROUND is not supported.
//----------------------------------------------------------------------------
function processLibFunctions_C(e) {

    var pre = "";
    var ret = "";
    var post = "";
    var sep = ", "

    for (var i = 0; i < e.exprArr.length; i++) {

        if (i > 0) ret += sep;
        ret += expr2Cstring(e.exprArr[i]);

    }
    post = ")";

    if (e.str == "Math.mod") { //TODO:C:

        pre = "(";
	ret = expr2Cstring(e.exprArr[0]) + "%" + expr2Cstring(e.exprArr[1]);
	post = ")";

    } else if (e.str == "Math.cos") {

        pre = "cos(";

    } else if (e.str == "Math.sin") {

        pre = "sin(";

    } else if (e.str == "Math.tan") {

        pre = "tan(";

    } else if (e.str == "Math.max") { //TODO:C: Add macro.

        pre = "max(";

    } else if (e.str == "Math.min") { //TODO:C: Add macro.

        pre = "min(";

    } else if (e.str == "Math.exp") {

        pre = "exp(";

    } else if (e.str == "Math.log") {

        pre = "log(";

    } else if (e.str == "Math.sqrt") {

        pre = "sqrt("; //TODO:C:sqrtf?

    } else if (e.str == "Math.abs") {

        pre = "abs(";

    } else if (e.str == "Math.ceil") {

        pre = "ceil(";

    } else if (e.str == "Math.floor") {

        pre = "floor(";

    } else if (e.str == "Math.pow") { //TODO:C:

        pre = "pow(";
        ret = expr2Cstring(e.exprArr[0]) + ", " +
            expr2Cstring(e.exprArr[1])
        post = ")";

    } else if (e.str == "System.runCommand") {

        pre = "system(";
        ret = "\'" + e.exprArr[0].str + "\'";

    } else if (e.str == "Math.random") { //TODO:C: seed?

        // TODO: Must be a REAL grid cell or a REAL grid name.
        pre = "rand("

    } else if (e.str == "FileInput.loadCSRFile") {
	
	//DC99:
	// Build appropriate function call (call + arguments).
	pre = "readCSR(";
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

	if (LibFunctionsCode_C.indexOf("readCSR") == -1) {
                    
	    LibFunctionsCode_C = LibFunctionsCode_C.replace(
             	               "__COMMONVARS__","");

	    LibFunctionsCode_C += LibLoadCSR_C;

	}

    }


    if(e.str.indexOf("Math.") != -1 && e.str != "Math.mod" && 
	e.str != "Math.min" && e.str != "Math.max") {

        InclMath = 1;

    }

    return pre + ret + post;

}


//----------------------------------------------------------------------------
// Grid Language identifiers may not always be acceptable to C -- e.g.,
// reserved keywords. One strategy is to prefix every identifier with "ft_" 
// C variable names must start with a letter.
//----------------------------------------------------------------------------
function var2C(str) {

    return "ft_" + str;

}


//----------------------------------------------------------------------------
// Method called to get C code for an expression.
//----------------------------------------------------------------------------
function expr2Cstring(e) {

    var ret = "";
    var pre = "";
    var post = "";
    var sep = "";

    if (!e) { // Empty string.

    } else if (e.str == "!=") {

        ret = "!=";

    } else if (e.str == "<") {

        ret = "<";

    } else if (e.str == "<=") {

        ret = "<=";

    } else if (e.str == "==") {

        ret = "==";

    } else if (e.str == ">=") {

        ret = ">=";

    } else if (e.str == ">") {

        ret = ">";

    } else if (e.str == ".TRUE." || e.str == ".FALSE.") {

	//DC4:
	ret = (e.str == ".TRUE." ? 1 : 0);

    } else if (e.str == "AND") {

	//DC10:
	ret = "&&";

    } else if (e.str == "OR") {

	//DC10:
	ret = "||";

    } else if (e.str == "NOT") {

	//TODO: Need to remove space between '!' and what follows.
	//DC10:
	ret = "!";

    } else if ((e.isOperator() || e.isNumber() || e.isFormulaKeyword()) &&
        !e.isLetName()) {
        // Covering all other operators, except LT,LE,EQ,NE,GT,GE.

        ret = e.str;

    } else if (e.isString()) { // For taking care of abc string literals.

        ret = "\"" + e.str + "\"";

    } else if (e.isLeaf()) { // Other type of leaf

        // If it is pointing to a grid, append <gridName>_ 
        // to the title name (since this is how we declare them
        // as variables in C program to discern use of
        // same title name in different grids).
        if (e.type == ExprType.Title) {

            ret = e.gO.caption + "_" + e.str;

        } else if (e.isGridRef() && e.gO.typesInDim != -1) {

            // TODO: CAUTION: When does this case occur? Need to take care of 
	    // AoS case!
            ret = "typvar_" + e.gO.caption;

        } else {

            ret = var2C(e.str);

        }

    } else if (e.isGridCell()) { // Any non-scalar grid

        // If the grid has multiple data types we will need to generate
	// special code than for "typical" grids with a global (single) type.
        if (e.gO.typesInDim != -1 /*&& e.gO.numDims > 1*/) {

            pre = "typvar_" + e.gO.caption;
            var lastd = e.exprArr.length - 1;

            // Need to find title in the dimension where we have titles AND
	    // the dimensions. We don't care about the other titles (we just 
	    // substitute their values in C since declared).
            // We want to find which dimX (as declared by convention in our 
	    // derived C struct).
            // CAUTION: e.exprArr[e.gO.typesInDim].str contains 
	    // e.g., d3Tab3, not <gridName_>d3Tab3!
            var titleSrch = e.exprArr[e.gO.typesInDim].exprArr[0].str;
	    var dimChosen = e.gO.dimTitles[e.gO.typesInDim].indexOf(titleSrch);

            if (Soa) {

                pre += ".dim" + dimChosen + "[";

	    } else {
            
		    pre += "["; //TODO:C:
	    
	    }

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
            for (var i = lastd; i >= 0; i--) { // Print higher dims.

		if (i != e.gO.typesInDim) {

		    ret += expr2Cstring(e.exprArr[i]);

		    for (var j=i-1; j>=0; j--) {

			if (j!=e.gO.typesInDim) {

			    var dim = (e.gO.dimDynSize[j]) ? 
	               	        var2C(e.gO.dimDynSize[j]) : e.gO.dimActSize[j];

			    ret += "*" + dim + "+";

			}

		    }

		}

            }


            if (Soa) {

                ret += "]";

	    } else {
		    
                ret += "]" + ".dim" + dimChosen;

	    }

        } else {

            pre = var2C(e.gO.caption);

            // Last dimension. For a scalar, lastd = -1 so none of the 
            // statements below get executed -- i.e., ret = "";
            var lastd = e.exprArr.length - 1;

            if (e.gO.inArgNum >= 0 && e.gO.numDims == 0) { // i.e., scalar arg

                pre = "fun_" + e.gO.caption; //KK:FUN 
		// Replace in formulas with this (newly) declared,so that we 
		// achieve passing by value (this was necessary for FORTRAN,we
		// kept for C for compatibility with parallelization functions
		// (TODO: parallelization should be agnostic to the language 
		// and only based on the internal JS data structures - it IS,
		// except only this).

            }

	    // Now, reverse row and col index and iterate over them and add 
	    // those remaining,
            // i.e., excluding the dim that was "transformed" into the struct 
	    // containing different data types for each one of its 
	    // "dimensions".

            var tmp_expr_indices = new Array();
	    var tmp_dyn_size = new Array();
	    var tmp_act_size = new Array();

            for (var i = 0; i <= lastd; i++) {

                //e.g., row, col, ind3, ...
                tmp_expr_indices[i] = expr2Cstring(e.exprArr[i]);
            	tmp_dyn_size[i] = e.gO.dimDynSize[i];
	 	tmp_act_size[i] = e.gO.dimActSize[i];	

            }

	    if (lastd > 0) {
            
	        var tmp_switch = tmp_expr_indices[ColDimId];
            	tmp_expr_indices[ColDimId] = tmp_expr_indices[RowDimId];
            	tmp_expr_indices[RowDimId] = tmp_switch;
	    
		tmp_switch = tmp_dyn_size[ColDimId];
		tmp_dyn_size[ColDimId] = tmp_dyn_size[RowDimId];
		tmp_dyn_size[RowDimId] = tmp_switch;

		tmp_switch = tmp_act_size[ColDimId];
		tmp_act_size[ColDimId] = tmp_act_size[RowDimId];
		tmp_act_size[RowDimId] = tmp_switch;

	    }
			
            if (lastd != -1) pre += "["; 

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
            for (var i = lastd; i >= 0; i--) { // Print higher dims.

		if (i != e.gO.typesInDim) {

		    ret += tmp_expr_indices[i];

		    for (var j=i-1; j>=0; j--) {

			if (j!=e.gO.typesInDim) {

			    var dim = (tmp_dyn_size[j]) ? 
	               	        var2C(tmp_dyn_size[j]) : tmp_act_size[j];

			    ret += "*" + dim;

			}

		    }

		    if (e.gO.numDims != 1 && i != 0) ret += " + ";

		}

            } 

            if (lastd != -1) ret += "]";
             
        }

	// The code below takes care of the corner case of 
	// 1D structs, where the normal rules above do not
	// apply.
        if (e.gO.numDims == 1 && e.gO.typesInDim != -1) {
            pre = pre.replace("[", "");
	    if (Soa) {
	        ret = ""; // In case of 1D struct.
	    } else {
		ret = ret.substring(ret.indexOf("."))
	    }
        } 

    } else { // Compound statement or function.

        if (e.isLibFuncCall()) {

            // TODO: Make a better job organizing/going through libs/functions
            // By using something like "searchLibFunctions(string)" that 
            // searches on libraries that have been loaded or else.
            // Make strings constants and work with replacing codes (__XXX__).
            if (e.str == "FileInput.loadCSVFile" || e.str ==
                "FileOutput.saveCSVFile") {
					
					//TODO:C
                	alert("TODO:C:")

            } else {

                ret = processLibFunctions_C(e);

            }
            // TODO: Here, put code for handling other LIBRARY function calls.

        } else if (e.isUserFuncCall()) {

            pre = var2C(e.str);
            pre += "(";
            post = ")";
            sep = ", ";

	    var arr_dynValues = new Array();

            for (var i = 0; i < e.exprArr.length; i++) {

	        var cur_gO = e.exprArr[i].gO;
		var dynValues = ", ";
				
		// If it is a non-scalar grid argument.
		if(cur_gO != null && cur_gO.numDims > 0) {

		    // Loop through all dimensions and record variable ones.
		    for (var j = 0; j < cur_gO.dimActSize.length; j++) {

		        if(cur_gO.dimDynSize[j] != null) {

			    var t_sz = var2C(cur_gO.dimDynSize[j]);

			    if(arr_dynValues.indexOf(t_sz) == -1) {

			        arr_dynValues.push(t_sz);
				dynValues += t_sz + ",";

			    }

			}

		    }

		}

                if (i > 0) ret += sep;

                if (dynValues != ", ") {

		    dynValues = dynValues.replace(/,+$/, "");
		    ret += expr2Cstring(e.exprArr[i]) + dynValues;

                } else {
               
		    // Do not print if scalar gO that has been 
		    // implicitly used in dynamic non-scalar grid.
		    var t_sz;
		    if (cur_gO != null) t_sz = var2C(cur_gO.caption);
		    if (cur_gO != null && cur_gO.numDims == 0 && 
			    arr_dynValues.indexOf(t_sz) == -1) {
                    	    
			    ret += expr2Cstring(e.exprArr[i]);
			    arr_dynValues.push(t_sz);	
		
		    } else if (cur_gO!= null && cur_gO.numDims == 0 &&
			       arr_dynValues.indexOf(t_sz) != -1) {

			// Has been declared, do nothing.
			// Trim last comma (unneeded).
			ret = ret.replace(/, $/, "");

		    } else {

			ret += expr2Cstring(e.exprArr[i]);

		    }

                }

            }

        } else {

            sep = " ";

            for (var i = 0; i < e.exprArr.length; i++) {

                if (i > 0) ret += sep;
                ret += expr2Cstring(e.exprArr[i]);

            }
        }

    }

    return pre + ret + post;

}


//----------------------------------------------------------------------------
// Save the C string generated for the program.
// parallel: save parallel version if 1. Serial if 0.
// show: To be passed in showCstr to alert(code) if 1.
// 	 Else, just to return the code string to caller.
//----------------------------------------------------------------------------
function saveCstr(parallel, show) {

    // Get the JSON string
    //Regenerate code every time, otherwise may save old one if updated
    TypesAllFuncs = new Array();
    NamesAllFuncs = new Array();
    var str = encodeURIComponent(showCstr(0, parallel, show));
    createAPI(0); //API: 
    //TODO: Is SoA by default (hence '1' as first argument)
    download2SaveFortran(str, CurProgObj.fileName);

}


//----------------------------------------------------------------------------
// Show the C string generated for the program.
// strOfArr: Use structures of arrays (SoA) if 1. Arrays of structures (AoS)
// 	     if 0.
// show: Show code in JS (using alert) if 1. Just return code string if 0.
//----------------------------------------------------------------------------
function showCstr(strOfArr, parallel, show) {

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

    var code = getCstr();
    if (show) {

        alert(code);

    } else {

        return code;
    }

}


//----------------------------------------------------------------------------
// Returns C for the current step in current funtion that the
// user is currently working on.
//----------------------------------------------------------------------------
function getCstr() {

    var mO = CurModObj;

    // First, generate code for all functions, including 'main()'
    // Will be elements of the func_code array.
    var func_code = new Array(); // Used to store code for EACH function.
    
    // Variable to store all include statements needed.
    var inclStmts = "#include <stdio.h>\n" +
	    	    "#include <stdlib.h>\n";
    if (ShowParallel) {

        inclStmts += "#include <omp.h>\n";

    }

    TypeStr = ""; // Used to store TYPEs (i.e., structures).
    Func_prototypes = new Array(); 
    LibFunctionsCode_C = "";  
 
    // A single string that contains all function code.
    var func_code_all = "";

    // TODO: Be careful with global variables. If not initialized every time, 
    // they'll hold the value, until exiting program!
    GID_function = 0;

    for (var f = mO.FuncStartID; f < mO.allFuncs.length; f++) {


        if (ShowParallel && !funcContainsParStep(mO, f) && !FuncHasSerVer[f])
    		CalledFromSer = 1;

        func_code[f] = getCstr4Func(mO, f);

	if (ShowParallel)
 	    CalledFromSer = 0;

	//Note: Be careful where TypesAllFuncs starts != mO.allFuncs[start]
	func_code_all += TypesAllFuncs[f - mO.FuncStartID] + " " +
            func_code[f];

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
    if (ShowParallel) {
	    
        main_call += addIndentation(0) + "omp_set_nested(0);\n";

    }

    main_call += addIndentation(0) +
        var2C(CurModObj.allFuncs[DefMainFuncInd].allGrids[0].caption) +
        " = " + var2C(DefMainFuncName) +
        "(" + var2C(CurModObj.allFuncs[DefMainFuncInd].allGrids[1].caption) +
        ");\n";

    main_call += "}\n";

    // Check if any math functions have been called in the program, so as to 
    // add the math library in the include statements.
    if (InclMath) {
  
        inclStmts += "#include <math.h>\n\n";

    } else {
    
    	inclStmts += "\n";

    }

    // Construct function prototypes.
    var func_protos = "";
    for (var i = mO.FuncStartID; i < mO.allFuncs.length; i++) {

        func_protos += TypesAllFuncs[i - mO.FuncStartID] + " " + 
		       Func_prototypes[i - mO.FuncStartID];

	// If we are in ShowParallel, and we have a serial only version (in
	// case of a function that contains (even a single) parallel step(s).
	// we will need its prototype, too.
	if (ShowParallel && FuncHasSerVer[i]) {
 
            func_protos += TypesAllFuncs[i - mO.FuncStartID] + " " + 
		       Func_prototypes[i - mO.FuncStartID].replace("(",
				       "_ser(");
	    
	    
	}

    }
    func_protos += "\n\n";


    // Final generated code will contain the TYPEs code, the library functions
    // code (e.g., for read/write CSV), the functions' code (that contains
    // all functions, including ft_Main), and the PROGRAM "int main" that calls
    // Main function and subsequently any other functions.
    //DC99: Added LibFunctionsCode_C
    var returnedCode = inclStmts + TypeStr + LibFunctionsCode_C + func_protos + 
		       func_code_all + "\n" + main_call;

    returnedCode = returnedCode.replace(/UNIQUEIND/g, "int"); //TT

    return returnedCode;

}


//----------------------------------------------------------------------------
// Returns integer code for string of data type.
// TODO: When supporting all C types, revise this with new types.
//----------------------------------------------------------------------------
function dataTypeStrToInt_C(dt_string) {

    if (dt_string === "int")
        return 0;
    else if (dt_string === "double")
        return 1;
    else if (dt_string === "char *") //TODO:C:
        return 2;
    else if (dt_string === "bool")
        return 3;
    else if (dt_string === "char")
        return 4;
    else if (dt_string === "float")
	return 5;
    else if (dt_string === "void") //DC5:
	return 8;
    else
        alert("TYPE NOT YET SUPPORTED");

}


//----------------------------------------------------------------------------
// Returns data type string for given integer code.
// TODO: When supporting all C types, revise this with new types.
//----------------------------------------------------------------------------
function dataTypeIntToStr_C(typecode) {

    switch (typecode) {

        case 0:
            return "int";
            break;
        case 1:
            return "double";
            break;
        case 2:
            return "char[128]"; 
	    //TODO:C:
            break;
        case 3:
            return "bool";
            break;
        case 4:
            return "char";
            break;
        case 5:
            //alert("Data type: real4 not supported in C");
            //TODO:C: Support it.
	    return "float";
            break;
        case 6:
            //alert("Data type: const not supported yet"); //TODO:C:
	    return "INTEGER"; //TT TODO:CAUTION (how it interacts w/ rest).
            break;
        case 7:
            alert("Data type: func not supported yet"); //TODO:C:
            break;
	case 8: //DC5:
	    return "void";
	    break;
        default:
            alert("Invalid input as data type");

    }

}


//----------------------------------------------------------------------------
// Returns C code for a single function.
//----------------------------------------------------------------------------
function getCstr4Func(mO, f) {

    var fO = mO.allFuncs[f];
    
    // This is the GID_function id of current function corresponding to 
    // TypesAllFuncs and NamesAllFuncs.
    var gID_f = 0;
    
    // Initialize current step numbering for current function to zero.
    CurStep = 0;
    
    // Initialize GridsInFunc.
    GridsInFunc = new Array();
    Loop_var_per_dim = new Array();
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
    // Initialize to blank at the start of each new function.
    Func_decl = "";

    // See their declaration (global scope) for details on below:
    Row_col_decl = "";
    Index_end_decl = "";
    Grids_new_decl = "";
    TitleDefs_decl = "";
    AllocFreePerFunc = "";

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

		if (gO.inArgNum > 0) func_head += ", "; // arg separator.

	    	func_head += getDataTypeString_C(gO) + " ";

		if (!Soa) func_head += " *";
                //TODO: CAUTION: This is for the case of TYPE variable
                //	passed using its name (i.e., no specific element).
                func_head += "typvar_" + gO.caption;

            } else {

		if (gO.numDims >= 1) { 

		    if (gO.inArgNum > 0) func_head += ", "; // arg separator.

	    	    func_head += getDataTypeString_C(gO) + " ";

                    func_head += "*" + var2C(gO.caption); 
		    // Grid caption as arg name.

	    	} else {

		    // Ensure has not been implicitly declared via a
		    // dynamically sized non-scalar grid (see below).
		    var regex = new RegExp(var2C(gO.caption + "[, )]")); 
		    if (func_head.search(regex) == -1) {
		        
			if (gO.inArgNum > 0) func_head += ", "; // arg separator.

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

		    func_head += dynVals.replace(/,+$/, "");
		    
		}

	    } else {
		
		// For scalar-grids, we have to copy the src value into a temp
	   	// variable called fun_<src_var_name>. 
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

	    step_code += getCstr4Step(fO, fO.allSteps[s], mO);

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
    function_string += func_vars + //KK:FUN 
        Row_col_decl + Index_end_decl + TitleDefs_decl + Grids_new_decl +
        func_val_init + step_code + AllocFreePerFunc;
    
    // If no return statement added (e.g., when "void"), we return 1 (since 
    // ALL Grid Language functions need a return type).
    //if (function_string.indexOf("return") == -1) {

    //	function_string += addIndentation(0) + "return 1;\n";

    //}

    function_string += "}\n\n";

    return function_string;

}


//----------------------------------------------------------------------------
// Returns C code for a given step in a given function
//----------------------------------------------------------------------------
function getCstr4Step(fO, sO, mO) {

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

                    allocatablesOfStep += addIndentation(0) +
                        gridNam + " = " +
                        "(" +  tmp_type + " *)malloc(sizeof(" +
                        tmp_type + ")*" + tmp_dim_alloc + ");\n";

                }

		// Commenting.
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

	var forStrArr = new Array(); // REORDER.

        for (var iv = 0; iv < num_index_vars; iv++) {

            // STEP: get code for foreach loop .
            //
            var rexpr = rangeExpr.exprArr[iv];

            assert(rexpr.gO, "Range expr must have a gO");

            // TODO: FIX FIX FIX
            var ivar = var2C(rexpr.labelExpr.str);

            // Define the value of literal 'end'.
            var endv = var2C(DefEndName + rexpr.selDim);

            //TODO: Why did I change the below to the above?			
            //var endv = expr2Cstring(rexpr.exprArr[RangeFields.End]);

            // IMPORTANT NOTE: semasiology is SAVE if assignment 
            // is at the time of declaration. Saved between function calls. 
            // That's why here we separate declaration and initialization.

	    // We do not allow re-declaration of the same end variable across 
	    // steps.
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
            var endval = (dynendval) ? var2C(dynendval)
                 : actendval;

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
		    //DC9:
                    forstr2 += addIndentation(0) + endv + " = " + endval +
                        "-1;\n";

                }

            } else {
		//DC9:
                forstr += addIndentation(0) + endv + " = " + endval +
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


            // Step: Start/End/Step expressions.

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

        // Step: handle mask statment if/else/elseif/breakif.
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

                // condition with child expression  -- if/elseif/breakif.
                //

                if (boxexpr.isIf()) {

                    mask_unmatched++; // Increase unmatched if stmts.
                    
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

		    // Commenting.
		    if (sO.boxAttribs[box].comment != null) 
		        stmt += addIndentation(indent + index_extra_indent) + 
				"// " + sO.boxAttribs[box].comment + "\n";   

                    last_if_indent = indent; // So that we can close it later.
                    
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
                    
		    // Commenting.
		    if (sO.boxAttribs[box].comment != null) 
			stmt += addIndentation(indent + index_extra_indent) + 
				"// " + sO.boxAttribs[box].comment + "\n";

                } else {
                    // TODO: What is this case doing? (breakif).
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

            // Step: Process a formula statement.

	    // Commenting.
	    if (sO.boxAttribs[box].comment != null) 
	        stmt += addIndentation(indent + index_extra_indent) + 
                        "// " + sO.boxAttribs[box].comment + "\n";  

            // TODO: Do some more checking on the following. May be trickier 
            // than that.
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

		// 'If' refers to +=, etc. cases, 'else' to normal assignments.
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
			private_vars.indexOf(boxexpr.exprArr[0].str + ",") == -1) {

			    private_vars += var2Fortran(boxexpr
			    .exprArr[0].str) + ", ";

			}

		    }

                    currFormLine = addIndentation(indent + index_extra_indent) +
                        expr2Cstring(boxexpr) + ";\n";

                }

                // In current formula box: Loop through the exprArr[] 
                // array and detect if there are functions (isFuncCall()).
                for (var lp = 2; lp < boxexpr.exprArr.length; lp++) {
                    // TODO: If +=, -=, etc. then checking 2 is redundant?


                    // If it is a function, save its type to pass when 
                    // building the function code.
                    // Note: For library function calls we don't need 
		    // explicit declaration.
                    if (boxexpr.exprArr[lp].isUserFuncCall()) {

                        // Get global id of function within module.
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
                // anywhere else).

                var return_expression = "";
                var ret_val_assignment = "";

                // Parse the expression to be returned and save in order to
                // assign to the function's name (i.e., value to be returned).
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

		    //DC10: Avoid duplicate declaration of same-named LET stmts
		    // If grid has been already declared within THIS function, 
 	            // do not re-declare.
		    // Note: For LET stmts we don't use gId, but their name.
		    var gId = boxexpr.exprArr[0].exprArr[0].str;
         	    if (!gridDeclaredInFunc(gId)) {

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

		    	// Push into list of grids that have been declared (in the 
	                // context of the current function being parsed).
        	        GridsInFunc.push(gId);

		    }

		    
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
    // TODO: Could as well use only mask_unmatched in for loop,should be ok.
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


    // In order to obtain the dynamic variables we parse the allocatablesOfStep
    // variable and Struct_el_decl and process accordingly.
    var allocatablesFree = allocatablesOfStep.replace(/^\tft_(.+) = .+$/mg, 
		       "\tfree(ft_$1);");
    // This takes care of AoS.
    allocatablesFree = allocatablesFree.replace(/^\ttypvar_(.+) = .+$/mg, 
		       "\tfree(typvar_$1);");
    // This takes care of elements of SoA.
    allocatablesFree += Struct_el_decl.replace(/^\ttypvar_(.+) = .+$/mg, 
		        "\tfree(typvar_$1);");
    // TODO: Also, make sure AoS/SoA frees function correctly.
    AllocFreePerFunc += allocatablesFree;



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


//----------------------------------------------------------------------------
// Returns data type from TypesArray[] as string for a given grid
// TODO: Special care is needed for multiple data types when titles
// are present (when multiple data types for rows/columns)
// TODO: CAUTION: wrong for multidimensional, but correct. Only called from
// 				  where gO.typesInDIm == -1? If so, fix.
// TODO: Added parameter 'e' for when we have an expression and want to find
// 	the data type from the appropriate dimension for multidimensional
//	grids with multiple data types for the dimension which has types.
//	If e = null AND gO.typesInDim == -1 then it is a scalar grid (e.g., 
//	return value) OR non-scalar grid reference.
//	Otherwise, it is EITHER a gridcell OR a typevar reference.
//----------------------------------------------------------------------------
function getDataTypeString_C(gO, e) {

    // Where to search for dataType (if global, search in position 0).
    var typeDim;
    if (gO.typesInDim == -1) {

        typeDim = 0;

    } else {

        if (e != null) {

            // Which index's value should we get the value from (e.g., d3Tab2):
            var indx = expr2Cstring(e.exprArr[gO.typesInDim]);
            
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

            return findTypeVarType_C(gO, 1);

        }
    }

    if (gO.dataTypes[typeDim] === undefined) {

        alert("THIS SHOULDN'T OCCUR");

    } else {

        switch (gO.dataTypes[typeDim]) {

            case 0:
                return "int";
                break;
            case 1:
                return "double";
                break;
            case 2:
                return "char *"; 
		//TODO:C:
                break;
            case 3:
                return "bool";
                break;
            case 4:
                return "char";
                break;
            case 5:
                //alert("Data type: real4 not supported"); //TODO:C:
		return "float";
                break;
            case 6:
                //alert("Data type: const not supported"); //TODO:C:
		return "UNIQUEIND"; //TT
                break;
            case 7:
                alert("Data type: func not supported"); //TODO:C:
                break;
	    case 8:
		return "void";
		break;
            default:
                alert("Invalid input as data type");

        }

    }

}


//----------------------------------------------------------------------------
// Returns dimensions of a grid as a string.
// This information is used in declaring variables' dimensions.
// Takes care of multiple data type grids (to construct TYPE).
// TODO: Scalars are declared without dimensions, so no scalar
// gO should be called with getDimensionString_C().
//----------------------------------------------------------------------------
function getDimensionString_C(gO) {

    //TODO: Be careful of dimDynSize.
    var modarr = gO.dimActSize.slice(0); 

    modarr[0] = gO.dimDynSize[ColDimId] ? 
	    var2C(gO.dimDynSize[ColDimId]) : gO.dimActSize[ColDimId];
    modarr[1] = gO.dimDynSize[RowDimId] ? 
	    var2C(gO.dimDynSize[RowDimId]) : gO.dimActSize[RowDimId];

    for (var i = modarr.length - 1; i >= 2; i--) {

        modarr[i] = gO.dimDynSize[i] ? var2C(gO.dimDynSize[i]) : gO.dimActSize[i];

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
		
	if (i == modarr.length - 1) dimensions_string += "[";
		
        if (i != typesInDimAlt) {
			
            dimensions_string += modarr[i];

            // Do not print comma if types in last dimension or if last 
	    // dimension is 1 (1D).
            if (!(typesInDimAlt == 0 && i == 1) && modarr[0] != 1)
                dimensions_string += "][";
            //else if(!(typesInDimAlt == 0 && i == 1) && modarr[0] == 1)
            //	 dimensions_string += "]";

	}

    }

    if (typesInDimAlt != 0) {

        if (modarr[0] != 1) // If 1D (last dimension is 1) do not print it.
            dimensions_string += modarr[0] + "]";
        else
            dimensions_string += "]";
	
    } else if (typesInDimAlt ==0) {

    	dimensions_string += "]";
    	
    }

    // For the cases where we have 1D structs, elements won't need brackets.
    if (gO.numDims == 1 && gO.typesInDim == 0) {
  	dimensions_string = dimensions_string.replace("[", "");
	dimensions_string = dimensions_string.replace("]", "");
    }

    return dimensions_string;

}


//----------------------------------------------------------------------------
// Returns the string to create the appropriate structs in C for grids
// that need one (i.e., the ones that have multiple types per dimension).
//----------------------------------------------------------------------------
function createTypeString_C(gO) {

    var gridDefinition = "";

    if (gO.numDims == 0) {

        gridDefinition = getDataTypeString_C(gO, null) + " " +
            var2C(gO.caption) + ";\n";
        // For scalars no need to display DIMENSIONS().

    } else if (gO.numDims == 1 && gO.typesInDim == -1) {

        // 1D cannot have different data types, unless a struct.
        // This also applies to 1D (vectors),in which case will be: 
	// DIMENSIONS(1,X).
        // This is a grid AND is NOT an incoming arg. 
        gridDefinition = getDataTypeString_C(gO, null) + " *" + 
            var2C(gO.caption) + getDimensionString_C(gO) + ";\n";

    } else { // gO.numDims > 1

	// 'If' refers to the case where we have a TYPE.    
        if (gO.typesInDim != -1) {

            gridDefinition = findTypeVarType_C(gO, 0);

        } else {
	// 'Else' case refers to a "typical" grid.

            gridDefinition = getDataTypeString_C(gO, null) + " *" +
                var2C(gO.caption) +  getDimensionString_C(gO) + ";\n";

        }

    }

    return gridDefinition;

}


// --------------------------------------------------------------------------
// Function used to return declaration of structs.
// If declType is 1, this means this is the first time we are declaring this
// type, so we need to append to TypeStr the declaration of the struct and  
// its body.
// --------------------------------------------------------------------------
function findTypeVarType_C(gO, onlyDataType) {

    var gridDefinition = "";
    var typename = "";

    // Convention: variable named as <typvar_><grid name> 
    // (e.g., Out -> typvar_Out).
    var typeVariable = "typvar_" + gO.caption;

    var tmp_type_body = "";

    var tmp_length = gO.dimDynSize[gO.typesInDim];
    tmp_length = (tmp_length) ? tmp_length : gO.dimActSize[gO.typesInDim];
    // TODO:C: This should NOT be allowed by GUI.
    if(isNaN(tmp_length)) 
        alert("Number of elements in a struct cannot be dynamic.");

    // 'If' case refers to structure of arrays, 'else' to array of structures.
    // In either case, we have a different way of declaring the struct's body,
    // as the difference of SoA vs. AoS is.
    if (Soa) {
        
	for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

	    // For SoA, elements are dynamically allocated (since
	    // stack may not be big enough, and to allow passing
	    // of pointers when passign a struct as a parameter).
            tmp_type_body += dataTypeIntToStr_C(gO.dataTypes[i]) +
                " *" + "dim" + i + ";\n";
            
	    // Get dimensions of struct elements, so as to allocate in function.
	    var tmp_alloc_str = getDimensionString_C(gO);
	    tmp_alloc_str = tmp_alloc_str.replace("][", "*"); 
	    tmp_alloc_str = tmp_alloc_str.replace(/^\[|\]$/g, "");
	    if (tmp_alloc_str == "") tmp_alloc_str = 1; 
	    Struct_el_decl += addIndentation(0) + typeVariable + ".dim" + i + 
		    " = " + "(" + dataTypeIntToStr_C(gO.dataTypes[i]) + 
		    " *)malloc(sizeof(" + dataTypeIntToStr_C(gO.dataTypes[i]) + 
		    " )*" + tmp_alloc_str + ");\n";
        
	}

    } else {

        for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

            tmp_type_body += dataTypeIntToStr_C(gO.dataTypes[i]) + " " +
                "dim" + i + ";\n";

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

    //TODO:C:
    var toSearch = "struct TYP_\(.+\) {\n";
    toSearch += tmp_type_body.replace(/\*/g, '\\*');
   
    toSearch = toSearch.replace(/\]/g, '\\]');
    
    var toSearch2 = "}"; // In C, this denotes end of struct.

    var match_expr = new RegExp(toSearch + toSearch2);
    
    var res = TypeStr.match(match_expr);

    if (res != null) {
       
        // Here, in contrast to Fortran, we need to get the name from
        // the start of the struct declaration (before its body start).
        typename = res[1];

    } else {
        
        // TODO: We may want to follow a more general convention (another 
	// naming scheme based on IDs).
        typename = gO.caption;

    }

    gridDefinition = "struct TYP_" + typename;

    if (!onlyDataType){
    
	if (Soa) {  

            gridDefinition += " " + typeVariable + ";\n";

        } else {

	    if(gO.numDims > 1) {

                gridDefinition += " *" + typeVariable + 
			getDimensionString_C(gO) + ";\n";	
	    
	    } else {

                gridDefinition += " " + typeVariable + 
			getDimensionString_C(gO) + ";\n";
				
	    }

	}
		
    }

    if (res == null) {

        TypeStr += "struct TYP_" + typename + " {\n" + tmp_type_body + "};\n\n";

    }

    return gridDefinition;

}


//API:
// Generates the glaf_api.h/lib file that includes the prototypes
// and information on the input/output arguments of a function
// for which the user has requested inclusion to the API. 
// language_id: 0-C, 1-Fortran.
function createAPI(language_id) {

    var mO = CurModObj;
    var fileName = "glaf_api.";
    var str = "";
    var comm;
    var comments;

    if (language_id == 0) { // C.
        
	fileName += "h";
	comm = "// ";

    } else if (language_id == 1){ // Fortran.
	
	fileName += ".lib";
	comm = "! ";

    }

    for (var i = mO.FuncStartID; i < mO.allFuncs.length; i++) {

	 var fO = mO.allFuncs[i];
	 // If user has selected the appropriate box, which
	 // set FuncObj.createAPI.
	 if (fO.createAPI == true) {
	     
	      comments = comm + "Function name: " + fO.funcCallExpr.str + "\n";
    	      // Add function comment if any.
	      if (fO.allSteps[0].title != null)    
	          comments += comm + "Description: " + fO.allSteps[0].title + 
			      "\n"; 
              
	      comments += comm + "Return value:\n";
	      comments += comm + addIndentation(0) + 
		     var2C(fO.allGrids[0].caption) + " Type: ";
	      if (language_id == 0)
		     comments += getDataTypeString_C(fO.allGrids[0],null) + 
			         "\n";
	      else if (language_id == 1)
		     comments += getDataTypeString(fO.allGrids[0],null) + "\n";
	      if (fO.allGrids[0].comment != null) 
		      comments += comm + addIndentation(0) + "Description: " +
			          fO.allGrids[0].comment + "\n";
	      
	      comments += comm + "Parameter(s):\n";
	      // TODO: Add grid comments if any
	      for (var g = 1; g < fO.allGrids.length; g++) {

	           comments += comm + addIndentation(0) + 
		               var2C(fO.allGrids[0].caption) + " Type: ";
	           if (language_id == 0) {

		       comments += getDataTypeString_C(fO.allGrids[0],null) + 
			           "\n";

	      	   } else if (language_id == 1) {
			   
		       comments += getDataTypeString(fO.allGrids[0],null) + 
			           "\n";

		   }
	           if (fO.allGrids[g].comment != null) 
	  	      comments += comm + addIndentation(0) + "Description: " +
			     fO.allGrids[g].comment + "\n";

	      }

	      // If no input arguments (i.e., only return value).
	      //if (fO.allGrids.length != 1) str += "\n";

	      // Construct function prototype
	      str += comments + TypesAllFuncs[i - mO.FuncStartID] + " " + 
		     Func_prototypes[i - mO.FuncStartID];

	      str += "\n\n";

	      
	      // If we are in ShowParallel, and we have a serial only 
	      // version (in case of a function that contains (even a 
	      // single) parallel step(s) we will need its prototype, 
	      // too.
	      // Also, re-add the above.
	      // TODO: Need to add _ser in the function name.
	      if (ShowParallel && FuncHasSerVer[i]) {
 
                      str += comments + TypesAllFuncs[i - mO.FuncStartID] + 
			     " " + Func_prototypes[i - mO.FuncStartID].replace("(", "_ser(");
		      str += "\n\n";
	    
	       }
	       


	 }

    }

    // TODO: Write the final string to the output file.
    //alert(str);

}


