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
//Purpose: Grid Language Parallelism Detection Algorithms
//Author : Konstantinos Krommydas
//Date   : May 19, 2014
//----------------------------------------------------------------------------


// Used to keep grid references used in function calls from a step.
// Only keeps the NAME and is unique (only add at first instance).
// Reinitialized per step.
var GridRefsPerStep;

// Used to keep grid references used in function calls from a step.
// Keeps expression and box number for all occurences.
// Reinitialized per step.
var GridRefsPerStepAll;

// Used to store dimensions iterated over during a step (re-initialized 
// per step).
var DimsWrittenInStep;

// Like above.
var DimsIteratedOn;

// Used to store the start:end:step grids/constants for each of the dims  
// iterated over in step in the order recorded in DimsWrittenInStep.
// RangeExprsInStep[0]=dimension
// RangeExprsInStep[1][0]=start
// RangeExprsInStep[1][1]=end
// RangeExprsInStep[1][2]=step
// If start/end/step is CONSTANT then we store -1 (by convention).
var RangeExprsInStep;

// Used to store all the information for all boxes of ONE step.
// TODO: Will need to extend if we want to store for ALL steps.
var StepCFGinfo;

// Used to store all the information for ALL functions in current module.
// Array of LoopGridAnalysisObj objects. Each element of the array corresponds
// to each function in the module. USer defined functions start at element [3]
// Object elements at GridsPerFunc[0] - [2] are undefined.
// TODO: Will need to adapt for multiple modules (if needed).
var GridsPerFunc;

// Used to store ALL function calls from Main().
// Will be re-initialized per STEP of main, since parallelism analysis is per
// step of main. Its elements are FuncsCalledPerFuncInfo objects. One object
// per function, i.e., NO duplicates for same function in the array.
var FuncsFromCaller;


// Initialized once per program in 0. It is switched to 1 when we are creating
// a function whose steps should call serial versions of (potentially) parallel
// functions (since otherwise we would have nested parallelism).
var CalledFromSer;

// Used to identify if a functin is called by a step which is parallel (even
// if only once). In that case the function under consideration (callee) needs
// to have a serial version, so that we avoid nested parallelism for the
// specific call chain. If called from a serial step the parallel version will
// be called (IF the callee has any parallel steps that is).
var FuncHasSerVer;

// Used to store the parallel/ble dimensions for all steps of all functions.
// Starts from element [2][1] for first step of Main function of current 
// module and [0][X], [1][X], [2][0] will be initialized to "".
// In codegen, 1st dim will correspond to function ID, and 2nd dim corresponds
// to step ID (i.e., as code is parsed and generated).
// Initialized once per program in analyzeParallelismAll().
var Pragma_str;

// Used to store the reduction information for all steps of all functions.
// Starts from element [2][1] for first step of Main function of current 
// module and [0][X], [1][X], [2][0] will be initialized to "".
// In codegen, 1st dim will correspond to function ID, and 2nd dim corresponds
// to step ID (i.e., as code is parsed and generated).
// Initialized once per program in analyzeParallelismAll().
var Pragma_reduction;

// Used to store all the HintsObj objects for each scalar/non-scalar grids
// that prohibit parallelization. Reinitialized per step.
var HintObjArray;

// Used to store the different types of errors for parallelization hints
var ErrorHintType = {

    ScalarRegular: 0, 
    // Parallelism broken because of scalar dependency.
    NonScalarRegularWAW: 1, 
    // Parallelism broken because of non-scalar grid dependency WAW.
    NonScalarRegularRAWWAR: 2, 
    // Parallelism broken because of non-scalar grid dependency RAW
    ScalarIndexWrite: 3, 
    // Parallelism broken because of scalar write used in index range
    NonScalarAllConstants: 4, 
    // Parallelism broken because of constant indices used in a 
    // dim in all instances of a non-scalar grid
    ScalarReduction: 5, 
    // For scalar reduction 
    // TODO: Eventually parallelize this.
    NonScalarWithinFuncRW: 6, 
    // For non-scalar: read-only in step, written in function
    NonScalarWithinFuncRR: 7, 
    // For non-scalar: read-only in step, read in function
    NonScalarWithinFuncForLoop: 8 
    // For non-scalar: used in function but non-parall because looped over IN 
    // function.

};


//----------------------------------------------------------------------------
// Given the error type returns appropriate string message.
//----------------------------------------------------------------------------
function printErrorHintType(errorType) {

    switch (errorType) {

        case (ErrorHintType.ScalarRegular):
            return "Dependency on scalar grid";
            break;

        case (ErrorHintType.NonScalarRegularWAW):
            return "Dependency on non-scalar grid (WAW)";
            break;

        case (ErrorHintType.NonScalarRegularRAWWAR):
            return "Dependency on non-scalar grid (RAW/WAR)";
            break;

        case (ErrorHintType.ScalarIndexWrite):
            return "Index variable (start/end/step) written in step";
            break;

        case (ErrorHintType.NonScalarAllConstants):
            return "Same constant index used in non-scalar grid";
            break;

        case (ErrorHintType.NonScalarWithinFuncRW):
            return
                "Dependency non-scalar grid: " + 
	        "read in step, written in function call ";
            break;

        case (ErrorHintType.NonScalarWithinFuncRR):
            return "Dependency non-scalar grid: within called function";
            break;

        case (ErrorHintType.NonScalarWithinFuncForLoop):
            return
                "Dependency non-scalar grid: " + 
	        "looped-over within called function";
            break;


        case (ErrorHintType.ScalarReduction):
            return "Scalar reduction";

    }

}


//----------------------------------------------------------------------------
// Object to hold string for function name and number for step number based on
// step object.
//----------------------------------------------------------------------------
function FuncStepPair() {

    this.funcName;
    this.stepNum;

}


//----------------------------------------------------------------------------
// Object to hold information for a parallelization error instance.
//----------------------------------------------------------------------------
function HintInstanceObj(errorType) {

    this.errorType = errorType; 
    // What type of error is the particular instance
    // TODO: CAUTION: May have *DIFFERENT* error types for different 
    // dimensions.
    // e.g., when looped-over WITHIN function for one dim, and RAW 
    // for another dim not looped-over.

    this.dimensions; 
    // 1D Array: each element is the dimension id (order of appearance in 
    // sO.dimNameExprs).
    // e.g., value of: 0=row, 1=col, 2=indX, etc.

    this.pairInstance = new Array(); 
    // 1D Array: Contains 2 elements of type hintInstDetails

}


//----------------------------------------------------------------------------
// Object used in storing details about a hint instance.
//----------------------------------------------------------------------------
function HintInstDetails(funcName, stepNum, boxNum) {

    this.funcName = funcName; 
    // Function where instance is found.
    
    this.stepNum = stepNum; 
    // Step number in function funcName where instance is found.

    this.boxNum = boxNum; 
    // Box number in stepNum in funcName where instance is found.

    this.index = new Array(); 
    // Specific index for this instance (corresponds to dimension indicated
    // by HintInstanceObj.dimensions[i], where i corresponds to the pair which
    // contains this HintInstDetails object).

}


//----------------------------------------------------------------------------
// Object used in presenting the hints about what hindered parallelization
// during a step.
//----------------------------------------------------------------------------
function HintsObj(type, name) {

    this.name = name;
    this.type = type; //0: Scalar, 1: Non-scalar
    this.hintInstance = new Array(); 
    // 1D Array: contains elements of type HintInstanceObj

}


//----------------------------------------------------------------------------
// Returns string for function name and number for step number based on step 
// object.
//----------------------------------------------------------------------------
function getFuncStepPair(sO) {

    // TODO: Generalize for all modules.
    var mO = CurModObj;

    var functionStepPair = new FuncStepPair();
    functionStepPair.funcName = "NOT FOUND";
    functionStepPair.stepNum = -99;

    for (var i = 0; i < mO.allFuncs.length; i++) {
        for (var j = 0; j < mO.allFuncs[i].allSteps.length; j++) {

            if (sO === mO.allFuncs[i].allSteps[j]) { // TODO: === vs ==

                functionStepPair.funcName = mO.allFuncs[i].funcCallExpr.str;
                functionStepPair.stepNum = j;
                break;

            }

        }

    }

    return functionStepPair;

}


//----------------------------------------------------------------------------
// Used to update information of HintObjArray about cases where
// parallelization is hindered due to dependencies.
// dimWritten: (for non-scalar grids) The dimension iterated over on which we 
// found a dependency (0=row, 1=col, 2=indX, etc.)
// j, k: The indices within scalar/non-scalar object instances of grid 
// instances that are relevant to pair breaking dependency.
//----------------------------------------------------------------------------
function addToHintsObjArray(type, errorType, grid, grid2, dimWritten, j, k) {

    var name; // Name of grid 
    var fname1, fname2; 
    // The name of the function from where we adding box1, box2, respectively.
    
    var step1, step2; 
    // The number of the step from where we adding box1, box2, respectively.
    
    var box1, box2; 
    // The box id where we're finding the dependency.
    
    var ind1, ind2; 
    // (for non-scalar grids) The indices on which we found a dependency for 
    // corresponding dimWritten.

    name = grid.name;

    // Depending on the type and errorType, access the appropriate fields of 
    // the grid parameter (can be scalar or non-scalar object type).
    // And some fields may be null (using the same checks we print/give hints 
    // in GUI)
    if (type == 1) {

        if (errorType == ErrorHintType.NonScalarRegularWAW) {

            var functionStepPair = getFuncStepPair(grid.stepsWrittenLHS[j]);
            fname1 = functionStepPair.funcName;
            fname2 = functionStepPair.funcName;
            step1 = functionStepPair.stepNum;
            step2 = functionStepPair.stepNum;
            box1 = grid.dimsWrittenLHSboxNum[j];
            box2 = grid.dimsWrittenLHSboxNum[k];
            ind1 = grid.dimsWrittenLHS[dimWritten][j];
            ind2 = grid.dimsWrittenLHS[dimWritten][k];

        } else if (errorType == ErrorHintType.NonScalarRegularRAWWAR) {

            var functionStepPair = getFuncStepPair(grid.stepsWrittenLHS[j]);
            fname1 = functionStepPair.funcName;
            fname2 = functionStepPair.funcName;
            step1 = functionStepPair.stepNum;
            step2 = functionStepPair.stepNum;
            box1 = grid.dimsWrittenLHSboxNum[j];
            box2 = grid.dimsWrittenRHSboxNum[k];
            ind1 = grid.dimsWrittenLHS[dimWritten][j];
            ind2 = grid.dimsWrittenRHS[dimWritten][k];

        } else if (errorType == ErrorHintType.NonScalarAllConstants) {

            var functionStepPair = getFuncStepPair(grid.stepsWrittenLHS[j]);
            fname1 = functionStepPair.funcName;
            fname2 = null;
            step1 = functionStepPair.stepNum;
            step2 = null;
            box1 = null;
            box2 = null;
            ind1 = grid.dimsWrittenLHS[dimWritten][j];
            ind2 = null;

        } else if (errorType == ErrorHintType.NonScalarWithinFuncRW) {

            var subtype = 1;
            var functionStepPair1 = getFuncStepPair(grid.stepsWrittenRHS[
                j]);
            if (functionStepPair1.stepNum == -99) {
                subtype = 0; // USE LHS
                // Corresponds to a call of addHintObj from the second place
		// in doDependAnalForNonScalarGridsPassedToAllFuncs.
                functionStepPair1 = getFuncStepPair(grid.stepsWrittenLHS[
                    j]);
            }
            var functionStepPair2 = getFuncStepPair(grid2.stepsWrittenLHS[
                k]);
            fname1 = functionStepPair1.funcName;
            fname2 = functionStepPair2.funcName;
            step1 = functionStepPair1.stepNum;
            step2 = functionStepPair2.stepNum;

            if (subtype)
                box1 = grid.dimsWrittenRHSboxNum[j];
            else
                box1 = grid.dimsWrittenLHSboxNum[j];

            box2 = grid2.dimsWrittenLHSboxNum[k];

            if (subtype)
                ind1 = grid.dimsWrittenRHS[dimWritten][j];
            else
                ind1 = grid.dimsWrittenLHS[dimWritten][j];

            ind2 = grid2.dimsWrittenLHS[dimWritten][k];

        } else if (errorType == ErrorHintType.NonScalarWithinFuncRR) {

            var subtype = 1;
            var functionStepPair1 = getFuncStepPair(grid.stepsWrittenRHS[
                j]);
            if (functionStepPair1.stepNum == -99) {
                subtype = 0; //USE LHS
                // Corresponds to a call of addHintObj from the second place
		// in doDependAnalForNonScalarGridsPassedToAllFuncs.
                functionStepPair1 = getFuncStepPair(grid.stepsWrittenLHS[
                    j]);
            }
            var functionStepPair2 = getFuncStepPair(grid2.stepsWrittenRHS[
                k]);
            fname1 = functionStepPair1.funcName;
            fname2 = functionStepPair2.funcName;
            step1 = functionStepPair1.stepNum;
            step2 = functionStepPair2.stepNum;

            if (subtype)
                box1 = grid.dimsWrittenRHSboxNum[j];
            else
                box1 = grid.dimsWrittenLHSboxNum[j];

            box2 = grid2.dimsWrittenRHSboxNum[k];

            if (subtype)
                ind1 = grid.dimsWrittenRHS[dimWritten][j];
            else
                ind1 = grid.dimsWrittenLHS[dimWritten][j];

            ind2 = grid2.dimsWrittenRHS[dimWritten][k];

        } else if (errorType == ErrorHintType.NonScalarWithinFuncForLoop) {

            var subtype = 1;
            var functionStepPair1 = getFuncStepPair(grid.stepsWrittenRHS[
                j]);
            if (functionStepPair1.stepNum == -99) {
                subtype = 0; // USE LHS
                // Corresponds to a call of addHintObj from the second place
		// in doDependAnalForNonScalarGridsPassedToAllFuncs.
                functionStepPair1 = getFuncStepPair(grid.stepsWrittenLHS[
                    j]);
            }
            var functionStepPair2 = getFuncStepPair(grid2.stepsWrittenLHS[
                k]);
            fname1 = functionStepPair1.funcName;
            fname2 = functionStepPair2.funcName;
            step1 = functionStepPair1.stepNum;
            step2 = functionStepPair2.stepNum;

            if (subtype)
                box1 = grid.dimsWrittenRHSboxNum[j];
            else
                box1 = grid.dimsWrittenLHSboxNum[j];

            box2 = grid2.dimsWrittenLHSboxNum[k];

            if (subtype)
                ind1 = grid.dimsWrittenRHS[dimWritten][j];
            else
                ind1 = grid.dimsWrittenLHS[dimWritten][j];

            ind2 = grid2.dimsWrittenLHS[dimWritten][k];

        }


    } else {

        if (errorType == ErrorHintType.ScalarReduction) {

            var functionStepPair = getFuncStepPair(grid.stepsWrittenLHS[j]);
            fname1 = functionStepPair.funcName;
            fname2 = functionStepPair.funcName; //Will be in same function/step
            step1 = functionStepPair.stepNum;
            step2 = functionStepPair.stepNum; // Will be in same function/step
            box1 = grid.isWrittenBox[j];
            box2 = grid.isWrittenBox[k];
            ind1 = null;
            ind2 = null;

        } else if (errorType == ErrorHintType.ScalarRegular) {

            var functionStepPair = getFuncStepPair(grid.stepsWrittenLHS[j]);
            fname1 = functionStepPair.funcName;
            fname2 = functionStepPair.funcName; //Will be in same function/step
            step1 = functionStepPair.stepNum;
            step2 = functionStepPair.stepNum; // Will be in same function/step
            box1 = grid.isReadBox[j];
            box2 = grid.isWrittenBox[k];
            ind1 = null;
            ind2 = null;

        } else if (errorType == ErrorHintType.ScalarIndexWrite) {

            var functionStepPair = getFuncStepPair(grid.stepsWrittenLHS[j]);
            fname1 = functionStepPair.funcName;
            fname2 = null;
            step1 = functionStepPair.stepNum;
            step2 = null;
            box1 = grid.isWrittenBox[j];
            box2 = null;
            ind1 = null;
            ind2 = null;

        }

    }

    // Search if there already exists an entry for this name.
    // If not, create/initialize a new object and push to HintObjArray.
    // Else, only add the appropriate values to existing object.
    var found = -1; // Will be -1 if not found, or ID in HintObjArray if found

    for (var i = 0; i < HintObjArray.length; i++) {

        if (name == HintObjArray[i].name) {
            
	    // Update what needs to be updated (name and type are present).
            found = i;
            break;

        }

    }

    var hintObjId; 
    // The ID in array of HintObjArray of current object created/updated.

    // If we didn't find object, push a new one and initialize type and name.
    if (found == -1) {

        HintObjArray.push(new HintsObj(type, name));

    }

    hintObjId = (found == -1) ? HintObjArray.length - 1 : found;

    // TODO: CAUTION: If adding another pair of boxes/functions BUT just for a
    // different dimension that is being broken, do NOT push NEW (?)

    var hintInstanceId = -1;

    // If a non-scalar grid under the same name has been added, need to check 
    // if this pair of instances are the same, i.e., same pair of 
    // funcName:stepNum:boxNum, but just breaking dep on a different dimension
    // TODO: CAUTION: Does not cover the case where we have multiple instances
    // of a grid in same line that break dep.

    var curHintObj = HintObjArray[hintObjId];

    if (type == 1 && found != -1) {

        for (var i = 0; i < curHintObj.hintInstance.length; i++) {

            var tmp_instObj1 = curHintObj.hintInstance[i].pairInstance[0];
            var tmp_instObj2 = curHintObj.hintInstance[i].pairInstance[1];

            if (tmp_instObj1.funcName == fname1 && tmp_instObj2.funcName ==
                fname2 &&
                tmp_instObj1.stepNum == step1 && tmp_instObj2.stepNum ==
                step2 &&
                tmp_instObj1.boxNum == box1 && tmp_instObj2.boxNum ==
                box2) {

                // i.e., SAME pair of instances, with the only possible 
		// exception dimension.
                hintInstanceId = i;

            }

        }

    }

    if (hintInstanceId == -1) { 
	// i.e., not found an instance with same func names, steps, boxes OR 
	// it is a scalar grid.
        
        found = -1;
        hintInstanceId = curHintObj.hintInstance.push(new HintInstanceObj(
            errorType)) - 1;
        curHintObj.hintInstance[hintInstanceId].pairInstance
            .push(new HintInstDetails(fname1, step1, box1));
        curHintObj.hintInstance[hintInstanceId].pairInstance
            .push(new HintInstDetails(fname2, step2, box2));

    } // else hintInstanceId points to existing one.


    // For non-scalar grids we have to initialize dimensions and indices.
    if (type == 1) {

        if (found == -1) {

            curHintObj.hintInstance[hintInstanceId].dimensions = new Array();

        }

        curHintObj.hintInstance[hintInstanceId].dimensions.push(dimWritten);
        // TODO: Need to remove ft_ if it is a variable (if constant leave as 
	// is).
        curHintObj.hintInstance[hintInstanceId].pairInstance[
            0].index.push(ind1);
        curHintObj.hintInstance[hintInstanceId].pairInstance[
            1].index.push(ind2);

    }

}


//----------------------------------------------------------------------------
// Print contents of HintObjArray (for debugging).
//----------------------------------------------------------------------------
function printHintsObjArr() {

    //alert(HintObjArray.length);
    var info;
    for (var i = 0; i < HintObjArray.length; i++) {
        info = "";
        info += "Grid name: " + HintObjArray[i].name + ", Grid type: " +
            HintObjArray[i].type + "\n";

        // For all instances for this particular grid name:
        for (var j = 0; j < HintObjArray[i].hintInstance.length; j++) {

            info += "ErrorType:" + 
		printErrorHintType(HintObjArray[i].hintInstance[j].errorType) +
	       	"\n\n";

            // Info of first element of the instance pair: 
	    // "funcName:stepNum:boxNum"
            info += HintObjArray[i].hintInstance[j].pairInstance[0].funcName +
                ":" + HintObjArray[i].hintInstance[j].pairInstance[0].stepNum +
                ":" +
                HintObjArray[i].hintInstance[j].pairInstance[0].boxNum +
                ", ";

            if (HintObjArray[i].type == 1) {

                for (var k = 0; k < HintObjArray[i].hintInstance[j].dimensions
                    .length; k++) {

                    info += HintObjArray[i].hintInstance[j].pairInstance[
                        0].index[k] + " ";

                }

            }

            info += "\n\n";

            // Info of first element of the instance pair: 
	    // "funcName:stepNum:boxNum"
            info += HintObjArray[i].hintInstance[j].pairInstance[1].funcName +
                ":" + HintObjArray[i].hintInstance[j].pairInstance[1].stepNum +
                ":" +
                HintObjArray[i].hintInstance[j].pairInstance[1].boxNum +
                ", ";

            if (HintObjArray[i].type == 1) {

                for (var k = 0; k < HintObjArray[i].hintInstance[j].dimensions
                    .length; k++) {

                    info += HintObjArray[i].hintInstance[j].pairInstance[
                        1].index[k] + " ";

                }

            }

            info += "\n\n";

        }

        alert(info);

    }

}


//----------------------------------------------------------------------------
// Object used in creating Control Flow Graph (CFG) and identifying 
// reductions/scalar grids parallelization.
//----------------------------------------------------------------------------
function CFGObj() {

    // Points to the nearest enclosing IF box (for CFG construction).
    // 0 is ROOT node box (imaginary). Counting starts from 1 for foreach box.
    this.myIFbox = 0;

    // Points to the merge box (in control flow graph).
    this.mergeBox = -1;

    // True edge on CFG for this box.
    this.trueEdge = -1;

    // False edge on CFG for this box.
    this.falseEdge = -1;

}


//----------------------------------------------------------------------------
// Object used in detecting loop parallelism for scalar grids.
//----------------------------------------------------------------------------
function LoopScalarAnalysisObj(myname, isLHS, isRHS) {

    // Name of the scalar grid
    this.name = myname;

    // TODO: Alternatively could just check the length of appropriate arrays.
    // TODO: Will I need to initialize to zero as with non-scalars?

    // Indicate whether scalar grid appears in LHS (written) of a statement.
    this.isLHS = (isLHS != null) ? isLHS : this.isLHS;

    // Indicate whether scalar grid appears in RHS (read) of a statement.
    this.isRHS = (isRHS != null) ? isRHS : this.isRHS;

    // For each of the two possibilities (LHS/RHS) create a list of boxes in 
    // which var is written/read.
    this.isWrittenBox = new Array();
    this.isReadBox = new Array();
    this.stepsWrittenLHS = new Array();
    this.stepsWrittenRHS = new Array();

    this.isParallelizable = 1;

}


//----------------------------------------------------------------------------
// Object used in detecting loop parallelism for non-scalar grids.
//----------------------------------------------------------------------------
function LoopGridAnalysisObj(myname, isLHS, isRHS) {

    // Name of the non-scalar grid
    this.name = myname;

    // TODO: Alternatively could just check the length of appropriate arrays.

    // Indicate whether the non-scalar grid appears in LHS (written) of a 
    // statement.
    // TODO: Make default zero (and remove checks for undefined if any)
    // For scalars (above) not needed since not used in functions (yet?).
    this.isLHS = 0;
    this.isRHS = 0;
    this.isLHS = (isLHS != null) ? isLHS : this.isLHS;
    this.isRHS = (isRHS != null) ? isRHS : this.isRHS;

    // For each dimension (*not* only those corresponding to foreach loop)
    // create a list of indices written/read in current step.
    // First [] is dimension of GRID (irrespective if exists in foreach loop).
    // Second [] is the instance. 
    // For example for x = Out[row+4][col], in a loop where only col is looped
    // over: dimsWrittenRHS[0][0] is row+4 and dimsWrittenRHS[1][0] = col.
    //
    this.dimsWrittenLHS = new Array();
    this.dimsWrittenRHS = new Array();

    // The array index of the arrays below corresponds to the array index of  
    // the elements from the above arrays.

    // Saves the indentation on which given LHS/RHS grid appears.
    this.dimsWrittenLHSindent = new Array();
    this.dimsWrittenRHSindent = new Array();

    // Saves the step object on which given LHS/RHS grid appears.
    // So that it is used in reporting parallelization hints to user
    // when non-scalar deps are across functions/steps.
    // NOTE: By having the step object, we can find the function it 
    // belongs to, by scanning all modules/functions/steps.
    this.stepsWrittenLHS = new Array(); //HINTS
    this.stepsWrittenRHS = new Array(); //HINTS

    // Saves the box id on which given LHS/RHS grid appears.
    this.dimsWrittenLHSboxNum = new Array();
    this.dimsWrittenRHSboxNum = new Array();

    // Used to store IDs of INDEX VARIABLES looped over (only valid values for  
    // main function's grids, as added by addDimsWrittenLHS - not for 
    // addDimsWrittenLHSforFunc).
    // Examples:
    // If row only is iterated over: [0] = 0
    // If only col is iterated over: [0] = 1
    // If row and ind3 iterated over: [0] = 0, [1] = 2
    // Do *not* confuse this with the notion of DIMENSION. This concerns
    // the *index variables* (and they may be used to index *any* dimension).
    this.dimensionIdsWritten = new Array();

    // For each of the dimensions (NOT only those specified by the  
    // foreach stmt, but for all dimensions of grid - is 1 by default)
    // indicate whether loop is parallelizable across this dimension.
    // This information is used in conjuction with whether an index variable
    // exists at least in one instance as an index in this dimension, so the
    // appropriate OpenMP pragma can be constructed later if such index 
    // variable is present in parallelizable dimensions *only*. This occurs
    // at estimateOverallParallelism() and information for each 
    // *index variable* is stored in overallParallelism variable. Each dim is 
    // by default initialized at 1. Changed to 0 when dependency is detected.
    this.dimParallelizable = new Array();

    // Both first and second dimension size of this array is the number of 
    // dimensions of the grid, and has the following meaning:
    // containsParVar[0][0] -> the row dimension contains the index 
    // variable 'row', if 1, otherwise 0.
    // containsParVar[1][0] -> the col dimension contains the index
    // variable 'row', if 1, otherwise 0.
    // Do not confuse a DIMENSION with an INDEX VARIABLE (an index variable,
    // e.g., 'row' may be used to index the second -i.e., column, dimension
    // of a grid.
    this.containsParVar=new Array();

}


//----------------------------------------------------------------------------
// Used to return the NUMBER that corresponds to an index variable name,
// given an index var name (e.g., 'row' returns 0, col=1, indX=X).
//----------------------------------------------------------------------------
function convertIndexVarNameToNum(ind_var_name){

    if (ind_var_name == "ft_row") return 0;
    else if (ind_var_name == "ft_col") return 1;
    else {

        var regex = new RegExp("ind([0-9]*)");
        var numberString = ind_var_name.match(regex);
        // Make integer number from string:
        var x = parseInt(numberString[1]);
        return x;
        
    }

}


//----------------------------------------------------------------------------
// Used to return the string of an index variable name that corresponds to
// a number, given a number (e.g., 0 returns 'ft_row', 1=ft_col, X=ft_indX).
//----------------------------------------------------------------------------
function convertIndexVarNumToName(num){

    if (num == 0) return "ft_row";
    else if (num == 1) return "ft_col";
    else {

        return "ft_ind" + num; 

    }

}


//----------------------------------------------------------------------------
// Loops over an expression and detects if parallelism
// is broken because of 1D grids that are not uniqInd
// or because there are more than one uniqInd 1D grids.
// INPUTS:
// expr0: the expression whose subexpressions to check.
// sO: what pos in the expression to start from (for
// LET case we need to start from 1, otherwise for
// direct indexing start from 0).
// gridLHS grid object we are working on.
// i: dimension of gridLHS on which we are working.
// OUTPUTS:
// result[0]: returns string of expression.
// result[1]:
// Returns number of 1D grids of uniqInd type
// SIDE EFFECTS:
// Sets gridLHS.dimParallelizable[i] = 0 if needed.
//----------------------------------------------------------------------------
function checkGridsForUniqueInd(expr0, sO, gridLHS, i){

    var result = ["", 0];
    var numGridsInLet = 0;
    var numGridsUniqueInd = 0;
    var numGridsNonUniqueInd = 0;

    if(expr0.exprArr!=null) {

    for (var s = sO; s < expr0.exprArr.length; s++) {

        result[0] += expr2FortranString(expr0.exprArr[s]);
                    
        if (expr0.exprArr[s].gO != null && expr0.exprArr[s].gO.numDims > 0) {
			       
	    numGridsInLet++;

	    if (expr0.exprArr[s].gO.dataTypes[0] == DataTypes.UniqInd &&
		 expr0.exprArr[s].gO.numDims == 1) {

	        numGridsUniqueInd++;

	    } else {

		 // Is a grid, non-scalar, not UniqueInd.
		numGridsNonUniqueInd++;

	    }

	 }

    }

    if (gridLHS.dimParallelizable[i] == 1) {
        if (numGridsInLet == 0){ 

	    gridLHS.dimParallelizable[i] = 1; // Value denoting
	       			              // no NS_grids.
        } else if (numGridsInLet == 1) {

            if (numGridsUniqueInd == 1){

		gridLHS.dimParallelizable[i] = 1;	     
		// If >1 uniqInd across multiple LET
		// expressions, will later declare
		// dimension not-parallelizable.

	    }else gridLHS.dimParallelizable[i] = 0;

        } else { //numGridsInLet >1

	    gridLHS.dimParallelizable[i] = 0;

        }

    }

    result[1] = numGridsUniqueInd;

    }

    return result;

}


//----------------------------------------------------------------------------
// Used to search for a letName in boxes of current step and return the
// expression to be added in addDimsWrittenLHS().
// Also, returns in result[1] the number of grids that are 1D and uniqInd.
//----------------------------------------------------------------------------
function getLetString(sO, letName, gridLHS, i) {

    var result = ["", 1];

    for (var b = CodeBoxId.BoxStart; b < sO.boxExprs.length; b++) {

	var expr0 = sO.boxExprs[b];

        if (expr0) {

            if (expr0.exprArr[0] && expr0.exprArr[0].isLet()) {

                // Note: Let name is present in exprArr[0] position.
                if (expr0.exprArr[0].exprArr[0].str == letName) {

		    result = checkGridsForUniqueInd(expr0, 1, gridLHS, i);

                    // OUTSIDE at caller we should ADD UP the ones we get
		    // from HERE so if >1, still do dimensionParall=0 outside
		    // of the loop on sub-expressions.
		    // TODO: IF multi-dim grids are a CONSTANT cell then OK.
                    
                }
                
            }

        }
        
    }
    
    // If this function is called, there IS a let name defined
    // in the step.
    return result;

}


//----------------------------------------------------------------------------
// Used to add dimension indices for loop analysis in subsequent steps.
//----------------------------------------------------------------------------
function addDimsWrittenLHS(e, gridLHS, indices, isLHS, indent, box_id, sO) {

    var name = var2Fortran(e.gO.caption);

    // lastd correponds to the number of dimensions of the grid.
    var lastd = e.exprArr.length - 1;

    // Find the index variable IDs (e.g., 0 for row, 1 for column) that 
    // correspond to the ones that appear in the foreach loop and store 
    // in dimensionIdsWritten[]
    var dimensionIdsWritten = new Array();

    // For length of indices (this is "Index Names", as appears in GUI)
    // check foreach statement to see which ones are iterated over
    // (foreach loop might iterate over *less* than all available dims).
    for (var i = 0; i < indices.length; i++) {

        for (var j = 0; j < DimsWrittenInStep.length; j++) {

            if (DimsWrittenInStep[j] == var2Fortran(indices[i].str)) {

                dimensionIdsWritten.push(i);

            }

        }

    }

    // ONLY fill this ONCE per step per grid.
    if (gridLHS.dimensionIdsWritten.length == 0) {

        for (var i = 0; i < indices.length; i++) {

            for (var j = 0; j < DimsWrittenInStep.length; j++) {

                if (DimsWrittenInStep[j] == var2Fortran(indices[i].str)) {

                    gridLHS.dimensionIdsWritten.push(i);

                }

            }

        }

    }

    // TODO: CAUTION with +1 how to handle. Right now I include it (and I use 
    // it in comparisons! So take care.)--NO, I AM NOT INCLUDING IT (see below
    // expr2FortranString(e.exprArray[i]))
    // ***CAUTION*** row/col are intertwinned!!! [1/0] or follow my 
    // convention as to what is in my variables  i.e.: [0] is row, [1] is
    // col [...] higher dimensions. What if col only in foreach? Then 0 
    // is col???
    // Alternatively I can initialize EVERY dimension, just if not 
    // changing it will be empty. So, row will be zero, col will always be
    // [1], etc. This is what I'm doing now.


    var found = 0;


    for (var i = 0; i <= lastd; i++) {

        if (isLHS) {

            if (gridLHS.dimsWrittenLHS[i] === undefined) {

                // Create NEW array for position [i]
                gridLHS.dimsWrittenLHS.push(new Array());

            }

            if (gridLHS.dimParallelizable[i] === undefined) {
                // Dimension parallelizable by default.
		// TODO: If no foreach loop, does it become zero later?
                gridLHS.dimParallelizable.push(1);
            }

	} else {

	    if (gridLHS.dimsWrittenRHS[i] === undefined) {

                // Create NEW array for position [i]
                gridLHS.dimsWrittenRHS.push(new Array());

            }

            if (gridLHS.dimParallelizable[i] === undefined) {
                // Dimension parallelizable by default.
                gridLHS.dimParallelizable.push(1);
            }

	}

	if (gridLHS.containsParVar[i] === undefined) {

            // Create NEW array for position [i].
            gridLHS.containsParVar.push(new Array());

            for (var j = 0; j <= lastd; j++) {
                // By default index variables (e.g., row,col) are NOT 
	        // contained in DIMENSION i of grid.	
                gridLHS.containsParVar[i][j]=0;
            }

        }


	var tmp_index_instance = ""; 	

	// Counts how many single-instance 1D grids that are uniqInd
	// are encountered in LET uses in a given index.
	var numUniqIndInLets = 0;
		
	// Each index is a concat and contains one or more sub-expressions.
	// Loop through all sub-expressions and get the string.
	// In the case of LET use, expand the LET expression string by calling
	// getLetString().
	for (var s = 0; s < e.exprArr[i].exprArr.length; s++) {

	    // If the index is a let name, we need to return its
            // string, so it can be used to check for row/col/indX
            // existence
	    if(e.exprArr[i].exprArr[s].isLetName()) {

                var getLetRes = new Array();
                getLetRes = getLetString(sO, e.exprArr[i].exprArr[s].str, gridLHS, i);
                
                // The string corresponding to the let name.
                tmp_index_instance += getLetRes[0];
               
                numUniqIndInLets += getLetRes[1];


            } else {

		tmp_index_instance += expr2FortranString(e.exprArr[i].exprArr[s]);

	    }

	}

	if (numUniqIndInLets > 1)
	    gridLHS.dimParallelizable[i] = 0;



	// 
	var tmp_res = checkGridsForUniqueInd(e.exprArr[i], 0, gridLHS, i);
	if (tmp_res[1] > 1)
	    gridLHS.dimParallelizable[i] = 0;


	for(var j=0; j < sO.dimNameExprs.length; j++){
            
	    if(tmp_index_instance.indexOf(sO.dimNameExprs[j].str)!=-1) {

               	//alert(tmp_index_instance + ": DIM= " + i + 
		//	"contains indexVar= " + sO.dimNameExprs[j].str);
                gridLHS.containsParVar[i][j]=1;

            }

        }


	if (isLHS) {

	    gridLHS.dimsWrittenLHS[i].push(tmp_index_instance);
        
	    if (found == 0) {

                gridLHS.dimsWrittenLHSindent.push(indent);
                gridLHS.dimsWrittenLHSboxNum.push(box_id);
                gridLHS.stepsWrittenLHS.push(sO);

            }

	} else {

	    gridLHS.dimsWrittenRHS[i].push(tmp_index_instance);

            if (found == 0) {

                gridLHS.dimsWrittenRHSindent.push(indent);
                gridLHS.dimsWrittenRHSboxNum.push(box_id);
                gridLHS.stepsWrittenRHS.push(sO);

            }

	}

	found = 1;

    }

}


// TODO: Instead of using var2Fortran, I use the base name in the context
// of loop dependency analysis.


//----------------------------------------------------------------------------
// Used to analyze all non-scalar grids that are written at least once (in 
// whichever dimension) within a step of a function and change 
// nonScalarGridInstances relevant structures (whether a dimension of the 
// grid is parallelizable).
// TODO: name has NOT been converted using var2Fortran, keep in mind when 
// comparing.
//----------------------------------------------------------------------------
function doDependAnalysisForNonScalarGrids(nonScalarGridInstances) {

    // TODO: HERE WE NEED ASSERTIONS THAT ANY TABLES USED ARE NOT UNDEFINED!!!
    // TODO: Once parallelism is BROKEN, do NOT check the rest that are 
    // redundant for this dimension.

    for (var i = 0; i < nonScalarGridInstances.length; i++) {

	var curNonScalarGrid = nonScalarGridInstances[i];

        // If appears even once in LHS (i.e., written). 
        // TODO: Need to do same for functions! Treat similarly.

        if (curNonScalarGrid.isLHS == 1) {

            for (var dim = 0; dim < curNonScalarGrid.dimParallelizable.length; 
			    dim++) {  

                //Detect WAW
                for (var j = 0;
                    (curNonScalarGrid.dimsWrittenLHS[dim] !== 
		     undefined) &&
                    (j < curNonScalarGrid.dimsWrittenLHS[dim].length - 1); j++)
                  {
                    //Last one has been covered by previous iterations

                    for (var k = j + 1; k < curNonScalarGrid.dimsWrittenLHS[
                            dim].length; k++) {

                        if (curNonScalarGrid.dimsWrittenLHS[dim][j] !=
                            curNonScalarGrid.dimsWrittenLHS[dim][k]) {

                            curNonScalarGrid.dimParallelizable[dim] = 0;
                            
                            addToHintsObjArray(1, 
				ErrorHintType.NonScalarRegularWAW,
                                curNonScalarGrid, null, dim, j, k);

                        }

                    }

                }

                //Detect WAR and RAW
                for (var j = 0;
                    (curNonScalarGrid.dimsWrittenLHS[dim] !== undefined) &&
                    (j < curNonScalarGrid.dimsWrittenLHS[dim].length); j++) {

                    for (var k = 0;
                        (curNonScalarGrid.dimsWrittenRHS[dim] !==
                            undefined) &&
                        (k < curNonScalarGrid.dimsWrittenRHS[dim].length); k++)
                    {

                        // TODO: Here later we'll need to detect cases where 
                        // we can split statements (and switch them 
                        // potentially based on the direction of dependency)
                        // based on the difference of the index across one 
                        // direction, if we have a WAR (i.e., box id 
                        // differences is positive/negative).
                        if (curNonScalarGrid.dimsWrittenLHS[dim][j] !=
                            curNonScalarGrid.dimsWrittenRHS[dim][k]) {

                            curNonScalarGrid.dimParallelizable[dim] = 0;
                            addToHintsObjArray(1, 
				ErrorHintType.NonScalarRegularRAWWAR,
                                curNonScalarGrid, null, dim, j, k); //HINTS

                        }

                    }

                }

            }

        }
        // else ignore

    }

}


//----------------------------------------------------------------------------
// Analyzes indices for occurrences of constants. If all indices are constants
// then we mark the dimension on which only (same) constants are used as 
// non-parallelizable. Effectively, this function takes care of marking as 
// non-parallelizable the case where a dimension has the same indices written 
// in a dimension in many iterations. In doDependAnalysisForNonScalarGrids() 
// this case is marked as parallelizable, since we are comparing strings.
// TODO: CAUTION: Does NOT analyze functions at all, nor recurses if using
// multiple gridcells as indices.
// TODO: CAUTION: It ALLOWS variables/lets to be used, as well as (gridcells, 
// and functions) as long as they do not include row/col/indX.
// Is this correct in the general case? Probably not.
//----------------------------------------------------------------------------
function analyzeConstantIndices(nonScalarGridInstances, scalarGridInstances) {

    var found;
    var j_found;

    for (var i = 0; i < nonScalarGridInstances.length; i++) {

	var curNonScalarGrid = nonScalarGridInstances[i];

        // If appears >= once in LHS (i.e., written). 
        // TODO: Need to do same for functions. Treat similarly.
        if (curNonScalarGrid.isLHS == 1) {

            j_found = curNonScalarGrid.dimParallelizable.length;

            for (var dim = 0; dim < curNonScalarGrid.dimParallelizable.length; 
		dim++) {

                found = 0; // Per dimension

                // Check LHS uses of current grid.
                for (var j = 0;
                    (curNonScalarGrid.dimsWrittenLHS[dim] !==
                        undefined) && (j < curNonScalarGrid.dimsWrittenLHS[
                        dim].length); j++) {

                    // In indexOf search for ALL dims (row/col/indX), in case 
                    // user does so.
                    for (var dim2 = 0; dim2 < DimsWrittenInStep.length; dim2++) 		    {

                        if (curNonScalarGrid.dimsWrittenLHS[dim][j].indexOf(
                                DimsWrittenInStep[dim2]) != -1) {

                            found = 1;
                            break;

                        }

                    }

                    if (found)
                        break;

                }

                // Check RHS uses of current grid (that appears as LHS at 
                // least once), IF not found = 1 yet.
                if (!found) {

                    for (var k = 0;
                        (curNonScalarGrid.dimsWrittenRHS[dim] !==
                            undefined) &&
                        (k < curNonScalarGrid.dimsWrittenRHS[dim].length); k++)
                    {

                        //In indexOf search for ALL dims (row/col/indX), in 
                        //case user does so.
                        for (var dim2 = 0; 
			dim2 < DimsWrittenInStep.length; dim2++) {

                            if (curNonScalarGrid.dimsWrittenRHS[dim][k].indexOf(
                                    DimsWrittenInStep[dim2]) != -1) {

                                found = 1;
                                break;

                            }

                        }

                        if (found)
                            break;
                    }
                }

                if (!found) {

                    // All are non-row/col/etc, so last private is OK

                    // TODO: For j we use 0, since being here assures we have 
		    // ONE instance of written at this step.
                    // And since this case (nonscalarallconstants) is within 
		    // the SAME step, this is sufficient.
                    addToHintsObjArray(1, ErrorHintType.NonScalarAllConstants,
                        nonScalarGridInstances[i], null, dim, 0, -1);

                    // Mark this dimension of this grid as non-parallelizable.
                    nonScalarGridInstances[i].dimParallelizable[dim] = 0;

                    // If j_found is zero after checking all dimensions of
                    // current non-scalar grid, this means that none of the
                    // index variables in the foreach loop was present,
                    // and is therefore to be treated as scalar.
                    j_found--;
                    

                }
                // else keep what was the result of analysis previously

            }

            if(j_found == 0) {

		var tmpScal = addNonScalarAsScalar(nonScalarGridInstances[i]);
                scalarGridInstances.push(tmpScal);

            }

        }

    }

}


//----------------------------------------------------------------------------
// Add a non-scalar grid with the (same) constant indices (as indicated by
// analyzeConstantIndices()) to the scalar grid instances structure, so as
// to later be taken into account for scalar dependency analysis. That is,
// a non-scalar grid of that type may be used in a reduction clause.
//----------------------------------------------------------------------------
function addNonScalarAsScalar(nonscalargrid) {

    var scalGridInst = new LoopScalarAnalysisObj(nonscalargrid.name, 
                        nonscalargrid.isLHS, nonscalargrid.isRHS);


    for(var i=0; i<nonscalargrid.stepsWrittenLHS.length ; i++) {

        scalGridInst.stepsWrittenLHS.push(nonscalargrid.stepsWrittenLHS[i]);
        scalGridInst.isWrittenBox.push(nonscalargrid.dimsWrittenLHSboxNum[i]);

    }

    for(var i=0; i<nonscalargrid.stepsWrittenLHS.length ; i++) {

        scalGridInst.stepsWrittenRHS.push(nonscalargrid.stepsWrittenRHS[i]);
        scalGridInst.isReadBox.push(nonscalargrid.dimsWrittenRHSboxNum[i]);

    }

    return scalGridInst;

}


//----------------------------------------------------------------------------
// Check if (any) scalar grid variables used as an index range 
// (start:end:step) are written within current step's loop. If so, dimension 
// is non-par/ble.
// TODO: Compares only against scalar grids, not non-scalar grids (>0 dims).
//----------------------------------------------------------------------------
function analyzeIndexRanges(dim, scalarGridInstances) {

    for (var j = 0; j < 3; j++) {

        // If start/end/step is NOT constant search if written in step.
        if (RangeExprsInStep[dim][j] != -1) {

            for (var k = 0; k < scalarGridInstances.length; k++) {

                if (RangeExprsInStep[dim][j] == scalarGridInstances[k].name &&
                    scalarGridInstances[k].isWrittenBox.length) {

                    // TODO: Returns only the first occurence of write
                    addToHintsObjArray(0, ErrorHintType.ScalarIndexWrite,
                        scalarGridInstances[k], null, dim, 0, -1);

                    // Even if found once written, in even one of 
                    // start/end/step, no need to continue across this dim
                    return 0;

                }

            }

        }

    }

    // If scalar grid var in start/end/step not written, then dim is still by 
    // default parallelizable.
    return 1;

}


//----------------------------------------------------------------------------
// Print contents of nonScalarGridInstances structure.
//----------------------------------------------------------------------------
function printNonScalarGridInstances(nonScalarGridInstances) {

    for (var iter = 0; iter < nonScalarGridInstances.length; iter++) {

	var curNonScalarGrid = nonScalarGridInstances[iter];

        for (var i = 0;
            i < curNonScalarGrid.dimsWrittenLHS.length; i++) {

            alert("LHS: Dimension: " + i + " of grid: " +
                curNonScalarGrid.name + " is parallelizable? " +
                curNonScalarGrid.dimParallelizable[i]);

            for (var j = 0; j < curNonScalarGrid.dimsWrittenLHS[i].length; j++) 	    {

                alert("(LHS) For: " + curNonScalarGrid.name + " dimension: " +
                    curNonScalarGrid.dimsWrittenLHS[i][j] + ", boxId=" +
                    curNonScalarGrid.dimsWrittenLHSboxNum[j] + ", indent=" +
                    curNonScalarGrid.dimsWrittenLHSindent[j]);

            }

        }

        for (var i = 0;
             i < curNonScalarGrid.dimsWrittenRHS.length; i++) {

            alert("RHS: Dimension: " + i + " of grid: " +
                curNonScalarGrid.name + " is parallelizable? " +
                curNonScalarGrid.dimParallelizable[i]);

            for (var j = 0; j < curNonScalarGrid.dimsWrittenRHS[i].length; j++) 	    {

                alert("(RHS)For: " + curNonScalarGrid.name + " dimension: " +
                    curNonScalarGrid.dimsWrittenRHS[i][j] + ", boxId=" +
                    curNonScalarGrid.dimsWrittenRHSboxNum[j] + ", indent=" +
                    curNonScalarGrid.dimsWrittenRHSindent[j]);

            }

        }

        for (var i = 0;
            (i < curNonScalarGrid.containsParVar.length); i++) {

                for(j=0; j<curNonScalarGrid.containsParVar[i].length; j++) {
                    alert("["+i+"]["+j+"] = " + 
			curNonScalarGrid.containsParVar[i][j]);
		}

        }

    }

}


//----------------------------------------------------------------------------
// Print contents of scalarGridInstances
//----------------------------------------------------------------------------
function printScalarGridInstances(scalarGridInstances) {

    for (var iter = 0; iter < scalarGridInstances.length; iter++) {

        alert("Is: " + scalarGridInstances[iter].name + " parallelizable? " +
            scalarGridInstances[iter].isParallelizable);

        for (var j = 0; j < scalarGridInstances[iter].isWrittenBox.length; j++) 	{

            alert("(LHS) For: " + scalarGridInstances[iter].name +
                ", boxId=" + scalarGridInstances[iter].isWrittenBox[j]);

        }

        for (var j = 0; j < scalarGridInstances[iter].isReadBox.length; j++) {

            alert("(RHS) For: " + scalarGridInstances[iter].name +
                ", boxId=" + scalarGridInstances[iter].isReadBox[j]);

        }

    }

}


//----------------------------------------------------------------------------
// Backward-traces boxes in order to find the nearest enclosing IF box for 
// each box. Used in control flow graph construction.
//----------------------------------------------------------------------------
function findIFBoxes(sO) {

    for (var box = sO.boxAttribs.length - 1; box > CodeBoxId.Range + 1; box--) {

        //alert("Box: " + box + " indent: " + sO.boxAttribs[box].indent + 
        //		" " + expr2FortranString(sO.boxExprs[box]));

        for (var box2 = box - 1; box2 >= CodeBoxId.Range + 1; box2--) {

            //alert("Box2: " + box2 + " " + 
            //		expr2FortranString(sO.boxExprs[box2]));
            //alert(sO.boxExprs[box2].isIf() + " changing:" + 
            //		sO.boxAttribs[box2].indent  + " constant:" + 
            //		sO.boxAttribs[box].indent + " " + 
            //		sO.boxExprs[box].isElse())


            if (sO.boxExprs[box].isElse() || sO.boxExprs[box].isElseIf()) {
                if ((sO.boxExprs[box2].isIf()) &&
                    (sO.boxAttribs[box2].indent ==
                        sO.boxAttribs[box].indent)) {

                    //alert("AAMatching if for box: " + box + " is box: " + 
                    //		box2  + " changing:" + 
                    //		sO.boxAttribs[box2].indent  + 
                    //		" constant:" + sO.boxAttribs[box].indent);
                    StepCFGinfo[box].myIFbox = box2;
                    break;

                }

            } else {

                if ((sO.boxExprs[box2].isIf()) &&
                    (sO.boxAttribs[box2].indent ==
                        sO.boxAttribs[box].indent - 1)) {

                    //alert("AAMatching if for box: " + box + " is box: " + 
                    //		box2);
                    StepCFGinfo[box].myIFbox = box2;
                    break;

                }

            }

        }

    }

}


//----------------------------------------------------------------------------
// Forward-traces boxes in order to find the merge-box for each.
// Used in control flow graph construction.
//----------------------------------------------------------------------------
function findMergeBoxes(sO) {

    for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

        //alert("Box: " + box + " indent: " + sO.boxAttribs[box].indent + 
        //		" " + expr2FortranString(sO.boxExprs[box]));

        for (var box2 = box + 1; box2 < sO.boxAttribs.length; box2++) {

            //alert("Box2: " + box2 + " " + 
            //		expr2FortranString(sO.boxExprs[box2]));
            //alert(sO.boxExprs[box2].isIf() + " changing:" + 
            //		sO.boxAttribs[box2].indent  + " constant:" + 
            //		sO.boxAttribs[box].indent + " " + 
            //sO.boxExprs[box].isElse())

            if (((sO.boxExprs[box2].isIf()) &&
                    (sO.boxAttribs[box2].indent == sO.boxAttribs[box].indent)
                ) ||
                ((sO.boxAttribs[box2].isFormula()) &&
                    (sO.boxAttribs[box2].indent == sO.boxAttribs[box].indent)
                )) {

                //alert("AAMatching if for box: " + box + " is box: " + 
                //		box2  + " changing:" + 
		//		sO.boxAttribs[box2].indent  + 
                //		" constant:" + sO.boxAttribs[box].indent);

                StepCFGinfo[box].mergeBox = box2;
                break;

            } else if (sO.boxAttribs[box2].indent <
                sO.boxAttribs[box].indent) {

                //Merge box remains -1 (i.e., N/A)
                break;

            }

        }

    }

}


//----------------------------------------------------------------------------
// Records the edges for formula boxes.
// TODO: TAKE CARE OF return/break statements! (see Ruchira's e-mail)
// trueEdge = -1 for formulas means that it points to the END BOX.
// TODO: CHECK if I'll need trueEdge from START BOX to codeBox.Range+1
// probably not, just start scanning tree from codeBox.Range+1.
//----------------------------------------------------------------------------
function recordFormulaEdges(sO) {

    // Reminder: formulas only have trueEdge (falseEdge will always remain -1)
    for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

        if (sO.boxAttribs[box].isFormula()) {

            //alert(box + " " + expr2FortranString(sO.boxExprs[box]));

            if ((sO.boxExprs[box].exprArr[0] !== undefined) &&
                (sO.boxExprs[box].exprArr[0].type == ExprType.Return ||
                    sO.boxExprs[box].exprArr[0].type == ExprType.Break)) {

                //alert("Found return or break statement");
                StepCFGinfo[box].trueEdge = -1;

            } else if (StepCFGinfo[box].mergeBox != -1) {

                StepCFGinfo[box].trueEdge = StepCFGinfo[box].mergeBox;

            } else {

                var recurs_mergebox;
                var recurs_node = StepCFGinfo[box].myIFbox;
                do {

                    recurs_mergebox = StepCFGinfo[recurs_node].mergeBox;
                    recurs_node = StepCFGinfo[recurs_node].myIFbox;

                    //alert("Mergebox to get value=" + recurs_mergebox + 
                    //		" node to check next recursively=" + 
		    //		recurs_node);

                } while (recurs_mergebox == -1 && recurs_node != 0)

                // If we reach myIFbox = 0 this stops even if 
                // recurs_mergebox = -1.
                // This in turn means that the edge from this box is END BOX.
                StepCFGinfo[box].trueEdge = recurs_mergebox;

            }

        }

    }

}


//----------------------------------------------------------------------------
// Records the edges for mask boxes. For "else" boxes we only have trueEdges.
// An edge to '-1' means that it points to the END BOX.
//----------------------------------------------------------------------------
function recordMaskEdges(sO) {

    for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

        if (sO.boxExprs[box].isIf()) {

            var prev_mask = box;
            var contains_elseelseif = 0;

            // TODO: If cannot be the last statement in a step (assert)
            StepCFGinfo[box].trueEdge = box + 1;

            // TODO: CAUTION may not have subsequent boxes, need to check? And
            // start from box+2?
            for (var box2 = box + 1; box2 < sO.boxAttribs.length; box2++) {

                //alert("checking: "+ box2 + " for: " + box);
                if (((sO.boxExprs[box2].isIf()) &&
                        (sO.boxAttribs[box2].indent == sO.boxAttribs[box]
                            .indent)) || ((sO.boxAttribs[box2].isFormula()) &&
                        (sO.boxAttribs[box2].indent ==
                            sO.boxAttribs[box].indent))) {

                    //alert("AAMatching if for box: " + box + " is box: " + 
                    //		box2  + " changing:" + 
		    //		sO.boxAttribs[box2].indent +
                    //		" constant:" + sO.boxAttribs[box].indent);

                    if (!contains_elseelseif) {

                        // TODO: Or prev_mask instead of box?
                        StepCFGinfo[box].falseEdge = box2;

                    } else {

                        var recurs_mergebox;
                        var recurs_node = StepCFGinfo[prev_mask].myIFbox;

                        do {

                            recurs_mergebox = StepCFGinfo[recurs_node].mergeBox;
                            recurs_node = StepCFGinfo[recurs_node].myIFbox;

                            //alert("Mergebox to get value=" + recurs_mergebox
                            //		+ " node to check next recursively=" +
                            //		recurs_node);

                        } while (recurs_mergebox == -1 && recurs_node !=
                            0)

                        // If we reach myIFbox = 0 this stops even if 
                        // recurs_mergebox = -1.
                        // This in turn means that the edge from this box is 
                        // END BOX.
                        StepCFGinfo[prev_mask].falseEdge =
                            recurs_mergebox;

                    }

                    break;

                } else if ((sO.boxAttribs[box2].indent <
                        sO.boxAttribs[box].indent) && 
			(sO.boxExprs[box2].isElse() ||
                        sO.boxExprs[box2].isElseIf())) {

                    // This takes care when IF is not followed by else/elseif 
                    // and an else/else if follows in an indentation less than
                    // IF's.
                    // In this case we need to point the falseEdge to AFTER 
                    // the next (imaginary) ENDIF by finding the mergeBox of 
                    // the else/elseif (recursively if needed).

                    var recurs_mergebox;
                    var recurs_node = StepCFGinfo[prev_mask].myIFbox;

                    do {

                        recurs_mergebox = StepCFGinfo[recurs_node].mergeBox;
                        recurs_node = StepCFGinfo[recurs_node].myIFbox;

                        //alert("Mergebox to get value=" + recurs_mergebox + 
                        //		" node to check next recursively=" + 
                        //		recurs_node);

                    } while (recurs_mergebox == -1 && recurs_node != 0)

                    // If we reach myIFbox = 0 this stops even if 
                    // recurs_mergebox = -1
                    // This in turn means that the edge from this box is END 
                    // BOX
                    StepCFGinfo[prev_mask].falseEdge = recurs_mergebox;

                    break;

                } else if (sO.boxAttribs[box2].indent <
                    sO.boxAttribs[box].indent) {

                    if (!contains_elseelseif) {

                        StepCFGinfo[box].falseEdge = box2;

                    } else {

                        var recurs_mergebox;
                        var recurs_node = StepCFGinfo[prev_mask].myIFbox;

                        do {

                            recurs_mergebox = StepCFGinfo[recurs_node].mergeBox;
                            recurs_node = StepCFGinfo[recurs_node].myIFbox;

                            //alert("Mergebox to get value=" + recurs_mergebox
                            //		+ " node to check next recursively=" +
                            //		recurs_node);

                        } while (recurs_mergebox == -1 && recurs_node !=
                            0)

                        // If we reach myIFbox = 0 this stops even if 
                        // recurs_mergebox = -1
                        // This in turn means that the edge from this box is 
                        // END BOX.
                        StepCFGinfo[prev_mask].falseEdge =
                            recurs_mergebox;

                    }

                    break;

                } else if ((sO.boxExprs[box2].isElse() &&
                        (sO.boxAttribs[box2].indent == sO.boxAttribs[box]
                            .indent)) || (sO.boxExprs[box2].isElseIf() &&
                        (sO.boxAttribs[box2].indent == sO.boxAttribs[box]
                            .indent))) {

                    StepCFGinfo[prev_mask].falseEdge = box2;
                    prev_mask = box2;
                    contains_elseelseif = 1;

                    // TODO: CAUTION When it finds ELSE should it break!?
                    if (sO.boxExprs[box2].isElse()) {

                        // Do NOT add false edge for else.
                        break;

                    }

                }
                // TODO: Other valid case to be tested?

            }

        } else if (sO.boxExprs[box].isElse() ||
            sO.boxExprs[box].isElseIf()) {

            // TODO: 'If' cannot be the last statement in a step (assert).
            StepCFGinfo[box].trueEdge = box + 1;

        }

    }

}


//----------------------------------------------------------------------------
// Prints all information for current Control Flow Graph (CFG).
//----------------------------------------------------------------------------
function printCFGinfo(sO) {

    //Reminder: formulas only have trueEdge (falseEdge will always remain -1).
    for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

        alert("Box=" + box + " myIFbox=" + StepCFGinfo[box].myIFbox +
            " mergeBox=" + StepCFGinfo[box].mergeBox + " trueEdge=" +
            StepCFGinfo[box].trueEdge + " falseEdge=" +
            StepCFGinfo[box].falseEdge);

    }

}


//----------------------------------------------------------------------------
// Finds dependencies across scalar variables and sets appropriate variables.
// TODO: CAUTION if we want to be detailed we need to take care of reads in
// rangeExprs if scalar used as start/end/step?
//----------------------------------------------------------------------------
function doScalarDependAnalysis(scalarGridInstances, funcID, stepnum) {

    for (var i = 0; i < scalarGridInstances.length; i++) {

        if (scalarGridInstances[i].isLHS) { // Written at least once.

            var RootNode = CodeBoxId.Range + 1; // Start search from root node.
            var stack = new Array();
            stack.push(RootNode);

            var WAR = 0; // Write after read.
            var pointerLHS = 0; // Pointer to current index for LHS.
            var pointerRHS = 0; // Pointer to current index for RHS.

            // TODO: Do we cover tha case where X is written and read in same 
            // box?
            while (stack.length) {

                var N = stack.pop(); 
		// Pops last element from array (i.e., current tree level).

                //Find if var X is written in current node.
                pointerLHS = scalarGridInstances[i].isWrittenBox.indexOf(N);

                if (pointerLHS != -1) { // i.e., X is written in N.

                    // If there is a read of the same variable in the same
                    // box, it is a reduction (WAR).
                    // TODO: CAUTION: Handle appropriately and do not break 
		    // parallelism. If multiple boxes, then not OMP reduction
		    // posibility.
                    pointerRHS = scalarGridInstances[i].isReadBox.indexOf(N);
                    if (pointerRHS != -1) {

                        var operation = getReductionOperator(scalarGridInstances[i], N);
                        
                        // TODO: CAUTION: Need to check that there is NO other
                        // use of scalar variable in this step, otherwise
                        // reduction not possible!
                        if (operation == -1) {

                            WAR = 0;
                            // TODO: Write after read in SAME line
                            //addToHintsObjArray(0, ErrorHintType.ScalarRegular,
                            //    scalarGridInstances[i], null, pointerRHS, 0);
                            //break; 

                        } else if (operation == -2) { 

                            // TODO: CAUTION: This means this is a function call.
                            // Won't work if we allow REDUCTION between function
                            // calles in the same line.
                            //alert("TODO");

                        } else {

                            WAR = 0;

                            // Fill the reduction clause string.
                            // TODO: Supports one reduction variable.
                            Pragma_reduction[funcID][stepnum] = 
				"!$OMP REDUCTION(" +
                                operation + ": " + 
                                var2Fortran(scalarGridInstances[i].name) +
			       	")\n";

                            addToHintsObjArray(0, ErrorHintType.ScalarReduction,
                                scalarGridInstances[i], null, pointerLHS,
                                pointerRHS);
                            break;

                        }

                    }

                    // Do nothing. It is parallelizable?
                    // TODO:No need to explore subtree.
                    WAR = 0;

                } else { // i.e., X is not written in N.

                    pointerRHS = scalarGridInstances[i].isReadBox.indexOf(N);

                    if (pointerRHS != -1) {

                        WAR = 1;
                        // TODO: For k we only give 0, but we may create all 
			// pairs if more writes exist after our read.
                        addToHintsObjArray(0, ErrorHintType.ScalarRegular,
                            scalarGridInstances[i], null, pointerRHS, 0);
                        break;

                    } else {

                        
                        // TODO: I can mark as read if I want to NOT do 
                        // redundant work.
                        // TODO: CAUTION First put false, then true, so 
                        // that POP pops true (nearest path) - 
                        // DFS traversal.
                        // CAUTION: For ELSE and FORMULA falseEdge has been
                        // set to -1 during building CFG.

                        // Will be -1: else and formulas (and if/elseif 
                        // that point to END).
                        if (StepCFGinfo[N].falseEdge != -1) {

                            stack.push(StepCFGinfo[N].falseEdge);

                        }

                        // Will be -1: anything that points to END
                        if (StepCFGinfo[N].trueEdge != -1) {

                            stack.push(StepCFGinfo[N].trueEdge);

                        }

                    }

                }

            } // while loop

            if (WAR) {

                scalarGridInstances[i].isParallelizable = 0;

            }
            // else by default is 1

        }

    } // outer for loop

}


//----------------------------------------------------------------------------
// Gets as an input a scalar object that has been indicated to be part of and
// reduction operation. Then traverses the box in which the reduction takes
// place and finds the reduction operation: a) either if it is in the form 
// of +=, *=, etc., or b) [x = x {+,-,*,/,...} ...]. 
// IF it is a RAW hazard within the same box, i.e., NO OPERATOR (returns -1).
// TODO: CAUTION. Not general enough? (e.g., x = x + x, or x += x).
// and not supporting INTRINSICS (e.g., MIN, MAX). Also, '/' should NOT be
// supported.
//----------------------------------------------------------------------------
function getReductionOperator(scalargrid, n) {

    var operator = -1;  // Default: no operator/no reduction.

    // TODO: CAUTION: How to know WHICH stepsWrittenLHS[]?
    var box = scalargrid.stepsWrittenLHS[0].boxExprs[n];


    if(!box.exprArr[0].isFuncCall()) {

        // Scalar variable for reduction is on LHS, so if
        // we find XAssignOp, this is the reduction operation.
        // Otherwise, loop until we find the scalar grid
        // under consideration. If it is followed by an operator,
        // this is what we want, otherwise 
        // TODO: CAUTION: There are cases not covered by this:
        // e.g., x=5-x+... (when it appears as negative).
        if (box.exprArr[1].isXAssignOp()) {

            //alert("Found XAssign operator:" + box.exprArr[i].str);
            // Extract the operator (before '=') and return it.
            operator = box.exprArr[1].str[0];

        } else {

            // If of length 3, it is a single assignment (i.e., NO reduction).
            if(box.exprArr.length == 3) {

                operator = -1;  // By convention this means NO reduction.

            } else {

                // Loop over box's exprArray to detect operator.
                // Loop until second-to-last position. If by that point we
                // haven't found scalar grid, it means it is the last
                // one (see CAUTION: TODO above for this case).
                for (var i = 2; i < box.exprArr.length - 2; i++) {

                    if(box.exprArr[i].str == scalargrid.name) {

                        operator = box.exprArr[i+1].str;

                    }

                }

                // If the scalar variable is the last one, use the operator
                // preceding it as the reduction operation.
                // TODO: CAUTION: '-' is NOT an allowable operation in this case.
                if(box.exprArr[box.exprArr.length-1].str == scalargrid.name) {

                        operator = box.exprArr[box.exprArr.length-2].str;

                }

            }

        }

    } else {

        operator = -2;  // Convention (for when called by function).

    }

    return operator;

}


//----------------------------------------------------------------------------
// Traverses the CFG (used for debugging CFG).
//----------------------------------------------------------------------------
function traverseCFG() {

    // Start search from root node.
    var RootNode = CodeBoxId.Range + 1;
    var stack = new Array();
    stack.push(RootNode);

    while (stack.length) {

        var N = stack.pop();
        alert("Node:" + N);

        if (StepCFGinfo[N].falseEdge != -1) {

            stack.push(StepCFGinfo[N].falseEdge);
            alert("Pushing false:" + StepCFGinfo[N].falseEdge);

        }

        if (StepCFGinfo[N].trueEdge != -1) {

            stack.push(StepCFGinfo[N].trueEdge);
            alert("Pushing true:" + StepCFGinfo[N].trueEdge);

        }

    }

}


//----------------------------------------------------------------------------
// Search if non-scalar grid exists and, if not, add in appropriate structure.
//----------------------------------------------------------------------------
function addNonScalarGridInstance(e, nonScalarGridInstances, box, sO, isRHS) {

    var gridAsLHSfound = 0;
    var iter = 0;

    for (iter = 0; iter < nonScalarGridInstances.length; iter++) {

        if (e.str == nonScalarGridInstances[iter].name) {

            gridAsLHSfound = 1;
            break;

        }

    }

    if (!gridAsLHSfound) {

        if (isRHS) {

            nonScalarGridInstances.push(new LoopGridAnalysisObj(e.str, 
		null, 1));

        } else {

            nonScalarGridInstances.push(new LoopGridAnalysisObj(e.str, 
		1, null));

        }

    } else {

        if (isRHS) {

            nonScalarGridInstances[iter].isRHS = 1;

        } else {

            nonScalarGridInstances[iter].isLHS = 1;

        }

    }

    addDimsWrittenLHS(e, nonScalarGridInstances[iter], sO.dimNameExprs, !isRHS,
        sO.boxAttribs[box].indent, box, sO);

}


//----------------------------------------------------------------------------
// Search if scalar grid exists and, if not, add in appropriate structure.
//----------------------------------------------------------------------------
function addScalarGridInstance(e, scalarGridInst, box, sO, isRHS) {

    var scalarAsLHSfound = 0;
    var iter = 0;

    for (iter = 0; iter < scalarGridInst.length; iter++) {

        if (e.str == scalarGridInst[iter].name) {

            scalarAsLHSfound = 1;
            break;

        }

    }

    if (!scalarAsLHSfound) {

        if (isRHS) {

            scalarGridInst.push(new LoopScalarAnalysisObj(e.str, null, 1));

        } else {

            scalarGridInst.push(new LoopScalarAnalysisObj(e.str, 1, null));

        }

    } else {

        if (isRHS) {

            scalarGridInst[iter].isRHS = 1;

        } else {

            scalarGridInst[iter].isLHS = 1;

        }

    }

    if (isRHS) {

        scalarGridInst[iter].isReadBox.push(box);
        scalarGridInst[iter].stepsWrittenRHS.push(sO);

    } else {

        scalarGridInst[iter].isWrittenBox.push(box);
        scalarGridInst[iter].stepsWrittenLHS.push(sO);

    }

}


//----------------------------------------------------------------------------
// Processes if/elseif stmts and adds grids/scalars read in the RHS structure
// used later for dependency analysis.
//----------------------------------------------------------------------------
function processIfElseifRHS(e, nonScalarGridInst, scalarGridInst, box, sO) {

    for (var i = 0; i < e.exprArr.length; i++) {

        if (e.exprArr[i].isGridCell()) {

            if (!e.exprArr[i].gO.numDims) {

                //alert("Scalar grid:" + e.exprArr[i].str);
                addScalarGridInstance(e.exprArr[i], scalarGridInst, box, sO, 1);

            } else {

                //alert("Non-scalar grid:" + e.exprArr[i].str);
                addNonScalarGridInstance(e.exprArr[i], nonScalarGridInst, box, 
				sO, 1);

            }

        } else {

            // TODO: Take care of functions or anything else that may  
            // appear in if/elseif stmt as RHS variable
            // TODO: CAUTION: What if row/col/endX appear here? 
            // We should not change them, so should be ok.

        }

    }

}


//----------------------------------------------------------------------------
// Return 0 if at least one of the steps of a given function is 
// parallelizable.
//----------------------------------------------------------------------------
function funcContainsParStep(mO, f) {

    var fO = mO.allFuncs[f];

    for (var i = 0; i < fO.allSteps.length; i++) {

        if (Pragma_str[f][i] != "") {

            return 1;

        }

    }

    return 0;

}


//----------------------------------------------------------------------------
// Starts parallelism analysis code. Runs constructing information for all
// functions in current module and then starts by analyzing parallelism on the
// highest level (main function of current module).
//----------------------------------------------------------------------------
function analyzeParallelismAll(fO, single_step_anal) {

    var mO = CurModObj;
    //var fO = mO.allFuncs[getFuncIdByName(mO, "Main")];

    // Initialize pragma strings and pragma_reduction.
    // 1st dimension is the function ID, starting from 2 for Main and 3 for 
    // user-defined functions.
    // 2nd dimension is the step ID, starting from 1.
    // CAUTION: 1st dimension [0] and [1] will have "".
    // CAUTION: 2nd dimension [1] is "".
    Pragma_str = new Array();
    Pragma_reduction = new Array();
    FuncHasSerVer = new Array();
    CalledFromSer = 0; // To be changed whenever we are creating a step while
    		       // creating a "_ser" version of a function, i.e., called
		       // from a parallel step of another function, so all calls		       // from it must be serial (if there is also a parallel
		       // version of such callee functions).

    for (var i = 0; i < mO.allFuncs.length; i++) {

        Pragma_str[i] = new Array();
        Pragma_reduction[i] = new Array();
        FuncHasSerVer[i] = 0;

        for (var j = 0; j < mO.allFuncs[i].allSteps.length; j++) {

            Pragma_str[i][j] = "";
            Pragma_reduction[i][j] = "";
        }

    }

    // Array elements 0,1,2 will be undefined.
    GridsPerFunc = new Array();

    // CAUTION: To occur only ONCE per program.
    // Collect info on grids written/read per function and the detailed 
    // indices.
    recordGridsAndIndicesOfStepBoxes();
    
    //PrintGridsPerFunc();

    // First call is on higher level: current module's main function.
    findParallelismInFunction(mO, fO, single_step_anal);

}


//----------------------------------------------------------------------------
// Loops through all the expressions in the right-hand side (RHS) in a formula
// box or a LET statement and adds as RHS scalar, non-scalar grids and
// analyzes functions that are called.
//----------------------------------------------------------------------------
function parseRightHandSide(boxexpr, nonScalarGridInst, box, sO, scalarGridInst, mO) {

    // lp=1 is not needed for formula boxes, BUT IS needed for LET boxes.
    // In either case, it is not wrong, for the latter it is just NOOP.
    for (var lp = 1; lp < boxexpr.exprArr.length; lp++) {
        
	// TODO: If +=, -=, etc. then checking 2 is redundant?


        // TODO: Here check all RHS grids and run analysis 
        // function.
        // In that will set variable per dimension is 
        // parallelizable at the end will add openmp directives 
        // across parallelizable dims.
        // How about if there are separable statements??? How to? 
        // (later).


        // Here we are in an assignment statement, so examine 
        // LHS and add its needed properties in the corresponding 
        // object.
        // TODO: we need to check if we are IN A LOOP, 
        // otherwise don't care.
        if (boxexpr.exprArr[lp].isGridCell() && boxexpr.exprArr[lp].gO.numDims) 	{ 
	    // LHS is a grid

            addNonScalarGridInstance(boxexpr.exprArr[lp], nonScalarGridInst, 
		box, sO, 1); // 1 means as RHS	

        } else if (boxexpr.exprArr[lp].isGridCell() && !boxexpr.exprArr[
                lp].gO.numDims) {

            addScalarGridInstance(boxexpr.exprArr[lp], scalarGridInst, box, sO,
                1); // 1 means as RHS

        } else {

            //alert("Other:" + boxexpr.exprArr[lp].str);

            // START parse function
            if (boxexpr !== undefined && boxexpr.exprArr[lp] !==
                undefined && boxexpr.exprArr[lp].isUserFuncCall()) {
                parseFunctionCall(mO, boxexpr, box, lp);
            }
            // END parse function

        }

    }

}


//----------------------------------------------------------------------------
// Runs parallelism analysis code. Called initially for current module's Main.
// If no parallelism within a step of main, called for ALL called functions
// from within the step to identify parallelism at the function's level (and
// goes on recursively).
//----------------------------------------------------------------------------
function findParallelismInFunction(mO, fO, single_step_anal) {

    var mainstepval = 1; // Default value for overall parallelism
    var stepTotal = fO.allSteps.length; 
    // Default value for overall parallelism

    // When we 'show Data', we only care about parallelism of CURRENT (single)
    // step. So, we change the values above as below:
    if (single_step_anal) {
        mainstepval = fO.curStepNum;
        stepTotal = mainstepval + 1;
    }

    for (var mainstep = mainstepval; mainstep < fO.allSteps.length; mainstep++) 	{

        var sO = fO.allSteps[mainstep];

        // Reinitialize for EACH step analyzed.
        StepCFGinfo = new Array();

        // Caller is called for parallelization analysis per step.
        // TODO: We're showing hints only for current step. So this
        // needs not be initialized so many times (unless we 
        // extend per function/program).
        
        HintObjArray = new Array();

        // Create one CFGObj for each box of current step and push into 
        // StepCFGinfo. Starting from 0, to be consistent using indices.
        for (var box = 0; box < sO.boxAttribs.length; box++) {

            StepCFGinfo.push(new CFGObj());

        }

        // To store information about grids needed for dependency analysis.
        // Array elements will be of type LoopGridAnalysisObj.
        var nonScalarGridInst = new Array();

        // To store information about scalar variables needed for 
	// dep. analysis.
        // Array elements will be of type LoopScalarAnalysisObj.
        var scalarGridInst = new Array();

        GridRefsPerStep = new Array();
        GridRefsPerStepAll = new Array();

        // Re-initialize for every step
        DimsWrittenInStep = new Array();
        RangeExprsInStep = new Array();

        // Re-initialized per step
        FuncsFromCaller = new Array();

        // TODO: Make below as a function (reused at least once more in this 
        // file).
        // Note: rangeExpr.exprArr[] contains root range expressions.
        var rangeExpr = sO.boxExprs[CodeBoxId.Range];

        // If there are index variables.
        if (rangeExpr && rangeExpr.exprArr && rangeExpr.isForeach()) {

            var num_index_vars = rangeExpr.exprArr.length;

            for (var iv = 0; iv < num_index_vars; iv++) {

                var rexpr = rangeExpr.exprArr[iv];
                assert(rexpr.gO, "Range expr must have a gO");

                // TODO: FIX FIX FIX
                var ivar = var2Fortran(rexpr.labelExpr.str);

                // TODO: CAUTION using the ft_<name> type of names.
		//       We only care for grid name (if grid), otherwise
		//       we will save -1.
                // Note: if CONSTANT then we store -1 (convention)
                var start = rexpr.exprArr[RangeFields.Start].exprArr[0].str;
                var end = rexpr.exprArr[RangeFields.End].exprArr[0].str;
                var step = rexpr.exprArr[RangeFields.Step].exprArr[0].str;

                // TODO: CAUTION when using gridCells with multiple dimensions.
                if (!(rexpr.exprArr[RangeFields.Start].exprArr[0].isGridCell() &&
                        (rexpr.exprArr[RangeFields.Start].exprArr[0].gO.numDims == 0)
                    )) {

                    //alert("start IS not GRID CELL");
                    start = -1;

                }

                if (!(rexpr.exprArr[RangeFields.End].exprArr[0].isGridCell() &&
                        (rexpr.exprArr[RangeFields.End].exprArr[0].gO.numDims == 0))) {

                    //alert("end IS not GRID CELL");
                    end = -1;

                }

                if (!(rexpr.exprArr[RangeFields.Step].exprArr[0].isGridCell() &&
                        (rexpr.exprArr[RangeFields.Step].exprArr[0].gO.numDims == 0)
                    )) {

                    //alert("step IS not GRID CELL");
                    step = -1;

                }

                var rangeFields = new Array(start, end, step);
                //alert(rangeFields[0] + " " + rangeFields[1] + " " + 
                //		rangeFields[2]);
                RangeExprsInStep.push(rangeFields);
                //alert(RangeExprsInStep[iv][0] + " " + 
                //		RangeExprsInStep[iv][1] + " " + 
                //		RangeExprsInStep[iv][2]);

                //These are the dimensions across which loop iterates.
                DimsWrittenInStep.push(ivar);

            }

        }

        if (!DimsWrittenInStep.length) {

            // No FOREACH (i.e., is empty or forever loop), so step is de
            // facto not parallelizable
            //alert("NO LOOP IN CURRENT STEP. dimsWRittenInStep:" 
            //		+ DimsWrittenInStep.length + " isForever:" 
            //		+ rangeExpr.isForever());

            //alert("NOT PARALLEL- TODO: Go deeper FIX");

            // TODO: Continue parsing, so that if functions are called, they 
	    // will
            // be run for findParallelismInFunction on THEM. Detecting other LHS
            // RHS doesn't matter since in constructParallelismString() 
            // DimsWrittenInStep.length will be zero, so par_string="".
            // MAY have dimParallelizable etc. that don't make sense.
            // Correct for main test case.
            // MAKE SURE THAT IT IS CORRECT for all cases.

        }

        // Debugging:
        var temp_concat = "";
        for (var i = 0; i < DimsWrittenInStep.length; i++)
            temp_concat += " " + DimsWrittenInStep[i];
        //alert(temp_concat);

        // CFG building steps
        findIFBoxes(sO);
        findMergeBoxes(sO);
        recordMaskEdges(sO);
        recordFormulaEdges(sO);
        //printCFGinfo(sO);
        //traverseCFG();

        for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

            //alert("Box: " + box + " " + 
	    //	   expr2FortranString(sO.boxExprs[box]));

            var boxexpr = sO.boxExprs[box];

            if (boxexpr.isIf() || boxexpr.isElseIf()) {

                processIfElseifRHS(boxexpr, nonScalarGridInst, scalarGridInst,
                    box, sO);

            }

            // TODO: CAUTION: DO we need to worry about reading of variables
            // in IF/ELSEIF statements?
            if (!sO.boxAttribs[box].isMask()) {

                // Step: process a formula statement

                //alert("Expression: " + expr2FortranString(boxexpr));

                if (sO.boxAttribs[box].isFormula() && boxexpr.exprArr.length >
                    2 && (boxexpr.exprArr[1].isAssignOp() || boxexpr.exprArr[
                        1].isXAssignOp())) {
                    // Should cover all cases: simple equality OR
                    // the case of +=, -=, *=, /= (assertion of valid 
                    // exprArr[1] is guarranteed by previous steps in GUI)

                    // TODO: Here what if name changes (or is set by LET). 
                    // Need to take special care.
                    // TODO: Also, will need to take special care for multiple
                    // types per ROW/COL.


                    // Here we are in an assignment statement, so examine LHS 
                    // and add its needed properties in the corresponding 
		    // object.
                    // TODO: we need to check if we are IN A LOOP, otherwise 
                    // don't care.

                    // If +=, -=, etc., we'll need to save this LHS as RHS,too
                    // We push this temporarily so that it is processed by 
		    // later loop and at the end we pop it when no longer 
		    // needed.
                    if (boxexpr.exprArr[1].isXAssignOp()) {

                        boxexpr.exprArr.push(boxexpr.exprArr[0]);
                        // No need to copy fields, since whole object is 
			// pointed to
                        // boxexpr.exprArr[length-1].isGridCell
                        // boxexpr.exprArr[length-1].gO

                    }

                    if (boxexpr.exprArr[0].isGridCell() && boxexpr.exprArr[
                            0].gO.numDims) { // LHS is a grid.

                        // 0 means as LHS.	
                        addNonScalarGridInstance(boxexpr.exprArr[0], 
			    nonScalarGridInst, box, sO, 0);

                    } else { // LHS is a scalar.
                        // TODO: Is there any other subcase?

                        // 0 means as LHS.		
                        addScalarGridInstance(boxexpr.exprArr[0], 
			    scalarGridInst, box, sO, 0);	

                    }
                    // TODO: Function should be an option here!!! (as void)
                    // TODO: Need to check for reductions too in subsequent 
		    // functions.


                    // In current formula box: Loop through the exprArr[] 
                    // array and detect if there are functions (isFuncCall()).
                    // This will take care of any +=, -= LHS as RHS cases 
		    // (pushed earlier in boxexpr.exprArr last position).
                    parseRightHandSide(boxexpr, nonScalarGridInst, box, sO,
                        scalarGridInst, mO);

                    // This will pop the RHS instance of LHS in case of +=,etc
                    if (boxexpr.exprArr[1].isXAssignOp()) {

                        boxexpr.exprArr.pop();

                    }

                } else if (boxexpr.exprArr[0] != null && boxexpr.exprArr[
                        0].type == ExprType.Return) {

                    // Check if return statement in position 0 (can't be 
                    // anywhere else).

                    // TODO: CAUTION: Do we need to record if it is a return
		    // to add returned grid (if non-scalar) as read (RHS)?

                    var return_expression = "";
                    var ret_val_assignment = "";

                    // Parse the expression to be returned and save in order
                    // to assign to the function's name (i.e., value to be 
		    // returned).
                    for (var i = 1; i < boxexpr.exprArr.length; i++) {

                        if (i > 0) return_expression += " ";
                        return_expression += expr2FortranString(
                            boxexpr.exprArr[i]);

                    }

                    if (boxexpr.exprArr.length != 1) {

                        ret_val_assignment = var2Fortran(fO.funcCallExpr.str) +
                            " = " + return_expression;

                    } else {

                        ret_val_assignment = var2Fortran(fO.funcCallExpr.str) +
                            " = " + var2Fortran(fO.allGrids[0].caption);

                    }

                } else {

                    //alert("DEAD CODE?/break/incomplete statement?" + 
                    //		expr2FortranString(boxexpr));

                    if (boxexpr !== undefined && boxexpr.exprArr[0] !==
                        undefined && boxexpr.exprArr[0].isLet()) {

                        // We need to add any grids and/or variables and/or 
			// functions that we'll need to analyze for 
			// parallelism.
                        // Need to analyze only RHS (since LHS is 
			// "LET name =").
                        parseRightHandSide(boxexpr, nonScalarGridInst, box, sO,
                            scalarGridInst, mO);

                    }

                    // TODO: CAUTION: Analyze void/standaline function calls! 
		    // (user/library?)
                    // TODO: Fuse to a function call (also used by 
		    // parseRightHandSide similarly).

                    // Start standalone function
                    if (boxexpr !== undefined && boxexpr.exprArr[0] !==
                        undefined && boxexpr.exprArr[0].isUserFuncCall()) {
                        parseFunctionCall(mO, boxexpr, box, 0);
                    }
                    // End standalone function.

                }

            }

        }

        // By now we have all information on nonScalarGridInstances 
	// that contains 
	// non-scalar grid appearance in RHS/LHS, we have info on scalar 
	// appearances as LHS/RHS and information on all functions -besides 
	// main- w.r.t. where non-scalar grids in argument list of each 
	// function appear as RHS/LHS in all steps of the function (including 
	// calling function from within a function). We also have information 
	// on what functions are called from within main and what are the 
	// non-scalar arguments in the arg list of each, so we can "match"
	// them to the information we havefrom each function as discussed 
	// above.
	// This way we can perform the analysis for functions called from main
	// when they contain non-scalar grids used in main.

        //printFuncCalledPerFuncInfo();

        // TODO: Here (or later) we can analyze functions-OPTIMIZE: do not do 
        // if a given dim is already non-parallelizable as given by other 
        // reasons).
        // TODO: CAUTION: We pass scalarGridInst, so as to treat any
        // non-scalar grids with NO index variable in their indices,
        // as scalar (see in analyzeConstantIndices, and below for more
        // details).
        doDependAnalForNonScalarGridsPassedToAllFuncs(nonScalarGridInst);


        for (var t = 0; t < FuncsFromCaller.length; t++) { 
	    // For all functions called by current step.

            for (var g = 0; g < GridsPerFunc[FuncsFromCaller[t].gID].length; 
		g++) {
		// For all non-scalar grids used in those functions as 
		// arguments.

                for (pr = 0; pr < FuncsFromCaller[t].args[g].length; pr++) {

                    for (q = 0; q < GridRefsPerStepAll.length; q++) { 
			// Loop through all occurences of gridRefs in current 
			// step.

                        if (GridRefsPerStepAll[q][0].str == FuncsFromCaller[
                                t].args[g][pr] && GridRefsPerStepAll[q][2] ==
                            g && FuncsFromCaller[t].name ==
                            GridRefsPerStepAll[q][3]) {

                            // Now, add this gridReference's name to be
			    // treated as scalar in scalar analysis later.
                            // For a normal scalar grid in a box, it would 
			    // either be RHS OR LHS.
                            // Since within a function it may be used in BOTH 
			    // RHS and LHS, we will add this as so for the 
			    // caller's box.
                            if (GridsPerFunc[FuncsFromCaller[t].gID][g].isRHS) {

                                addScalarGridInstance(GridRefsPerStepAll[q][0],
                                    scalarGridInst,
                                    GridRefsPerStepAll[q][1], sO, 1);

                            }

                            if (GridsPerFunc[FuncsFromCaller[t].gID][g].isLHS) {

                                addScalarGridInstance(GridRefsPerStepAll[q][0],
                                    scalarGridInst,
                                    GridRefsPerStepAll[q][1], sO, 0);

                            }

                        }

                    }

                }

            }

        }

        doDependAnalysisForNonScalarGrids(nonScalarGridInst);
        //printNonScalarGridInstances(nonScalarGridInst);
        
	analyzeConstantIndices(nonScalarGridInst, scalarGridInst);
        //printNonScalarGridInstances(nonScalarGridInst);
        
    var funcID = getFuncIdByName(mO, fO.funcCallExpr.str);
	doScalarDependAnalysis(scalarGridInst, funcID, mainstep);
        //printScalarGridInstances(scalarGridInst);

        // TODO: Now, we have all the information we need.
        // (a) If any scalar variable is non-parallelizable step is no 
        //     parallelizable at all [stop here].
        // (b) If all scalar variables are parallelizable, then look per 
        //     dimension that is in foreach loop.
        //	- If a dimension isParallelizable == 1 then add this to a 
	//	pragma omp parallel for loop.
        // Return string for parallelization: EMPTY or pragma omp for row/col 
	// etc. (this will be saved in an array where each element correspond 
	// to each step).

        estimateOverallParallelization(scalarGridInst, nonScalarGridInst, mO,
            fO, mainstep, single_step_anal);

    }

}


//----------------------------------------------------------------------------
// Constructs the parallelization string for current step.
//----------------------------------------------------------------------------
function estimateOverallParallelization(scalarGridInstances, 
    nonScalarGridInstances, mO, fO,
    stepnum, single_step_anal) {

    // OMP pragmas are added in grid_codegen.js, based on info collected.
    var par_string = "";
    var isParallel = 1;

    var funcID = getFuncIdByName(mO, fO.funcCallExpr.str);

    // Max number of dimensions across all grids in step.
    var num_dims = fO.allSteps[stepnum].dimNameExprs.length;


    // Checking if no parallelism allowed overall if scalar dependency.
    for (var i = 0; i < scalarGridInstances.length; i++) {

        if (!scalarGridInstances[i].isParallelizable) {

            isParallel = 0;
            break;

        }

    }

    if (isParallel) { // Check grids.

	// dimensionParallelOverall[dim] indicates whether an *INDEX variable*
	// is independent across loop iterations.
	// dimensionParallelOverall[0] corresponds to row
	// dimensionParallelOverall[1] corresponds to col
	// etc.
        var dimensionParallelOverall = new Array();

        // Need to find dependencies (or lack thereof) across ALL index 
	// variables, indexing all dimensions of all grids in current step.
	
        for (var dim = 0; dim < num_dims; dim++) {
            
            if (DimsWrittenInStep.indexOf(convertIndexVarNumToName(dim))!=-1) {
	        
		// If the dimension is iterated over by default make 1
                dimensionParallelOverall[dim]=1;

            } else { 

                dimensionParallelOverall[dim]=0;
	    
	    }

        }

        // Check if dimensionParallelOverall[dim] should be zero, if 
        // start/end/stop is written within the step. Default is 1.
        for (var dim = 0; dim < DimsWrittenInStep.length; dim++) {

            // Get dimension 
            var dim_to_check = convertIndexVarNameToNum(DimsWrittenInStep[dim]);
           
            // Need to pass dim in function, because it uses RangeExprsInStep 
	    // which only has the ones in foreach loop.
            dimensionParallelOverall[dim_to_check] = analyzeIndexRanges(dim,scalarGridInstances);

        }


        // If a dimension was marked 0 above, it will remain here (or written 
        // as 0 again).
        // TODO: CAUTION && condition check if wrong?

	// For each index variable in foreach loop of step.
        for (var kk = 0; kk < DimsWrittenInStep.length; kk++) {
		
            dim = convertIndexVarNameToNum(DimsWrittenInStep[kk]);

            if(dimensionParallelOverall[dim] !=0) {

		// For all non-scalar grids G in step S
                for (var iter = 0; iter < nonScalarGridInstances.length; 
		iter++) {

                    var curNonScalarGrid = nonScalarGridInstances[iter];

		    // For all dimensions D of G (may be less than num_dims).
                    for (var i = 0; 
			i < curNonScalarGrid.dimParallelizable.length; 
                    	i++) {

                        //alert("ith dim=" + i + " is it parble?" +
                        //	curNonScalarGrid.dimParallelizable[i]);
                       
                        if (curNonScalarGrid.dimsWrittenLHS.length!=0 && !curNonScalarGrid.dimParallelizable[i] && 
			curNonScalarGrid.containsParVar[i][dim]) {

                            // TODO: CAUTION: Doesn't cover the case where we 
			    // LOOP over an index var, but NOT USE it at all.
                            // We should not allow this in the GUI's checks?
            
                            dimensionParallelOverall[dim] = 0;

                            // TODO: Optimization: No need to check same 
			    // dimension for any other grid name.

                         } 

                    }

                }
                
            }

        }

        // TODO: CREATE a structure to hold if a dim is parallelizable as it 
        // is doing the analysis earlier and have it ready by now.
        // TODO: In those variables similarly on the fly make zero ALL of them
        // if any scalar variable has WAR dependency.

        // CAUTION: Also covers the case when there is NO foreach loop (i.e., 
        // the step is defacto not parallelizable since no foreach),
        // but to avoid redundant computation in such cases we terminate the 
        // findParallelismInFunction() function early on, when no dims are found
        // in foreach box.
        for (var dim = 0; dim < num_dims; dim++) {

            if (dimensionParallelOverall[dim]) {

                par_string += convertIndexVarNumToName(dim) + " ";

            }

        }

    }

    // FOR DEBUGGING PURPOSES UNCOMMENT ALERTS.
    if (par_string != "") {

        //alert(par_string+": funcID=" + funcID + 
        //		" stepNum=" + stepnum);
        Pragma_str[funcID][stepnum] = par_string;

    } else {

        //alert("NOT PARALLEL: funcID=" + funcID + " stepNum=" + stepnum);
        //Pragma_str[funcID][stepnum] = "";	// Already initialized to "".

    }


    // In this case we go "deeper" to search for parallelism in lower
    // levels always (so we can use such info for vectorization/ivdep).
    // So, parallelism analysis will occur even if a function is ONLY 
    // ALWAYS called from a parallelizable step (at a higher level).
    // Rest produced code at grid_parallel.js will be correct, i.e.
    // will NOT run in parallel if parallel on a higher level.
    // Previously it was in the above 'else' (e.g., if no parallelism 
    // in current step in main, try parallelize all functions called 
    // by that step of Main- if any).

    for (var i = 0; i < FuncsFromCaller.length; i++) {

        if (!single_step_anal) { 

	    // Do NOT analyze others if we only care about current step.
	    // This is the case when we just SHOW data (and parallelism
	    // for the current step/screen in GUI).
	    //
	    // Before calling findParallelismInFunction() we need to save
	    // a (deep) copy of the array of objects FuncsFromCaller, and
	    // restore it after findParallelismInFunction(). If we didn't
	    // do this, FuncsFromCaller is re-initialized for the called
	    // functions and current function's information would be lost.
	    //
	    // TODO: CAUTION: Take care of RECURSIVE CALLS!
            var tmpFuncsFromCaller = doFuncCallDeepCopy(FuncsFromCaller);
	    findParallelismInFunction(mO, mO.allFuncs[FuncsFromCaller[i].gID], 		0);
	    FuncsFromCaller = tmpFuncsFromCaller;	

	}

    }

}


//----------------------------------------------------------------------------
// Return index of non-scalar grid named name in nonScalarGridInstances.
//----------------------------------------------------------------------------
function getGridIdInNonScalarGridInstances(nonScalarGridInstances, name) {

    for (var i = 0; i < nonScalarGridInstances.length; i++) {

        if (nonScalarGridInstances[i].name == name) {

            return i;

        }

    }

    //alert("Grid not found");
    return -1; // TODO: Handle in caller.

}


//----------------------------------------------------------------------------
// Part of dependency analysis when the non scalar grid under consideration
// is read-only in current step being analyzed.
//----------------------------------------------------------------------------
function doDependAnalReadOnlyInStep(i, j, indexInLHS, nonScalarGridInstances) {

    // If grid is READ-ONLY in function, too.
    if (!GridsPerFunc[FuncsFromCaller[i].gID][j].isLHS) {

        //alert("WRT function it is parallelizable. " + 
        //	"Only read in main and function");

        // TODO: Mark all dimensions as parallelizable (or leave as are, 
	// depending on when I call 
        // doDependAnalForNonScalarGridsPassedToAllFuncs).

    } else {
        // Grid is written (or both written and read) in 
        // function.

        // TODO: Need to analyze indices across all dimensions read in step 
	// and written and read in function as defined in 
	// nonScalarGridInstances appropriate variables.
        //
        //for all dimensions read in this grid in main
        //	for all instances of indices read for this grid in main
        //		for all instance2 of indices for this grid written in 
	//		function
        //			if(index of main of this instance for this 
	//			dimension!==index of function of instance2
        //			for this dimension)
        //				break all and mark this dimension for 
	//				this grid in nonScalarGridInstances as 
        //				non-parallelizable
        //		if still parallelizable then check similarly for all 
	//		instance2 of indices for this grid read in function
        //
        // TODO: CAUTION: MUST CHECK for RAW/WAR/WAW WITHIN the function too 
	// if not already not parallelizable.


        // For all dimensions this grid is used in main (in foreach loop).
        for (var t = 0; t < nonScalarGridInstances[indexInLHS]
	    .dimensionIdsWritten.length; t++) {

            // Used to find/index in FUNCTION'S indices that are all stored
            // In main's we ONLY store the ones that appear in foreach.
            var dim_tmp = nonScalarGridInstances[indexInLHS]
		.dimensionIdsWritten[t];
            

            // For all instances of indices read for this grid in main 
	    // (and WHICH are in foreach loop)
            for (var u = 0;
                (nonScalarGridInstances[indexInLHS].dimsWrittenRHS[
                        dim_tmp] !== undefined && u <
                    nonScalarGridInstances[indexInLHS].dimsWrittenRHS[
                        dim_tmp].length); u++) {


                // For all instances of this grid in function in the dimension 
		// above (i.e. used in main's foreach loop) is WRITTEN
                // BUT if dimension for this grid has been marked as 
		// non-parallelizable mark this dim in main's grid structure 
		// as such, too, and break.
                if (!GridsPerFunc[FuncsFromCaller[i].gID]
                    [j].dimParallelizable[dim_tmp]) {

                    nonScalarGridInstances[indexInLHS].dimParallelizable[
                        dim_tmp] = 0;

                    addToHintsObjArray(1,
                        ErrorHintType.NonScalarWithinFuncForLoop,
                        nonScalarGridInstances[indexInLHS],
                        GridsPerFunc[FuncsFromCaller[i].gID][j], 
			dim_tmp, 0, 0);

                    break;

                }

                for (var v = 0;
                    (GridsPerFunc[FuncsFromCaller[i].gID]
                        [j].dimsWrittenLHS[dim_tmp] !==
                        undefined && v < GridsPerFunc[
                            FuncsFromCaller[i].gID][j].dimsWrittenLHS[
                            dim_tmp].length); v++) {


                    var break_var = whatToCompare(
			nonScalarGridInstances[indexInLHS], t,
                        FuncsFromCaller[i],
                        GridsPerFunc[FuncsFromCaller[i].gID]
                        [j].dimsWrittenLHS[dim_tmp][v],
                        nonScalarGridInstances[indexInLHS].dimsWrittenRHS[
                            dim_tmp][u]);

                    	nonScalarGridInstances[indexInLHS].dimParallelizable[
                            dim_tmp] = !break_var;


                    // If grid already marked as non-parallelizable no need 
		    // to check further instances.
                    if (break_var) {

                        addToHintsObjArray(1,
                            ErrorHintType.NonScalarWithinFuncRW,
                            nonScalarGridInstances[indexInLHS],
                            GridsPerFunc[FuncsFromCaller[i].gID
                            ][j], dim_tmp, u, v);

                        break;

                    }

                }

                // If parallelelism is not broken so far on writes, check for 
		// conflicts on reads (a conflict between main's index and 
                // function's read is equal to checking for RAWS/WAWs/WARs 
		// WITHIN the function)
                // 
                // TODO: CAUTION: is it ok? May as well directly check 
		// combinations within the function for more detailed hints.
                if (nonScalarGridInstances[indexInLHS].dimParallelizable[
                        dim_tmp]) {

                    //alert("Still parallelizable");
                    //
                    // For all instances of this grid in function in the 
		    // dimension above (i.e., used in main's foreach loop)
                    // is READ.
                    for (var v = 0;
                        (GridsPerFunc[FuncsFromCaller[i]
                                .gID][j].dimsWrittenRHS[dim_tmp] !==
                            undefined && v <
                            GridsPerFunc[FuncsFromCaller[i].gID]
                            [j].dimsWrittenRHS[dim_tmp].length); v++
                    ) {

                        var break_var = whatToCompare(
			    nonScalarGridInstances[indexInLHS], t,
                            FuncsFromCaller[i],
                            GridsPerFunc[FuncsFromCaller[i].gID
                            ][j].dimsWrittenRHS[dim_tmp][v],
                            nonScalarGridInstances[indexInLHS]
                            .dimsWrittenRHS[dim_tmp][u]);

                            nonScalarGridInstances[indexInLHS]
                            .dimParallelizable[dim_tmp] = !break_var;

                        // If grid already marked as 
                        // non-parallelizable no need to 
                        // check further instances.
                        if (break_var) {

                            addToHintsObjArray(1,
                                ErrorHintType.NonScalarWithinFuncRR,
                                nonScalarGridInstances[indexInLHS],
                                GridsPerFunc[FuncsFromCaller[i].gID][j],
                                dim_tmp, u, v);

                            break;

                        }

                    }

                    // TODO: CAUTION CAUTION: Here if I had 'i' in above loop, 
		    // then this would be used in outer loop ALTHOUGH it 
		    // should have been a different scope!?!?!?

                }

            }

        }

    }

}


//----------------------------------------------------------------------------
// Part of dependency analysis when the non scalar grid under consideration
// is written (or read and written) in current step being analyzed.
//----------------------------------------------------------------------------
function doDependAnalWrittenInStep(i, j, indexInLHS, nonScalarGridInstances) {

    // TODO: Need to analyze indices across all dimensions written and read 
    // in step and written and/or read in function as defined in 
    // nonScalarGridInstances appropriate variables.
    //
    //for all dimensions read in this grid in main
    //	for all instances of indices read for this grid in main
    //		for all instance2 of indices for this grid written in function
    //			if(index of main of this instance for this dimension !==
    //			index of function of instance2 for this dimension)
    //				break all and mark this dimension for this grid
    //				in nonScalarGridInstances as non-parallelizable
    //		if still parallelizable then check similarly for all 
    //		instance2 of indices for this grid read in function

    //if still parallelizable then:			
    //for all dimensions written in this grid in main
    //	for all instances of indices written for this grid in main
    //		for all instance2 of indices for this grid written in function
    //			if(index of main of this instance for this dimension!==
    //			index of function of instance2 for this dimension)
    //				break all and mark this dimension for this grid
    //				in nonScalarGridInstances as non-parallelizable
    //		if still parallelizable then check similarly for all instance2 
    //		of indices for this grid read in function


    // For all dimensions this grid is used in main (in 
    // foreach loop).
    for (var t = 0; t < nonScalarGridInstances[indexInLHS].dimensionIdsWritten
        .length; t++) {

        // In main's we ONLY store the ones that appear 
        // in foreach.
        var dim_tmp = nonScalarGridInstances[indexInLHS].dimensionIdsWritten[
            t]; // TODO: Useless, could as well use t...

        // For all instances of indices written for this grid in main 
	// (and WHICH are in foreach loop).
        for (var u = 0;
            (nonScalarGridInstances[indexInLHS].dimsWrittenLHS[
                    dim_tmp] !== undefined && u <
                nonScalarGridInstances[indexInLHS].dimsWrittenLHS[
                    dim_tmp].length); u++) {


            // For all instances of this grid in function in the dimension 
	    // above (i.e. used in main's foreach loop) is WRITTEN.
            // BUT if dimension for this grid has been marked as 
	    // non-parallelizable mark this dim in main's grid structure 
	    // as such, too, and break.
            if (!GridsPerFunc[FuncsFromCaller[i].gID][j]
                .dimParallelizable[dim_tmp]) {

                nonScalarGridInstances[indexInLHS].dimParallelizable[
                    dim_tmp] = 0;
                addToHintsObjArray(1, ErrorHintType.NonScalarWithinFuncForLoop,
                    nonScalarGridInstances[indexInLHS],
                    GridsPerFunc[FuncsFromCaller[i]
                        .gID][j], dim_tmp, 0, 0);

                break;

            }

            for (var v = 0;
                (GridsPerFunc[FuncsFromCaller[i].gID][j]
                    .dimsWrittenLHS[dim_tmp] !==
                    undefined && v < GridsPerFunc[
                        FuncsFromCaller[i].gID][j].dimsWrittenLHS[
                        dim_tmp].length); v++) {


                var break_var = whatToCompare(
		    nonScalarGridInstances[indexInLHS], t,
                    FuncsFromCaller[i],
                    GridsPerFunc[FuncsFromCaller[i]
                        .gID][j].dimsWrittenLHS[
                        dim_tmp][v], nonScalarGridInstances[
                        indexInLHS].dimsWrittenLHS[
                        dim_tmp][u]);

                        nonScalarGridInstances[
                        indexInLHS].dimParallelizable[
                        dim_tmp] = !break_var;

                // If grid already marked as non-parallelizable no need to check
                // further instances.
                if (break_var) {

                    addToHintsObjArray(1,
                        ErrorHintType.NonScalarWithinFuncRW,
                        nonScalarGridInstances[indexInLHS],
                        GridsPerFunc[
                            FuncsFromCaller[i].gID]
                        [j], dim_tmp, u, v);

                    break;

                }

            }

            // If parallelelism is not broken so far on writes, check for 
	    // conflicts on reads (a conflict between main's index and 
	    // function's read is equal to checking for RAWS/WAWs/WARs 
	    // WITHIN the function).
            // TODO: Is it ok?
            if (nonScalarGridInstances[indexInLHS].dimParallelizable[
                    dim_tmp]) {

                // Also covers the case where we have write in main and 
		// read only in function.
                // For all instances of this grid in function in the 
		// dimension above (i.e., used in main's foreach loop) is READ.
                for (var v = 0;
                    (GridsPerFunc[FuncsFromCaller[i].gID]
                        [j].dimsWrittenRHS[dim_tmp] !==
                        undefined && v < GridsPerFunc[
                            FuncsFromCaller[i].gID][j].dimsWrittenRHS[
                            dim_tmp].length); v++) {


                    var break_var = whatToCompare(
			nonScalarGridInstances[indexInLHS], t,
                        FuncsFromCaller[i],
                        GridsPerFunc[
                            FuncsFromCaller[i].gID]
                        [j].dimsWrittenRHS[
                            dim_tmp][v],
                        nonScalarGridInstances[indexInLHS].dimsWrittenLHS[
                            dim_tmp][u]);

                        nonScalarGridInstances[indexInLHS].dimParallelizable[
                            dim_tmp] = !break_var;    

                    // If grid already marked as non-parallelizable no need 
		    // to check further instances.
                    if (break_var) {

                        addToHintsObjArray(1,
                            ErrorHintType.NonScalarWithinFuncRR,
                            nonScalarGridInstances[indexInLHS],
                            GridsPerFunc[
                                FuncsFromCaller[i].gID
                            ][j], dim_tmp, u, v);

                        break;

                    }

                }
                
		// TODO: CAUTION: CAUTION: Here if I had 'i' in above loop, 
		// then this would be used in outer loop. 
                // ALTHOUGH it should have been a different scope!?!?!?
                
            }

        }

        // TODO: The below is probably redundant (since checking reads in 
	// main with writes in func is similar to checking writes and reads 
	// in main only.
        // For the case we have write AND read in main (this covers the reads 
	// part only)
        // (not needed, see TODO above)

    }

}


//----------------------------------------------------------------------------
// Analyze function calls and non-scalar grid uses from main function
// TODO: CAUTION: Be careful: 1st user defined function has gID = 3, BUT 
// first user defined function is at GridsPerFunc[3], too! 0,1,2 are 
// undefined.
//----------------------------------------------------------------------------
function doDependAnalForNonScalarGridsPassedToAllFuncs(nonScalarGridInstances) {

    //For all functions called from Main
    for (var i = 0; i < FuncsFromCaller.length; i++) {

        //alert("[ANALYSIS]Function Name called from main: " + 
	//	FuncsFromCaller[i].name + " args.length=" + 
	//	FuncsFromCaller[i].args.length +
        //	" FuncsFromCaller.length=" + FuncsFromCaller.length);

        // If this is not empty, then there are non-scalar grids passed in 
	// this function.
        for (var j = 0; j < FuncsFromCaller[i].args.length; j++) {

            //For all those grids...
            for (var k = 0; k < FuncsFromCaller[i].args[j].length; k++) {

                // Below is the correspondence between caller argument and 
		// callee argument for current function.
                // Will effectively need to search for indices of callee arg 
		// name, instead of caller's arg name.
                //alert("Caller: " + FuncsFromCaller[i].args[j][k] + 
		//	", Callee: " +	
		//	GridsPerFunc[FuncsFromCaller[i].gID][j].name);

                // Find non-scalar grid from caller in nonScalarGridInstances to
		// check if only read or written, too.
                var indexInLHS = getGridIdInNonScalarGridInstances(
		    nonScalarGridInstances, FuncsFromCaller[i].args[j][k]);
                //alert(nonScalarGridInstances[indexInLHS].name+" isWritten=" + 
		//	nonScalarGridInstances[indexInLHS].isLHS + " isRead=" + 
		//	nonScalarGridInstances[indexInLHS].isRHS);
                
		// TODO: CAUTION: need to correspond scalar arguments, too, in
		// order to correctly compare indices!

                if (indexInLHS != -1) {
                    // If grid is READ-ONLY in current step of main.
                    if (nonScalarGridInstances[indexInLHS].isRHS && 
			!nonScalarGridInstances[indexInLHS].isLHS) {

                       doDependAnalReadOnlyInStep(i, j, indexInLHS, 
				       nonScalarGridInstances); 
                        //TODO: Do we need analyzeConstantIndices?
                    } else { 
			// Grid is written and/or read in current step of main
			// AND written in step of function (or both written and
			// read).


                       doDependAnalWrittenInStep(i, j, indexInLHS,
				       nonScalarGridInstances);
                        //TODO: Do we need analyzeConstantIndices?
                    }
		    
                } else {
                    // In this case we have a function call WITH non-scalar 
		    // parameter, BUT no use within main.
                    // So we need to check as we do in main for RAW/WAR/WAW 
		    // within the function.

                    //alert("Not found");
                    // TODO: DO not use CurModObj.
                    doDependAnalysisForNonScalarGrids(GridsPerFunc[
			FuncsFromCaller[i].gID]);
			        // TODO: CAUTION: Is the below needed here?
                    //analyzeConstantIndices(GridsPerFunc[FuncsFromCaller[
                    //    i].gID], scalarGridInstances);
                    //printNonScalarGridInstances(GridsPerFunc[
		    //	FuncsFromCaller[i].gID]);

                }

            }

        }

    }

}


//----------------------------------------------------------------------------
// Analyzed for dependency for a dimension between caller and callee, after
// corresponding arguements between caller and callee (if there is such).
// Also, if one the arguments in callee corresponds to one of the pre-defined
// index variables (row, col, indX), then update the containsParVar[][] info
// of the non-scalar grid at the caller site (nonscalargrid). This covers the 
// case, where one of the dimensions in caller has only CONSTANT values (and
// so its containsParVar[dim][index_var]=0. In this case, in
// estimateOverallParallelism(), even if dimParallelizable[dim]=0, it would
// not be taken into account if containsParVar[dim][index_var] is NOT 1.
// Returns 1 if there is a dependency and 0 if there is not.
//----------------------------------------------------------------------------
function whatToCompare(nonscalargrid, t, f, calleeName, curDimsWritten) {

    var myArgs = new Array();
    var break_var = 0;

    // Get all scalar args of function in myArgs
    // TODO: Get correct module as needed
    var calledFunctionID = f.gID;
    var fO = CurModObj.allFuncs[calledFunctionID];

    // alert("Current function alanyzed:" + fO.funcCallExpr.str);

    for (var g = 0; g < fO.allGrids.length; g++) {

        var gO = fO.allGrids[g];

        // We only need to analyze incoming arguments.
        if (gO.inArgNum >= 0) { // this grid is an incoming arg.

            // Record all input grid arguments so as to do the "renaming" 
            // during "inlining" called functions.
            myArgs.push(gO.caption);

        }
    }

    for (var k = 0; k < f.allArgs.length; k++) {

        var found = -1;

        // Need to search for callee name WITHOUT fun_ prefix.
        // TODO: Should not have it in the first place.
        var callee_name_nofun = calleeName.replace("fun_", "");

        for (var i = 0; i < myArgs.length; i++) {

            //alert("Comparing: " + myArgs[i] + " with: " + 
            //		callee_name_nofun);

            if (myArgs[i] == callee_name_nofun) {

                found = i;
                break;

            }

        }

        var XXX;
        // If found replace what will be added with the name of callee.
        if (found != -1) {

            //alert("A Will search for: " + var2Fortran(f.allArgs[found][k]));
            XXX = var2Fortran(f.allArgs[found]);

        // else keep this (won't be added since not in callee anyway).
        } else {

            //alert("B Will search for: " + calleeName);
            XXX = callee_name_nofun;

        }

	// indVar contains 0 if row, 1 if col, or X if indX. NaN, otherwise.
        var indVar = convertIndexVarNameToNum(XXX); 
        
	if (indVar < nonscalargrid.dimParallelizable.length) {  

            nonscalargrid.containsParVar[t][indVar]=1; 

        }

        if (curDimsWritten != XXX) {

            //alert("[2]Read/write dependency between main and function: " + 
            //		indexInLHS + " " + dim_tmp);
            // Mark as non parallelizable
            break_var = 1;
            
            break;

        }

    }

    if (break_var) {

        return 1;

    } else {

        return 0;

    }

}


//----------------------------------------------------------------------------
// Saves LHS/RHS information per function.
// Information is stored in GridsPerFunc[functionID][X], where X is the id of 
// each grid that appears in the arguments list. It is of type 
// LoopGridAnalysisObj that contains name and instances as RHS/LHS, the
// indices themselves, as well as isRHS/isLHS per grid variable.
// The idea is that while we traverse main function's steps, we will record 
// the function names and grids as arguments (in order of appearance). This
// order will coincide with the order of appearance of grids in the 
// GridsPerFunc structure. So we will be able to "match" them and check reads 
// and writes and identify if parallelism is broken.
//----------------------------------------------------------------------------
function recordGridsAndIndicesOfStepBoxes() {

    // TODO: Need for all modules???
    var mO = CurModObj;

    // Used to store name and instances as RHS/LHS, as with 
    // nonScalarGridInstances.
    var gridsInFunc;

    // Used to store functions visited so far (in the context of exploring one
    // function). Used to keep track of recursive function calls from a 
    // function to another.
    var funcStack;

    // Used to store current (caller) function's arguments.
    var functionArgs;

    // We need information for all functions.
    // Starting from 3 (main not included in analysis).
    for (var f = 3; f < mO.allFuncs.length; f++) {

        gridsInFunc = new Array(); // Re-initialized 

        funcStack = new Array(); // Re-initialized

        // Here I need to initialize with CURRENT function's argument names.
        functionArgs = new Array();

        var fO = mO.allFuncs[f];
        //alert("Current function alanyzed:" + fO.funcCallExpr.str);

        funcStack.push(fO.funcCallExpr.str); // Add current to stack.

        for (var g = 0; g < fO.allGrids.length; g++) {

            var gO = fO.allGrids[g];

            // We only need to analyze inc arguments.
            if (gO.inArgNum >= 0) { // This grid is an incoming arg.

                // We only need to analyze grids passed by reference,
                // i.e., multidimensional grids only (scalar passed by value).
                if (gO.numDims > 0) {

                    //alert(g + " is the grid id of:" + gO.caption);

                    gridsInFunc.push(new LoopGridAnalysisObj(gO.caption,
                        null, null));

                }

                // Record all input grid arguments, so as to do the "renaming"
                // during "inlining" called functions.
                // TODO: CAUTION: In actual code in main look how I do it
                // below (getting caller's arguments when calling!)
                functionArgs.push(gO.caption);

            }

        }

        var tmp_args = "";

        for (var i = 0; i < functionArgs.length; i++) {

            tmp_args += "," + functionArgs[i];

        }
        //alert("tmp_args=" + tmp_args);

        // And finally traverse the boxes to add RHS/LHS, like "inlining".
        traverseStepBoxes(functionArgs, fO, gridsInFunc, funcStack);

        // TODO: CAUTION Reference copied (when gridsInFunc=new Array() this  
        // is not lost?)
        GridsPerFunc[f] = gridsInFunc;

    }

}


//----------------------------------------------------------------------------
//Prints all function grids information for debugging purposes.
//----------------------------------------------------------------------------
function PrintGridsPerFunc() {

    // TODO: Need for all modules???
    var mO = CurModObj;

    for (var f = 3; f < mO.allFuncs.length; f++) {

        alert("Function name:" + mO.allFuncs[f].funcCallExpr.str);

        for (var i = 0; i < GridsPerFunc[f].length; i++) {

            alert("XEXE" + GridsPerFunc[f][i].name + "isLHS/isRHS: " +
                GridsPerFunc[f][i].isLHS + "/" + GridsPerFunc[f][i].isRHS
            );
            /*
	    var curGrPerF = GridsPerFunc[f][i];
            for (var j=0; j<curGrPerF.dimsWrittenLHS.length; j++){
				
	        for (var k=0; k<curGrPerF.dimsWrittenLHS[j].length; k++) {
			
		    alert("LHS:" + curGrPerF.dimsWrittenLHS[j][k] + 
			", BOX: " + curGrPerF.dimsWrittenLHSboxNum[k]);
								
		}
				
	    }
	
	    for (var j=0; j<curGrPerF.dimsWrittenRHS.length; j++){
			
	         for (var k=0; k<curGrPerF.dimsWrittenRHS[j].length; k++) {
			
		     alert("RHS:" + curGrPerF.dimsWrittenRHS[j][k] + 
			", BOX: " + curGrPerF.dimsWrittenRHSboxNum[k]);
				
		}
				
	    }
	    */
        }

    }

}


//----------------------------------------------------------------------------
// Return the argument as called from main that corresponds to the argument 
// in current function's argument list or if local, then the local one.
//----------------------------------------------------------------------------
function whatToFind(functionArgs, myArgs, curName) {

    var found = -1;

    for (var i = 0; i < myArgs.length; i++) {

        //alert("Comparing: " + myArgs[i] + " with: " + curName);

        if (myArgs[i] == curName) {

            found = i;
            break;

        }

    }

    // If found replace what will be added with the name of callee
    if (found != -1) {

        //alert("A Will search for: " + functionArgs[found]);
        return functionArgs[found];

        // else keep this (won't be added since not in callee anyway)
    } else {

        //alert("B Will search for: " + curName);
        return curName;

    }

}


//----------------------------------------------------------------------------
// Traverse all boxes of all steps of a function to record uses of non-scalar 
// grid arguments as LHS or RHS within the function boxes in all steps.
//----------------------------------------------------------------------------
function traverseStepBoxes(functionArgs, fO, gridsInFunc, funcStack) {

    // TODO: Need for all modules???
    var mO = CurModObj;

    // TODO: Do I Have to do this for all steps?
    // TODO: In myArgs save MY parameters
    var myArgs = new Array();

    // Now loop through all steps of current function to identify uses of
    // grids of interest (i.e., the ones identified as inc arguments).

    //alert("[B]Current function alanyzed:" + fO.funcCallExpr.str);

    for (var g = 0; g < fO.allGrids.length; g++) {

        var gO = fO.allGrids[g];

        // We only need to analyze inc arguments
        if (gO.inArgNum >= 0) { // This grid is an incoming arg

            myArgs.push(gO.caption);

        }
    }

    // TODO: CAUTION starting with one step support only. Do for all.
    for (var stepNum = 1; stepNum < fO.allSteps.length; stepNum++) {

        var sO = fO.allSteps[stepNum];
        DimsIteratedOn = new Array(); // Like DimsWrittenInStep.
    
        // Here get information from index range box (if it is not empty).
        // Will be used during traversing the rest of the boxes to detect
        // any reads or writes on indices containing row/col/indX of the
        // variable grids passed as parameters in the function, in which case
        // the dimension of such grids will be marked non-parallelizable.

        // TODO: CAUTION if ft_row, etc when compared, too, later.

        // Note: rangeExpr.exprArr[] contains root range expressions.
        var rangeExpr = sO.boxExprs[CodeBoxId.Range];

        // if there are index variables
        if (rangeExpr && rangeExpr.exprArr && rangeExpr.isForeach()) {

            var num_index_vars = rangeExpr.exprArr.length;

            for (var iv = 0; iv < num_index_vars; iv++) {

                var rexpr = rangeExpr.exprArr[iv];
                assert(rexpr.gO, "Range expr must have a gO");

                // TODO: FIX FIX FIX
                var ivar = var2Fortran(rexpr.labelExpr.str);

                // These are the dimensions across which loop iterates.
                DimsIteratedOn.push(ivar);

            }

        }

        for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

            var boxexpr = sO.boxExprs[box];

            if (boxexpr.isIf() || boxexpr.isElseIf()) {


                processIfElseifRHSforFunc(functionArgs, myArgs, boxexpr,
                    gridsInFunc, box,
                    sO);

            }

            // TODO: CAUTION DO we need to worry about reading of variables in
            // IF/ELSEIF statements?
            if (!sO.boxAttribs[box].isMask()) {

                // Step: process a formula statement.

                //alert("Expression: " + expr2FortranString(boxexpr));

                if (sO.boxAttribs[box].isFormula() && boxexpr.exprArr.length >
                    2 && (boxexpr.exprArr[1].isAssignOp() || boxexpr.exprArr[
                        1].isXAssignOp())) {
                    // Should cover all cases: simple equality OR
                    // the case of +=, -=, *=, /= (assertion of valid 
                    // exprArr[1] is guarranteed by previous steps in GUI).

                    // TODO: Here what if name changes (or is set by LET). 
                    // Need to take special care.
                    // TODO: Also, will need to take special care for multiple
                    // types per ROW/COL.


                    // Here we are in an assignment statement, so examine LHS 
                    // and add its needed properties in the corresponding 
		    // object.
                    // TODO: we need to check if we are IN A LOOP, otherwise 
                    // don't care.

                    // If +=, -=, etc., we'll need to save this LHS as RHS, 
		    // too.
                    // We push this temporarily so that it is processed by 
		    // later loop and at the end we pop it when no longer 
		    // needed.
                    if (boxexpr.exprArr[1].isXAssignOp()) {

                        boxexpr.exprArr.push(boxexpr.exprArr[0]);
                        // No need to copy fields, since whole object is 
			// pointed to:
                        // boxexpr.exprArr[length-1].isGridCell
                        // boxexpr.exprArr[length-1].gO

                    }

                    if (boxexpr.exprArr[0].isGridCell() && boxexpr.exprArr[
                            0].gO.numDims) { // LHS is a grid.

                        var XXX = whatToFind(functionArgs, myArgs,
                            boxexpr.exprArr[0].gO.caption);
                        // 0 means as LHS.	
                        
                        addNonScalarForFunc(functionArgs, myArgs, XXX,
                            boxexpr.exprArr[0], gridsInFunc, box, sO,
                            0);

                    } else { // LHS is a scalar.
                        // TODO: Is there any other subcase?

                        // Do nothing.

                    }


                    // In current formula box: Loop through the exprArr[] 
                    // array and detect if there are functions (isFuncCall()).
                    // This will take care of any +=, -= LHS as RHS cases 
		    // (pushed earlier in boxexpr.exprArr last position)
                    for (var lp = 2; lp < boxexpr.exprArr.length; lp++) {
                        // TODO: If +=, -=, etc. then checking 2 is redundant?


                        // TODO: Here check all RHS grids and run analysis 
                        // function
                        // In that will set variable per dimension is 
                        // parallelizable at the end will add openmp
                        // directives across parallelizable dims.
                        // How about if there are separable statements??? How 
			// to? (later)


                        // Here we are in an assignment statement, so examine 
                        // LHS and add its needed properties in the 
			// corresponding object. 
                        
                        // TODO: we need to check if we are IN A LOOP, 
                        // otherwise don't care.
                        if (boxexpr.exprArr[lp].isGridCell() && boxexpr.exprArr[
                                lp].gO.numDims) { // LHS is a grid

                            var XXX = whatToFind(functionArgs, myArgs,
                                boxexpr.exprArr[lp].gO.caption);
                            
                            addNonScalarForFunc(functionArgs, myArgs, XXX,
                                boxexpr.exprArr[lp], gridsInFunc, box,
                                sO, 1); // 1 means as RHS	

                        } else if (boxexpr.exprArr[lp].isGridCell() && !
                            boxexpr.exprArr[lp].gO.numDims) {

                            // Do nothing.

                        } else { // TODO: Do for lib func calls too.

                            //alert("Other:" + boxexpr.exprArr[lp].str);

                            if (boxexpr.exprArr[lp].isUserFuncCall()) {

                                // TODO: CAUTION: If NOT including as parameter
                                // any of the parameters, no need to analyze.
                                // Or just proceed and next traverse will not 
				// add what's NOT in the args list (?) [this is
				// what is done at this point].

                                // Scan through stack. If you don't find 
				// current, push in stack and proceed with 
				// traversing it.
                                // TODO: No need for pops, etc? Is this check 
				// the only thing needed? Test.
                                var found = 0;
                                for (var i = 0; i < funcStack.length; i++) {

                                    if (funcStack[i] == boxexpr.exprArr[
                                            lp].str) {

                                        // Function has been analyzed, no need
					// to again.
                                        found = 1;
                                        break;

                                    }

                                }

                                // If called function has NOT been analyzed in
				// the current series of calls, analyze it, 
				// else ignore it.
                                if (!found) {
                                    funcStack.push(boxexpr.exprArr[lp].str);

                                    var functionArgs_t = new Array();

                                    // This prints ALL arguments PASSED to the
				    // function.
                                    for (var p = 0; p < boxexpr.exprArr[
                                            lp].exprArr.length; p++) {

                                        functionArgs_t.push(boxexpr.exprArr[
                                            lp].exprArr[p].str);

                                    }

                                    var f = getFuncIdByName(mO, boxexpr.exprArr[
                                        lp].str);
                                    var fO_t = mO.allFuncs[f]

                                    // TODO: Caution do for all steps/for all 
				    // modules?
                                    // var sO = mO.allFuncs[f].allSteps[1];
                                    traverseStepBoxes(functionArgs_t,
                                        fO_t, gridsInFunc, funcStack)

                                    // TODO: Here we'll probably need to take 
				    // care of functions.
                                    // Now it covers anything else besides 
				    // grids and scalars.
                                    
                                }

                            }

                        }

                    }

                    // This will pop the RHS instance of LHS in case of +=,etc
                    if (boxexpr.exprArr[1].isXAssignOp()) {

                        boxexpr.exprArr.pop();

                    }

                } else if (boxexpr.exprArr[0] != null && boxexpr.exprArr[
                        0].type == ExprType.Return) {

                    // Check if return statement in position 0 (can't be 
                    // anywhere else).

                    var return_expression = "";
                    var ret_val_assignment = "";

                    // Parse the expression to be returned and save in order
                    // to assign to the function's name (i.e., value to be 
		    // returned).
                    for (var i = 1; i < boxexpr.exprArr.length; i++) {

                        if (i > 0) return_expression += " ";
                        return_expression += expr2FortranString(
                            boxexpr.exprArr[i]);

                    }

                    if (boxexpr.exprArr.length != 1) {

                        ret_val_assignment = var2Fortran(fO.funcCallExpr.str) +
                            " = " + return_expression;

                    } else {

                        ret_val_assignment = var2Fortran(fO.funcCallExpr.str) +
                            " = " + var2Fortran(fO.allGrids[0].caption);

                    }

                } else {

                    //alert("DEAD CODE?/break/incomplete statement?" + 
                    //		expr2FortranString(boxexpr));

                }

            }

        }

    }

}


//----------------------------------------------------------------------------
// Search if in arg list and if so add non-scalar grid in appropr. structure.
// TODO: Read e.gO.caption instead of e.str, since this may change. 
//----------------------------------------------------------------------------
function addNonScalarForFunc(functionArgs, myArgs, argName, e, 
    nonScalarGridInstances, box, sO, isRHS) {

    //alert("NON-SCALAR for func=" + e.gO.caption + " (" + 
    //		argName + "), isRHS:"+ isRHS);

    var gridAsLHSfound = 0;
    var iter = 0;

    for (iter = 0; iter < nonScalarGridInstances.length; iter++) {

        //alert("Comparing for: " + isRHS + " :" + argName + " with: " + 
        //		nonScalarGridInstances[iter].name);

        if (argName == nonScalarGridInstances[iter].name) {

            //alert("Grid exists in analysis table");
            gridAsLHSfound = 1;
            break;

        }

    }

    if (gridAsLHSfound) {

        if (isRHS) {

            nonScalarGridInstances[iter].isRHS = 1;

        } else {

            nonScalarGridInstances[iter].isLHS = 1;

        }

        // In this case indent and box may not matter if coming from nested
        // function call.
        addDimsWrittenLHSforFunc(functionArgs, myArgs, e, 
	    nonScalarGridInstances[iter],
            sO.dimNameExprs, !isRHS, sO.boxAttribs[box].indent, box,
            sO);

    }

}


//----------------------------------------------------------------------------
// Checking if any of the row/col/indX are contained in current index being
// added.
//----------------------------------------------------------------------------
function checkForeachIndex(index) {

    for (var i = 0; i < DimsIteratedOn.length; i++) {

        if (index.indexOf(DimsIteratedOn[i]) != -1) {

            return 1;

        }

    }

    return 0;

}


//----------------------------------------------------------------------------
// Analyzing function LHS/RHS (irrespective of foreach dims).
// TODO: Indent & box are valid only for current function, not called ones.
//----------------------------------------------------------------------------
function addDimsWrittenLHSforFunc(functionArgs, myArgs, e, gridLHS, indices,
    isLHS, indent, box_id, sO) {

    // TODO: We should allow scalars, too? May cover this too?
    assert(e.isGridCell(), "NOT A GRID CELL, SO NO DIMENSIONS")

    // Read catption instead of e.str because caption can be changed
    var name = var2Fortran(e.gO.caption);

    var lastd = e.exprArr.length - 1;

    var found = 0;

    if (isLHS) {

        for (var i = 0; i <= lastd; i++) {

            //TODO: Will never happen? Delete?
            if (gridLHS.dimsWrittenLHS[i] === undefined) {

                // Create NEW array for position [i]
                gridLHS.dimsWrittenLHS.push(new Array());
                

            }

            if (gridLHS.dimParallelizable[i] === undefined) {

                // Dimension parallel/ble by default, unless changed later.
                gridLHS.dimParallelizable.push(1);
                
            }

            if (gridLHS.containsParVar[i] === undefined) {

                // Create NEW array for position [i]
                gridLHS.containsParVar.push(new Array());

                for (var j = 0; j <= lastd; j++) {

                    // By default row,col, etc. is NOT contained in DIMENSION j	
                    gridLHS.containsParVar[i][j]=0;

                }

            }

            // For functions only: Check if index CONTAINS any of the 
	    // row/col/indX.
            // If so, then the grid that is passed as a parameter, is written 
	    // or read in multiple locations, so under certain circumstances 
	    // (i.e., any case besides grid read-only in both main and 
	    // function) is non-parallelizable. So, mark as such.
            var check_foreach_index = checkForeachIndex(var2Fortran(
                indices[i].str));

            if (check_foreach_index) {

                gridLHS.dimParallelizable[i] = 0;

            }

            // Here "translate" to caller's parameter if it is a function 
	    // parameter instead of pushing expr2FortranString(e.exprArr[i]).
            // TODO: Make sure we cover ALL possible cases (esp. in 'else').
            var XXX = whatToFind(functionArgs, myArgs, e.exprArr[i].str);
            if (XXX == "") {
                XXX = expr2FortranString(e.exprArr[i]);
            } else {
                if (!e.exprArr[i].isNumber())
                    XXX = "fun_" + XXX;
            }

            
            for(var j=0; j<sO.dimNameExprs.length; j++){

                if(XXX.indexOf(sO.dimNameExprs[j].str)!=-1) {

                    //alert(XXX + ": DIM= " + i + 
		    //	"contains indexVar= " + sO.dimNameExprs[j].str);
                    gridLHS.containsParVar[i][j]=1;

                }

            }

            gridLHS.dimsWrittenLHS[i].push(XXX);

            if (found == 0) {

                gridLHS.dimsWrittenLHSindent.push(indent);
                gridLHS.dimsWrittenLHSboxNum.push(box_id);
                gridLHS.stepsWrittenLHS.push(sO); 
		// To save step (to use in analyzeMainsFunctions).

            }

            found = 1;

        }

    } else {

        for (var i = 0; i <= lastd; i++) {

            //TODO: Will never happen? Delete?
            if (gridLHS.dimsWrittenRHS[i] === undefined) {

                // Create NEW array for position [i].
                gridLHS.dimsWrittenRHS.push(new Array());
         
            }

            if (gridLHS.dimParallelizable[i] === undefined) {
                // Dimension parallel/ble by default, unless changed later.
                gridLHS.dimParallelizable.push(1);
            }

            if (gridLHS.containsParVar[i] === undefined) {

                // Create NEW array for position [i]
                gridLHS.containsParVar.push(new Array());

                for (var j = 0; j <= lastd; j++) {
	
                    gridLHS.containsParVar[i][j]=0;

                }

            }

            // For functions only: Check if index CONTAINS any of the 
	    // row/col/indX.
            // If so, then the grid which is passed as a parameter, is written
	    // or read in multiple locations, so under certain circumstances 
	    // (i.e., any case besides grid read-only in both main and 
	    // function) is non-parallelizable. So, mark as such.
            var check_foreach_index = checkForeachIndex(var2Fortran(
                indices[i].str));

            if (check_foreach_index) {

                gridLHS.dimParallelizable[i] = 0;

            }

            // Here "translate" to caller's parameter if it is a function 
	    // parameter instead of pushing expr2FortranString(e.exprArr[i]).
            // TODO: Make sure we cover ALL possible cases (esp. in 'else').
            var XXX = whatToFind(functionArgs, myArgs, e.exprArr[i].str);
            if (XXX == "") {
                XXX = expr2FortranString(e.exprArr[i]);
            } else {
                if (!e.exprArr[i].isNumber())
                    XXX = "fun_" + XXX;
            }


            for(var j=0; j<sO.dimNameExprs.length; j++) {

                if(XXX.indexOf(sO.dimNameExprs[j].str)!=-1) {

                    //alert(XXX + ": DIM= " + i + "contains indexVar= " + 
		    //     sO.dimNameExprs[j].str);
                    gridLHS.containsParVar[i][j]=1;

                }

            }

            gridLHS.dimsWrittenRHS[i].push(XXX);

            if (found == 0) {

                gridLHS.dimsWrittenRHSindent.push(indent);
                gridLHS.dimsWrittenRHSboxNum.push(box_id);
                gridLHS.stepsWrittenRHS.push(sO); 
		// To save step (to use in analyzeMainsFunctions).

            }

        }

    }

}


//----------------------------------------------------------------------------
// Processes if/elseif stmts and adds grids appearing in args list of 
// function.
//----------------------------------------------------------------------------
function processIfElseifRHSforFunc(functionArgs, myArgs, e, 
    nonScalarGridInstances, box, sO) {

    for (var i = 0; i < e.exprArr.length; i++) {

        if (e.exprArr[i].isGridCell()) {

            if (!e.exprArr[i].gO.numDims) { // Non-scalar.

                //alert("scalar grid:" + e.exprArr[i].str);
                //addScalarGridInstance(e.exprArr[i], 
		//	scalarGridInstances, box, sO, 1);
                //Do nothing

            } else { // Scalar grid.

                //alert("non-scalar grid:" + e.exprArr[i].str);
                var XXX = whatToFind(functionArgs, myArgs, e.exprArr[i].gO
                    .caption);

                addNonScalarForFunc(functionArgs, myArgs, XXX, e.exprArr[
                    i], nonScalarGridInstances, box, sO, 1);

            }

        } else {

            // TODO: Take care of functions or anything else that may  
            // appear in if/elseif stmt as RHS variable.
            // TODO: CAUTION: What if row/col/endX appear here? 
            // We should not change them, so should be ok.

        }

    }

}


//----------------------------------------------------------------------------
// Used to add information for function calls of main function, so as to 
// find any dependencies by "inlining" called functions.
//----------------------------------------------------------------------------
function FuncsCalledPerFuncInfo(name, f, functionArgs_t, allArgs_t) {

    // Name of called function from main.
    this.name = name;

    // Id of function, as returned by getFuncIdByName() - 0 is main
    // rest are 1,2,... in the order they are defined.
    this.gID = f;

    // Array containing ARRAYS of arguments (will be created when needed)
    // args[0][0], args[0][1], etc. will contain all instances of 1st  
    // non-scalar grid that appears in the function arguments list.
    // args[1][0], args[1][1], etc. the second, and so on.
    // No duplicates will be allowed, each grid will appear ONCE.
    this.args = new Array();

    // Array containing names of ALL args at function call for all call 
    // instances, e.g. if func(Out, row, col), func(Out2, row+4,col) will be:
    // allArgs[0][0]=Out, [0][1]=Out2
    // allArgs[1][0]=row, [1][1]=row+4
    // allArgs[2][0]=col, [2][1]=col, etc.
    this.allArgs = new Array();

    // Initialization
    for (var i = 0; i < functionArgs_t.length; i++) {

        this.args[i] = new Array();
        this.args[i].push(functionArgs_t[i]);

    }

    for (var i = 0; i < allArgs_t.length; i++) {
        //alert("ADDING:"+allArgs_t[i]);
        this.allArgs[i] = new Array();
        this.allArgs[i].push(allArgs_t[i]);

    }

}


//----------------------------------------------------------------------------
// Performs a deep copy of the array of FuncsCalledPerFuncInfo objects.
// Used to save current function's array before analyzing a nested function.
// and to retrieve it afterwards.
//----------------------------------------------------------------------------
function doFuncCallDeepCopy(origFuncsFromCaller) {

    var funcsFromCallerDeepCopy = new Array();

    for (var i=0; i<origFuncsFromCaller.length; i++) {

        var name = origFuncsFromCaller[i].name;
        var gID = origFuncsFromCaller[i].gID;
        var args = new Array();
        var ctr = 0;
        for (var j=0; j < origFuncsFromCaller[i].args.length; j++) {
            for (var k=0; k < origFuncsFromCaller[i].args[j].length; k++) {
                args[ctr] = origFuncsFromCaller[i].args[j][k];
                ctr++;
            }
        }

        var allArgs = new Array();
        ctr = 0;
        for (var j=0; j < origFuncsFromCaller[i].args.length; j++) {
            for (var k=0; k < origFuncsFromCaller[i].args[j].length; k++) {
                allArgs[ctr] = origFuncsFromCaller[i].allArgs[j][k];
                ctr++;
            }
        }
        
        funcsFromCallerDeepCopy.push(new FuncsCalledPerFuncInfo(name, gID, args, allArgs));

    }

    return funcsFromCallerDeepCopy;
}


//----------------------------------------------------------------------------
// Prints all information for function calls from Main.
//----------------------------------------------------------------------------
function printFuncCalledPerFuncInfo() {

    var arguments_per_position;

    for (var i = 0; i < FuncsFromCaller.length; i++) {

        arguments_per_position = "";

        alert("Function Name called from main: " + FuncsFromCaller[i].name);

        for (var j = 0; j < FuncsFromCaller[i].args.length; j++) {

            for (var k = 0; k < FuncsFromCaller[i].args[j].length; k++) {

                if (k < FuncsFromCaller[i].args[j].length - 1) {

                    arguments_per_position += FuncsFromCaller[i].args[j][k] +
                        ", ";

                } else {

                    arguments_per_position += FuncsFromCaller[i].args[j][k];

                }

            }

            alert("Arguments for grid no: " + j + " are: " +
                arguments_per_position);
            arguments_per_position = "";

        }

        arguments_per_position = "";

        for (var j = 0; j < FuncsFromCaller[i].allArgs.length; j++) {

            for (var k = 0; k < FuncsFromCaller[i].allArgs[j].length; k++) {

                if (k < FuncsFromCaller[i].allArgs[j].length - 1) {

                    arguments_per_position += FuncsFromCaller[i].allArgs[j]
                        [k] +
                        ", ";
                } else {

                    arguments_per_position += FuncsFromCaller[i].allArgs[j]
                        [k];

                }

            }

            alert("All arguments for grid no: " + j + " are: " +
                arguments_per_position);
            arguments_per_position = "";

        }

    }

}


//----------------------------------------------------------------------------
// Takes care of a function call parsing and adding as necessary to  analysis
// exprArrInd is 0 when called for standalone functions and non-zero, when
// called for RHS (from parseRightHandSide).
//----------------------------------------------------------------------------
function parseFunctionCall(mO, boxexpr, box, exprArrInd) {

    // TODO: Here we'll probably need to take care of 
    // functions.

    var functionArgs_t_all = new Array();
    var functionArgs_t = new Array();

    // This prints ALL arguments PASSED to the function.
    for (var p = 0; p < boxexpr.exprArr[exprArrInd].exprArr.length; p++) {

        functionArgs_t_all.push(boxexpr.exprArr[exprArrInd].exprArr[p].str);

        if (boxexpr.exprArr[exprArrInd].exprArr[p].isGridRef()) {
            if (GridRefsPerStep.indexOf(boxexpr.exprArr[exprArrInd].exprArr[
                    p].str) == -1)
                GridRefsPerStep.push(boxexpr.exprArr[exprArrInd].exprArr[
                    p].str);
            // Keep ALL instances, so we can treat as a scalar appearing in 
	    // multiple boxes.
            // Will need this information in order for addScalar to add as a 
	    // scalar.
            GridRefsPerStepAll.push(new Array(boxexpr.exprArr[exprArrInd]
                .exprArr[p], box, p, boxexpr.exprArr[exprArrInd].str
            ));

        }

    }

    var f = getFuncIdByName(mO, boxexpr.exprArr[exprArrInd].str);
    var fO_t = mO.allFuncs[f];

    for (var g = 0; g < fO_t.allGrids.length; g++) {

        var gO = fO_t.allGrids[g]; // 

        // We only need to analyze inc arguments.
        if (gO.inArgNum >= 0) { // this grid is an incoming arg.

            // We only need to analyze grids passed by reference.
            // i.e., multidimensional grids only (scalar passed by value).
            if (gO.numDims > 0) {

                functionArgs_t.push(functionArgs_t_all[g - 1]); 
		// Shifted by one since g starts including return value which
		// is not present in caller arguments.
		//
            }

        }

    }

    // Here record id of function called in main AND the non-scalar grid 
    // arguments list that corresponds to the list that has been 
    // captured earlier for all functions PER STEP of main.
    var found = -1;
    for (var i = 0; i < FuncsFromCaller.length; i++) {

        if (f == FuncsFromCaller[i].gID) {

            found = i;

        }

    }

    if (found == -1) {

        // Function is called for first time in Main.
        // Initialize name, gID and arguments.
        FuncsFromCaller.push(new FuncsCalledPerFuncInfo(boxexpr.exprArr[
	    exprArrInd].str, f, functionArgs_t, functionArgs_t_all));

    } else {

        // Function has been called again from Main.
        // Just add new arguments in existing element 
        // i of FuncsFromCaller that has been found.
        // Need to check for DUPLICATE arguments, so 
        // we don't add them again.
        for (var j = 0; j < FuncsFromCaller[found].args.length; j++) {

            var found_2 = 0;

            for (var k = 0; k < FuncsFromCaller[found].args[j].length; k++) {

                if (FuncsFromCaller[found].args[j][k] == functionArgs_t[j]) {

                    found_2 = 1;
                    break;

                }

            }

            // If argument not found in previous call, add 
            // it in its appropriate order of call.
            if (!found_2) {

                FuncsFromCaller[found].args[j].push(functionArgs_t[j]);

            }

        }

        // TODO: CAUTION check after Ruchira's fix on DELETED.
        for (var j = 0; j < FuncsFromCaller[found].allArgs.length; j++) {
	
            FuncsFromCaller[found].allArgs[j].push(functionArgs_t_all[j]);

        }

    }

}
