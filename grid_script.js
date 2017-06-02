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

// Purpose: Main script that implements user interface
// Author : Ruchira Sasanka
// Date   : Nov 1, 2013

/****************************** Explanations ******************************/

// Titles vs. Indices:
//-------------------
// - Titles are non-editable after configuration. Indices are editable and
//   customizable for each usage of the grid. Hence, indices can be different
//   for a given step (we need to clone indices but not titles).
// - As per above, we need to have both indices AND titles displayed for
//   each dimension.
// - Any dimension can have either titles OR indices displayed.

// ReturnValue convention:
//-----------------------
// If the user returns something explicitly with the return statement,
// that value is returned (and implicityly written to the ReturnValue).
// If the user has a return without any argument, the ReturnValue will
// be returned by default.


/*************************** Global Constants ****************************/

// These are the HTML IDs given to various structures (tables) drawn
//
var AllHtmlGridIds = ['outMenu', 'src1Menu', 'src2Menu', 'src3Menu',
    'src4Menu', 'src5Menu', 'src6Menu', 'src7Menu',
    'src8Menu', 'src9Menu', 'src10Menu',
    'src11Menu', 'src12Menu', 'src13Menu', 'src14Menu',
    'src15Menu', 'src16Menu', 'src17Menu', 
    'src18Menu', 'src19Menu', 'src20Menu', 
    'src21Menu', 'src22Menu', 'src23Menu', 'src24Menu',
    'src25Menu', 'src26Menu', 'src27Menu', 
    'src28Menu', 'src29Menu', 'src30Menu', 
    'src31Menu', 'src32Menu', 'src33Menu', 'src34Menu',
					  //DC5: Increased, otherwise only
					  // supports up to 11 grids (src
					  // and output).
];

// Data types supported:
//
// NOTE: These types MUST match the following DataTypes enumeration
//
// NOTE: UniqInd (unique index) type facilitates easy parallelization. It 
//       means a given row (or column) with that data type has unique
//       integer values (keys)
//
// NOTE: TODO: 'func' data type is NOT implemented yet. 
//       It would  allow to specify methods of 'objects'. This
//       allows an array of functions (similar to array of function pointers),
//       which is useful for implementing event handlers -- e.g., vectored
//       interrupt/exception handlers. If 'func' names are not constant, this
//       would allow late binding. 
//
var TypesArr = ['integer', 'real', 'string', 'boolean', 'char', 'real_sp',
    'uniqInd', 'func', 'void' //DC5: Added void datatype
];
//
var DataTypes = {
    Integer: 0,     // integers
    Real: 1,        // double precision real numbers by default
    String: 2,      // character strings
    Bool: 3,        // boolean
    Char: 4,        // a single character  
    Real4: 5,       // single precision real numbers
    UniqInd: 6,     // unique index -- integer type w/ no duplicate values
    Func: 7,
    Void: 8	    //DC5: added void data-type (for function retVal type)
};


// The following are the keywords (resevered words) of the language
//
var Keywords =
    ['foreach', 'forever', 'for', 'end',
        'let', 'break', 'return', 'continue',
        'if', 'else',
        'row', 'col', 'ind2', 'ind3', 'ind4', 'ind5'
    ];



// HTML ID given to the output grid
//
var OutHtmlId = AllHtmlGridIds[0];

// Actual Size Selction drawn at this HTML ID
//
var ActSzSelectHtmlId = AllHtmlGridIds[1];

// Grids are drawn in this HTML ID for 'preview', while they are selected
//
var PreviewHtmlGridId = 'previewDiv';

var INVID = -500;       // invalid ID. Must be smaller than -1. 
var MININT = -10000;    // minimum integer value (for init)
var ROOTPOS = -1;       // special id when child is the root expression

// The following MUST be zero. They are used just to make code more readable.
//
var FuncHeaderStepId = 0;     // Step ID of Function Header (in a function)
var TemplateFuncId = 0;       // Func ID of a template (in module)


/***************************** Default Values ******************************/

// Default show cols/rows. Same are used for default act cols/rows
//
var DefShowCols = 7;
var DefShowRows = 7;
//
// Number of tabs in a dimension by default.
//
var DefNumTabsInDim = 5;
//
var DefDimTitleRoot = "Tab";
var DefDimIndRoot = "dim";
//
// Grid names (captions) given by default to out/copy/return/... grids
//
var DefOutGridName = "Out";
var DefCopyGridName = "Copy";
var DefRetValName = "ReturnValue";
//
// If you change name 'StartUpGrid', you must change the same variable name
// in calcData()
//
var DefStartupGridName = "StartUpGrid";      // Actual startup grid name
var DefGlobalScopeName = "Global";
var DefStartModName = "Module1";
var DefStartupArgsName = "StartUpArgs";      // startup arg name (param)
var DefMainFuncName = "Main";
var DefMainFuncInd = 2;
var DefStepTitle = "Title of Step";
var DefHeaderTitle = "Function Header";
var DefProgName = "prog1.grd";
var DefEndName = "end";
var DefStartNameExt = "startLoc"; //MPI:
var DefEndNameExt = "endLoc"; //MPI:
var DefDeletedName = "DELETED";
var DefNeedUpdateName = "UPDATE";
//
var DefFuncName = "func";
var DefRowIndName = "row";
var DefColIndName = "col";
var DefDimIndNameRoot = "ind";
var DefTitleRoots = ['RowTitle', 'ColTitle'];


// Whether to check the validity of 'output' grid. 
// Make this 'true' for actual usage cases
//
var CheckDefOutGridName = false;

// Define the ids for row/col dimensions. These are constants.
// Some algorithms assume Row to the 0th dimension. If these values are
// changed, thorough validation must be done.
//
var RowDimId = 0;
var ColDimId = 1;

// Colors of 2D panes (tabs). These colors are repeated for 'higher' tabs. 
//
var TabColors = ["fafafa", "f2f2f2", "dcdcdc", "c3c3c3"];

// Function IDs of functions present by default in a module
//
var FuncID = {
    Template: 0,
    Global: 1,
    Main: 2
};

// Code box Id. We also use this as code box TYPE. 
//
var CodeBoxId = {
    INVALID: -1,
    Index: 0,
    BoxStart:1,     /* Point where code boxes start */
    Range: 1,
    Mask: 2,
    Formula: 3,     /* This is the 1st formula */
};


// To indicate which stage we are in while building a step
//
var StageInStep = {
    New: 0,
    ConfigStarted: 1,         // started configuring new grid 
    ConfigDone: 2,            // finished configuring output (new/exist) grid
    SrcSelectStarted: 3,      // Started adding sources
    GridSelectDone: 4,        // Finished adding sources
    AllDone: 4,               // Same as last valid stage
};

// To indicate the state of an item that is deleted by the user, etc.
//
var DeletedState = {
    None: 0,
    Deleted: 1,               // expr is deleted
    NeedUpdate: 2,            // expr needs updating
    PlaceHolder: 3            // place holder (e.g., a func arg)
};


// File acations enumeration to load/store files
//
var FileActions = {
    Read: 0,
    Write: 1,

};

// Show data enumeration to capture show data state
//
var ShowDataState = {
    None: 0,             // not showing data
    DataOnly: 1,         // Only data is showing
    DataAndColor: 2,     // Data and color
    DataImage: 3,        // Data shown as an image
};


// Type (location) of the comment
//
var CommentType = {
    //
    StepTitle:1,         // comment in step title
    StepBox:2,           // comments in boxes in step
    Grid:3,              // comment for a grid

    // There can be comments in row/col/... titles. DimTitle defines the
    // comment type for th *0th* dimension. So, the comment type for 1st 
    // dimension would be in "DimTitle+1", etc.
    //
    DimTitle: 10

};


// Enumeration of options in the main menu
//
var MMenu = {
    Open: 1,
    Save: 2,
    SaveAs: 3,
    ShowData: 4,
    Colorize: 5,
    DataImage: 6,
    HideData: 7,
    ShowCode: 8,
    AddModule: 9,
    UnusedGrids: 10,
    //
    // TODO: Delete the following (used for debugging) --------------------
    //
    ShowFortranCode: 20, // Show FORTRAN generated code
    ShowFortranCodeParallel: 21, // Show FORTRAN parallel generated code
    SaveFortranCode: 22, // Save FORTRAN generated code
    SaveFortranCodeParallel: 23, // Save FORTRAN parallel generated code
    AnalyzeParallelism: 24, // Analyze parallelism 
    ShowCcode: 28, //Show C generated code
    SaveCcode: 29, //Save C generated code
    SaveOCLcode: 30, //OCL: Save OpenCL generated code
    SaveMPICcode: 31, //MPI: Save MPI (C) code
    SaveCcodeParallel: 32 // Save C parallel generated code
};


// Enumeration of options for the grid-config Context Menu
//
var GCMenu = {
    //
    ShowSizes:1,
    Delete:2,    
    Rename:3,
    ShowAllIndices:4,    // ?? show indices for all dimensions
    Copy:5,              // ?? do we allow copy/paste
    Paste:6,
}


// Enumeration for types of expressions
//
var ExprType = { 
    None: 0,        // invalid
    Grid: 1,        // An entire grid (grid name)
    GridCell: 2,    // Cell in a grid
    GridRef:3,      // Grid name as a reference (e.g., func arg, global grid)
    FuncCall: 4,    // function call
    Operator: 5,    // plus, minus, equal, etc
    Index: 6,       // an index var as row, col
    Title: 7,       // a col/row/... title
    Number: 8,      // number
    Range: 9,       // an index range like row=(1:end)
    String: 10,     // a string constant
    Literal:11,     // predefined (literal) name like 'end0' , ... 
                    // TODO: Change this to EndLiteral and use this only for
                    //       end label. Any other literal name should have
                    //       a unique type. Also, ExprObj.grid of this literal
                    //       should be set to point to a grid.
    Concat:12,      // just a concatenation of expression 
    ConcatEdit:13,  // a concat type while being edited (spaces enabled)
    //
    ExprStmt: 20,   // an Eexpression Statement (as in C) -- e.g.., assignment 
    //
    If:21,          // If statement
    Else:22,        // Else statement
    ElseIf:23,      // Else if statement
    BreakIf:24,     // break if
    //
    Foreach:30,     // foreach loop
    Forever:31,     // forever loop
    //
    Return:40,      // Return expression
    Break:41,       // Break
    Continue:43,    // Continue
    //
    Let:42,         // let *definition* for literal substitution
    LetName:43,     // KK: let *use* (just the let name)
    //
    LibConst:50,    // Constant from a library (e.g., PI)
    LibFuncCall:51, // function call from a library
    ConstGridRef:52,// constant grid reference (for libraries)

    END:60
};

// Types of syntax errors
//
var SynErr = {
    None: 0,
    MisplacedOp: 1, // misplaced operator   
    TwoIdents: 2,   // two identifies next to each other
    TwoOps:3,       // two operators next to each other
    GridRefMixed:4, // grid reference misplaced
    OpAtEnd:5,      // operator at the end
	
};

// Fileds of a range -- used to define a range. They must be in the
// same order (starting at 0) as we expect them to appear -- e.g.,
// row(start:end:step)
//
var RangeFields = {
    GridName:0,
    Start:1,
    End:2,
    Step:3
};

// Enumeration for tips for user based on the context
//
var TipId = {
    DimInsert: 0,
    IndexEdit: 1,
    TitleClick: 2,
    EditTitles: 3,
    NoPeriod: 4,
    RootEdit: 5,
    SelectGrids: 6,
    GridRefClick:7,
    NoScalarGrid:8,
    SelectKeyword:9,
    ScalarGridRefClick:10,
    CellIndReplace:11,
    ReConfigSizes:12,
    AvoidBreak:13,
    AvoidReturn:14,

    End:15
}

var TipArr = [
    // 0
    'First click on an index name in Index Range OR an index cell in a grid',
    // 1
    'Click on Index Name (below the grid) or operator/number to edit',
    // 2
    'To edit a title, select Configure. To insert a title name into a field ' +
    'in  Index Range, first select it',
    // 3
    'To edit the grid name or a title, click on it. ' +
    'Use the drop down list to change data type ' +
    'of table/rows/columns/etc ',
    // 4
    'Decimal numbers not accepted for index expressions',
    // 5
    'Click on a keyword button to replace a keyword. To delete an entire ' +
    'mask, press "Delete Mask" button',
    // 6
    'Please complete grid selection first',
    // 7
    'A Grid Name can be inserted only into as a function argument or ' +
    'a range dimension',
    // 8
    'No scalar (integer) grids exist yet. You can pick a value only from ' +
    'a scalar (integer) grid',
    // 9
    'Please select a keyword first to replace it',
    // 10
    'Cannot click on a scalar grid name. Click on the scalar grid cell instead',
    // 11
    'Directly editing array expression disables cell highlighting. Consider ' +
    ' editing grid indices instead',
    // 12
    'Edit sizes of the grid.',
    // 13
    'Please avoid using break when possible. To improve auto-parallelization' + 
    ', instead, consider using an if-else condition.',
    // 14
    'Please avoid using return statement to return from the middle of a loop.'+
    ' Instead, assign to ReturnValue (and use if-else condition if necessary)'
// 

];


/*************************** Global Variables ****************************/

var CurHtmlGridId;  // HTML Grid name currently operating on (w/ event handlers)

var NewGridObj;     // When a *new* grid is created, we keep a reference
                    // to it here while it is configured (w/ event handlers)
                    // used only upto a *new* grid is configured

var CommentObj;     // comments are edited in this object

// References to current module, function, step that the user is working on.
// These serves as short cut references
//
var CurProgObj;         // current program object
var CurModObj;          // current module object
var CurFuncObj;         // current function object
var CurStepObj;         // Current step object

var CurFuncSavId;       // while saving, current function id processed

var CurIndObj;          // while reconfiguring a grid, to store index obj

// These variables are used only during saving
//
var SavModObj;
var SavFuncObj;

// These variables are used while generating code, to provide information
// about the location of errors
//
var SynErrLoc = null;


// Temporary array used to store valid existing grids that the user
// can select from. This array does not need to be saved.
//
var ExistingGridList;

// Json string for the entire program (to be saved)
//
var ProgJsonStr = "";

// TEMPORARY: This is a temporary solution for demo purposes only
// Program file names. The very first one is a dummy one -- it will be 
// displayed but will not be accepted as a file name
//
var ProgFileNames = ["Select", DefProgName, 'code1.txt', 'ex1.grd', 'ex2.grd',
    'filter.grd', 'filter2.grd', 'tmp.txt'
];

// File name of the program loaded
//
var ProgFileName;


// Timer to detect double clicks
//
var Timer = null;


//DC4: Flag for integration mode. Used to show menus/options/etc. and perform
//     appropriate code generation (i.e., without prefixes, e.g., ft_) when
//     we are working in generating code to integrate with existing code.
var CodeIntegrationMode = 1;



/* ********************* SECTION A: Main Objects *************************** */

// ---------------------------------------------------------------------------
// Entire program - to be saved and restored
// ---------------------------------------------------------------------------
function ProgObj(fname) {

    // All library modules. 
    //
    this.libModules = new Array();

    // All user defined modules in the current program
    //
    this.allModules = new Array();

    // Current module the user is working on
    //
    this.curModNum = 0;

    // File name of the current program
    //
    this.fileName = fname;

    // Whether we are displaying data in grids for the entire program.
    // We have this var at the ProgObj level, so that user can see all
    // *global* scope data in all *modules* -- this is required for
    // 'object orientation' support. 
    //
    this.showData = ShowDataState.None;
}



// ---------------------------------------------------------------------------
// Object to represent an entire moudule
// Current thinking:
// A module is a *stand alone*, a separately compilable unit. It can be made
// into a library module as well, which can be linked with many programs.
// Hence a module can be saved on to disk as a separate unit without being
// part of a larger program. Therefore, if a module refers to grids/funcs of
// other modules, such references must be through copies (or names) -- e.g.,
// a template grid from another module is entirely copied. Similarly, a
// module level variable (grid) cannot be directly referenced by another 
// module (but that grid can be an arg to function of another module). 
//
// ---------------------------------------------------------------------------
function ModuleObj(myname, skipInit) {

    this.name = myname;

    this.allFuncs = new Array();   // list of all function is this module

    this.curFuncNum = 0;           // function number user is working on

    // funtions start from this ID -- before this ID, we have global scope
    // grids. This is a constant. 
    //
    this.FuncStartID = 2;          // Functions start after global scope
    this.GlobalScopeID = 1;        // global scope just before funcs start

    // It is important to create this 'global' seq# at the module level
    // because there are function args, module level variables, etc, we
    // have to create expressions for. However, we don't have this at the
    // ProgramObj level in order to facilitate module portability.
    //
    this.nextExprSeq = 1;         // Seq # given to next ExprObj created

    // --------------- initialization --------------------------------

    if (!skipInit) {

	// Must be done afer initializing this.nextExprSeq
	//
	this.templateFunc = new FuncObj();  // grid/func templates
	this.globalFunc = new FuncObj();    // global (static) scope

    }

    // If this module is a library, the implementation of the library
    // is stored in this variable. This need not be saved. This should be
    // initialized when each library is loaded -- which should happen every
    // time a prog is loaded
    //
    this.libObj = null;

}

// --------------------------------------------------------------------------
// Object to represent a function (and function scope)
// --------------------------------------------------------------------------
function FuncObj(skipHeader) {

    // NOTE: These are all grids local to a function. All grids created 
    //       within that function has 'function scope'. There is no separate
    //       'step scope' to limit grids to a step. At the module level,
    //       model the module level as a dummy 'global()' function, which 
    //       can cotain grids on its own (plus formula). When a module 
    //       refer to a library, by default, only the grids within this
    //       global() are visible to the outside.
    // NOTE: The incoming args (grids) are added to this array as well. 
    //
    this.allGrids = new Array();   // array that stores all gridObjs in a func

    this.allSteps = new Array();   // all step objects in the currrent method

    // Does this function represent the global scope. A module level has
    // one global level scope (which is modelled as a function because it
    // can contain both grids and steps -- e.g., to init global grids)
    // TODO: Decide whether this global scope is static or extern
    //       Currently, we treat it as 'static' to avoid having unintended
    //       interactions through global variables. 
    //
    this.isGlobal = false;

    // Does this function represent the template scope - which is the 
    // declaration scope for all grids/functions. This scope is especially
    // useful for libraries
    //
    this.isTemplateScope = false;

    // Is this function callable (visible) from *other* modules
    //
    this.isExtern = false;


    // The function call expr that respresents this function.
    // NOTE: To check arguments to a function, we must use the function
    //       header Step.
    //
    // NOTE: The following must be assigned by the caller to create a 
    //       reference.
    //
    this.funcCallExpr = null; 
    //
    // Return value of the function. This should be of type grid (even when
    // the return value is a scalar, it is a scalar grid)
    //
    this.retExpr = null;

    this.curStepNum = 0;            // step # the user is working on

    // Args are added for selection (select existing grid) and then removed
    // later when drop down list is removed. 
    // -1 : indicates no arg OR return value added
    //  0 : indicates return value added but no incoming args
    //  X : the return value AND X incoming args have been added
    //
    this.argsAdded = -1;

    // ------------------------- Library spcific attribs ------------------

    // Whether this is from a library 
    //
    this.isLibFunc = false;
    //
    // Java script code emulating this library
    //
    //this.libJSstr = "";      

    // TODO: Set this variable for each lib func
    // Library functions need to have the return data type because there
    // is no associated header step to find the data type. Note that
    // we always return a scalar from a function (e.g., int, real, etc)
    //
    this.retDataType = DataTypes.Integer;

    //API: The flag below denotes that the user has selected to include
    // this library as an entry point, and therefore requires the function
    // API to be included in a glaf_api.h auto-generated file.
    this.createAPI = false; // Do not include by default.


    // ---------------------- Initialization -----------------------------

    if (!skipHeader) {

	// Create a function heder StepObj (step 0) and add that as the very 
	// first (0th) step to this FuncObj
	//
	var sO = new StepObj();
	assert((this.allSteps.length == FuncHeaderStepId), "header not step 0");
	//
	this.allSteps.push( sO );
	sO.isHeader = true;
	sO.stageInStep = StageInStep.AllDone;         // trivially mark complete
    }

    this.isRegularFunc = function isRegularFunc() {
	return (!(this.isGlobal || this.isTemplateScope || this.isLibFunc));
    }


}

//----------------------------------------------------------------------------
// Object to represent a 'step' in the program
//
// TODO: arrays to store indices
//
//----------------------------------------------------------------------------
function StepObj() {

    // Grid IDs that belong to this step. GriIDs are at the function scopt
    // -- i.e., in FuncObj
    //
    this.allGridIds = new Array();
    this.allIndObjs = new Array();

    // Code window has multiple 'box'es -- e.g., for range/mask/formula/...
    // Each 'box' contains a root expression (which has sub expressions). 
    // Each root expression is stored in the following array
    // Note: First two entries of the array are always range/mask. The third
    //       entry is a formula. After that, a box can contain either a 
    //       formula or a mask (if/else/elseif)
    //
    this.boxExprs = new Array();     // box expressions
    this.boxAttribs = new Array();   // indentation
    //
    // These define the index variable names for a step
    //
    this.dimNameExprs = new Array(); // names given to each index var

    // is the output grid in this step a new grid the user created for
    // this step? This is used for dynamically allocating a grid. This _could_
    // also be useful in deallocating user deleted grids
    //
    this.isOutGridNew = false;

    // Title of this step
    //
    this.title = null;

    // The following two variables MUST be set/changed together. They indicate
    // which box has active focus. 'focusBoxExpr' contains the root expression
    // of the currently focused box. 
    //
    this.focusBoxId = CodeBoxId.Formula;     // by default, formula box active
    this.focusBoxExpr = null;                // which expr (mask/...) has focus

    // for changing an html object like a grid cell (instead of a code box)
    //
    this.focusHtmlObj = null;


    // The following fields are used while a formula is being built. We need
    // a mechanism to map an html span drawn in a code box to the an
    // exprObj that internally represents an expression. 
    // We locate the active expression/space using tuple
    // <activeParentPath, activeChildPos>. activeParentPath gives a linear
    // string of sequence numbers to describe the parent node in an 
    // ExprObj For this ExprObj, the child
    // at postion activChildPos is the active expression/space. A child
    // expression or its leading space have the same position. When the 
    // leading space is active, isSpaceActive is true; otherwise, 
    // isSpaceActive is false which means that the child ExprObj is active
    // NOTE: We need a tuple (rather than a linear path to the expression
    //       itself) because when we are replacing an expression, we need
    //       to locate exprArr position with activeChildPos. See function
    //       replaceOrAppendExpr()
    // NOTE: Word 'active' is used to describe the expression where input
    //       will go to. When an expression is active AND isSpaceActive=false,
    //       the active expression is *highlighted*. If isSpaceActive=true
    //       then a space related to the active expression is highligted. 
    //
    this.activeParentPath = null;
    this.activeChildPos = INVID;   // which child within above ExprObj is active
    this.activeParentExpr = null;  // active parent expression (ExprObj)
    this.isSpaceActive = false;    // whether space active (instead of ExprObj)
    //
    // when the user is entering a number with the keyboard, the digits
    // entered so far are kept here
    //
    this.numberExpr = null;

    this.editDimNames = false;

    // Comments are stored in the following
    // TODO: convert to an array with constant indices
    // 
    this.indexComment = "";
    this.ifComment = "";
    this.formulaComment = "";
    this.stepComment = "";

    // Parallelism of the step, indicated by the parallelism meter
    //
    this.parallelism = 0;

    // the stage of the step
    //
    this.stageInStep = StageInStep.New;

    // whether we are displaying data in grids
    //
    //this.showData = ShowDataState.None;    

    this.maxDataVal = MININT;     // maximum data value

    this.isHeader = false;        // is this a function header

    this.keyboardEnabled = true;  // whether keyboard input is enabled

    this.potOCLstep = 0; // OCL: Convert to OpenCL if a parallel step.
    			 //      If 0, then serial or OpenMP if parallel.

    this.showOptions = false; // Whether to show options for step.

    // Method to add a source grid
    //
    this.addGridId = function addGridId(gId) {
        this.allGridIds.push(gId); // store grid
    }

}


// ---------------------------------------------------------------------------
// Constructor to create a Grid. This creates a basic Grid object.
// 
// Create an object as:
//  var OutGrid = createGrid(id, "OutGrid", 4, 5, ......);
//
// See: http://www.w3schools.com/js/js_objects.asp
// ---------------------------------------------------------------------------
function GridObj(gridId, caption, actCols, actRows, showCols, showRows,
		 hasColTitles, hasRowTitles, multiCol, multiRow) {

    this.gridId = gridId;              // my Id in CurFuncObj.allGrids[]
    this.caption = caption;
    this.multiCol = multiCol;           // whether multiple cols
    this.multiRow = multiRow;           // whether multiple rows
    this.isTemplate = false;            // is this a template

    // The number of *array* dimensions. 
    // Note: A scalr is treated as 0-dim since it does not have 
    //       column/row titles/indices. Hence, for a sclar multiCol=multiRow=0
    //
    this.numDims = 0;                   // actual number of dimensions
    //
    if (RowDimId == 0) {

	// If there are rows only, then numDims is 1 
	// (only one dimension present). if there are columns, then row 0 
	// is also present by default. All columns go to dim 2. 
	//
	if (multiRow) this.numDims = 1; // if row titles/indices are present
	if (multiCol) this.numDims = 2; // if col titles/indices are present

    } else {
	if (multiRow) this.numDims = 2; // if row titles/indices are present
	if (multiCol) this.numDims = 1; // if col titles/indices are present
    }


    // TODO: Consider grouping the following properties of a dim to a new 
    //       class called DimAttribs. 

    // Each of these is an array of arrays
    // See notes in email about multidimensional javascript
    // Note: The caller must 'push' items into these arrays
    //
    this.dimTitles = new Array();
    //
    this.dimComments = new Array();     // comments for each title in each dim

    // Whether a given dimension has titles OR indices
    // NOTE: The code has the capbility to display both titles AND indices
    //       However, we display only Titles OR indices -- not both.
    //       If a table has titles there is no need for indices -- titles
    //       can be used as start/end of a range, if a range is inserted.
    //
    // WARN: dimHasTitles, dimHasIndices, dimShowSize, dimActSize can contain
    //       extra entry when numDim == 1. TODO: Fix this. 
    //
    this.dimHasTitles = new Array(2);
    this.dimHasTitles[ColDimId] = hasColTitles;
    this.dimHasTitles[RowDimId] = hasRowTitles;
    //
    this.dimHasIndices = new Array(2);
    this.dimHasIndices[ColDimId] = true; // multiCol && !hasColTitles;
    this.dimHasIndices[RowDimId] = multiRow && !hasRowTitles;
    //
    this.dimShowSize = new Array(2);
    this.dimShowSize[RowDimId] = showRows;
    this.dimShowSize[ColDimId] = showCols;
    //
    this.dimActSize = new Array(2);
    this.dimActSize[RowDimId] = actRows;
    this.dimActSize[ColDimId] = actCols;
    //
    // It the actual size of a dimension is given by a *scalar* grid,
    // its *name* is stored in this array. We store name (rather than a
    // gridId, because we should support global variables). At code gen
    // time, we will fill dimActSize[d] using the value of dimDynSize[d].
    //
    this.dimDynSize = new Array(2);

    //MPI:
    this.dimIsExtended = new Array(2);
    this.dimIsExtended[RowDimId] = false;
    this.dimIsExtended[ColDimId] = false;
    this.isGlobal = false;
    this.isDistributed = false;

    this.inExternalMod = false; //NS: Default:false, if in external module, true
    this.nameExternalMod = ""; //NS: Keeps the name of external module if inExternalMod = true

    this.isCommon = false; //NS2: Default:false, if in common block, true
    this.nameCommon = ""; //NS2: Keeps the name of common block if isCommon = true
    this.structCaption = null; //DC3: Implies that grid belongs to an existing
			       // struct, i.e., we want to integrate GLAF code 
			       // with existing code. 
		  	       // This applies when we are in 
			       // GLOBAL scope and select that current grid
		 	       // belongs in existing module.

    this.dimSelectTabs = new Array();   // selected tab/index per dim
    
    // Data types. Only one dimension can have data types. So, we need
    // only a 1D array. However, the location of the type can be any
    // dimension (given by typeInDim). 
    // If the type is *global*, then the fist entry in the array contains
    // that type
    //
    this.dataTypes = new Array();  // containd type *ind* -- from TypesArr[]
    this.typesInDim = -1;          // a neagative value indicates global

    this.isGridRef = false;        // is this grid a ref to another grid
                                   // (Set for incoming func args OR 
                                   //  global grid -- as a reference )
    this.globalRefId = -1;         // if this referes to a global grid, its id

    this.inArgNum = -1;            // input func argument number
    this.isRetVal = false;         // is this the return value
    this.isConst  = false;         // is this a read-only grid

    // Whether there is manully entered init data
    //
    this.hasInitData = false;

    // Data is dynamically allocated as an array of array of .... as 
    // required by the number of dimensions.
    // NOTE: This array MUST be allocated and addressed in the JavaScript
    //       index order -- i.e., raw major style. This is because we 
    //       pass this array to eval() of JavaScript interpreter. If we
    //       delcalre RowDimId=0, then our dimension ordering is NOT in
    //       row-major order. In that case, conversions are necessary. If we
    //       define ColDimId=0, then no conversion is necessary. 
    //
    this.data = null;

    this.dataMax = 1.0;

    // If a grid is deleted (e.g., an incoming arg)
    // 
    this.deleted = DeletedState.None;

    // If this grid is copy/reference of template/another grid, 
    // the following contains the name of the original master grid used to
    // create this grid. This is required for propagating any updates to
    // titles, dimension sizes, etc. The original master can be in the
    // current function or a different function (e.g., template/global scope)
    // or the template scope of a different module (e.g., for a library)
    //
    this.masterName = null;


    // ********* STEP dependent variables (valid only within a step *********
    //           (valid only within the current step)
    //
    // Note: Do NOT clone the following fields
    // 

    // ASSUMPTION: This assumes a gridId is 1:1 associated with an HtmlId in
    //             a step.
    //
    // When we draw a grid on the screen, we set its htmlId with the grid.
    // This assumes the same grid is NOT drawn on the same screen twice --
    // -- i.e., we clone the grid when we want to draw it again.
    //
    this.htmlId = "";

    this.hasSelection = false;     // whether this grid has a cell selection
    this.selIndDim = -1;           // index range highlighted in this dim 
    //
    // An incoming grid cell arg can be represented either as a scalar 
    // value OR a grid cell reference. When we represent a grid cell as a
    // scalar we keep the original reference grid obj here to switch if
    // the user requests to do so.  
    //
    this.refgO = null;             // 

    //this.showData = ShowDataState.None;

    //this.letClicked = false;
    this.curBox = -1;              // for passing box number

    // This is used only while a new template grid is being 'extended' 
    // -- i.e., reconfigured
    //
    this.numDimsOrig = -1;

    // When we are displaying data, if the grid sizes are too large, we will
    // show only portions of a grid. This array indicates the starting position
    // for each dimension for displaying
    //
    this.dimShowStart = new Array(2);
    this.dimShowStart[RowDimId] = 0;
    this.dimShowStart[ColDimId] = 0;

    // Comment string for this grid
    //
    this.comment = null;


    // ------------------ Temporary
    //
    // is this grid obj is being reconfigured (after initiali configuration)
    //
    this.beingReConfig = false;


}

//----------------------------------------------------------------------------
// Index object assicated with a grid -- to show index names like row, row+1
// etc. in a step.
// NOTE: We also store step specific grid properties in the index object
//       E.g., whether a given grid is constant in a given step
//----------------------------------------------------------------------------
function IndexObj(gO) {

    // Dim index expressions are stored in this array
    //
    this.dimIndExprs = new Array();

    // if we have to init indices for a given gridObj, do so now
    //
    if (gO) {
	
	for (var d=0; d < gO.numDims; d++) {               // go thru each dim

	    this.dimIndExprs[d] = new Array();
	    
	    for (var i=0; i < gO.dimShowSize[d]; i++) {     // each ind

		this.dimIndExprs[d].push(null);

	    } // for each ind in dim

	} // for each dim
    }


    // .......................................................................
    // Step specific other attributs
    // .......................................................................
    //
    //this.isGridConst = gO.isConst;

}


//----------------------------------------------------------------------------
// Attributs of a code-window box (e.g., a formula box, mask box, etc)
//----------------------------------------------------------------------------
function BoxAttribs(indent, id) {

    this.indent = indent;
    this.type = id;

    // Relative *forced* indentation (relative to previous box). 
    // Only the user can change this value. Allowed values are:
    //  0 : no indentation from previous 
    // -x : x left tabs from previous  
    // +x : x rigtht tabs from previous 
    //
    this.forceIndent = 0;

    // Comment for this box
    //
    this.comment = null;

    this.isRange = function isRange() {
        return (this.type == CodeBoxId.Range);
    }

    this.isMask = function isMask() {
        return (this.type == CodeBoxId.Mask);
    }

    this.isFormula = function isFormula() {
        return (this.type == CodeBoxId.Formula);
    }


    this.isIndex = function isIndex() {
        return (this.type == CodeBoxId.Index);
    }
}


//----------------------------------------------------------------------------
// Represents an expression that is used in a formula
// e.g., row in out[row], row+1 in out[row+1], b[row] in out[b[row]]
//       an out[row] itself. 
//  Note: expressions are defined recursively -- i.e., expressions consist
//        of expressions. The leaf expressions are variable/grid names 
//        (and operators, as described below).
//  Note: This considers an operator as a special form of expression, which
//        is not true according to the traditional definition of an expression.
//        However, this allows easier representation.  
//  Note: CurStepObj must be set before calling this method. This uses
//        CurStepObj.nextExprSeq
//  Note: 'myseq' specified only for restoring (from saved image)
//----------------------------------------------------------------------------
function ExprObj(isLeaf, exprTy, label, myseq) {

    // An expression itself can contain a list of expressions (if not a leaf)
    // Note: if this is not a leaf node, this *caller* must push 
    //       (sub) expressions into exprArr[]
    //
    this.exprArr = (isLeaf) ? null : new Array();

    this.type = exprTy;

    this.str = label;              // const literal name 

    this.deleted = DeletedState.None;

    // my sequence number and the sequence # of my parent. 
    // if myseq is already provided, use that
    //
    this.mySeq = (myseq) ? myseq : CurModObj.nextExprSeq++;

    // If the label of this expression should ref to another expression,
    // keep a reference to that source expression, beause, if the name of
    // the source expression changes, the label has to change. Therefore,
    // for any mutable identifier (e.g., a var name), we use the following
    //
    this.labelExpr = null;
    //
    // if this expression refers to a grid, a reference to it.
    // NOTE: gO in an expression can refer to a grid in current function 
    //       only -- actually current step (althought this is not 
    //       strictly required). 
    //
    this.gO = null;           // gridObj this expr is pointing to (to highlight)
    this.dimIds = new Array();// selected dim titles/indices of gO

    // if this expression refers to a range, its attribs
    //
    this.selDim = RowDimId;

    this.addSubExpr = function addSubExpr(expr) {
        this.exprArr.push(expr);
    }

    // TODO: The following various test functions must be added to the 
    //       'prototype' of the JavaScript class, in order to avoid each
    //       ExprObj of containing copies of these functions. It is a
    //       performance optimization
    //
    this.isLeaf = function isLeaf() {
        return (this.exprArr == null);
    }

    this.isNumber = function isNumber() {
        return (this.type == ExprType.Number);
    }

    this.isIndex = function isIndex() {
        return (this.type == ExprType.Index);
    }

    this.isString = function isString() {
        return (this.type == ExprType.String);
    }

    this.isConcat = function isConcat() {
        return (this.type == ExprType.Concat);
    }

    this.isConcatEdit = function isConcatEdit() {
        return (this.type == ExprType.ConcatEdit);
    }

    this.isGridCell = function isGridCell() {
        return (this.type == ExprType.GridCell);
    }

    this.isGridRef = function isGridRef() {
        return (this.type == ExprType.GridRef);
        return (this.type == ExprType.ConstGridRef);
    }

    this.isFuncCall = function isFuncCall() {
        return ((this.type == ExprType.FuncCall) ||
            (this.type == ExprType.LibFuncCall));
    }

    this.isUserFuncCall = function isUserFuncCall() {
        return (this.type == ExprType.FuncCall);
    }

    this.isLibFuncCall = function isLibFuncCall() {
        return (this.type == ExprType.LibFuncCall);
    }

    this.isExprStmt = function isExprStmt() {
        return (this.type == ExprType.ExprStmt);
    }
    
    this.isStatement = function isStatement() { 
	return ((this.type == ExprType.ExprStmt) || 
		(this.type == ExprType.If) ||
		(this.type == ExprType.Else) ||
		(this.type == ExprType.ElseIf) ||
		(this.type == ExprType.BreakIf) ||
	       	(this.type == ExprType.Foreach) ||
	       	(this.type == ExprType.Forever) ||
	       	(this.type == ExprType.MaskHead) ||
		(this.type == ExprType.ConcatEdit) );
    }

    this.isScalarExpr = function isScalarExpr() {

        return ((this.type == ExprType.Number) ||
                (this.type == ExprType.Title) ||
                (this.type == ExprType.Index) ||
                (this.type == ExprType.Literal) ||
                (this.type == ExprType.LetName));
    }

    this.isScalarOrGridCell = function isScalarOrGridCell() {
        return (this.isScalarExpr() || this.isGridCell());
    }


    this.isLiteral = function isLiteral() {
        return (this.type == ExprType.Literal);
    }


    this.isForeach = function isForeach() {
        return (this.type == ExprType.Foreach);
    }

    this.isForever = function isForever() {
        return (this.type == ExprType.Forever);
    }

    this.isIf = function isIf() {
        return (this.type == ExprType.If);
    }

    // A formula keyword treated as a leaf-node (literal)
    //
    this.isFormulaKeyword = function isFormulaKeyword() {
        return ((this.type == ExprType.Return) ||
            (this.type == ExprType.Break) ||
            (this.type == ExprType.Continue));
    }

    this.isElse = function isElse() {
        return (this.type == ExprType.Else);
    }

    this.isLet = function isLet() {
        return (this.type == ExprType.Let);
    }

    this.isLetName = function isLetName() {
        return (this.type == ExprType.LetName);
    }

    this.isElseIf = function isElseIf() {
        return (this.type == ExprType.ElseIf);
    }

    this.isBreakIf = function isBreakIf() {
        return (this.type == ExprType.BreakIf);
    }

    this.isCondition = function isCondition() {
        return ((this.type == ExprType.If) ||
            (this.type == ExprType.Else) ||
            (this.type == ExprType.ElseIf) ||
            (this.type == ExprType.BreakIf)
        );
    }

    this.isLoopStmt = function isLoopStmt() {
        return ((this.type == ExprType.Foreach) ||
            (this.type == ExprType.Forever))
    }


    this.isOperator = function isOperator() {
        return (this.type == ExprType.Operator);
    }


    // Assignment operator: '='
    //
    this.isAssignOp = function isAssignOp() {
        return ((this.type == ExprType.Operator) && (this.str == '='));
    }


    // To identify +=,-=,*=,/= because all of them use the same ExprType
    // which is ExprType.Operator
    //
    this.isXAssignOp = function isXAssignOp() {
        return ((this.type == ExprType.Operator) &&
            ((this.str == "+=") || (this.str == "-=") ||
                (this.str == "*=") || (this.str == "/="))
        );
    }

    this.isUnaryOperator = function isUnaryOperator() {
        return ((this.type == ExprType.Operator) &&
            ((this.str == "-") || (this.str == "NOT"))
        );
    }

    this.isOpenParen = function isOpenParen() {
        return ((this.type == ExprType.Operator) && (this.str == "("));
    }

    this.isCloseParen = function isCloseParen() {
        return ((this.type == ExprType.Operator) && (this.str == ")"));
    }


    // A single character operator like: +, -, *, /
    //
    this.isUniOperator = function isUniOperator() {
        return ((this.type == ExprType.Operator) && (this.str.length ==
            1));
    }


    this.isRange = function isRange() {
        return (this.type == ExprType.Range);
    }

    this.isIdentOrNum = function isIdentOrNum() {

        return ((this.type == ExprType.GridCell) ||
            (this.type == ExprType.Index) ||
            (this.type == ExprType.LetName) ||
            (this.type == ExprType.Literal) ||
            (this.type == ExprType.Number)
        );

    }


    // out[row-k] : 
    //  -- models as expr 'out' (with isGridCell=true)
    //       -- with exprArr=['row', '-', 'k']
    // out[row,col]:
    //  -- expr 'out' as above (i.e., with isGridCell=true)
    //      -- exprArr in turn has 2 expressions: (a) row (b)col
    // note: The comma separator is implied for each sub-expression of a grid
    //       Hence, the comma is not explicitly recorded

}

//----------------------------------------------------------------------------
// Code Location Object. A helper object to contain info about which
// module : function : step : box we are generating code for. This is
// useful in determing the code location of a syntax err, etc.
//----------------------------------------------------------------------------
function CodeLocationObj() {

    var module = ""
    var func = ""
    var step = "";
    var box = "";

}


/* **************** SECTION B: Clones of Main Objects  ********************* */


//----------------------------------------------------------------------------
// Clone a step object, when we duplicate a step
//----------------------------------------------------------------------------
function cloneStepObj(s) {

    // All out/src grids in this step. 
    // NOTE: When we are cloning, we are doing a deep cloning by cloning
    //       the gridObjs as well. This is necessary because some properties
    //       like row/col indices are private to a step.
    //
    this.allGridIds = new Array();
    this.allIndObjs = new Array();

    for (var i = 0; i < s.allGridIds.length; i++) {
        var gId = s.allGridIds[i];
        this.allGridIds.push(gId);

        // TODO: We don't need to clone indices later. Remove this.
        this.allIndObjs.push(s.allIndObjs[i]);
    }

    this.boxExprs = s.boxExprs.slice(0);
    this.boxAttribs = s.boxAttribs.slice(0);

    // VERIFY: should we clone dim names? 
    //
    this.dimNameExprs = s.dimNameExprs.slice(0);

    // VERIFY: Since we are cloning an existing grid for duplicating a step
    //         the ouput grid is not new.
    //
    this.isOutGridNew = false;
    //
    this.title = s.title;


    this.focusBoxId = s.focusBoxId;
    this.focusBoxExpr = s.focusBoxExpr;
    //
    this.focusHtmlObj = s.focusHtmlObj;

    // VERIFY: We may not need to copy all active properties but rather
    //         reset them to initial values, when a step obj is cloned
    //
    this.activeParentPath = s.activeParentPath;
    this.activeChildPos = s.activeChildPos;
    this.activeParentExpr = s.activeParentExpr;
    this.isSpaceActive = s.isSpaceActive;

    // when the user is entering a number with the keyboard, the digits
    // entered so far are kept here
    //
    this.numberExpr = null;
    this.editDimNames = s.editDimNames;

    // Comments are stored in the following
    // 
    this.indexComment = s.indexComment;
    this.ifComment = s.ifComment;
    this.formulaComment = s.formulaComment;
    this.stepComment = s.stepComment;

    // Parallelism of the step, indicated by the parallelism meter
    //
    this.parallelism = s.parallelism;

    this.stageInStep = s.stageInStep;

    /*    
    this.configStarted = s.configStarted;
    this.configDone = s.configDone;      // out configuration done
    this.gridSelectDone = s.gridSelectDone; // all grids have been selected
    */

    // whether we are displaying data in grids
    //
    //this.showData = ShowDataState.None; 

    this.maxDataVal = MININT;     // maximum data value
    this.isHeader = s.isHeader;   // is this a function header

    this.keyboardEnabled = s.keyboardEnabled;
    this.potOCLstep = s.potOCLstep;
    this.showOptions = s.showOptions;

    // Method to add a source grid
    //
    this.addGridId = s.addGridId;
}


//----------------------------------------------------------------------------
// clone a grid object (Do a deep clone of every field -- i.e., no references)
//
// TODO: it is not necessary to copy all the array fields if they are 
//       replaced later.
//----------------------------------------------------------------------------
function cloneGridObj(newGId, s) {

    this.gridId = newGId;             // gridId in function

    this.caption = s.caption;         
    this.actCols = s.actCols;         // actual number of columns
    this.actRows = s.actRows;         // actual number of rows
    this.showCols = s.showCols;       // displayed number of columns
    this.showRows = s.showRows;       // displayed number of rows
    this.multiCol = s.multiCol;       // whether multiple cols
    this.multiRow = s.multiRow;       // whether multiple rows


    this.isTemplate = s.isTemplate;   // is this a template

    this.numDims = s.numDims;        // actual number of dimensions

    // Each of these is an array of arrays
    // VERIFY: Verify slicing copies all arrays of arrays
    //
    this.dimTitles = s.dimTitles.slice(0);
    this.dimComments = s.dimComments.slice(0);

    // Whether a given dimension has titles. 
    //
    this.dimHasTitles = s.dimHasTitles.slice(0);
    this.dimHasIndices = s.dimHasIndices.slice(0);

    this.dimShowSize = s.dimShowSize.slice(0);
    this.dimActSize = s.dimActSize.slice(0);
    this.dimDynSize = s.dimDynSize.slice(0);

    this.dimSelectTabs = s.dimSelectTabs.slice(0);

    this.dataTypes = s.dataTypes.slice(0);
    this.typesInDim = s.typesInDim;


    // data
    //
    this.data = (s.data) ? s.data.slice(0) : null;
    this.dataMax = s.dataMax;

    this.isGridRef = s.isGridRef;
    this.globalRefId = s.globalRefId;

    this.isRetVal = s.isRetVal;
    this.isConst = s.isConst;

    this.deleted = s.deleted;

    this.dimShowStart = s.dimShowStart.slice(0);

    this.comment = null;   // comment not cloned

    //MPI:
    this.isGlobal = s.isGlobal;
    this.isDistributed = s.isDistributed;
    this.dimIsExtended = s.dimIsExtended.slice(0);

    this.inExternalMod = s.inExternalMod; //NS:
    this.nameExternalMod = s.nameExternalMod; //NS:

    this.isCommon = s.isCommon; //NS2:
    this.nameCommon = s.nameCommon; //NS2:
    this.structCaption = s.structCaption; //DC3:

    // ---------- STEP specific variabiles: Do NOT clone -----------------
    //
    this.htmlId = "";
    this.hasSelection = false;     // whether this grid has a selection
    this.selIndDim = -1;
    this.inArgNum = -1;



    // this.captionEdit = false;   // is caption editable 
}




/* ************* SECTION C: Images of Main Objects for Saving  ************ */


//----------------------------------------------------------------------------
// 'Save image' of a whole prog. 
// The program consists of all modules and expression bodies
//
//----------------------------------------------------------------------------
function ProgObjSav(pO) {

    this.fileName = pO.fileName;

    this.allModules = new Array();      // all modules in this prog

    for (var m = 0; m < pO.allModules.length; m++) {
        this.allModules.push(new ModuleObjSav(pO.allModules[m]));
    }

    // Currently active module
    //
    this.curModNum = pO.curModNum;
}


//----------------------------------------------------------------------------
// 'Save image' of a module obj
//----------------------------------------------------------------------------
function ModuleObjSav(mO) {

    // exprBody array is unique to each module. We keep exprBody array 
    // separate for each module so that we can combine different modules 
    //
    this.exprBodyArr = new Array(mO.nextExprSeq);

    // Set global variable for module saved
    //
    SavModObj = mO;

    this.name = mO.name;

    this.allFuncs = new Array();   // list of all function is this module

    // Save all functions including template/global scopes (functions)
    //
    for (var f = 0; f < mO.allFuncs.length; f++) {

        this.allFuncs.push(new FuncObjSav(mO, mO.allFuncs[f],
            this.exprBodyArr));
    }

    this.curFuncNum = mO.curFuncNum;

    this.nextExprSeq = mO.nextExprSeq;

    // NOTE: We saved template/global functions above, so don't do it again


}

//----------------------------------------------------------------------------
// 'Save image' of a function object
//----------------------------------------------------------------------------
function FuncObjSav(mO, fO, exprBodyArr) {

    assert(exprBodyArr, "exprBodyArr must be defined in Func Sav");


    // Note: SavFuncObj & SavModObj must be set before we can save 
    //       any ExprObj in this function (e.g., in StepObj box expressions)
    //       e.g. SavFuncObj used by getGridObjIdInFunc
    //
    SavModObj = mO;
    SavFuncObj = fO;

    // Capture all grids of the function. 
    //
    this.allGrids = new Array();
    //
    for (var g = 0; g < fO.allGrids.length; g++) {

        this.allGrids.push(new GridObjSav(fO.allGrids[g]));
    }

    // Capture all steps of this function
    //
    this.allSteps = new Array();
    //
    for (var s = 0; s < fO.allSteps.length; s++) {

        this.allSteps.push(new StepObjSav(fO.allSteps[s], exprBodyArr));
    }

    this.isGlobal = fO.isGlobal;

    this.isTemplateScope = fO.isTemplateScope;

    this.isExtern = fO.isExtern;

    this.funcCallExprSeq = new ExprSeqSav(fO.funcCallExpr, exprBodyArr);

    this.retExprSeq = new ExprSeqSav(fO.retExpr, exprBodyArr);

    this.curStepNum = fO.curStepNum;

    this.argsAdded = fO.argsAdded;

    this.createAPI = fO.createAPI; //API:

}


//----------------------------------------------------------------------------
// Fields that need to be saved to the file are cloned here
//----------------------------------------------------------------------------
function StepObjSav(sO, exprBodyArr) {

    assert(exprBodyArr, "exprBodyArr must be defined in Step Sav");

    // We save grid IDs and index object.
    //
    this.allGridIds = sO.allGridIds.slice(0);

    // Save index objects
    //
    this.allIndObjs = new Array();
    //
    for (var d = 0; d < sO.allIndObjs.length; d++) {
        this.allIndObjs.push(new IndexObjSav(sO.allIndObjs[d],
            exprBodyArr));
    }


    // Save all boxExprs -- note that we save ExprSeqSav objects and 
    // the body of the expressions are entered into exprBodyArr
    //
    this.boxExprSeqs = new Array();
    //
    for (var box = CodeBoxId.Range; box < sO.boxExprs.length; box++) {

        this.boxExprSeqs.push(new ExprSeqSav(sO.boxExprs[box],
            exprBodyArr));
    }

    // BoxAttribs objects
    //
    this.boxAttribs = sO.boxAttribs.slice(0);

    // Dim name expresion sequences
    //
    this.dimNameExprSeqs = new Array();
    //
    for (var d = 0; d < sO.dimNameExprs.length; d++) {
        this.dimNameExprSeqs.push(new ExprSeqSav(sO.dimNameExprs[d],
            exprBodyArr));
    }

    this.isOutGridNew = sO.isOutGridNew;
    this.title = sO.title;

    this.focusBoxId = sO.focusBoxId;

    this.stageInStep = sO.stageInStep;

    /*
    this.configStarted = sO.configStarted;
    this.configDone = sO.configDone;
    this.gridSelectDone = sO.gridSelectDone;
    */

    this.isHeader = sO.isHeader;        // is this a function header

    this.potOCLstep = sO.potOCLstep; // OCL:

}


//----------------------------------------------------------------------------
// 'save image' of a grid object
//----------------------------------------------------------------------------
function GridObjSav(gO) {

    this.gridId = gO.gridId;
    this.caption = gO.caption;

    this.isTemplate = gO.isTemplate;
    this.numDims = gO.numDims;

    this.dimTitles = gO.dimTitles.slice(0);
    this.dimComments = gO.dimComments.slice(0);

    this.dimHasTitles = gO.dimHasTitles.slice(0);
    this.dimHasIndices = gO.dimHasIndices.slice(0);

    this.dimShowSize = gO.dimShowSize.slice(0);
    this.dimActSize = gO.dimActSize.slice(0);
    this.dimDynSize = gO.dimDynSize.slice(0);

    this.dimSelectTabs = gO.dimSelectTabs.slice(0);

    this.dataTypes = gO.dataTypes.slice(0);
    this.typesInDim = gO.typesInDim;

    this.isGridRef = gO.isGridRef;
    this.globalRefId = gO.globalRefId;

    this.inArgNum = gO.inArgNum;
    this.isRetVal = gO.isRetVal;
    this.isConst = gO.isConst;

    this.hasInitData = gO.hasInitData;
    this.deleted = gO.deleted;

    if (gO.hasInitData && gO.data)
        this.data = gO.data.slice(0);

    // TODO: We don't need to save this but when loading, we just have
    //       to create this array
    //
    this.dimShowStart = gO.dimShowStart.slice(0);

    this.comment = gO.comment;

    //MPI:
    this.isGlobal = gO.isGlobal;
    this.isDistributed = gO.isDistributed;
    this.dimIsExtended = gO.dimIsExtended.slice(0);

    this.inExternalMod = gO.inExternalMod; //NS:
    this.nameExternalMod = gO.nameExternalMod; //NS:

    this.isCommon = gO.isCommon; //NS2:
    this.nameCommon = gO.nameCommon; //NS2:
    this.structCaption = gO.structCaption; //DC3:

}

//----------------------------------------------------------------------------
// 'save image' of an index object
// Note: dimIndExprsSeqs[][] is an Array of Arra.
//----------------------------------------------------------------------------
function IndexObjSav(iO, exprBodyArr) {

    this.dimIndExprSeqs = new Array();

    // Go thru each dimension
    //
    for (var d = 0; d < iO.dimIndExprs.length; d++) {

        // Create a new array for dimension 'd'
        // NOTE: We create this array for every diemension if the dimension
        //       does not have indices (OPTIMIZE)
        //
        this.dimIndExprSeqs[d] = new Array();

        for (var i = 0; i < iO.dimIndExprs[d].length; i++) {


            // NOTE: To save space, we save an ExprSeqSav obj only for
            //       non-empty indices. For empty index cells, 'null' is saved
            //
            if (iO.dimIndExprs[d][i]) {

                this.dimIndExprSeqs[d][i] =
                    new ExprSeqSav(iO.dimIndExprs[d][i], exprBodyArr);


                //if (this.dimIndExprSeqs[d][i].mySeq > 0)
                //    alert("saved ind: " + this.dimIndExprSeqs[d][i].mySeq);

            } else {

                // If we saved 'null' for an index cell
                //
                this.dimIndExprSeqs[d][i] = null;

            }
        }
    }
}



//----------------------------------------------------------------------------
// Clone an ExprObj for saving. Because of the recursive nature of an 
// ExprObj, this is done in two parts:
// (1) We create a recursive ExprSeqSav object. This contains only sequence 
//     numbers of the original expression.
// (2) For each ExprSeqSav object created, if it is already not entered into
//     a global arrray (exprBodyArr), enter it. It is possible that same
//     sub-expression is part of many parent expressions -- e.g., an index
//     expression is included by many parent expressions.
//
//  We keep a separate exprBodyArr to reduce the amount of things saved.
//  -- i.e., there are many common sub-expressions and saving body only once
//     saves space
//
// TODO: Review each field
//----------------------------------------------------------------------------
// This is the object that contains the recursive part (seq # tree) 
// of an expression: Arguments:
// eO      : source ExprObj (coulb be null)
// exprBodyArr : array to enter all ExprBodySav objects into
//
// TODO: after creating exprBodyArr for *all* expressions in a program, 
//       we may need 
//       to compress it by removing holes (holes in seq# happen due to 
//       deletions)
//----------------------------------------------------------------------------
function ExprSeqSav(eO, exprBodyArr) {

    this.mySeq = (eO) ? eO.mySeq : -1;

    // STEP 1:
    //
    // Go thru sub expresssions of eO and add them
    // 
    this.exprArrSeq = null;
    //
    if (eO && eO.exprArr) {             // this node is not a leaf

        this.exprArrSeq = new Array();
        //
        for (var i = 0; i < eO.exprArr.length; i++) {

            assert((eO.exprArr[i].mySeq > 0), "invalid expr seq for save");

            this.exprArrSeq.push(new ExprSeqSav(eO.exprArr[i],
                exprBodyArr));
        }
    }


    // STEP 2:
    //
    // Enter the 'body' (everything except seq #s), into the global array
    // exprBodyArr (if it is not already entered) 
    //
    if (eO && !exprBodyArr[eO.mySeq]) {
        exprBodyArr[eO.mySeq] = new ExprBodySav(eO, exprBodyArr);
    }
}

//----------------------------------------------------------------------------
// this is the object that contains the non-recursive parts ('body') of an 
// expression object. Each expression needs only one of these objects. Each
// is entered into a global array by the caller. 
//
// TODO: Verify all required fields are saved
//
// eO : ExprObj

// PRE: SavModObj and SavFuncObj must be set properly to set gOId and fOId
//----------------------------------------------------------------------------
function ExprBodySav(eO, exprBodyArr) {

    assert(eO.mySeq, "No seq # for ExprObj to save body");

    this.mySeq = eO.mySeq;

    this.type = eO.type;

    this.str = eO.str;

    this.deleted = eO.deleted;

    this.mySeq = eO.mySeq;

    // Modified fields
    //
    this.labelExprSeq = new ExprSeqSav(eO.labelExpr, exprBodyArr);


    // Grid id of this eO.gO
    // NOTE: ASSUMPTION: We assume all gO fields in an expression refer
    //       to the current function (actually current step) because these
    //       gO's are used for highlighting purposes (and range expressions)
    //
    this.gOId = (eO.gO) ? getGridObjIdInFunc(eO.gO) : -1;
    this.fOId = (eO.gO) ? getFuncIdOfCurFunc() : -1;

    if (eO.gO) {
        logMsg(" grid: " + eO.gO.caption + " mod:" + SavModObj.name);
        assert((SavModObj.allFuncs[this.fOId].allGrids[this.gOId] == eO.gO),
            "fOId:gOId does not match with grid object");
    }


    this.dimIds = eO.dimIds.slice(0);

    this.selDim = eO.selDim;
}




/* ************** SECTION D: Restoring Images of Main Objects  ************* */


//----------------------------------------------------------------------------
// restore (load) saved program object
//----------------------------------------------------------------------------
function loadProgObj(pOsav, fname) {

    // Create a new program object
    //
    CurProgObj = new ProgObj(fname);

    var pO = CurProgObj;                // shortcut

    // load moduleObj's of this program
    //
    for (var m = 0; m < pOsav.allModules.length; m++) {

        pO.allModules.push(loadModuleObj(pOsav.allModules[m]));
    }

    pO.curModNum = pOsav.curModNum;

    // load library modules
    // NOTE: This changes curModObj
    //
    loadLibModules(pO);

    // Set CurModObj to point to module we just loaded.
    // Set gO references for all ExprObj's. This can be done only after
    // CurModObj is fully restored. 
    //
    CurModObj = pO.allModules[pO.curModNum];

    // TODO: Cater to multiple modules
    //
    //pO.allModules.push(CurModObj);



    return pO;
}


//----------------------------------------------------------------------------
// restore saved module object
// s: ModuleObjSav
//----------------------------------------------------------------------------
function loadModuleObj(s) {

    // Temporary array to store expressions while they are loaded
    //
    var exprArr = new Array();

    var mO = new ModuleObj(s.name, true);

    for (var f = 0; f < s.allFuncs.length; f++) {

        mO.allFuncs.push(loadFuncObj(s.allFuncs[f],
            s.exprBodyArr,
            exprArr));
    }

    mO.curFuncNum = s.curFuncNum;

    mO.nextExprSeq = s.nextExprSeq;

    // NOTE: template/global scopes restored with the above loop

    // Since template/global funcs are already present in allFuncs, just
    // set two references to them
    //
    mO.templateFunc = mO.allFuncs[FuncID.Template];
    mO.globalFunc = mO.allFuncs[FuncID.Global];

    // Restore gO filed in each ExprObj for exprs in this module
    //
    restoreGridObjInExprs(exprArr, s.exprBodyArr, mO);

    return mO;
}

//----------------------------------------------------------------------------
// restore saved func object
// s: FuncObjSav
//----------------------------------------------------------------------------
function loadFuncObj(s, exprBodyArr, exprArr) {

    var fO = new FuncObj(true);         // FuncObj with no header added

    // Load all grids and steps
    //
    for (var g = 0; g < s.allGrids.length; g++) {

        fO.allGrids.push(loadGridObj(s.allGrids[g]));
    }
    //
    for (var st = 0; st < s.allSteps.length; st++) {

        fO.allSteps.push(loadStepObj(s.allSteps[st], exprBodyArr, exprArr));
    }

    fO.isGlobal = s.isGlobal;

    fO.isTemplateScope = s.isTemplateScope;

    fO.isExtern = s.isExtern;

    fO.funcCallExpr = loadExprObj(s.funcCallExprSeq, exprBodyArr, exprArr);

    fO.retExpr = loadExprObj(s.retExprSeq, exprBodyArr, exprArr);

    fO.curStepNum = s.curStepNum;

    fO.argsAdded = s.argsAdded;

    fO.createAPI = s.createAPI; //API:

    return fO;

}



//----------------------------------------------------------------------------
// restore a saved step object
// s: StepObjSav
//----------------------------------------------------------------------------
function loadStepObj(s, exprBodyArr, exprArr) {

    var sO = new StepObj();

    // Restore all grid IDs and IndexObjects
    //
    sO.allGridIds = s.allGridIds.slice(0);
    //
    for (var d = 0; d < s.allIndObjs.length; d++) {

        sO.allIndObjs.push(loadIndexObj(s.allIndObjs[d], exprBodyArr,
            exprArr));
    }

    // Restore box expressions and box attributes
    //
    for (var b = 0; b < s.boxExprSeqs.length; b++) {

        var box = b + CodeBoxId.Range;
        sO.boxExprs[box] = loadExprObj(s.boxExprSeqs[b], exprBodyArr,
            exprArr);

    }
    //
    sO.boxAttribs = new Array();
    //
    for (var b = 0; b < s.boxAttribs.length; b++) {

        sO.boxAttribs.push(loadBoxAttrib(s.boxAttribs[b]));
    }

    // restore dimension based index variable names (expresions)
    //
    for (var d = 0; d < s.dimNameExprSeqs.length; d++) {
        sO.dimNameExprs.push(loadExprObj(s.dimNameExprSeqs[d],
            exprBodyArr,
            exprArr));
    }

    sO.isOutGridNew = s.isOutGridNew;
    sO.title = s.title;

    // Note: We just set the focux box ID. In drawStep() we will set the 
    //       focusBoxExpr when we change to it
    //
    sO.focusBoxId = s.focusBoxId;



    sO.stageInStep = s.stageInStep;

    sO.isHeader = s.isHeader;

    sO.potOCLstep = s.potOCLstep; // OCL:

    return sO;

}




//----------------------------------------------------------------------------
// restore a saved grid object
// s : saved GridObjSav object
//----------------------------------------------------------------------------
function loadGridObj(s) {

    var gO = new GridObj(s.gridId, s.caption);

    assert(RowDimId == 0, "following not valid if row is not dim 0");
    //
    gO.multiRow = (s.numDims > 0);
    gO.multiCol = (s.numDims > 1);


    gO.isTemplate = s.isTemplate;
    gO.numDims = s.numDims;

    gO.dimTitles = s.dimTitles.slice(0);
    gO.dimComments = s.dimComments.slice(0);

    gO.dimHasTitles = s.dimHasTitles.slice(0);
    gO.dimHasIndices = s.dimHasIndices.slice(0);

    gO.dimShowSize = s.dimShowSize.slice(0);
    gO.dimActSize = s.dimActSize.slice(0);

    assert((gO.dimActSize.length <= (gO.numDims+2)), 
	   "Grid:" + gO.caption +
	   " ActSize.length:" + gO.dimActSize.length + 
	   " numDim:" + gO.numDims);

    // TODO: after file conversion is done, remove if-else check
    //
    if (s.dimDynSize)         // needed for backward compatibility w/ old progs
	gO.dimDynSize = s.dimDynSize.slice(0);
    else
        gO.dimDynSize = new Array(s.numDims);


    gO.dimSelectTabs = s.dimSelectTabs.slice(0);

    gO.dataTypes = s.dataTypes.slice(0);

    gO.typesInDim = s.typesInDim;

    gO.isGridRef = s.isGridRef;
    gO.globalRefId = (s.globalRefId >= 0) ? s.globalRefId : -1;

    gO.inArgNum = s.inArgNum;
    gO.isRetVal = s.isRetVal;
    gO.isConst = s.isConst;

    gO.hasInitData = s.hasInitData;

    gO.deleted = s.deleted;

    // VERIFY: Does slicing copy multi-dimensionsal arrays?
    //
    if (s.hasInitData && s.data) {
        gO.data = s.data.slice(0);
        alert("Loading init data for " + gO.caption);
    }

    // TODO: We don't need to save/restore this. We can initialize this
    //       object with 0
    //
    gO.dimShowStart = s.dimShowStart;

    gO.comment = s.comment;

    //MPI:
    gO.isGlobal = s.isGlobal;
    gO.isDistributed = s.isDistributed;
    gO.dimIsExtended = s.dimIsExtended.slice(0);


    gO.inExternalMod = s.inExternalMod; //NS:
    gO.nameExternalMod = s.nameExternalMod; //NS:

    gO.isCommon = s.isCommon; //NS2:
    gO.nameCommon = s.nameCommon; //NS2:
    gO.structCaption = s.structCaption; //DC3:

    return gO;

}

//----------------------------------------------------------------------------
// Restore saved attributes of a formula/mask box
//----------------------------------------------------------------------------
function loadBoxAttrib(s) {

    var bA = new BoxAttribs(s.indent, s.type);
    bA.forceIndent = s.forceIndent;
    bA.comment = s.comment;

    return bA;
}


//----------------------------------------------------------------------------
// restore index object
// s: IndexObjSav object
//----------------------------------------------------------------------------
function loadIndexObj(s, exprBodyArr, exprArr) {

    var iO = new IndexObj(null);

    for (var d = 0; d < s.dimIndExprSeqs.length; d++) {

        // For every dimension, we have saved an array of indices.
        // So, restore it (OPTIMIZE: when a dim does not have indeces)
        //
        iO.dimIndExprs[d] = new Array(s.dimIndExprSeqs.length);

        // If dimension 'd' has indices
        // 
        if (s.dimIndExprSeqs[d]) {

            for (var i = 0; i < s.dimIndExprSeqs[d].length; i++) {

                // If there is an index expr seq saved, create expression
                //
                if (s.dimIndExprSeqs[d][i]) {

                    iO.dimIndExprs[d][i] = loadExprObj(s.dimIndExprSeqs[d]
                        [i],
                        exprBodyArr,
                        exprArr);

                    // if (iO.dimIndExprs[d][i])
                    // alert("dim expr loaded:" + iO.dimIndExprs[d][i].str);

                } else {

                    iO.dimIndExprs[d][i] = null;

                }

            }
        } else {
            alert("dimension not available -- must have saved all dims");
        }

    }

    return iO;
}


//----------------------------------------------------------------------------
// Creates a new ExprObj from a saved image. If the ExprObj is already 
// created (i.e., found in gExprArr), just return it.
// Arguments:
//   seqO     : ExprSeqSav object (image saved)
//   gExprArr : global expression array where ExprObjs are entered 
//
// NOTE: All ExprObj's (even when it is null) is saved as an ExprSeqSav object.
//       A null expression is saved with mySeq value of -1.
//
//----------------------------------------------------------------------------
function loadExprObj(seqO, gBodyArr, gExprArr) {

    assert(seqO, "seqO must be defined");
    assert(seqO.mySeq, "seqO must have a sequence #");

    // We use a negative seq # to indicate a null expression. Note that 
    // even null expressions are saved as ExprSeqSav objects. 
    //
    if (seqO.mySeq < 0)
        return null;

    // if the global expression array already contains mySeq, just return the 
    // ExprObj found at that place
    //
    if (gExprArr[seqO.mySeq]) {

        assert((gExprArr[seqO.mySeq].mySeq == seqO.mySeq),
            "seq# mismatch");

        return gExprArr[seqO.mySeq];
    }

    // Allocate a new ExprObj, enter it into gExprArr, and call 
    // restoreExprObj to flesh it out (i.e., to create sub expressions
    // and fill other fields from saved image). It is important to do these
    // three steps in this order to bottom out recursion because 
    // restoreExprObj in turn calls loadExprObj.
    //
    var isleaf = (seqO.exprArrSeq == null);
    //
    gExprArr[seqO.mySeq] = new ExprObj(isleaf, null, null, seqO.mySeq);
    //
    var eO = gExprArr[seqO.mySeq];                // shortcut
    //
    // Fill out both (1) sub-expressions and (2) other 'body' parts
    //
    restoreExprObj(eO, seqO, gBodyArr, gExprArr);

    // var msg = JSON.stringify(eO);
    // alert("restored Expr:(" +  eO.str + ")" + msg);

    // Returns newly created and fleshed out ExprObj
    //
    return eO;
}

//----------------------------------------------------------------------------
// Fill out both (1) sub-expressions and (2) other 'body' fields of a 
// newly crated ExprObj (eO). The sub-expressions are filled out using
// ExprSeqSav object (seqO) and 
//----------------------------------------------------------------------------
function restoreExprObj(eO, seqO, gBodyArr, gExprArr) {

    assert(seqO, "seqO must be defined");

    // Add all subexpressions required for ExprObj eO.
    //
    if (seqO.exprArrSeq) {

        for (var i = 0; i < seqO.exprArrSeq.length; i++) {

            assert((seqO.exprArrSeq[i].mySeq), "must have seq #");

            eO.exprArr.push(
                loadExprObj(seqO.exprArrSeq[i], gBodyArr, gExprArr)
            );
        }
    }

    // Now, flesh out other filds of eO 

    var savedBody = gBodyArr[seqO.mySeq];

    assert(savedBody, "No saved body for seq#: " + seqO.mySeq);
    assert((savedBody.mySeq == eO.mySeq),
        "seq #s of body and expr must match");

    // Restore other fields from saved image
    //
    assert((savedBody.labelExprSeq.mySeq), "labelexpr must have seq #");

    eO.str = savedBody.str;

    eO.type = savedBody.type;

    eO.deleted = savedBody.deleted;

    eO.labelExpr = loadExprObj(savedBody.labelExprSeq, gBodyArr, gExprArr);
    //
    eO.dimIds = savedBody.dimIds.slice(0);

    eO.selDim = savedBody.selDim;

    // The following has to be set in restoreGridObjInExprs after all
    // grids of the function are loaded
    //
    eO.gO = null;

    // TODO: restore all required fields
}





//----------------------------------------------------------------------------
// Restore gO field in each ExprObj in gExprArr. 
//----------------------------------------------------------------------------
function restoreGridObjInExprs(gExprArr, gExprBodyArr, mO) {

    // Go thru all ExprObjs (in gExprArr) we loaded
    //
    for (var i = 0; i < gExprArr.length; i++) {

	// If the ExprObj is valid
	//
	if (gExprArr[i] && (gExprArr[i].mySeq > 0) ) {

	    var seq = gExprArr[i].mySeq;          // expr seq #
	    var body = gExprBodyArr[seq];         // body of the expr
	    var fid = body.fOId;                  // func id recoreded in body

	    assert((seq == body.mySeq), "Seq # mismatch");

	    var gId = body.gOId;                  // grid id corresponding 
	    
	    if (gId >= 0) {                       // if grid Id is valid

		var fO = mO.allFuncs[fid];     // fO corresponding to fid
		assert((fid < mO.allFuncs.length), "invalid func id");

		assert((gId < fO.allGrids.length), "invalid grid id");
		var gO = fO.allGrids[gId];        
		gExprArr[seq].gO = gO;            // set gO in ExprObj

	
		assert((mO.allFuncs[fid].allGrids[gId] == gO),
		       "fid:gId does not match with grid object"); 


		//alert("Set gO in expr");
	    }
	}
    }
}



/* ************* SECTION E: Helper Functions for Main Objects  ************** */


//----------------------------------------------------------------------------
// add/update initial dim variable names to a step object
//----------------------------------------------------------------------------
function updateDimIndVarNames(sO, gO) {

    // If gO has more dims than sO has recorded, push initial names
    // and update sO.maxDims
    //
    for (var d = sO.dimNameExprs.length; d < gO.numDims; d++) {
        sO.dimNameExprs.push(getInitialDimNameExpr(d));
    }
}

//----------------------------------------------------------------------------
// get the default index variable names
//----------------------------------------------------------------------------
function getInitialDimNameExpr(d) {

    var name = DefDimIndNameRoot + d;             // e.g., dim3, dim4, etc
    //
    if (d == RowDimId) name = DefRowIndName;      // e.g., row
    if (d == ColDimId) name = DefColIndName;      // e.g., col

    var expr = new ExprObj(true, ExprType.Index, name);
    return expr;
}

//----------------------------------------------------------------------------
// get default index expression for a dim
//----------------------------------------------------------------------------
function getDefDimIndExpr(d) {

    var varExpr = getDefDimIndVarNameExpr(d);
    var concExpr = new ExprObj(false, ExprType.Concat, "");
    concExpr.addSubExpr(varExpr);

    return concExpr;
}

//----------------------------------------------------------------------------
// MPI: Starting index of a dim -- usually 'startExt'
// TODO: Introduce a StartLiteral type and use it here instead of Literal
//----------------------------------------------------------------------------
function getDefIndStartExprExt(g, d) {

    var varExpr = new ExprObj(true, ExprType.Literal, 
			    DefStartNameExt + d + "_" + g.caption);
    var concExpr = new ExprObj(false, ExprType.Concat, "");
    concExpr.addSubExpr(varExpr);

    return concExpr;

}

//----------------------------------------------------------------------------
// MPI: Ending index of a dim -- usually 'endExt'
// TODO: Introduce a StartLiteral type and use it here instead of Literal
//----------------------------------------------------------------------------
function getDefIndEndExprExt(g, d) {

    var varExpr = new ExprObj(true, ExprType.Literal, 
			    DefEndNameExt + d + "_" + g.caption);
    var concExpr = new ExprObj(false, ExprType.Concat, "");
    concExpr.addSubExpr(varExpr);

    return concExpr;

}

//----------------------------------------------------------------------------
// Starting index of a dim -- usually '0'
//----------------------------------------------------------------------------
function getDefIndStartExpr() {

    var varExpr = new ExprObj(true, ExprType.Number, "0");
    var concExpr = new ExprObj(false, ExprType.Concat, "");
    concExpr.addSubExpr(varExpr);

    return concExpr;

}

//----------------------------------------------------------------------------
// Ending index of a dim -- usually 'end'
// TODO: Set ExprObj.grid to the Literal so that an endname is always 
//       associated with a grid. Caller should do this.
//----------------------------------------------------------------------------
function getDefIndEndExpr(d) {

    var varExpr = new ExprObj(true, ExprType.Literal, DefEndName + d);
    var concExpr = new ExprObj(false, ExprType.Concat, "");
    concExpr.addSubExpr(varExpr);

    return concExpr;

}



//----------------------------------------------------------------------------
// Default dimension names (index variable names) -- a pure index
// var should not be used for an index cell in a grid. 
// Rather create  a concat root expr by calling getDefDimIndExpr()
//----------------------------------------------------------------------------
function getDefDimIndVarNameExpr(d) {

    assert(CurStepObj.dimNameExprs[d], "Empty Dim Name Expression");

    return CurStepObj.dimNameExprs[d];
}


//----------------------------------------------------------------------------
// PRE: SavFuncObj must be set
// CAUTION: Call this method only for saving files
//----------------------------------------------------------------------------
function getGridObjIdInFunc(gO) {

    if (!gO) return (-1);

    assert(SavFuncObj, "SavFuncObj must be defined");

    var fO = SavFuncObj;

    for (var i = 0; i < fO.allGrids.length; i++) {

        if (fO.allGrids[i] == gO)
            return i;
    }

    assert(false, "grid obj not found in function for saving");
}

//----------------------------------------------------------------------------
// PRE: SavModObj and SavFuncObj  must be set
// CAUTION: Call this method only for saving files
//----------------------------------------------------------------------------
function getFuncIdOfCurFunc() {

    assert(SavModObj, "SavModObj must be defined");
    assert(SavFuncObj, "SavFuncObj must be defined");

    for (var i = 0; i < SavModObj.allFuncs.length; i++) {

        if (SavModObj.allFuncs[i] == SavFuncObj)
            return i;
    }

    assert(false, "func obj not found in module for saving");
}


//----------------------------------------------------------------------------
// Find the index within allGrids array for a given gO
//----------------------------------------------------------------------------
function findIndexOfGrid(fO, gO, skip_check) {

    for (var g = 0; g < fO.allGrids.length; g++) {

        if (fO.allGrids[g] == gO)
            return g;
    }

    if (skip_check)           // skiping asserting on failure
	return -1;

    assert(0, "Grid Object not found in function");
}


//----------------------------------------------------------------------------
// Whether any dimension in gO has a dynamic size
//----------------------------------------------------------------------------
function hasDynSize(gO) {

    for (var d = 0; d < gO.numDims; d++) {
        if (gO.dimDynSize[d]) {
            logMsg("has dyn size");
            return true;
        }
    }


    return false;
}

/*
//----------------------------------------------------------------------------
// Find a grid object in tempate/global scopes of module 'mO'
//----------------------------------------------------------------------------
function findGridInScopes(mO, gO) {

    // Serarch for gO in template/global scopes of module mO.
    //
    for (var f=FuncID.Template; f <= FuncID.Global; f++) {

	var fO = mO.allFuncs[f];
	
	for (var g=0; g < fO.allGrids.length; g++) {
	
	    if (fO.allGrids[g] == gO) 
		return fO.allGrids[g];
	}
    }

    return null;
}
*/

/* ************* SECTION F: HTML 'window' global functions  *************** */




//----------------------------------------------------------------------------
// Execute this function on page load. Starts the ball rolling.
//----------------------------------------------------------------------------
window.onload = function() {

    initCurProg(DefProgName); // init default program 

    initCurModule(DefStartModName); // init default module (CurModObj)
    //
    // init default function (CurFuncObj) & Header
    initMainFunc(DefMainFuncName);
    //
    addCommandLineArgs();     // add command line args
    //
    initCurStep();            // init current step (CurStepObj)

    drawProgStructHead();     // program structre heading at top of page
    //    
    doNextStage();            // call step sequencing function
}

//----------------------------------------------------------------------------
// Keyboard handling
//----------------------------------------------------------------------------
// Note: onkeydown event is called for ALL keys (e.g., backspace, shift etc).
//       We use this to handle only special keys as backspace.
//
window.onkeydown = myKeyDown;

// Note: onkeypress is called ONLY for keyboard characters -- e.g., +, -, etc
//       We use this event handler to handle most keyboard input because
//       most of the input we process are characters
//
window.onkeypress = myKeyPress;

//----------------------------------------------------------------------------
// Hander for handling spcial keys as backspace. This method is called for
// ALL keys but we handle only the special keys here.
//----------------------------------------------------------------------------
function myKeyDown(evt) {

    var isTextTag = false;

    // Disable default back-space action of the browser
    // Allow backsapce only when text is editable
    //
    if (evt.keyCode == 8) {        // if backspace
	
	var elem = evt.srcElement || evt.target;  // src/target element
	var tag = elem.tagName;
	//
	// alert("tag name: " + tag);             // use this to find tagname
	//
	var isTextTag = (tag == 'INPUT' || tag == 'TEXTAREA');

        // if contenteditable OR a text input element (or comment)
        //
        if (!(elem.isContentEditable || isTextTag)) {
            evt.preventDefault();
        }
    }


    // alert("Pressed ANY key : " + evt.keyCode + " : " + evt.charCode);

    var sO = CurStepObj;

    // Do not process keyboard input  if these conditions are true
    //
    if (!sO.keyboardEnabled || sO.isHeader ||
        (sO.stageInStep < StageInStep.GridSelectDone))
        return;

    var key = evt.keyCode;


    // Handle backspace
    //
    if (key == 8) {


        // Handle backspace only if the target of the event is not 
        // contenteditable -- if it is contenteditable, the default editing
        // will happen
        //
        if (!(evt.target.isContentEditable) && !isTextTag ) {
            handleBackSpace();

            // Don't allow default action on backspace (going back to prev page)
            //
            evt.preventDefault();
        }

    }

}



//----------------------------------------------------------------------------
// Handles clicks on the document body (empty space). 
// We can use a click on the body to stop whatever we are currently editing
//----------------------------------------------------------------------------
document.addEventListener(

    "click",

    function(e) {


        if (e.target === document.body) {

            if (CurStepObj.focusBoxId == CodeBoxId.Index) {
                stopIndexEditing();
            }

            // remove main menu body
            //
            removeMenuBody();
            removeGridContextMenuBody();
        }

        // var msg = (e.target === document.body) ?  " BODY " : " not body "; 
        // showDbg(e.target + " BODY ");


    });


//----------------------------------------------------------------------------
// Handler to handle key presses of ONLY character keys like +, -, /, etc.
//----------------------------------------------------------------------------
function myKeyPress(evt) {

    // alert("Pressed a char key : " + evt.keyCode + " : " + evt.charCode);

    // If the user has not configured/selected an output grid yet, no 
    // keys are enabled. 
    //
    var sO = CurStepObj;

    if (!sO.keyboardEnabled || sO.isHeader ||
        (sO.stageInStep < StageInStep.ConfigDone))
        return;

    // Check whether the user is editing a comment/text area, etc
    //
    var elem = evt.srcElement || evt.target;  // src/target element
    var tag = elem.tagName;

    // Note: cannot use 'INPUT' since index editing uses 'INPUT' tags
    //
    var isTextTag = (tag == 'TEXTAREA');
    // var isTextTag = (tag == 'INPUT' || tag == 'TEXTAREA');

    //
    // if contenteditable OR a text input element (or comment)
    //    
    if (elem.isContentEditable || isTextTag) {
	// alert("Return! tag = " + tag);
	return;
    }


    // STEP 1: Handle number input
    //
    // Note: Firefox needs charCode (not keyCode)
    //
    var key = evt.charCode;

    // Note: 46 is period (decimal point)
    //
    if (key >= 48 && key <= 57) 
	handleNumKey(key-48);
    if (key == 46) handleNumKey('.');   // decimal point


    // STEP 2: Handle operator input
    //
    switch (key) {

        case 33:
            addOpExpr('!');
            break;

        case 40:
            addOpExpr('(');
            break;

        case 41:
            addOpExpr(')');
            break;

        case 42:
            addOpExpr('*');
            break;

        case 43:
            addOpExpr('+');
            break;

        case 45:
            addOpExpr('-');
            break;

        case 47:
            addOpExpr('/');
            break;

        case 60:
            addOpExpr('<');
            break;

        case 61:
            addOpExpr('=');
            break;

        case 62:
            addOpExpr('>');
            break;

    }

}

//----------------------------------------------------------------------------
// Construct a number expression when the user is pressing number keys
// Handle number keys (including '.')
//----------------------------------------------------------------------------
function handleNumKey(digit) {

    var sO = CurStepObj;

    // Do not accept the decimal point into an index expression
    //
    if ((sO.focusBoxId == CodeBoxId.Index) && (digit == '.')) {
        showTip(TipId.NoPeriod);
        return;
    }

    // If the number expression is empty, then create a new ExprObj
    // to record the number AND record that expression in the 
    // activeExpr (whatever code box/index edit/... is active)
    // If the number expression is NOT empty (i.e., subsequent numbers)
    // then updte the number expression's string.
    //
    if (!sO.numberExpr) {

        sO.numberExpr = new ExprObj(true, ExprType.Number, digit.toString());
        replaceOrAppendExpr(sO.numberExpr);

    } else {

        sO.numberExpr.str = sO.numberExpr.str + "" + digit.toString();
    }

    // if we inserted '.' into a function call arg (which is a number),
    // change its type
    // 
    if (sO.activeParentExpr.isUserFuncCall()) {
        var pos = sO.activeChildPos - 1;
        updateFuncArgType(sO.activeParentExpr, pos, sO.numberExpr.str);
    }

    redrawAfterKey();
}

//----------------------------------------------------------------------------
// Handler for a spacial key (backspace)
//----------------------------------------------------------------------------
function handleBackSpace() {

    var sO = CurStepObj;

    // if we are currently editing a number string, just delete the last
    // digit
    //
    if (sO.numberExpr && sO.numberExpr.str && sO.numberExpr.str.length) {
        sO.numberExpr.str =
            sO.numberExpr.str.substr(0, sO.numberExpr.str.length - 1)

        // If we deleting a number in a function call, update the data
        // type accordingly -- because we could have deleted '.' decimal point
        // 
        if (sO.activeParentExpr.isUserFuncCall()) {
            updateFuncArgType(sO.activeParentExpr, sO.activeChildPos - 1,
                sO.numberExpr.str);
        }

    } else {

        // Note: We should allow function editing
        //
        delOpExpr();
    }


    // TODO: Handle deleting other expressions

    redrawAfterKey();

}

//----------------------------------------------------------------------------
// Redraws window(s) after a key is pressed
//----------------------------------------------------------------------------
function redrawAfterKey() {

    var sO = CurStepObj;

    if (sO.focusBoxId == CodeBoxId.Index) 
	redrawGridIndexExpr(sO);            // redraw the index *cell*
    else
	drawCodeWindow(sO);                 // redraw code window

}




//----------------------------------------------------------------------------
// Does the name exist in:
// (1) as a functin name in current module
// (2) as a grid name in current function
// (3) as a let/index var name in current step
// (4) as a keyword/reserved-word
// (5) TODO: column/row/... title
//----------------------------------------------------------------------------
function isExistingName(mO, fO, sO, name) {

    return (isExistingModuleOrLibName(name) ||
        isExistingFuncName(mO, name) ||
        isExistingGridNameInFunc(fO, name) ||
        isExistingLetNameInStep(sO, name) ||
        isExistingIndexVarNameInStep(sO, name) ||
        isKeyword(name));

}

//----------------------------------------------------------------------------
// Search all user and library modules to check for an exiting name
//----------------------------------------------------------------------------
function isExistingModuleOrLibName(name) {

    var pO = CurProgObj;

    for (var m = 0; m < pO.allModules.length; m++) // search all user modules
        if (pO.allModules[m].name == name)
            return true;

    for (var m = 0; m < pO.libModules.length; m++) // search all library modules
        if (pO.libModules[m].name == name)
            return true;

    return false;
}


//----------------------------------------------------------------------------
// Check whether a given function name alrady exists in a given module
//----------------------------------------------------------------------------
function isExistingFuncName(mO, fname) {

    for (var f = 0; f < mO.allFuncs.length; f++) {

        var funcO = mO.allFuncs[f];

        if (funcO.funcCallExpr.str == fname)
            return true;
    }

    return false;
}

//----------------------------------------------------------------------------
// Check for a name matching with an existing function name 
//----------------------------------------------------------------------------
function isExistingGridNameInFunc(fO, name) {

    for (var g = 0; g < fO.allGrids.length; g++) {

        if (fO.allGrids[g] == name)
            return true;
    }

    return false;
}

//----------------------------------------------------------------------------
// Check for a name matching with a let name
//----------------------------------------------------------------------------
function isExistingLetNameInStep(sO, name) {

    for (var b = CodeBoxId.BoxStart; b < sO.boxExprs.length; b++) {

        if (sO.boxExprs[b]) {

            // First expression in the expression statement
            //
            var expr0 = sO.boxExprs[b].exprArr[0];

            if (expr0 && expr0.isLet()) {

                // Note: Let name is present in exprArr[0] position
                //
                if (expr0.exprArr[0].str == name)
                    return true;
            }
        }
    }

    return false;
}

//----------------------------------------------------------------------------
// Check for name mathcing with index variable names
//----------------------------------------------------------------------------
function isExistingIndexVarNameInStep(sO, name) {

    for (var d = 0; d < sO.dimNameExprs.length; d++) {

        if (sO.dimNameExprs[d].str == name)
            return true;
    }

    return false;
}

//----------------------------------------------------------------------------
// Returns whether something is a keyword
//----------------------------------------------------------------------------
function isKeyword(str) {

    for (var k = 0; k < Keywords.length; k++) {
        if (str == Keywords[k])
            return true;
    }
    return false;
}



/* ************* SECTION G: init functions for Main Objects  ************** */



//----------------------------------------------------------------------------
// Initializes a program a load library modules
//----------------------------------------------------------------------------
function initCurProg(fname) {

    CurProgObj = new ProgObj(fname);
    loadLibModules(CurProgObj);
}


//----------------------------------------------------------------------------
// initilizes a module
//----------------------------------------------------------------------------
function initCurModule(name) {

    CurModObj = new ModuleObj(name);
    CurProgObj.allModules.push(CurModObj);

    // Create the Template scope, which is a global (external) scope to 
    // create grid/function templates for the module. This is useful for
    // passing arguments between functions (especially w/ libraries)
    // The template scope is similar to header file declarations in C.
    //
    CurModObj.templateFunc.funcCallExpr =
        new ExprObj(true, ExprType.FuncCall, 'Templates');
    //
    CurModObj.allFuncs.push(CurModObj.templateFunc);
    CurModObj.templateFunc.isTemplateScope = true;

    // Add a new step to template scope (func) so that the user starts with
    // step1 (instead of function header, which is empty)
    //
    CurModObj.templateFunc.allSteps.push(new StepObj());

    // Create the global scope. The global (static) scope is essentially 
    // another function. The only difference is that all the grids in the global
    // scope are accessible within ALL the functions in the module -- compared
    // to function parameters and local grids. 
    // So, cretate the global scope (function) and add it to the function 
    // list.
    //
    CurModObj.globalFunc.funcCallExpr =
        new ExprObj(true, ExprType.FuncCall, DefGlobalScopeName);
    //
    CurModObj.allFuncs.push(CurModObj.globalFunc);
    CurModObj.globalFunc.isGlobal = true;

    // Add a new step to global scope (func) so that the user starts with
    // step1 (instead of function header, which is empty)
    //
    CurModObj.globalFunc.allSteps.push(new StepObj());
}

//----------------------------------------------------------------------------
// Initializes a new Function
//----------------------------------------------------------------------------
function initMainFunc(name) {

    // First record the current function number here -- before we push a new
    // function. 
    //
    CurModObj.curFuncNum = CurModObj.allFuncs.length;
    assert((CurModObj.curFuncNum == DefMainFuncInd),
        "change value of DefMainFuncInd");


    // Create the default function call expression for Main
    //
    var fExpr = new ExprObj(false, ExprType.FuncCall, name);
    var defArgExpr = new ExprObj(true,
        ExprType.ConstGridRef,
        DefStartupArgsName);
    fExpr.addSubExpr(defArgExpr);

    // Create a new function object and add it to current module
    // Note: This creates a new Header step by default
    //
    CurFuncObj = new FuncObj();
    CurFuncObj.funcCallExpr = fExpr;
    CurModObj.allFuncs.push(CurFuncObj);

    // STEP: Create a function header for the main function
    //
    // Create the header step
    //
    var fO = CurFuncObj;
    fO.curStepNum = CurFuncObj.allSteps.length;
    //
    var newGId = fO.allGrids.length;
    //
    // Create a 'return value' grid to add to the header of Main
    //
    var newgO = new GridObj(newGId, DefRetValName, 1, 1,
        1, 1, false, false, false, false);
    newgO.isRetVal = true;
    //
    // Add default data type
    //
    newgO.dataTypes.push(DataTypes.Integer);
    assert((newgO.dataTypes.length == 1), "must have only global type");
    newgO.typesInDim = -1;

    // Add new grid to function and header step. Note that we have
    // already created header step in 'new FuncObj()' constructor.
    // NOTE: No IndexObj is necessary for a grid in a header step
    //
    fO.allGrids.push(newgO);
    fO.allSteps[FuncHeaderStepId].allGridIds.push(newGId);
    //
}


//----------------------------------------------------------------------------
// Add command lines args to the CurFuncObj
// PRE: CurFuncObj must be the Main (entry point) function
//----------------------------------------------------------------------------
function addCommandLineArgs() {

    var fO = CurFuncObj;

    // Create and add a command line argument vector (similar to argv in C)
    // This is a reference to a grid (1D array of strings)
    // NOTE: No IndexObj is necessary for a grid in a header step
    //
    var newGId = fO.allGrids.length;
    var newgOArgs = new GridObj(newGId, DefStartupArgsName, 1, 10,
        1, 4, false, false, false, true);
    //
    newgOArgs.inArgNum = 0;             // add as 0th arg
    newgOArgs.isConst = true;           // cannot write to startup args
    //
    newgOArgs.typesInDim = -1;          // Set global type to string
    newgOArgs.dataTypes[0] = DataTypes.String;
    //
    fO.allGrids.push(newgOArgs);
    fO.allSteps[FuncHeaderStepId].allGridIds.push(newGId);

    assert(fO.allSteps[FuncHeaderStepId].isHeader, "must be header");
}

//----------------------------------------------------------------------------
// initialize the current step, record it in the current function, and
// set the current step to the newly initialized step
//----------------------------------------------------------------------------
function initCurStep() {

    // First set the current step number (before we push below, since
    // curStepNum starts from 0
    //
    CurFuncObj.curStepNum = CurFuncObj.allSteps.length;
    //
    CurStepObj = new StepObj();              // create a new step
    CurFuncObj.allSteps.push(CurStepObj);    // and push it
}



//----------------------------------------------------------------------------
// Load library modules.
// TODO: We will do the actual library loading from a file (JSON string)
//       For the moment, we just load some demo libs
// NOTE: We create library functions using the FuncObj itself. This will
//       allow us to create libary modulses using user code itself, if
//       we want to.
//----------------------------------------------------------------------------
function loadLibModules(pO) {

    // Create math library
    //
    loadMathLibrary(pO);

    loadFileInputLibrary(pO);

    loadFileOutputLibrary(pO);

    loadSystemLibrary(pO);

    loadMatrixLibrary(pO);

    loadTempSensorLibrary(pO);

    // TODO: load other libraries here

}


//----------------------------------------------------------------------------
// Loads temperature sensor library
//----------------------------------------------------------------------------
function loadTempSensorLibrary(pO) {

    var fM = new ModuleObj("TempSensorLib");

    CurModObj = fM;

    // Create a function object, add a function call expression (header)
    //
    var name = "collectSensorData";
    var fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    var arg1 = new ExprObj(true, ExprType.GridRef, '1DGrid');
    var arg2 = new ExprObj(true, ExprType.String, 'fileName');
    fPI.funcCallExpr.addSubExpr(arg1);
    fPI.funcCallExpr.addSubExpr(arg2);
    fM.allFuncs.push(fPI);

    var name = "findAverageTemp";
    var fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    var arg1 = new ExprObj(true, ExprType.GridRef, '1DGrid');
    fPI.funcCallExpr.addSubExpr(arg1);
    fM.allFuncs.push(fPI);

    var name = "findMaxTemp";
    var fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    var arg1 = new ExprObj(true, ExprType.GridRef, '1DGrid');
    fPI.funcCallExpr.addSubExpr(arg1);
    fM.allFuncs.push(fPI);



    // Add module specific object (extends ModuleObj). This object handles
    // all file input specific functions
    //
    fM.libObj = new TempSensorObject();

    // Add the library module to the program object
    //
    pO.libModules.push(fM);

}


//----------------------------------------------------------------------------
// Implementation of the file input libarary
//----------------------------------------------------------------------------
function TempSensorObject() {

    // Thie method is called when asynchronous file reading is complete
    //
    this.loadCSVFileDone = function(str, gO) {

        var elems = str.split(",");

        for (var e = 0; e < elems.length; e++) {
            gO.data[e] = parseFloat(elems[e]);
        }

        // Set the actual nubmer of rows/columns
        //
        gO.dimActSize[RowDimId] = elems.length;

        // Redraw step to show the data read
        //
        drawStep(CurStepObj);

        //alert("Contents:\n" + str);	
    }

    this.loadCSVFileContd = function(fiO, filename, gO) {


        var x_rem_obj;
        var readText = "";

        x_rem_obj = new XMLHttpRequest();
        x_rem_obj.onreadystatechange = handleStateChange;
        x_rem_obj.open("GET", filename, false);
        x_rem_obj.send();

        function handleStateChange() {
            if (x_rem_obj.readyState == 4) {
                readText = (x_rem_obj.status == 200 ? x_rem_obj.responseText :
                    null);
                fiO.loadCSVFileDone(readText, gO);
            }
        }

    }


    // Main entry point to read a CSV file
    //
    this.collectSensorData = function(gO, filename) {

        // A CSV file can be read only into a 2D grid
        //
        if (gO.numDims != 1) {
            alert("Reading sensor data into " + gO.caption +
                ": Must be 1D grid");
            return;
        }

        var fiO = this;
        fiO.loadCSVFileContd(fiO, filename, gO);
    }


    this.findAverageTemp = function(gO) {

        if (gO.numDims != 1) {
            alert("Calculating on data from " + gO.caption +
                ": Must be 1D grid");
            return;
        }

        var total_temp = 0;
        for (var i = 0; i < gO.dimActSize[RowDimId]; i++)
            total_temp += getDataValOfCell(gO, i, 0);

        return total_temp / gO.dimActSize[RowDimId];

    }

    this.findMaxTemp = function(gO) {

        if (gO.numDims != 1) {
            return;
        }

        var max_val = -999;
        for (var i = 0; i < gO.dimActSize[RowDimId]; i++)
            if (getDataValOfCell(gO, i, 0) > max_val)
                max_val = getDataValOfCell(gO, i, 0);

        return max_val;
    }

}


//----------------------------------------------------------------------------
// Loads file input library
//----------------------------------------------------------------------------
function loadFileInputLibrary(pO) {

    var fM = new ModuleObj("FileInput");

    // Set current module object to this module. This is necessary for
    // creating expressions. As a result the expressions in this library
    // get seq numbers starting from 1. See description at ModuleObj.
    //
    // VERIFY: TODO:
    //
    CurModObj = fM;

    // Create a function object, add a function call expression (header)
    //
    var name = "loadCSVFile";
    var fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    //
    // Add args. 
    //
    var arg1 = new ExprObj(true, ExprType.GridRef, '2DGrid');
    var arg2 = new ExprObj(true, ExprType.String, 'fileName');
    fPI.funcCallExpr.addSubExpr(arg1);
    fPI.funcCallExpr.addSubExpr(arg2);

    // Push the added function to the Math module
    //
    fM.allFuncs.push(fPI);


    //DC99:
    name = "loadCSRFile";
    fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    //
    // Add args.
    //
    arg1 = new ExprObj(true, ExprType.GridRef, 'Scalar_Grid');
    arg2 = new ExprObj(true, ExprType.GridRef, 'Scalar_Grid');
    var arg3 = new ExprObj(true, ExprType.GridRef, '1DGrid');
    var arg4 = new ExprObj(true, ExprType.GridRef, '1DGrid');
    var arg5 = new ExprObj(true, ExprType.GridRef, '3DGrid');
    var arg6 = new ExprObj(true, ExprType.GridRef, '3DGrid');
    var arg7 = new ExprObj(true, ExprType.GridRef, '2DGrid');
    var arg8 = new ExprObj(true, ExprType.GridRef, '2DGrid');
    var arg9 = new ExprObj(true, ExprType.GridRef, '2DGrid');
    var arg10 = new ExprObj(true, ExprType.GridRef, '1DGrid');
    fPI.funcCallExpr.addSubExpr(arg1);
    fPI.funcCallExpr.addSubExpr(arg2);
    fPI.funcCallExpr.addSubExpr(arg3);
    fPI.funcCallExpr.addSubExpr(arg4);
    fPI.funcCallExpr.addSubExpr(arg5);
    fPI.funcCallExpr.addSubExpr(arg6);
    fPI.funcCallExpr.addSubExpr(arg7);
    fPI.funcCallExpr.addSubExpr(arg8);
    fPI.funcCallExpr.addSubExpr(arg9);
    fPI.funcCallExpr.addSubExpr(arg10);

    // Push the added function to the Math module
    //
    fM.allFuncs.push(fPI);



    // Add module specific object (extends ModuleObj). This object handles
    // all file input specific functions
    //
    fM.libObj = new FileInputObject();

    // Add the library module to the program object
    //
    pO.libModules.push(fM);

}


//----------------------------------------------------------------------------
// Loads file input library
//----------------------------------------------------------------------------
function loadFileOutputLibrary(pO) {

    var fM = new ModuleObj("FileOutput");

    // Set current module object to this module. This is necessary for
    // creating expressions. As a result the expressions in this library
    // get seq numbers starting from 1. See description at ModuleObj.
    //
    // VERIFY: TODO:
    //
    CurModObj = fM;

    // Create a function object, add a function call expression (header)
    //
    var name = "saveCSVFile";
    var fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    //
    // Add args. 
    //
    var arg1 = new ExprObj(true, ExprType.ConstGridRef, '2DGrid');
    var arg2 = new ExprObj(true, ExprType.String, 'fileName');
    fPI.funcCallExpr.addSubExpr(arg1);
    fPI.funcCallExpr.addSubExpr(arg2);

    // Push the added function to the Math module
    //
    fM.allFuncs.push(fPI);

    // Add module specific object (extends ModuleObj). This object handles
    // all file input specific functions
    //
    fM.libObj = new FileOutputObject();

    // Add the library module to the program object
    //
    pO.libModules.push(fM);

}

//----------------------------------------------------------------------------
// Loads a math library
//----------------------------------------------------------------------------
function loadMathLibrary(pO) {

    var mM = new ModuleObj("Math");

    // Set current module object to this module. This is necessary for
    // creating expressions. As a result the expressions in this library
    // get seq numbers starting from 1. See description at ModuleObj.
    //
    // VERIFY: TODO:
    //
    CurModObj = mM;

    // All math library names are listed here. Firs list functions without
    // no args. Then use 'ADDARG' tag to move to functions with one additional
    // arg
    //
    var mathFnames = ['random',
        'ADDARG',
        'abs', 'ceil', 'cos', 'exp', 'floor', 'log',
        'sqrt', 'round', 'random', 'sin', 'tan',
        'ADDARG',
        'min', 'max', 'pow', 'mod'
    ];

    var number_args = 0;

    // Go thru all math functions listed 
    //
    for (var i = 0; i < mathFnames.length; i++) {

        var name = mathFnames[i];
        //	
        if (name == 'ADDARG') { // if 'ADDARG' tag, increment args
            number_args++;
            continue;
        }

        // Create a function object, add a function call expression (header)
        // 
        var fPI = new FuncObj(true);
        fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
        fPI.isLibFunc = true;
        //
        // Add 'number_args' args. Since this is the Math library, all args
        // are numbers
        //
        for (var a = 0; a < number_args; a++) {

            var arg1 = new ExprObj(true, ExprType.Number, 'number');
            fPI.funcCallExpr.addSubExpr(arg1);

        }

        // Push the added function to the Math module
        //
        mM.allFuncs.push(fPI);
    }


    //DC4: For supporting sum(GRID, START, END).
    var name = "sum";
    var fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    var arg1 = new ExprObj(true, ExprType.GridRef, '1D Grid');
    fPI.funcCallExpr.addSubExpr(arg1);
    arg1 = new ExprObj(true, ExprType.Number, 'start');
    fPI.funcCallExpr.addSubExpr(arg1);
    arg1 = new ExprObj(true, ExprType.Number, 'end');
    fPI.funcCallExpr.addSubExpr(arg1);
    mM.allFuncs.push(fPI);


    // Add the Math module to the program object
    //
    pO.libModules.push(mM);

}


//----------------------------------------------------------------------------
// Loads system library (interacts with OS shell, utils)
//----------------------------------------------------------------------------
function loadSystemLibrary(pO) {

    var fS = new ModuleObj("System");

    // Set current module object to this module. This is necessary for
    // creating expressions. As a result the expressions in this library
    // get seq numbers starting from 1. See description at ModuleObj.
    //
    // VERIFY: TODO:
    //
    CurModObj = fS;

    // Create a function object, add a function call expression (header)
    //

    // Note: A command excuted via the system command should produce a file
    //       e.g., a csv file, which a program can read into a grid.
    //
    var name = "runCommand";
    var fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    //
    // Add args. 
    //
    var arg1 = new ExprObj(true, ExprType.String, 'system command line');
    fPI.funcCallExpr.addSubExpr(arg1);


    // Push the added function to the module
    //
    fS.allFuncs.push(fPI);

    // Add module specific object (extends ModuleObj). This object handles
    // all module specific functions
    //
    fS.libObj = new SystemObject();

    // Add the library module to the program object
    //
    pO.libModules.push(fS);

}




//----------------------------------------------------------------------------
// Loads Matrix Library
//----------------------------------------------------------------------------
function loadMatrixLibrary(pO) {

    var mO = new ModuleObj("Matrix");

    // Set current module object to this module. This is necessary for
    // creating expressions. As a result the expressions in this library
    // get seq numbers starting from 1. See description at ModuleObj.
    //
    // VERIFY: TODO:
    //
    CurModObj = mO;

    // Create a function object, add a function call expression (header)
    //

    // Note: A command excuted via the system command should produce a file
    //       e.g., a csv file, which a program can read into a grid.
    //
    var name = "add";
    var fPI = new FuncObj(true);
    fPI.funcCallExpr = new ExprObj(false, ExprType.LibFuncCall, name);
    fPI.isLibFunc = true;
    //
    // Add args. 
    //
    var arg1 = new ExprObj(true, ExprType.GridRef, '2D_dest_grid');
    fPI.funcCallExpr.addSubExpr(arg1);
    //
    var arg2 = new ExprObj(true, ExprType.ConstGridRef, '2D_src1_grid');
    fPI.funcCallExpr.addSubExpr(arg2);
    //
    var arg3 = new ExprObj(true, ExprType.ConstGridRef, '2D_src2_grid');
    fPI.funcCallExpr.addSubExpr(arg3);

    // Push the added function to the module
    //
    mO.allFuncs.push(fPI);

    // Add module specific object (extends ModuleObj). This object handles
    // all module specific functions
    //
    mO.libObj = new MatrixObject();

    // Add the library module to the program object
    //
    pO.libModules.push(mO);

}



//----------------------------------------------------------------------------
// Reset global variables to give a fresh start (e.g., when a prog is loaded) 
//----------------------------------------------------------------------------
function resetGlobalVars() {

    Timer = null;
}

//----------------------------------------------------------------------------
// Add a new user module
//----------------------------------------------------------------------------
function addNewModule() {

    // Prompt user for the new module name
    //
    var modname = prompt("Name of the new module", "Module");

    // if the user pressed cancel, just return
    //
    if (!modname)
        return;

    if (isExistingModuleOrLibName(modname) || isKeyword(modname)) {
        alert("Name " + name + " already exists!");
        return;
    }

    // TODO: Check duplicate module/lib/grid/func name
    //

    // Create a new module and init it. Make it the current module
    //
    initCurModule(modname);             // init module

    initMainFunc(DefMainFuncName);      // add main function and init it

    initCurStep();                      // add and init a step

    changeModule(CurProgObj.curModNum);
}


/* ************* SECTION H: Helper functions for app  ********************** */


//----------------------------------------------------------------------------
// Our own implementation of assert
//----------------------------------------------------------------------------
function assert(cond, msg) {
    if (!cond) { 
	alert("ASSERT FAIL: " + msg);
	throw 'Breakpoint for assert reached';
    }
}


//----------------------------------------------------------------------------
// Show a tip in the bottom of the window
// TODO: Dispaly a help link as well (indexed by the same tipId) that will
//       take you to a more detailed description.
//----------------------------------------------------------------------------
function showTip(tipId) {

    assert(tipId < TipId.End, "Invalid TipId");

    var tip1 = document.getElementById('tipWindow');
    var img = "<HR><img src='images/tip.jpg' width=16px height=16px>";
    tip1.innerHTML = img + TipArr[tipId] + "<BR><HR>";
}

//----------------------------------------------------------------------------
// Emits a warning message
//----------------------------------------------------------------------------
function warn(msg) {

    alert(msg);
}


//----------------------------------------------------------------------------
// Shows a debug message in the tip window
// TODO: We can also show debug messages in JS console
//----------------------------------------------------------------------------
function showDbg(msg) {

    var tip1 = document.getElementById('tipWindow');
    var img = "<img src='images/tip.jpg' width=16px height=16px> ";
    tip1.innerHTML = msg;
}

//----------------------------------------------------------------------------
// Emits a log message
//----------------------------------------------------------------------------
function logMsg(msg) {

    console.log(msg);
}

//----------------------------------------------------------------------------
// Clear the tip
//----------------------------------------------------------------------------
function clearTip() {

    var tip1 = document.getElementById('tipWindow');
    tip1.innerHTML = "";
}

//----------------------------------------------------------------------------
// Gets a background color for 'val' based on 'max'// Current implementation
// gives *gray scale*
// NOTE: Ligther colors have higher values -- i.e., ffffff is white
//----------------------------------------------------------------------------
function getHexColor(val, max) {

    if (!Number.isFinite(val))     // if error
	return "ff0000";           // return red

    var val2 = max - val;          // difference from max 
    var HiC = 255;                 // highest color value (lightest color)
    var LowC = 75;                 // lowest color value (darkest color)
    
    var x = LowC + (val2*(HiC  - LowC))/max

    var bin = Math.floor(x).toString(16);
    var color = bin + bin + bin;
    return color;
}



/* ************* SECTION I: Other Funcs To be Catergorized  **************** */




//----------------------------------------------------------------------------
// draw program structre heading at top of page
//----------------------------------------------------------------------------
function drawProgStructHead() {

    var pO = CurProgObj;
    var fO = CurFuncObj;
    var sO = CurStepObj;

    var com_opt = "style='float:left' ";
    var com_id = getCommentHtmlId(CommentType.StepTitle, 0);
    var comment = getCommentStr('div', com_id, com_opt, sO.stepComment);

    // STEP: Span that is placed on the left
    //
    var curmod = getModuleSelectStr(); // CurModObj.name;
    var str = "<span class='progStructHead'>" +
        " <span title='Current Module'>" + curmod + "</span> ::";

    var curfunc = getFuncSelectStr();
    str += " <span title='Current Function'>" + curfunc + "</span> :: ";

    //	+ " <span title='Current Function'>main()</span> :: ";

    // Step selection drop down box
    //
    str += "<span> " + getStepSelectStr() + "</span>";
    //
    // Get step advance arrow buttons
    //
    str += "<span> " + getStepArrowStr() + "</span>";

    str += "<span> &nbsp </span> <span> &nbsp </span> ";

    // Display the step title. If sO.title is empty (null), we use a
    // default title. 
    //
    var titletxt = sO.title;
    if (!titletxt) {
        titletxt = (sO.isHeader) ? DefHeaderTitle : DefStepTitle;
    }
    //
    // TODO: A text area causes a larger vertical space on Firefox. 
    //
    var onc = " onchange='recordStepTitle(this)' ";
    var title = "<textarea rows='1' cols='30' class='stepTitle' " +
        " height=16px maxlength=80" +

        "id='StepTitle' " + onc + ">" + titletxt + "</textarea>";

    //title = "<span class='stepTitleSpan' contenteditable>Title</span>";


    // Comment for this step
    // NOTE: Changing this order causes Firefox to draw incorrectly
    //
    str += "</span>" + title + comment;

    // have we finished grid configuration
    //
    var conf_done = (sO.stageInStep >= StageInStep.ConfigDone);

    // STEP: Start a new span that floats on to the right of the page
    //
    str += "<span style='float:right'>";             // place on the right
    var onc = "";

    // show parallelism meter only if configuration is done AND showing data
    // Further, do not show parallelism meter in the header step
    //
    if (conf_done && pO.showData && !sO.isHeader) {

        str += getParallelismMeterStr();
    } 

    // We can create a new step if config is done OR if we are in a header
    // AND if not showing data
    //
    if (!pO.showData && (conf_done || sO.isHeader)) {

        onc = " onclick='newNextStep(" + fO.curStepNum + ")' ";
        onc += " class='headbut' ";
        str += "<input type='button' value='Insert New Step' " + onc +
            ">";
    }

    // We can duplicate a step only after completing it.
    // Note: We cannot duplicate the function header step.
    // Note: do not show duplicate step while showing data
    //
    if (!pO.showData && conf_done && !sO.isHeader) {
        onc = " onclick='duplicateStep()' ";
        onc += " class='headbut' ";
        str += "<input type='button' value='Duplicate Step' " + onc + ">";
    }

    // We can cancel/delete a step only after starting to configure it
    // We can *never* cancel a header step
    // Note: Don't show this option while showing data
    //
    if ((sO.stageInStep >= StageInStep.New) && !sO.isHeader && !pO.showData) {
        onc = " onclick='deleteStep()' ";
        onc += " class='headbut' ";
        str += "<input type='button' value='Delete Step' " + onc + ">";
    }

    str += getMenuHeadStr();

    str += "</span><BR>"; // floating right span
    str += "<HR>"; // horizontal divider

    // Information Window
    //
    str += "<span id='infoWin'></span>";

    var head1 = document.getElementById('progHead');
    head1.innerHTML = str;

    // updateInfoWin("Sample Message");
}


// Finds a grid named 'name' in function 'fO' and
// returns its dynamic size
function findGridGetDynSize(fO, name) {

    for (var i = 0; i < fO.allGrids.length; i++) {

	if (fO.allGrids[i].caption == name) {

	    return fO.allGrids[i].data[0];

	}

    }

}

//----------------------------------------------------------------------------
// Parse the expression in a foreach loop, to evaluate start,end,
// step (in Java) so as to get values for parallelism meter.
//----------------------------------------------------------------------------
function getRexprVal(fO, rexpr, exprArr) {

    var retVal = "";

    // Loop through all exprObj in exprArr
    for (var i = 0; i < exprArr.exprArr.length; i++) {

	if (exprArr.exprArr[i].type == ExprType.Number) {

	    retVal += "parseInt(exprArr.exprArr[" + i + "].str) + ";

	} else if (exprArr.exprArr[i].type == ExprType.Operator) {

	    retVal = retVal.replace(/ \+ $/, "");
	    retVal += " " + exprArr.exprArr[i].str + " ";

	} else if (exprArr.exprArr[i].type == ExprType.Literal) {

	    // endX: Can be static or dynamic.
	    var actendval = rexpr.gO.dimActSize[rexpr.selDim];
            var dynendval = rexpr.gO.dimDynSize[rexpr.selDim];
            retVal += (dynendval) ? findGridGetDynSize(fO, dynendval) 
		      : "parseInt(rexpr.gO.dimActSize[rexpr.selDim]) - 1 + ";
	
	    // TODO: If DYNAMIC have to find the grid in internal represent
	    //       and get its value.

	} else if (exprArr.exprArr[i].type == ExprType.Concat) {

	    retVal += "parseInt(" + 
		getRexprVal(fO, rexpr, exprArr.exprArr[i]) + ") + ";

	} else if (exprArr.exprArr[i].type == ExprType.GridCell) {

	    // TODO: Expand to support multi-dimensional grid's cell
	    retVal += parseInt(exprArr.exprArr[i].gO.data[0]) + " + "; 

	} else {

	    alert("ERROR in getRexprVal()");

	}	


    }

    // Remove last " + "
    retVal = retVal.replace(/ \+ $/, "");

    //alert(retVal);

    return eval(retVal);

}


// --------------------------------------------------------------------------
// Returns the parallelism meter for a step (while showing data)
// --------------------------------------------------------------------------
function getParallelismMeterStr() {

    var pO = CurProgObj;
    var fO = CurFuncObj;
    var sO = CurStepObj;

    var str = "";

    // Code to get the information on parallelization AFTER 
    // parallelism  analysis has been run and Pragma_str has been 
    // filled for all (so including THIS) steps.
    // TODO: For efficiency we can just focus on analyzing CURRENT 
    // step only with findParallelismInFunction(fO,sO).
    //
    analyzeParallelismAll(CurFuncObj, 1);

    var rangeExpr = sO.boxExprs[CodeBoxId.Range];

    if (rangeExpr && rangeExpr.exprArr && rangeExpr.isForeach()) {

        var num_index_vars = rangeExpr.exprArr.length;

        for (var iv = 0; iv < num_index_vars; iv++) {

            var rexpr = rangeExpr.exprArr[iv];
            var ivar = rexpr.labelExpr.str;

	    var start = getRexprVal(fO, rexpr, rexpr.exprArr[RangeFields.Start]);
	    var end = getRexprVal(fO, rexpr, rexpr.exprArr[RangeFields.End]);
	    var step = getRexprVal(fO, rexpr, rexpr.exprArr[RangeFields.Step]);


	    /*

            // TODO: CAUTION: Below we need to support use of a scalar 
            // variable as end variable, or combinations thereof 
            // (evaluate value). Do same for start/step if allowed by 
            // GUI! Gets a constant number input by the user manually
            //

            // TODO: CAUTION: Need to generalize for multi-dimensional and
            // complex grids used here (if allowed).
            //
	    
	    // If not a number can be a concat (e.g., end0-4), where end0
	    // may be actSize or dynSize, or can be a grid (scalar/non-scalar),
	    // or can be endX (actSize or dynSize).
            if (isNaN(start)) {
                start = parseInt(rexpr.exprArr[RangeFields.Start].gO.data[0]);
	    }

            // If not a number (that would be a constant)
            if (isNaN(endv)) {

                // If it is a grid (scalar or non-scalar?)
                if(rexpr.exprArr[RangeFields.End].gO != null) {
		    // TODO: CAUTION: Previously this was available? Or just
		    //       wrong?
                    endv=parseInt(rexpr.exprArr[RangeFields.End].gO.data[0])+1;

                } else {    // Else it is end0, etc., so get it from internally.

                    endv = parseInt(rexpr.gO.dimActSize[rexpr.selDim]);

                }

            }

            if (isNaN(step))
                endv = parseInt(rexpr.exprArr[RangeFields.Step].gO.data[0]);
            

	    */
	    
	    
	    // TODO: CAUTION: What to do for non-round numbers? 
            // Now shows decimal exact result.
            //
            var num_iters = (end + 1 - start) / step;

            // alert("start: " + start + ", end: " + endv 
            // + ", step: " + step + ", num_iters: " + num_iters);

            // Get the string that contains all parallel dimensions 
            // for this step of this function
            //
            var fname = getFuncIdByName(CurModObj, fO.funcCallExpr.str);

            var loop_parallel_dims = Pragma_str[fname][fO.curStepNum];

            if (loop_parallel_dims.indexOf(ivar) != -1) {

                // If dimension is parallel (i.e., is present in 
                // Pragma_str[][])    

                str += "<div class='parallelDivFull'>" + ivar + ":" +
                    num_iters + "</div>";

            } else {

                // If dimension is NOT parallel (i.e., is present in 
                // Pragma_str[][])
                //
                str += "<div class='parallelDivEmpty'>" + ivar + ":" +
                    "0</div>"
            }

        } // for each index var

    } // if foreach


    // Title of the parallelism meter
    //
    var title  = "<table class='parallelMeter'><tr><td>Parallelism:</td><td>";

    // When the step does not cotain an index range, there is no parallelism
    // detected. Specify "None" in those cases
    //
    if (str.length == 0) {
	str = "None";
    } 
    //
    str = title + str + "</td></table>";

    return str;
}


// --------------------------------------------------------------------------
// Write 'str' to the infoWin just below the program structure heading
// --------------------------------------------------------------------------
function updateInfoWin(str) {

    var infowin = document.getElementById('infoWin');
    infowin.innerHTML = str + "<BR><HR>";
}

// --------------------------------------------------------------------------
// Clear infoWin just below the program structure heading
// --------------------------------------------------------------------------
function closeInfoWin() {

    var infowin = document.getElementById('infoWin');
    infowin.innerHTML = "<span id='infoWin'></span>";
}


// --------------------------------------------------------------------------
// We store the title only if it is different from the default. This is
// a space saving measure.
// --------------------------------------------------------------------------
function recordStepTitle(obj) {

    //alert("title change: " + obj.value);

    var sO = CurStepObj;

    var titletxt = (sO.isHeader) ? DefHeaderTitle : DefStepTitle;
    if (obj.value != titletxt) {
        sO.title = obj.value;
    }
}

// --------------------------------------------------------------------------
// Menu string
// TODO: Support automatic drop down when we mouseover -- need two 
//       getMenuStr() functions -- initial and to display everything
// TODO: When the user selects an item, don't show it as the selection --
//       restore to 'Menu' option
// --------------------------------------------------------------------------
function getMenuHeadStr() {

    var onc = " onclick='getMenuBodyStr(this)' ";

    var str =
        "<span>&nbsp;</span>" +
        "<span class='menu1Body' id='menu1body'></span>" // dummy body
        + "<span class='menu1Head' id='menu1head'" + onc + ">" // head
        + " Main Menu " + "</span>";

    return str;
}


// --------------------------------------------------------------------------
// Get an html span representing the 'body' (i.e., menu items) of the 
// main menu
// --------------------------------------------------------------------------
function getMenuBodyStr() {

    var pO = CurProgObj;
    var sO = CurStepObj;

    // The menu head element (which says 'Menu')
    //
    var head1 = document.getElementById('menu1head');
    //
    // Menu body with options
    //
    var body1 = document.getElementById('menu1body');

    // This is the menu body with options, formatted as a table
    //
    var str =
        "<span class='menu1Body' id='menu1body'>" +
        "<table class='ddmenu'>" + 

	"<tr><td onclick='menuClicked(" + MMenu.Open +
        ")'>Open</td></tr>" + 

	"<tr><td onclick='menuClicked(" + MMenu.Save +
        ")'>Save</td></tr>";
  
	/*	
	"<tr><td onclick='menuClicked(" + MMenu.UnusedGrids +
        ")'>Unused&nbsp;Grids</td></tr>";
	*/
	

    // if already showing data/color, display 'Hide data' menu item.
    // Else, show 'show Data' item
    //
    if (pO.showData) {
        str += "<tr><td onclick='menuClicked(" + MMenu.HideData +
            ")'>Hide&nbsp;Data</td></tr>";
    } else {

        // Not showing data or color. So, display show data
        //
        str += "<tr><td onclick='menuClicked(" + MMenu.ShowData +
            ")'>Show&nbsp;Data</td></tr>";
    }

    // if not already displaying color (with data), show option to display
    // color
    //
    if (pO.showData != ShowDataState.DataAndColor) {
        str += "<tr><td onclick='menuClicked(" + MMenu.Colorize +
            ")'>Colorize</td></tr>"
    }

    if (pO.showData != ShowDataState.DataImage) {
        str += "<tr><td onclick='menuClicked(" + MMenu.DataImage +
            ")'>Data&nbsp;Image</td></tr>"
    }

    /*
    str += "<tr><td onclick='menuClicked(" + MMenu.ShowCode +
        ")'>Show&nbsp;Code</td></tr>"
    */

    /*
    str += "<tr><td onclick='menuClicked(" + MMenu.AddModule +
        ")'>New&nbsp;Module</td></tr>"
    */

    /*
    str += "<tr><td onclick='menuClicked(" + MMenu.ShowCcode + 
    	")'>Show&nbsp;C&nbsp;Code</td></tr>"
        //KK: Used for showing relevant menu item when Main Menu is clicked: 
        //C generated code
    */

    str += "<tr><td onclick='menuClicked(" + MMenu.ShowAutotuneMenu +
        ")'>Generate code...</td></tr>"
        //AT: Used for showing the auto-tune menu page.

    /*
    str += "<tr><td onclick='menuClicked(" + MMenu.ShowFortranCode + 
    	")'>Show&nbsp;Fortran&nbsp;Code</td></tr>"
        //KK: Used for showing relevant menu item when Main Menu is clicked: 
        //FORTRAN generated code
    */

    /*        
    str += "<tr><td onclick='menuClicked(" + MMenu.ShowFortranCodeParallel + 
    	")'>Show&nbsp;Parallel&nbsp;Fortran&nbsp;Code</td></tr>"
    	//KK: Used for showing relevant menu item when Main Menu is clicked: 
    	//parallel FORTRAN generated code
    */

   
     
    str += "<tr><td onclick='menuClicked(" + MMenu.SaveCcode +
        ")'>Save&nbsp;C&nbsp;Code</td></tr>";
    

    /*
    str += "<tr><td onclick='menuClicked(" + MMenu.SaveCcodeParallel +
        ")'>Save&nbsp;Parallel&nbsp;C&nbsp;Code</td></tr>";
    */

    /*
    //MPI
    str += "<tr><td onclick='menuClicked(" + MMenu.SaveMPICcode +
        ")'>Save&nbsp;MPI C&nbsp;Code</td></tr>";
    */

    
    str += "<tr><td onclick='menuClicked(" + MMenu.SaveFortranCode +
        ")'>Save&nbsp;Fortran&nbsp;Code</td></tr>"
    

    /*
    str += "<tr><td onclick='menuClicked(" + MMenu.SaveFortranCodeParallel +
        ")'>Save&nbsp;Parallel&nbsp;Fortran&nbsp;Code</td></tr>";
    */

    /*    
    str += "<tr><td onclick='menuClicked(" + MMenu.SaveOCLcode +
    	")'>Save&nbsp;OpenCL code</td></tr>" +
	"</table>"; + "</span>";
    */

    // Get the bounding rectangle of 'head1'
    // and use that bottom/left position to place the body of the menu
    //
    var rect = head1.getBoundingClientRect();

    body1.style.top = rect.bottom + 'px';
    body1.style.left = rect.left + 'px';

    // set the innerHTML of the body. Note that the head remains as is
    //
    body1.innerHTML = str;         // update HTML of menu body
    body1.className = 'menu1Body'; // change class of htmlId

}

// --------------------------------------------------------------------------
// Function called when the user clicks on a menu item on the main menu
// --------------------------------------------------------------------------
function menuClicked(item) {

    switch (item) {

        case MMenu.Open:
            loadProg();
            break;

        case MMenu.Save:
            saveProg();
            break;

        case MMenu.SaveAs:
            saveProgAs();
            break;

        case MMenu.ShowData:
            showData(ShowDataState.DataOnly);
            break;

        case MMenu.Colorize:
            showData(ShowDataState.DataAndColor);
            break;

        case MMenu.DataImage:
            showData(ShowDataState.DataImage);
            break;

        case MMenu.HideData:
            showData(ShowDataState.None);
            break;

        case MMenu.ShowCode:
            showJavaScriptStr();
            break;

        case MMenu.AddModule:
            addNewModule();
            break;

        case MMenu.ShowFortranCode:
            showFortranStr(1, 0, 1); //KK: SoA by default
            break;

        case MMenu.ShowCcode:
            showCstr(1, 0, 1); //KK: SoA by default
            break;

	case MMenu.ShowAutotuneMenu:
            showAuTuMenu(); //AT: Show auto-tune menu
            break;

        case MMenu.ShowFortranCodeParallel:
            showFortranStr(1, 1, 1); //KK: SoA by default
            break;

        case MMenu.SaveFortranCode:
            saveFortranStr(0, 0);
            break;

        case MMenu.SaveCcode:
            saveCstr(0, 0);
            break;  

	case MMenu.SaveCcodeParallel:
            saveCstr(1, 0);
            break;  

	//MPI: Not supported in this version
        case MMenu.SaveMPICcode:
            saveMPICstr(0, 0);
            break; 

        case MMenu.SaveFortranCodeParallel:
            saveFortranStr(1, 0);
            break;

	//OCL: Not supported in this version
	case MMenu.SaveOCLcode:
	    saveOCLstr(1, 0); 

        case MMenu.AnalyzeParallelism:
            analyzeParallelismAll(CurFuncObj, 0);
            break;

        case MMenu.UnusedGrids:
            showUnusedGrids();
            break;

    };

    removeMenuBody();

}


// --------------------------------------------------------------------------
// Removes the main menu body
// --------------------------------------------------------------------------
function removeMenuBody() {

    var body1 = document.getElementById('menu1body');
    body1.innerHTML = "<span id='menu1body'></span>";


}

// --------------------------------------------------------------------------
// Show a drop down list of unused grids for the user to delete
// --------------------------------------------------------------------------
function showUnusedGrids() {


    var fO = CurFuncObj;
    var sO = CurStepObj;

    // If the user is already selecting grids, don't allow selection --
    // because we need to use PreviewHtmlGridId
    //
    if ((sO.stageInStep > StageInStep.New) &&
        (sO.stageInStep < StageInStep.AllDone)) {
        alert("Please complete grid selection in current step to start");
        return;
    }

    // No keyboard input handling during a grid selection
    //
    sO.keyboardEnabled = false;

    // STEP: ..............................................................
    // Create a drop-down menu and a pre-view pane to select
    // Note: We use a table with two rows to create a preview pane
    // TODO: change the preview pane when the user goes through the
    //       drop-down menu. Currently, the user has to make a selection
    // 
    var str = "<table> <tr><td><div id='" + PreviewHtmlGridId +
        "'> Disp </div></td></tr>";

    // Create the list of all previous grids for the drop down box
    // Note: We go in reverse direction to list the latest grids first
    //
    str += "<tr><td> Select Un-used Grid to delete: <select id='selWin' " +
        " onclick='changeUnusedGrid(this.selectedIndex)'> ";
    //
    ExistingGridList = new Array();
    //
    for (var i = fO.allGrids.length - 1; i >= 0; i--) {

        var gridO = fO.allGrids[i];

        // if the grid is not present in any step in the current function,
        // we add to the list as an unused grid
        //
        if (!isPresentInAPrevStep(gridO, null)) {

            // if the grid is alrady marked as deleted, just delete it because
            // there are no users of it within this function. Not necessary
            // to ask the user to delete such grids, which usually happens
            // when function arguments are replaced/deleted. 
            //
            if (gridO.deleted == DeletedState.Deleted) {
                removeUnusedGrid(gridO);
                continue;
            }

            str += "<option value='" + i + "'";
            str += ">" + gridO.caption + "</option>";

            ExistingGridList.push(gridO);
        }
    }


    if (ExistingGridList.length == 0) {

        confirm("No un-used grids present in this function");
        return;
    }

    // TODO:
    // Insert all global grids from GlobalFuncObj and templates. A user can pick
    // grids from:
    //       (1) Global templates (from CurFuncObj.TemplateFuncObj)
    //       (2) global grids (from CurModObj.GlobalFuncObj)
    //       (3) input parameters (args) to the function
    //       (4) local grids created within function


    str += "</select>";
    str +=
        "<input type='button' value='Delete' onclick='unusedSelDone()'>";
    str +=
        "<input type='button' value='Cancel' onclick='unusedSelCancel()'>";

    str += "<BR><BR><div id='selNameDiv'></div>";

    str += "</td></tr></table>";

    // change the innerHTML of the HTML object 
    //
    var gridId = sO.allGridIds.length;
    CurHtmlGridId = AllHtmlGridIds[gridId];
    var menuId = CurHtmlGridId;
    var menu1 = document.getElementById(menuId);
    menu1.innerHTML = str;

    // Set default grid seclection to 0 by manually calling changeGridSel()
    //
    changeUnusedGrid(0);

}

// --------------------------------------------------------------------------
// When the user changes the selection of unused grids, this function
// re-draws the grid
// --------------------------------------------------------------------------
function changeUnusedGrid(ind) {

    var sO = CurStepObj;

    assert((ind < ExistingGridList.length), "invalid index");
    var selObj = ExistingGridList[ind];

    // Note: we clone to associate grid Obj w/ a different htmlId. However,
    //       the clone is discarded. Hence, newGId is never registered with
    //       CurFuncObj
    //
    var newGId = CurFuncObj.allGrids.length;
    var ngO = new cloneGridObj(newGId, selObj);
    //
    var niO = null; // no indices to show
    drawGrid(ngO, niO, PreviewHtmlGridId, false);
}


// --------------------------------------------------------------------------
// When the user clicks on the 'delete' button to delete an unused grid,
// this 
// Note: Displays drop down box window at 'CurHtmlGridId'
// --------------------------------------------------------------------------
function unusedSelDone() {

    var sO = CurStepObj;
    var fO = CurFuncObj;

    // Reads the selction value from the 'selWin' id
    //
    var sel1 = document.getElementById('selWin');

    // Get the selected index from the drop down box
    //
    var selind = sel1.selectedIndex;
    assert((selind < ExistingGridList.length), "invalid index");
    var selObj = ExistingGridList[selind];


    // Find the index of this grid in the function
    //
    var ind = findIndexOfGrid(fO, selObj);

    // Displays the selected grid at 'CurHtmlGridId'
    //
    var gOsel = CurFuncObj.allGrids[ind];

    removeUnusedGrid(fO, gOsel, ind);

    // Redraw the current step
    //
    drawStep(sO);
}

// --------------------------------------------------------------------------
// Remove a selected *unused* grid from allGrids[] of a function and then
// adjust the gridIds in all steps in the function.
//
// NOTE: The caller MUST make sure that gOsel is actually *unused* by any
//       step in the function
// --------------------------------------------------------------------------
function removeUnusedGrid(fO, gOsel, ind) {

    logMsg("peremently deleting unused grid: " + gOsel.caption);

    // Just mark deleted (as a troubleshooting step)
    //
    gOsel.caption = "DELETED";

    // Remove the grid at 'ind' from allGrids in functions
    //
    fO.allGrids.splice(ind, 1);


    // Adjust gridIDs stored in all steps in this function after removing
    // a grid
    //
    // Go thru all steps in the function 
    //
    for (var s = 0; s < fO.allSteps.length; s++) {

        var stepO = fO.allSteps[s];

        // Go thru all grids in a step
        //
        for (var g = 0; g < stepO.allGridIds.length; g++) {

            var gid = stepO.allGridIds[g];

            // if the grid Id is after the deleted grid
            //
            if (gid > ind)
                stepO.allGridIds[g] = gid - 1;
        }
    }
}

// --------------------------------------------------------------------------
// Cancel un-used grid selection
// --------------------------------------------------------------------------
function unusedSelCancel() {

    drawStep(CurStepObj);
}


// --------------------------------------------------------------------------
// Returns the html string (for the drop down box) to select modules
// --------------------------------------------------------------------------
function getModuleSelectStr() {

    var selected_index = CurProgObj.curModNum;

    var str = "<select class='stepsel' " +
        " onchange='changeModule(this.selectedIndex)'>";

    for (var i = 0; i < CurProgObj.allModules.length; i++) {
        var val = CurProgObj.allModules[i].name;
        str += "<option value='" + val + "'";
        if (i == selected_index)
            str += "selected";
        str += ">" + val + "</option>";
    }

    str += "</select>";

    return str;
}

// --------------------------------------------------------------------------
// Change the current modeule 
// --------------------------------------------------------------------------
function changeModule(ind) {

    // TODO: Check what to do about current step -- whether to abandon it
    // (create a common func to do that). 

    CurModObj = CurProgObj.allModules[ind];
    CurProgObj.curModNum = ind;

    CurFuncObj = CurModObj.allFuncs[CurModObj.curFuncNum];

    drawProgStructHead(); // program structre heading at top of page

    drawStep(CurFuncObj.allSteps[CurFuncObj.curStepNum]);
}


// --------------------------------------------------------------------------
// get a string containing a list of functions for selection
// --------------------------------------------------------------------------
function getFuncSelectStr() {

    var selected_index = CurModObj.curFuncNum; // current Func Num

    var str = "<select class='stepsel' " +
        " onchange='changeFunc(this.selectedIndex)'>";

    for (var i = 0; i < CurModObj.allFuncs.length; i++) {
        var val = CurModObj.allFuncs[i].funcCallExpr.str;
        str += "<option value='" + val + "'";
        if (i == selected_index)
            str += "selected";
        var paren = (CurModObj.allFuncs[i].isRegularFunc()) ? "()" : "";
        str += ">" + val + paren + "</option>";
    }

    str += "</select>";

    return str;
}

// --------------------------------------------------------------------------
// Find the function id by name in a given module
// --------------------------------------------------------------------------
function getFuncIdByName(mO, fname) {

    for (var f = 0; f < mO.allFuncs.length; f++) {

        if (mO.allFuncs[f].funcCallExpr.str == fname)
            return f;
    }

    assert(0, "Function name " + fname + " not found in given module");
}


// --------------------------------------------------------------------------
// This funciton is called when a digit of a number within a function
// call is inserted/deleted (so that we can check against function header)
// --------------------------------------------------------------------------
function updateFuncArgType(fcallexpr, arg, str) {

    logMsg("checking type for arg:" + arg);

    var ind = getFuncIdByName(CurModObj, fcallexpr.str);
    var fO = CurModObj.allFuncs[ind];             // function
    var sO = fO.allSteps[FuncHeaderStepId];       // header step

    // Get grid ID corresponding to arg
    //
    var argpos = arg+1;                 // skip return value by adding 1      
    var gid = sO.allGridIds[argpos];    // grid id

    var gO = fO.allGrids[gid];          // grid object

    if (gO.numDims > 0) {
        alert("Argument type mismatch. Argument in Function Header is " +
            " not scalar.");
        return;
    }

    assert((gO.typesInDim < 0), "This func can change global type only");

    // Set the data type to real/integer based on '.'
    //
    var type = DataTypes.Integer;
    if (str.search(/\./) >= 0)
        type = DataTypes.Real;

    assert(gO.dataTypes.length, "Must have the global data type");

    // Check for argument type compatibility
    //
    var headTy = gO.dataTypes[0];

    // logMsg("checking type ... head:" + headTy + " ty:" + type);

    if ((headTy == DataTypes.Integer) || (headTy == DataTypes.UniqInd)) {
        if (type == DataTypes.Integer)
            return;
    }

    if ((headTy == DataTypes.Real) || (headTy == DataTypes.Real4)) {
        if (type == DataTypes.Real)
            return;
    }

    // TODO: We should be automatically update the data type of a number 
    //       argument in the function header in *some* instances. For instance
    //       if we can make sure that the number argument is added 'freshly'
    //       


    // We are here becaue 'headTy' and 'type' were incompatible. So display
    // an error.
    //
    alert("Argument data type mismatch. Argument in Function Header is '" +
        TypesArr[headTy] + "'. You may change data type in Function " +
        " Header");

}


// --------------------------------------------------------------------------
// Returns the data type of a scalar expression 
// --------------------------------------------------------------------------
function findDataTypeOfScalarExpr(expr) {

    if (expr.isNumber())                          // if number
	return findDataTypeOfNumberExpr(expr);
    else if (expr.isGridCell())                   // if grid cell
	return findDataTypeOfGridCellExpr(expr);
    else if (expr.isIndex())
	return DataTypes.Integer;                 // all indices are integer
    else
        assert("Unknown ExprType as a scalar");

}

// --------------------------------------------------------------------------
// Finds the data type (int/real) of a number expression
// --------------------------------------------------------------------------
function findDataTypeOfNumberExpr(expr) {

    assert(expr.isNumber(), "Expr must by of ExprType.Number");

    if (expr.str.match(/\./))
        return DataTypes.Real;
    else
        return DataTypes.Integer;
}

// --------------------------------------------------------------------------
// Find the data type of grid cell (for passing as an arg)
// --------------------------------------------------------------------------
function findDataTypeOfGridCellExpr(expr) {

    assert(expr.isGridCell(), "must be a grid cell expr");
    assert(expr.gO, "grid cell must have gO reference");

    var gO = expr.gO;

    if (gO.typesInDim < 0) {            // global type

        return gO.dataTypes[0];

    } else {                            // type is in each title

        // Data types are in gO.typesInDim dimension
        // Note: For any grid cell, dimIds contain which indices for picked
        //
        var indExpr = expr.exprArr[gO.typesInDim]; // index expr
        var indval = expr.dimIds[gO.typesInDim];
        var datatype = gO.dataTypes[indval];
        //
        // alert("dim:" + gO.typesInDim + " title:" + indExpr.str  
        //      + " ind:" + indval + " datatype:" + datatype);
        //
        return datatype;
    }
}

// --------------------------------------------------------------------------
// Checks whether a given function call argument is actually used within
// the called function steps
// --------------------------------------------------------------------------
function isArgUsedInFunc(fcallexpr, arg) {

    // First find the called function object
    //
    var ind = getFuncIdByName(CurModObj, fcallexpr.str);
    var fO = CurModObj.allFuncs[ind];

    var argpos = arg + 1;               // in header, 0th arg is ReturnValue   
    var gid = fO.allSteps[FuncHeaderStepId].allGridIds[argpos];

    // Go thru all steps of the function and see whether this grid is used
    //
    for (var s = FuncHeaderStepId + 1; s < fO.allSteps.length; s++) {

        var stepO = fO.allSteps[s];

        for (var g = 0; g < stepO.allGridIds.length; g++) {

            if (stepO.allGridIds[g] == gid)
                return true;
        }
    }

    return false;
}


// --------------------------------------------------------------------------
// Remove function arg from the function header, if allowed. If the arg
// is already used in a step in the function called, this does not do any
// removing -- it just returns false after displaying a warning.
// NOTE: This does NOT remove arg from the fcallexpr. 
// --------------------------------------------------------------------------
function removeFuncArg(fcallexpr, arg) {

    // find the function object called by fcallexpr
    //
    var ind = getFuncIdByName(CurModObj, fcallexpr.str);
    var fO = CurModObj.allFuncs[ind];

    assert((arg < fcallexpr.exprArr.length), "invalid arg position " +
        arg);

    // arg pos in the exprArr. We add 1 because the first expr is the return
    // expression
    //
    var argpos = arg + 1;
    //    
    // Particular argument expr in the function *call*
    //
    var argexpr = fcallexpr.exprArr[arg];
    assert(argexpr, "ERROR: argexpr cannot be null");

    if (isArgUsedInFunc(fcallexpr, arg)) {

        var msg = "This argument is used in steps of function '" +
            fcallexpr.str +
            "'. Please delete all uses of this argument (in all steps)" +
            " before deleting it.";

        alert(msg);
        return false;
    }

    // We are here because it is OK to delete the grid from the header
    // -- because the arg grid is NOT used in the steps of the function
    // So, mark the arg grid as deleted and remove it from the header.
    // The grid is permenently deleted at the end of the function with a
    // call to removeUnusedGrid().
    //
    // First obtain user approval to delete 
    //
    var conf = confirm(
        "Warning: This argument will be deleted from function " +
        "header AND all similar function calls (if any).");
    if (!conf)
        return false;
    //
    //
    var gidDel = fO.allSteps[FuncHeaderStepId].allGridIds[argpos];
    var gOdel = fO.allGrids[gidDel];
    gOdel.caption = DefDeletedName;
    gOdel.deleted = DeletedState.Deleted;
    //
    // Remove the grid from the header
    //
    fO.allSteps[FuncHeaderStepId].allGridIds.splice(argpos, 1);
    fO.argsAdded--;

    logMsg("removed from header");

    // Point the default functionCallExpr in function to this function call
    // because this updateOtherFuncCalls() depend on this
    //
    fO.funcCallExpr = fcallexpr;

    // If there are any other function calls to the same function, update
    // them -- i.e., delete the same argument we deleted from this function
    // call. 
    //
    updateOtherFuncCalls(fO, arg, true);

    // permently remove deleted grid
    //
    removeUnusedGrid(fO, gOdel, gidDel);

    return true;
}

// --------------------------------------------------------------------------
// This handles replacing a function argument. When an existing arg 
// is replaced, the newly inserted arg is checked agains function header.
// The user can change the global type of a grid in a function header,
// if the grid has a global (single) data type. However, NO *automatic*
// changes to function header will take place when we replace. 
// --------------------------------------------------------------------------
function replaceFuncArg(fcallexpr, pos, newExpr) {

    // find the function object of this function call
    //
    var ind = getFuncIdByName(CurModObj, fcallexpr.str);
    var fO = CurModObj.allFuncs[ind];
    //
    // find the gridObj corresponding to 'pos' in function header
    //
    var gidHead = fO.allSteps[FuncHeaderStepId].allGridIds[pos + 1];
    var gOhead = fO.allGrids[gidHead];

    // We ALWAYS check whether the newExpr and the grid in the function 
    // prototype matches, when we do a replace.
    //
    if (newExpr.isScalarOrGridCell()) { // newExpr is scalar


	if (newExpr.isLetName()) {

	    // if the new expression is a LetName, just check whether func arg
	    // is scalar (because let is always sclar value)
	    //
	    if (gOhead.numDims > 0) {
		alert("Existing argument in function header is not scalar");
		return false;
	    }
	    
        } else if (gOhead.numDims > 0) {

            // The function header has a non-scalar (i.e., a gridRef) arg
            //
            alert("Existing argument in function header is not scalar");
            return false;

        } else if (gOhead.dataTypes[0] != findDataTypeOfScalarExpr(newExpr)) {

            // For a scalar grid, the data types must match. If not, the user
            // can change the function header (or use a different arg)
            //
            alert(
                "Data Type mismatch. Use correct type arg OR change the " +
                " data type in function header");

            return false;
        }

    } else { // newExpr is not scalar

        if (gOhead.numDims < 1) {

            alert("Existing argument in function header is scalar");
            return false;
        }

        // TODO: Do other type checks here and return if not compatible.
        //       E.g., (1) check 



    }

    // We are here because the argument in function header and the newExr
    // match. So, we can safely replace the argument in the function call.
    // NO updates to the function header is made.
    //
    fcallexpr.exprArr.splice(pos, 1, newExpr);

}


// --------------------------------------------------------------------------
// Inserts a function argument to function headr. Called when the user 
// adds an argument in  a function *call* (AFTER adding
// the new expression in the funciton call).  This method updates the
// appropriate function header.
// Inputs:
//   fcallexpr : function call expression the user is modifying
//   arg       : argument position 
// --------------------------------------------------------------------------
function insertFuncArg(fcallexpr, arg) {

    var ind = getFuncIdByName(CurModObj, fcallexpr.str);
    var fO = CurModObj.allFuncs[ind];    // change CurFuncObj
    var sO = CurStepObj;

    assert((arg < fcallexpr.exprArr.length), "invalid arg position " +
        arg);

    // Particular argument expr in the function *call*
    //
    var argexpr = fcallexpr.exprArr[arg];
    assert(argexpr, "ERROR: argexpr cannot be null");

    // arg pos in the header. We add 1 because the first expr is the return
    // expression
    //
    var arggrid = arg + 1;

    // Point the default functionCallExpr in function to this function call
    // because updateOtherFuncCalls() depend on this 
    //
    fO.funcCallExpr = fcallexpr;

    // STEP:
    //
    // Process arg expr in the func *call* and add the corresponding grid
    // to function grids (and header step) 
    //
    if (argexpr.isScalarExpr() || argexpr.isGridCell() || argexpr.isLetName()) {

        // SCALAR args
        // if this is a number. Create a scalar grid and add push
        // to arg list
        //
        var newGId = fO.allGrids.length; // grid id at the end

        var newgO = new GridObj(newGId, "param" + arg, 1, 1,
            1, 1, false, false, false, false);

        // Add the new grid to new function
        // Note: newGId is for adding at the end (for pushing)
        //
        fO.allGrids.push(newgO);
        fO.argsAdded += 1;
        newgO.inArgNum = arg;
        newgO.isConst = true; // incoming arg const by default

        // Record this grid in function header
        //
        fO.allSteps[FuncHeaderStepId].allGridIds.splice(arggrid, 0,
            newGId);

        // If this is a grid cell AND user needs a reference (Don't know
        // how to detect this yet), create an *additional* reference
        // grid object and record it in newgO (scalar grid). 
        // We do this to enable an arg to be used as either a scalar
        // value or a reference
        //
        if (0 && argexpr.isGridCell()) { // <--- NOTE 0 

            alert("TODO: Handle this case if necessary");

            /*
	    // TODO: Currently we don't use this 'hidden' reference
	    //       grid. Allow switching to this.

	    // If we pass a grid cell as a *reference* 
	    // (not as a sclar val)
	    // it is similar to passing a whole grid reference plus
	    // a set of indices to point to the correct cell
	    // So, this case is similar to isGridRef case.
	    //
	    // NOTE: We use the same 'newGId' as the scalar because
	    //       this represents the same grid as scalar arg
	    //
	    var refgO = new cloneGridObj(newGId, argexpr.gO);
	    refgO.caption = "param" + arg;     // same name as salar


	    // This is a grid cell reference. Which will indicate to 
	    // draw index values in the arg like Src[arg1,arg2,arg3]
	    //
	    refgO.isGridCellRef = true;
	    
	    // record this reference grid obj in the scalar grid
	    //
	    newgO.refgO = refgO;

	    // Add the global data type to newgO
	    //
	    newgO.dataTypes.push(findDataTypeOfGridCellExpr(argexpr));
	    */

        } else if (argexpr.isGridCell()) {

            // scalar grid cell (passed by value)
            //
            newgO.dataTypes.push(findDataTypeOfGridCellExpr(argexpr));

        } else if (argexpr.isLetName()) {

            // alert("ARGUMENT IS LET");
            // Here do what needs to be done w.r.t. finding type of new grid

            var letType = findLetType(argexpr, CurModObj);
            newgO.dataTypes.push(letType);

        } else {

            // Add the global data type to newgO
            // NOTE: For all scalar types (even for a Number), we add type 
            //       integer. For a number, updateFuncArgType() is called
            //       just after this so it's type will be updated correctly
            //       
            newgO.dataTypes.push(DataTypes.Integer);
        }

        // Mark that scalar grid arg has globla data type
        //
        assert((newgO.dataTypes.length == 1),
            "must have only global type");
        newgO.typesInDim = -1;


    } else if (argexpr.isGridRef()) {

        // This is a grid reference. So, make a cloned grid 
        // and insert it to arg list and step
        // NOTE: When we genrate code for this new function, the caller
        //       generates something like func( _out), where 
        //       _out = out.data. So, on the caller side, just introduce
        //       an argumnet called argX. JavaScript will make
        //       argX = _out = out.data automatically. There is nothing
        //       more to do. Within the argument, we can refer to 
        //       the grid cells as argX[i][j] and it will point to 
        //       proper caller's data[]. 
        //
        var newGId = fO.allGrids.length;
        var newgO = new cloneGridObj(newGId, argexpr.gO);
        newgO.caption = "param" + arg;
        newgO.isGridRef = true; // mark this as a ref arg

        // Add the new grid to new function
        // Note: newGId is for adding at the end (for pushing)
        //
        fO.allGrids.push(newgO);
        fO.argsAdded += 1;
        newgO.inArgNum = arg;
        newgO.isConst = true; // a ref grid is constat by default

        // Record this grid in function header
        //
        fO.allSteps[FuncHeaderStepId].allGridIds.splice(arggrid, 0,
            newGId);

    } else {

        alert("TODO: Create grid for other ... arg");
    }

    // if there are any other function call expressions that call the same
    // function update them -- i.e., insert a NeedUpdate argument, or 
    // replace corresponding arg with a NeedUpdate arg
    //
    updateOtherFuncCalls(fO, arg, false);

}




// --------------------------------------------------------------------------
// Go thru each expression in every module, function, step, box, and 
// update any function call expressions
// --------------------------------------------------------------------------
function updateOtherFuncCalls(fOcur, arg, remove) {

    var pO = CurProgObj;

    var fname = fOcur.funcCallExpr.str;

    for (var m = 0; m < pO.allModules.length; m++) { // each module

        var mO = pO.allModules[m];

        for (var f = 0; f < mO.allFuncs.length; f++) { // each func

            var funcO = mO.allFuncs[f];

            for (var s = 0; s < funcO.allSteps.length; s++) { // each step

                var stepO = funcO.allSteps[s];

                for (var b = 0; b < stepO.boxExprs.length; b++) { // each box

                    var expr = stepO.boxExprs[b];
                    updateOtherFuncCallsRecursive(expr, fOcur, arg,
                        remove);
                }
            }
        }
    }
}

// --------------------------------------------------------------------------
// Update function calls upon a change to one function call (and func header).
// We need to do this recursively since we are going through all expressions
// and expressions are defined recursively. 
// Args:
//   -- expr: root expression we want to examine for a function call
//   -- fOcur: original function object
//   -- arg: agument number
//   -- replace: whether to replace
//   -- remove: whether to delete
// --------------------------------------------------------------------------
function updateOtherFuncCallsRecursive(expr, fOcur, arg, remove) {

    // if remove is false, it means just insert the arg
    //
    var insert = !remove;

    if (!expr) {

        // skip null expressions

    } else if (expr.isUserFuncCall()) {

        // If the expression is a function call
        // if the function call has the same name BUT not fOcur (since
        // we are going through all func calls we have to skip fOcur)
        //
        if ((expr.str == fOcur.funcCallExpr.str) &&
            (expr != fOcur.funcCallExpr)) {

            var origExprArr = fOcur.funcCallExpr.exprArr;
            var type = origExprArr[arg].type;

            // Handle cases based on whether the user inserted/deleted/replaced
            // an arg in the original function call 
            //
            if (insert && (expr.exprArr.length < origExprArr.length)) {

                // if we the user just inserted an arg, we just have to insert
                // an arg to every other function call. This arg is marked
                // NeedUpdate in every other function call -- meaning, user has
                // to replace all those NeedUpdate expressions.
                //
                var newExpr = new ExprObj(true, type, DefNeedUpdateName);
                newExpr.deleted = DeletedState.NeedUpdate;
                expr.exprArr.splice(arg, 0, newExpr);
                logMsg("inserted func call arg to another func call");

            } else if (remove &&
                (expr.exprArr.length >= origExprArr.length)) {

                // if the user deleted an expression, just delete the same
                // expression from other function calls.
                //
                expr.exprArr.splice(arg, 1);
                logMsg("removed func call arg from another func call");


            } else {
                logMsg("Warn: other func call found. No changes made");
            }

        }

    } else if (expr.exprArr) {

        // Any other expression -- we need to traverse child expressions since
        // they may contain function calls
        //
        for (var e = 0; e < expr.exprArr.length; e++) {
            updateOtherFuncCallsRecursive(expr.exprArr[e], fOcur, arg,
                remove);
        }
    }

}


// --------------------------------------------------------------------------
// Changes to a function when the user selects a function name on the 
// drop down box in program structure heading. 
// --------------------------------------------------------------------------
function changeFunc(ind) {

    var fO = CurFuncObj;
    var sO = CurStepObj;

    // STEP : first see whether the user has come to a proper checkpoint
    //
    // if we have already started configuration but not yet completed
    // it, can't change steps.
    //
    if ((sO.stageInStep >= StageInStep.ConfigStarted) && !fO.isTemplateScope &&
        (sO.stageInStep <= StageInStep.ConfigDone)) {

        alert("func: Please complete configuration OR Cancel Step first");

        // Cancel user selected function
        //
        drawProgStructHead();

        return;
    }

    // If we are going to a next step while we are just going through 
    // new output grid selection menu, abandon current step
    //
    if (sO.stageInStep < StageInStep.ConfigStarted) {
        CurFuncObj.allSteps.pop();
        //alert("NOTE: Abondoned current step");
    }


    // STEP : Actually change the function 
    // Note : we have created the new function object in addNewFuncExpr()
    //
    CurFuncObj = CurModObj.allFuncs[ind];    // change CurFuncObj
    CurModObj.curFuncNum = ind;              // change focused function
    var fO = CurFuncObj;                     // shortcut reference


    // Always change to the last step of function. If the function has
    // only the header step (for a new func), this will go to the header
    // TODO: For each function remember which step the user was and change
    //       to that.
    //
    assert(fO.allSteps.length, "Func must have at least header step");
    changeStep(fO.allSteps.length - 1);
}



// --------------------------------------------------------------------------
// returns the html string with the step select dialog box
//----------------------------------------------------------------------------
function getStepSelectStr() {

    var fO = CurFuncObj;

    var selected_index = fO.curStepNum;   // current step

    var str = "<select class='stepsel' " +
        "onchange='changeStep(this.selectedIndex)'>";

    for (var i = 0; i < fO.allSteps.length; i++) {

        // step # or Header
        //
        var name = (i) ? "Step" + i : "Header";

        str += "<option value='" + name + "'";

        if (i == selected_index) str += "selected";
        str += "> " + name + "</option>";
    }

    str += "</select>";
    //
    return str;
}

//----------------------------------------------------------------------------
// Html string for 'arrows' at the top of page (prog struct head) for 
// easy changing of steps 
//----------------------------------------------------------------------------
function getStepArrowStr() {

    var fO = CurFuncObj;
    var oncL = "",
        oncR = "";

    // if a previous step exists, insert an onclic function. Otherwise,
    // gray it out
    //
    if (fO.curStepNum > 0)
        oncL = " onclick='changeStep(" + (fO.curStepNum - 1) + ")'";
    else
        oncL = " class=grayimage ";

    // if a next step exists, insert an onclic function. Otherwise,
    // gray it out
    //
    if (fO.curStepNum < fO.allSteps.length - 1)
        oncR = " onclick='changeStep(" + (fO.curStepNum + 1) + ")'";
    else
        oncR = " class=grayimage ";


    var str = "<img src='images/larrow.jpg' width=20px height=16px " +
        " title='previous step' " + oncL + ">" +
        "<img src='images/rarrow.jpg' width=20px height=16px " +
        " title='next step' " + oncR + " >";



    return str;
}

//----------------------------------------------------------------------------
// insert a new step (after 'curstepnum')
//----------------------------------------------------------------------------
function newNextStep(curstepnum) {

    var fO = CurFuncObj;

    assert((fO.allSteps.length > curstepnum), "invalid cur step");

    // Update any changes to the current step obj (e.g., comments) before
    // creating a new step
    //
    updateStepObj(CurStepObj);
    
    // Remove any highlighted expression and grid highlight from the 
    // current step because we are going to change this step. Not doing
    // so will keep highlights in grid (even in new steps)
    //
    removeExprAndGridHighlight();

    // Create new step, make it the current step, and insert it the 
    // next slot after the current step
    //
    CurStepObj = new StepObj();         // create a new step
    //
    fO.allSteps.splice(curstepnum + 1, 0, CurStepObj);
    fO.curStepNum = curstepnum + 1;
    //
    CurProgObj.showData = ShowDataState.None;

    drawStep(CurStepObj);
    /*
    initHtmlGridIds();                 // clear html elements
    drawProgStructHead();
    doNextStage();
    */
}

//----------------------------------------------------------------------------
// initialize all HTML grids
//----------------------------------------------------------------------------
function initHtmlGridIds() {

    // STEP: Reset all HTML elements and start a new step
    //
    document.getElementById('outMenu').innerHTML = "";
    document.getElementById('src1Menu').innerHTML = "";
    document.getElementById('src2Menu').innerHTML = "";
    document.getElementById('src3Menu').innerHTML = "";
    document.getElementById('src4Menu').innerHTML = "";
    document.getElementById('src5Menu').innerHTML = "";
    document.getElementById('src6Menu').innerHTML = "";
    document.getElementById('src7Menu').innerHTML = "";
    document.getElementById('src8Menu').innerHTML = "";
    document.getElementById('src9Menu').innerHTML = "";
    document.getElementById('src10Menu').innerHTML = "";

    //DC6: To support more source grids per step.
    //TODO: Can this be dynamic?
    document.getElementById('src11Menu').innerHTML = "";
    document.getElementById('src12Menu').innerHTML = "";
    document.getElementById('src13Menu').innerHTML = "";
    document.getElementById('src14Menu').innerHTML = "";
    document.getElementById('src15Menu').innerHTML = "";
    document.getElementById('src16Menu').innerHTML = "";
    document.getElementById('src17Menu').innerHTML = "";

    document.getElementById('src18Menu').innerHTML = "";
    document.getElementById('src19Menu').innerHTML = "";
    document.getElementById('src20Menu').innerHTML = "";
    document.getElementById('src21Menu').innerHTML = "";
    document.getElementById('src22Menu').innerHTML = "";
    document.getElementById('src23Menu').innerHTML = "";
    document.getElementById('src24Menu').innerHTML = "";
    document.getElementById('src25Menu').innerHTML = "";
    document.getElementById('src26Menu').innerHTML = "";
    document.getElementById('src27Menu').innerHTML = "";
    document.getElementById('src28Menu').innerHTML = "";
    document.getElementById('src29Menu').innerHTML = "";
    document.getElementById('src30Menu').innerHTML = "";
    document.getElementById('src31Menu').innerHTML = "";
    document.getElementById('src32Menu').innerHTML = "";
    document.getElementById('src33Menu').innerHTML = "";
    document.getElementById('src34Menu').innerHTML = "";




    document.getElementById('GridConfMenu').innerHTML = "";
    document.getElementById('codeWindow').innerHTML = "";
    
    document.getElementById('operatorWindow').innerHTML = "";
    document.getElementById('codeBotSep').innerHTML = "";
    document.getElementById('tipWindow').innerHTML = "";
    document.getElementById('stepCaption').innerHTML = "";

}

//----------------------------------------------------------------------------
// delete the current step. 
//----------------------------------------------------------------------------
function deleteStep() {

    var fO = CurFuncObj;
    var sO = fO.allSteps[fO.allSteps.length - 1];

    if (fO.curStepNum == 0) {
        alert("Cannot remove function header step");
        return;
    }

    // if we have already started configuring a new grid but not yet completed
    // it, we have to pop the newly added grid -- this is similar to pressing
    // back button while in config -- i.e., see configBack()
    //
    if ((sO.stageInStep >= StageInStep.ConfigStarted) && !fO.isTemplateScope &&
        (sO.stageInStep <= StageInStep.ConfigDone)) {

	popNewGrid();
    }

    // Remove any highlighted expression and grid highlight from the 
    // current step because we are going to delete this step. Not doing
    // so will keep highlights in grid (even in other steps)
    //
    removeExprAndGridHighlight();

    //fO.allSteps.pop();     // remove the CurStepObj from allSteps[]

    // Remove the current step
    //
    fO.allSteps.splice(fO.curStepNum, 1);

    fO.curStepNum--;       // update step number

    assert((fO.curStepNum >= 0) && "invalid previous step");

    // Go to previous step after deleting. There is always a previous step,
    // because of the function header step at postion 0
    //
    changeStep(fO.curStepNum);

}




// --------------------------------------------------------------------------
// Note: incoming 'ind' starts from 0
// NOTE: We can change steps only if the grid selection is complete.
//       This restriction arises because we don't have enough state within
//       a StepObject to capture whether we are currently configuring, or
//       selecting dest/source grids etc. If we add state, we can change 
//       steps at any point within a step
// NOTE: drawStep() set CurStepObj global var
// NOTE: The caller must have checked whether the previous step is complete
//       before changing to new step
// --------------------------------------------------------------------------
function changeStep(ind) {

    var sO = CurStepObj;
    var fO = CurFuncObj;

    // If we are going to a next step while we are just going through 
    // new output grid selection menu, abandon current step
    //
    if (sO.stageInStep < StageInStep.ConfigStarted) {

        CurFuncObj.curStepNum = ind;
        drawStep(CurFuncObj.allSteps[ind]);
        return;
    }

    // STEP : Actually change the step
    //
    if (ind < CurFuncObj.allSteps.length) {

	// Remove any highlighted expression and grid highlight from the 
	// current step because we are going to change this step. Not doing
	// so will keep highlights in grid (even in other steps)
	//
	removeExprAndGridHighlight();
	//
        CurFuncObj.curStepNum = ind;
        drawStep(CurFuncObj.allSteps[ind]);

    } else {
        alert("Step does not exist");
    }

}



//----------------------------------------------------------------------------
// Before going to a next step, update the CurStepObj w/ formula, etc.
//----------------------------------------------------------------------------
function updateStepObj(sO) {

    // Put any updates necessary here


}


//----------------------------------------------------------------------------
// Duplicate a step. Duplication of a step is useful if we are doing the
// a different operation with all the grids of a previous step
// --------------------------------------------------------------------------
function duplicateStep() {

    var fO = CurFuncObj;
    var sO = CurStepObj;

    // User must complete grid selection before duplicating
    //
    if ((sO.stageInStep < StageInStep.GridSelectDone) &&
        !fO.isTemplateScope) {

        alert("Please complete grid selection before duplicating");
        return;
    }

    updateStepObj(CurStepObj); // record code window

    // Remove any highlighted expression and grid highlight from the 
    // current step because we are going to change this step. Not doing
    // so will keep highlights in grid (even in other steps)
    //
    removeExprAndGridHighlight();

    // clone the current step. Note that when we clone, we do a deep 
    // cloning so that the *grid objects* held within the CurStepObj are
    // also cloned
    //
    var newStepObj = new cloneStepObj(CurStepObj); // clone current step

    CurStepObj = newStepObj;                       // assign to CurStepObj
    CurFuncObj.allSteps.push(CurStepObj);          // and push it
    CurFuncObj.curStepNum++;                       // update cur step #
    //
    CurProgObj.showData = ShowDataState.None;
    //
    drawProgStructHead();

    // TODO: draw structHead
    // TODO: clone indices because indices belong to a step
    //startCurStep();
}



//----------------------------------------------------------------------------
// Main method to do the next 'stage' within a current programming step
// Note: The current state is determined based on the size of the 
//       grids in CurStepObj.allGridIds
//
//----------------------------------------------------------------------------
function doNextStage() {

    clearTip();

    var sO = CurStepObj;

    // The stage in current step
    //
    var stage = CurStepObj.stageInStep;

    // Templates scope supports only the very first step of grid creation
    //
    if (CurFuncObj.isTemplateScope) {
        stage = StageInStep.New;
    }

    CurHtmlGridId = AllHtmlGridIds[stage];

    if (stage <= StageInStep.New) { // we have not created any output grids

	drawOutMenuTable();        // draw the menu to pick out grid

    } else if ((stage == StageInStep.ConfigDone) ||
        (stage == StageInStep.SrcSelectStarted)) {

        // we have added the output/src grid. Now, add src grid (more src grids)
        //
        var gridId = sO.allGridIds.length;
        CurHtmlGridId = AllHtmlGridIds[gridId];
        //
        sO.stageInStep = StageInStep.SrcSelectStarted;
        //
        drawSrcMenuTable(); // draw the menu to pick src grid

        /*	  
	// Since only an existing grid can be selected as a source, we 
	// call the following function directly
	//
	selectExistingGrid();
	*/


    } else if (stage <= StageInStep.GridSelectDone) {

        // We have added all out/src grids (and add src menu is gone)

        // The grid selection is complete. Update program structure and
        // draw the code window with buttons
        //
        drawProgStructHead(); // update program structre heading

        // Init and draw code window. 
        // NOTE: initialization happens only once -- initCodeOfStep() checks
        //       whether we have already initialized code of step
        //
        initCodeOfStep(CurStepObj); // init code of step
        drawCodeWindow(CurStepObj); // draw code window with buttons   

    }

}


//----------------------------------------------------------------------------
// Add initial expressions to code window based on the output grid
// in the step object
//----------------------------------------------------------------------------
function initCodeOfStep(sO) {

    var fO = CurFuncObj;

    // if we have alredy intialized code of the setp, nothing to do.
    //
    if (sO.boxExprs.length)
        return;

    var gId = sO.allGridIds[0];
    var gO = fO.allGrids[gId];    // gO = output grid object

    var ind = "";
    var numind = 0;

    // 3 bins are used for range/mask/formula. Push null expressions for those
    //
    sO.boxExprs.push(null);   // index (grid obj index editing)
    sO.boxExprs.push(null);   // range
    sO.boxExprs.push(null);   // mask
    sO.boxExprs.push(null);   // formula

    sO.boxAttribs.push(new BoxAttribs(0, CodeBoxId.Index)); // index editing 
    sO.boxAttribs.push(new BoxAttribs(0, CodeBoxId.Range)); // range
    sO.boxAttribs.push(new BoxAttribs(1, CodeBoxId.Mask));  // mask
    sO.boxAttribs.push(new BoxAttribs(2, CodeBoxId.Formula)); 


    // Foreach root expression. We add this for ALL grids -- even for 
    // scalars. If the index expressions are not added, the keyword
    // "foreach" is not displayed.
    //
    sO.boxExprs[CodeBoxId.Range] =
        new ExprObj(false, ExprType.Foreach, "foreach");

    if (gO.numDims > 0) { // if output gO is not a simple scalar



        // Create the main 'foreach' root expression 
        //	
        // Create shortcut name 
        //
        var rangeExpr = sO.boxExprs[CodeBoxId.Range];

        for (var d = 0; d < gO.numDims; d++) {

            if (!gO.dimHasTitles[d]) {

                // create a new sub expression (range) for each range
                // -- i.e., new range for dim with default output grid 
                //
                var gO = fO.allGrids[sO.allGridIds[0]];
                var rangeChild = newRangeExpr(sO, d, gO);
                //
                // add it to the rangeExpr of the step
                //
                rangeExpr.addSubExpr(rangeChild);
            }
        }
    }

    // STEP:
    // (1) Create default 'if' statement. A standalone if is displayed only if
    // the mask box has focs
    // (2) Create the default expression statement for a formula
    //
    sO.boxExprs[CodeBoxId.Mask] = new ExprObj(false, ExprType.If, "if");
    sO.boxExprs[CodeBoxId.Formula] = new ExprObj(false, ExprType.ExprStmt,
        "");

    setFocusToDefaultBox();

}



//----------------------------------------------------------------------------
// Set focus to the default formula box in the formula window
//----------------------------------------------------------------------------
function setFocusToDefaultBox() {

    var sO = CurStepObj;

    // set space active by default
    //
    sO.focusBoxId = CodeBoxId.Formula;
    sO.focusBoxExpr = sO.boxExprs[CodeBoxId.Formula];
    setDefaultActiveSpace(sO);
}




//----------------------------------------------------------------------------
// NOTE: Can draw only fully configured grids. If the user is in the middle
//       of configuring a new grid, it must be finished
//       This restriction can be removed if we capture the state in the
//       configuration window in the step object.
//----------------------------------------------------------------------------
function drawStep(sO) {

    CurStepObj = sO;
    initHtmlGridIds();

    // Draw header line at the top of the page for this step
    //
    drawProgStructHead();


    // If we are changing to a step that we have not begun, do a fresh start
    //
    if (sO.stageInStep < StageInStep.ConfigStarted) {

        doNextStage();
        return;
    }

    var fO = CurFuncObj;

    // for all grid objects in sO (stepObject)
    //
    for (var id = 0; id < sO.allGridIds.length; id++) {

        var htmlId = AllHtmlGridIds[id];
        var gO = fO.allGrids[sO.allGridIds[id]];
        var iO = sO.allIndObjs[id];

        CurHtmlGridId = htmlId;

        drawGrid(gO, iO, htmlId, false);
    }


    // Some steps have captions to describe the step (e.g., header) and 
    // get any options
    //
    if (sO.isHeader) {
        drawStepCaption();
    }


    // Templates do not have code window. Also, if the step is a function
    // Header, no code window.
    //
    if (fO.isTemplateScope || sO.isHeader)
        return;

    // if there is no focusBoxExpr (but there is a focusBoxId), 
    // set it and the active space
    // This can happen after loading a new program
    //
    if ((sO.focusBoxExpr == null) && sO.focusBoxId) {

        changeCWInputBox(sO.focusBoxId);

    } else {

        drawCodeWindow(sO);
    }

    // call the next-step logic to draw a next step if necessary
    // (e.g., to show a config menu, if grid selection is not complete)
    //
    doNextStage();

}


//API:
// If checkbox checked, then set the .createAPI flag of current function.
function markAddToAPI(checked) {

    CurFuncObj.createAPI = checked;

}


// OCL: If checkbox checked, then set the .OCLstep flag of current step to 1.
function markOCLstep(checked) {

    CurStepObj.potOCLstep = checked;

}


//----------------------------------------------------------------------------
//
//----------------------------------------------------------------------------
function drawStepCaption() {

    var fO = CurFuncObj;

    // Draw a caption for a regular function header
    //
    if (fO.isRegularFunc()) {

        var prop = "";

	//API: Added second checkbox, for adding to API.
        var str = "<div class='stepCap' id='stepCaption'>" +
            "Function Header for " + fO.funcCallExpr.str + "()" + 
	    "<BR>" + 
	    "<input type='checkbox' id='makeExtern' " + prop + ">" +
            "<span class='stepCapOpt'>" +
            "Allow this function to be called from other modules" +
            "<BR>" +
	    "<input type='checkbox' onclick='markAddToAPI(this.checked)' >" +
	    "Add function to external API" +
	    "</span>" +
	    "</div>";

        var cap = document.getElementById('stepCaption');
        cap.innerHTML = str;
    }


}

//----------------------------------------------------------------------------
// Redraw all the grids and code window of the current step
//----------------------------------------------------------------------------
function redrawCurStep() {


    var fO = CurFuncObj;
    var sO = CurStepObj;

    // for all grid objects in sO (stepObject)
    //
    for (var id = 0; id < sO.allGridIds.length; id++) {

        var htmlId = AllHtmlGridIds[id];
        var gO = fO.allGrids[sO.allGridIds[id]];
        var iO = sO.allIndObjs[id];

        CurHtmlGridId = htmlId;

        drawGrid(gO, iO, htmlId, false);
    }


    // Finally redraw code window
    //
    drawCodeWindow(sO);

}

//----------------------------------------------------------------------------
// Allocate data arrays for the entire program OR clear them if they have
// already been allocated
//----------------------------------------------------------------------------
function allocOrClearDataArrays4Prog() {

    for (var m = 0; m < CurProgObj.allModules.length; m++) {

        var mO = CurProgObj.allModules[m];

        for (var f = 0; f < mO.allFuncs.length; f++) {

            var fO = mO.allFuncs[f];
            allocDataArrays4CurFunc(fO);
        }
    }
}



//----------------------------------------------------------------------------
// Allocate data arrays for all steps in the function
//----------------------------------------------------------------------------
function allocDataArrays4CurFunc(fO) {

    // Go thru each step and initialize 
    //
    for (var s = 0; s < fO.allSteps.length; s++) {

        var sO = fO.allSteps[s];

        //alert("allocating data for step " + s + "grids:" 
        //+ sO.allGridIds.length);

        allocDataArraysOfStep(fO, sO);
    }
}

//----------------------------------------------------------------------------
// Allocate data arrays for all grids in the step, if and only if:
// (1) Data array has not been allocated before
// (2) The grid is not an input arg -- input grids are passed by reference so
//     the caller must have allocated grids 
//----------------------------------------------------------------------------
function allocDataArraysOfStep(fO, sO) {

    // create/init arrays for all grids in a step
    //
    for (var id = 0; id < sO.allGridIds.length; id++) {

        var gId = sO.allGridIds[id];
        var gO = fO.allGrids[gId];

        //alert("alloc for grid: " + gO.caption);

        // create arrays only if we haven't done so before for this grid AND
        // this grid is not an input arg OR scalar grid
        // NOTE: For a scalar grid, we always need to create an array of 1
        //       element to show the data value. Even when a scalar grid is
        //       an incoming arg, we have to explicitly write its data array
        //       in order to show the value of the incoming arg.  
        //
        // NOTE: If a grid refers to a global grid, we must not create
        //       data arrays for them (it's a grid reference)
        //
        if (((gO.inArgNum < 0) && (gO.globalRefId < 0)) ||
            (!gO.numDims && !gO.isGridRef)) {

	    if (!gO.data) {             // if we haven't allocated before
		createArrays(gO);       // create arrays
		// alert("Data alloced for " + gO.caption);
	    }
	    else if (gO.numDims > 0) {
		clearData(gO.numDims-1, gO.data, undefined);     // clear data
		logMsg("clearing: " + gO.caption);
	    } else {
		logMsg("No data alloc/clear for : " + gO.caption);
	    }
	} else {
	    logMsg("Skipping data alloc/clear entirely for : " + gO.caption);
	}
    }
}

//----------------------------------------------------------------------------
// Create initial arrays
// If isDynCall is true, this is called by *auto-generated* java script code
//----------------------------------------------------------------------------
function createArrays(gO, isDynCall) {

    logMsg("creating arrays for grid: " + gO.caption + " dyn:" + isDynCall);

    // logMsg("act size of dim 0:" + gO.dimActSize[0]);

    // if gO is a scalar, just create an array of 1 element
    //
    if (!gO.numDims) {
        gO.data = new Array(1);
        return;
    }

    // If this function is not called by auto-generated code AND
    // if this gO has dynamic sizes, don't do anything. This is because
    // dynamic arrays must be creaed at each step (while auto-generated
    // code is executing), using dynamic sizes
    //
    if (!isDynCall && hasDynSize(gO)) {
	logMsg("Skipping createArrays @ start becase gO has dynamic sizes");
	return;
    }

    // Create a new array of dimension sizes and make sure that
    // the number of *columns* is at the 0th index. This is because
    // gO.data array MUST be in row-major (JavaScript) index order
    //
    // Note: gO.dimActSize.length can be greater than gO.numDims, when 
    //       a grid has only 1 dimension. gO.numDims contain the correct 
    //       value.
    //
    var modarr = gO.dimActSize.slice(0);

    logMsg(" -- # of dims:" + gO.dimActSize.length);

    if (gO.numDims >= 0) 
	modarr[0] = gO.dimActSize[ColDimId];
    if (gO.numDims >= 1) 
	modarr[1] = gO.dimActSize[RowDimId];

    // First check that each value is an integer because we get user input
    //
    for (var i = 0; i < gO.numDims; i++) {
        assert((typeof modarr[i] === "number") && (modarr[i] % 1 == 0),
            "Non integer size " + modarr[i] + " in dim " + i);
    }

    // Do allocation
    //
    gO.data = createNDimArr(gO.numDims - 1, modarr);

    // fillDummyData(gO.numDims-1, gO.data, 1);

    /* Test for a 2D case: 
    alert("In: numrows:" + modarr[1] + " numcols:" + modarr[0]);
    alert("GR: numrows:" + gO.data.length + " numcols:" + gO.data[0].length);
    */
}


//----------------------------------------------------------------------------
// Create a
// NOTE: dim can be 0, 1, 2, 3, .... etc. Note that it starts with 0. 
//       The initial call to this method must be with dim = num_dims-1 -- i.e.,
//       pointing to the very last dim
//       dim_szs[] is an array containing the size of each dim
// NOTE: This method allocates the array in row-major (JavaScript) order. 
//       Therefore, dim_szs[0] MUST contain number of *columns* -- dim_szs[1]
//       contain number of rows, dim_sz[2] contains number of dim3 size, etc.
//----------------------------------------------------------------------------
function createNDimArr(dim, dim_szs) {

    var dimsz = dim_szs[dim];           // number of elements in this dim

    var parr = new Array(dimsz);        // create a new array for each dim

    if (dim == 0) {                     // needed only for init

        // Nothing to do here. We just return the newly created parr below
        //
    } else {

        // if not the 0th dim, create lower-dim arrays and assign
        //
        for (var i = 0; i < dimsz; i++) {
            parr[i] = createNDimArr(dim - 1, dim_szs);
        }
    }

    return parr;
}


//----------------------------------------------------------------------------
// Fills a data array with sequential dummy data -- NOT USED currently
//----------------------------------------------------------------------------
function fillDummyData(dim, arr, ival) {

    var val = ival;

    if (dim == 0) { // last dim 
        for (var i = 0; i < arr.length; i++)
            arr[i] = val++; // clear with vals
    } else { // non-last dim
        for (var i = 0; i < arr.length; i++) {
            val = fillDummyData(dim - 1, arr[i], val);
        }
    }

    return val;
}


//----------------------------------------------------------------------------
// Clears all entries of a data array with a given value
//----------------------------------------------------------------------------
function clearData(dim, arr, val) {


    assert((dim >= 0), "dim can't be negative");

    if (dim == 0) {
        for (var i = 0; i < arr.length; i++)
            arr[i] = val; // clear with vals
    } else if (arr) {
        for (var i = 0; i < arr.length; i++) {
            clearData(dim - 1, arr[i], val);
        }
    }
}

//----------------------------------------------------------------------------
// Finds the maximum value in a data array
//----------------------------------------------------------------------------
function getMaxDataVal(dim, arr, mval) {

    if (dim == 0) {
        for (var i = 0; arr && (i < arr.length); i++)
            if (Number.isFinite(arr[i]) && arr[i] > mval)
                mval = arr[i]; // set max val
    } else {
        for (var i = 0; arr && (i < arr.length); i++) {
            mval = getMaxDataVal(dim - 1, arr[i], mval);
        }
    }

    return (mval);
}


//----------------------------------------------------------------------------
// Returns the data value of grid cell
//----------------------------------------------------------------------------
function getDataValOfCell(gO, r, c) {

    // get data value. 
    // NOTE: If there is only rows present (only 1 dimension)
    //       data is stored as *columns* in the C/JS style data
    //       arrays. So, we use 'r' to index into 'columns' of 
    //       data arrays.
    //
    var val;

    if (gO.numDims > 1) {
        val = getDataVal(gO.numDims - 1, gO.data,
            r, c, gO.dimSelectTabs, gO.dimShowStart);
    } else if (gO.numDims > 0) {
        assert((gO.numDims <= 1), "must have only rows/scalar");
        val = getDataVal(gO.numDims - 1, gO.data,
            0, r, gO.dimSelectTabs, gO.dimShowStart);
    } else {
        return gO.data[0];
    }

    return val;
}


//----------------------------------------------------------------------------
// Returns the data value of a give dataArr
//----------------------------------------------------------------------------
function getDataVal(dim, dataArr, row, col, selDims, dimStarts) {

    // Note: Data arrays are stored in the JavaScript memory order (row-major)
    //       Therefore, col is ALWAYS the fastest changing index AND is at
    //       index 0 of the data array -- regardless of RowDimId=0 or 
    //       ColDimId=0;
    //
    if (dim == 0) return dataArr[col];                // if col
    else if (dim == 1) return dataArr[row][col];      // if row, col
    else {                                            // more than 2 dims
	var seldim = selDims[dim] + dimStarts[dim];
	return getDataVal(dim-1, dataArr[seldim], row, col, selDims, dimStarts);
    }
}



//----------------------------------------------------------------------------
// Draw a menu to pick the output grid.
//----------------------------------------------------------------------------
function drawOutMenuTable() {

    var menuId = CurHtmlGridId;
    var menu1 = document.getElementById(menuId);

    var str = "<table class='menuTable' id='" + menuId + "'><tr></tr>";
    var onclick = "";

    // First Row: New Grid and Existing Grid Options
    //
    onclick = "onClick=\"drawNewGridMenu('" + menuId + "')\" ";
    str += "<tr><td " + onclick + "><img src='images/new_grid.jpg' " +
        " width=65px height=75px><BR>New Grid</td>";
    //
    onclick = "onclick='selectExistingGrid()'";
    str += "<td> <img src='images/existing_grid.jpg' " +
        onclick + " width=60px height=75px> <BR>Existing Grid</td>";


    str += "</tr>";

    // Draw a title cell under the menu at the bottom
    //
    str += "<td class='caption' colspan=2 border='0'> " +
        " <BR>Select Output Grid for Step</td><tr>";

    menu1.innerHTML = str + "</table>";
    menu1.className = 'menuTable';
}

//----------------------------------------------------------------------------
// Draw a template (grid type choices) to create a New Grid 
//----------------------------------------------------------------------------
function drawNewGridMenu() {

    var menuId = CurHtmlGridId;
    var menu1 = document.getElementById(menuId);

    var str = "<table class='newGridMenu' id='" + menuId + "'><tr></tr>";

    var onclick = "";                   // tmp var to hold onClick() string

    // No keyboard input handling during a grid config/selection
    //
    CurStepObj.keyboardEnabled = false;
    clearTip();

    // ---------- Draw scalar, 1D/2D/3D arrays
    //
    onclick = "onClick='beginNewGridConfig(false, false, false, false)'";
    str += "<tr> <td " + onclick + "><img src='images/scalar.jpg' " +
        "width=30px height=30px> <BR><BR>Scalar</td>";
    //
    onclick = "onClick='beginNewGridConfig(false, false, false, true)'";
    str += "<td " + onclick + "> <img src='images/1D_array.jpg' " +
        "width=30px height=150px> <BR>Array</td>";
    //
    onclick = "onClick='beginNewGridConfig(false, false, true, true)'";
    str += "<td " + onclick + "> <img src='images/2D_array.jpg' " +
        "width=120px height=150px> <BR>2D Array</td>";
    //
    onclick =
        "onClick='beginNewGridConfig(false, false, true, true, true)'";
    str += "<td " + onclick + "> <img src='images/3D_array.jpg' " +
        "width=140px height=150px> <BR>3D Array</td>";


    //
    //    str += "<td> <img src='images/3D_array.jpg' " 
    //	+ " width=140px height=150px> <BR>3D Array</td>  </tr>";

    // --------- Draw tables with titles in columns/rows/both
    //
    onclick = "onClick='beginNewGridConfig(true, false, true, true)'";
    str += "<tr><td colspan=2 " + onclick +
        "><img src='images/table_col.jpg' " +
        "width=120px height=150px> <BR>Grid with<BR>Column Titles</td>";
    //
    onclick = "onClick='beginNewGridConfig(false, true, true, true)'";
    str += "<td " + onclick + "> <img src='images/table_row.jpg' " +
        "width=140px height=150px> <BR>Grid with<BR>Row Titles</td>";
    //
    onclick = "onClick='beginNewGridConfig(true, true, true, true)'";
    str += "<td " + onclick + "> <img src='images/table_row_col.jpg' " +
        "width=140px height=150px>" +
        "<BR>Grid with<BR>Row/Column Titles</td>  </tr>";



    // Draw a title cell under the menu at the bottom
    //
    str += "<tr>" + "<td class='caption'>" +
        "<input type='button' value='Back' onclick='drawOutMenuTable()'>" +
        "</td>" + "<td class='caption' colspan=3 border='0'> " +
        "<BR>Select Type of New Output Grid</td></tr>";

    menu1.innerHTML = str + "</table>";
    menu1.className = 'newGridMenu'; // change class of htmlId


    drawProgStructHead();

}


//----------------------------------------------------------------------------
// Configure a new grid with sizes, type location,etc. 
//----------------------------------------------------------------------------
function beginNewGridConfig(hasColTitles, hasRowTitles, 
			    multiCol, multiRow, is3D) {

    var actCols = (multiCol) ? DefShowCols : 1;
    var actRows = (multiRow) ? DefShowRows : 1;
    var showCols = actCols;
    var showRows = actRows;


    // STEP 1: Create a new Grid Object ......................................
    //
    var newGId = CurFuncObj.allGrids.length;
    NewGridObj =
        new GridObj(newGId, DefOutGridName, actCols, actRows,
            showCols, showRows, hasColTitles, hasRowTitles,
            multiCol, multiRow);


    CurStepObj.stageInStep = StageInStep.ConfigStarted;

    // Mark that we created a new grid with this step
    //
    CurStepObj.isOutGridNew = true;

    // We add permenently add the newly created grid. This is done because
    // other functions may want to access this grid -- e.g., when a 
    // title/tab name is changed. However, if we cancel configuring this grid
    // by going back or deleting the step, we have to pop these additions.
    // See configBack()
    //
    CurFuncObj.allGrids.push(NewGridObj);    // add grid Obj to function
    CurStepObj.addGridId(newGId);            // add output grid to current step


    var ngO = NewGridObj;              // create a shortcut

    // STEP 2a: Create dimensions
    //
    for (var d = 0; d < ngO.numDims; d++) {
        ngO.dimTitles[d] = new Array();
        ngO.dimComments[d] = new Array();
        ngO.dimSelectTabs[d] = 0; // selected tab0 by default
    }
    //
    // Map two shortcut names (colTitles, rowTitles) to dimTitles array
    //
    var rowTitles = ngO.dimTitles[RowDimId];
    var colTitles = ngO.dimTitles[ColDimId];

    // STEP 2b: initialize Col/Row titles and Col/Row Types ..................
    //
    for (var c = 0;
        (c < showCols) && multiCol; c++) {
        colTitles.push(DefTitleRoots[ColDimId] + c);
        //colIndExprs.push(null);    // empty indices
    }
    //
    for (var r = 0;
        (r < showRows) && multiRow; r++) {
        rowTitles.push(DefTitleRoots[RowDimId] + r);
        //rowIndExprs.push(null);    // empty indices
    }

    for (var i = 0;
        (i < showRows) || (i < showCols); i++)
        ngO.dataTypes.push(0);


    // STEP 3: draw the grid .................................................
    //         using values we just stored in ngO
    //
    drawGrid(ngO, null, CurHtmlGridId, true);
    drawGridConfigMenu(ngO);

    // if this is a 3D grid, add one additional dimension
    //
    if (is3D) addDim();
}


//----------------------------------------------------------------------------
// Draw the menu to select a src grid
//----------------------------------------------------------------------------
function drawSrcMenuTable() {

    var menuId = CurHtmlGridId;
    var menu1 = document.getElementById(menuId);

    var str = "<table class='menuTable' id='" + menuId + "'><tr></tr>";
    var onclick = "";

    // First Row: Existing Grid and Keyboard Input Options
    //
    //
    onclick = "onclick='selectExistingGrid()'";
    str += "<tr><td> <img src='images/existing_grid.jpg' " + onclick +
        " width=60px height=75px> <BR>Existing Grid</td>";

    str +=
        "<td class='clickText' onclick='removeMenu()'> Done </td></tr>";
    //DC4: Changed "Cancel" to "Done".

    // Draw a title cell under the menu at the bottom
    //
    str += "<tr><td class='caption' colspan=2 border='0'>" +
        "<BR>Select Source Grid</td></tr>";

    menu1.innerHTML = str + "</table>";
    menu1.className = 'menuTable';

}

//----------------------------------------------------------------------------
// Function called when src grid selection is done
//----------------------------------------------------------------------------
function removeMenu() {

    var menuId = CurHtmlGridId;
    var menu1 = document.getElementById(menuId);
    menu1.innerHTML = "<table></table>";

    // Since we have done adding sources (at least for now), mark as
    // grid selection complete
    //
    CurStepObj.stageInStep = StageInStep.GridSelectDone;


    // updateStepObj(CurStepObj);         // record any updated formula etc.

    doNextStage();               // to to the sequencer
}

//----------------------------------------------------------------------------
// Creates dropdown list of existing grids, with each selected grid shown
//
// Note: Displays at 'CurHtmlGridId'
//----------------------------------------------------------------------------
function selectExistingGrid() {

    var fO = CurFuncObj;
    var sO = CurStepObj;

    // No keyboard input handling during a grid selection
    //
    CurStepObj.keyboardEnabled = false;

    // STEP: ..............................................................
    // Create a drop-down menu and a pre-view pane to select
    // Note: We use a table with two rows to create a preview pane
    // TODO: change the preview pane when the user goes through the
    //       drop-down menu. Currently, the user has to make a selection
    // 
    var str = "<table class='selectWin'>" + " <tr><td><div id='" +
        PreviewHtmlGridId + "'> Disp </div></td></tr>";

    // Create the list of all previous grids for the drop down box
    // Note: We go in reverse direction to list the latest grids first
    //
    str += "<tr><td> Select Existing Grid: " +
        "<select id='selWin' onclick='changeGridSel(this.selectedIndex)'> ";
    //
    ExistingGridList = new Array();
    //
    for (var i = fO.allGrids.length - 1; i >= 0; i--) {

        var gridO = fO.allGrids[i];

        // if we are adding a source grid, do not add the "ReturnValue"
        // ASSUMPTIION: Return value is grid 0 in a function
        //
        if ((sO.stageInStep >= StageInStep.ConfigDone) && gridO.isRetVal) {
            continue;
        }

        // if we are adding an output grid and griO is a constant, skip
        //
        if ((sO.stageInStep < StageInStep.ConfigDone) && gridO.isConst) {
            continue;
        }

        // Do not add deleted grids to the drop down list
        //
        if (gridO.deleted != DeletedState.None)
            continue;


        // if grid is not already present in the current step, we add it
        // to the list
        // NOTE: We allow an existing grid to be added anywhere in the 
        //       function regardless in which step it was created. This
        //       is usually necessary for convenience -- e.g., user creates 
        //       a grid an does computation and now wants to insert an 
        //       initialization (because the user forgot earlier)
        // NOTE: Any grid created (in a step) has function scope by default
        //
        if ( /*isPresentInAPrevStep(gridO, sO) && */ !isPresentInStep(
                gridO, sO)) {

	    //DC6: "If" statement used to AVOID re-adding in the drop-down 
	    //	   menu GLOBAL grids that have been added as source grids 
	    //     in previous steps of current function (in which case a 
	    //     clone is created).
	    //TODO: Why was the clone needed in the first place? Remove?
	    //TODO: Show Data doesn't work (didn't before either!)
	    //if (gridO.constructor.name != "cloneGridObj") {
	    if (gridO.globalRefId == -1) {

                str += "<option "; //value='" + i + "'";
                str += ">" + gridO.caption + "</option>";
		ExistingGridList.push(gridO);

	    }

        }
    }


    // STEP: Add template grids (only for output grid, since user must
    //       create a new copy of a template, which can be done only for
    //       output grid)
    //
    if (sO.stageInStep == StageInStep.New) {
        var tfO = CurModObj.allFuncs[FuncID.Template];
        for (var i = 0; i < tfO.allGrids.length; i++) {

            var tgO = tfO.allGrids[i];
            str += "<option>" + tgO.caption + " (Template)</option>";

            ExistingGridList.push(tgO);
        }
    }


    // STEP: Add global grids 
    //
    var gfO = CurModObj.allFuncs[FuncID.Global];
    for (var i = 0; i < gfO.allGrids.length; i++) {

        var ggO = gfO.allGrids[i];
        str += "<option>" + ggO.caption + " (global)</option>";

        ExistingGridList.push(ggO);
    }



    // If a step has added all existing grids up to this point, then 
    // don't have anything else to add
    //
    if (ExistingGridList.length == 0) {
        alert("No additional existing grids available for this step");
        return;
    }


    // TODO:
    // Insert all global grids from GlobalFuncObj and templates. A user can pick
    // grids from:
    //       (1) Global templates (from CurFuncObj.TemplateFuncObj)
    //       (2) global grids (from CurModObj.GlobalFuncObj)
    //       (3) input parameters (args) to the function
    //       (4) local grids created within function


    str += "</select>";
    str += "<input type='button' value='Done' onclick='existSelDone()'>";
    str +=
        "<input type='button' value='Back' onclick='existSelCancel()'>";

    str += "<BR><BR>" + "<div id='selNameDiv'></div>";

    str += "</td></tr>" + "</table>";

    // change the innerHTML of the HTML object 
    //
    var menuId = CurHtmlGridId;
    var menu1 = document.getElementById(menuId);
    menu1.innerHTML = str;

    // Set default grid seclection to 0 by manually calling changeGridSel()
    //
    changeGridSel(0);
}

//----------------------------------------------------------------------------
// Checks whether gO is present in any prvious step. This is useful
// for displaying the list of exisiting grids that is available to insert
// into a step.
// If curstepO is null, then searches in all steps whether a grid is present
//----------------------------------------------------------------------------
function isPresentInAPrevStep(gO, curstepO) {

    var fO = CurFuncObj;

    // Go thru all steps (including header at step 0)
    //
    for (var s = 0; s < fO.allSteps.length; s++) {

        var stepO = fO.allSteps[s];

        // Don't go beyond current step
        //
        if (curstepO && (stepO == curstepO))
            break;

        // Go thru all grids in a step
        // OPTIMIZE: It is sufficient to look at output grid only -- except
        //           for the header
        //
        for (var g = 0; g < stepO.allGridIds.length; g++) {

            var gid = stepO.allGridIds[g];

            if (fO.allGrids[gid] == gO)
                return true;
        }
    }

    return false;

}


//----------------------------------------------------------------------------
// Whether gridObj gO is present in the step stepO
//----------------------------------------------------------------------------
function isPresentInStep(gO, stepO) {

    var fO = CurFuncObj;

    // Go thru all grids in a step
    //
    for (var g = 0; g < stepO.allGridIds.length; g++) {

        var gid = stepO.allGridIds[g];

        if (fO.allGrids[gid] == gO) // if present in the current step
            return true;
    }

    return false;
}



//----------------------------------------------------------------------------
// Changes grid displayed when the user clicks on a grid name
// ind -- index into ExistingGridList
//----------------------------------------------------------------------------
function changeGridSel(ind) {

    var sO = CurStepObj;

    assert((ind < ExistingGridList.length), "invalid index");
    var selObj = ExistingGridList[ind];

    // Note: we clone to associate grid Obj w/ a different htmlId. However,
    //       the clone is discarded. Hence, newGId is never registered with
    //       CurFuncObj
    //
    var newGId = CurFuncObj.allGrids.length;
    var ngO = new cloneGridObj(newGId, selObj);
    //
    // TODO: If the user wants the grid cell as a reference, we should draw
    //
    // var ngO = new cloneGridObj(newGId, selObj.refgO);

    // if the selected object is a function arg, mark the cloned object 
    // as such (to enable editing of caption)
    //
    ngO.inArgNum = selObj.inArgNum;
    //
    // Point NewGridObj to newly created clone. This is necessary for 
    // changeCaption() function to change name while selecting a grid
    //
    NewGridObj = ngO;

    var niO = null;      // no indices to show
    drawGrid(ngO, niO, PreviewHtmlGridId, false);


    // if we are adding an output grid, we give the user an option to 
    // make a copy of an existing grid. This is a convenience -- otherwise
    // the user will have to create templates for all grids that need 
    // to be created more than once. For instance, this allows easy creation
    // of temp grids
    // Note: We don't allow to make copy of an incoming parameter (arg) or
    //       the return value
    //
    var str = "<div style='text-align:left'>";
    //
    if ((sO.stageInStep < StageInStep.ConfigDone) &&
        (ngO.inArgNum < 0) && !ngO.isRetVal) { // if input param/retval

        // if this is a template, the user MUST make a copy
        //
        var prop = (ngO.isTemplate) ? " checked disabled " : "";

        str += "<input type='checkbox' id='makeAcopy' " + prop + ">" +
            " Make a new copy named: ";
        str += "<span onclick='checkBox4CopyGrid()'>" +
            getCaption4GridArg(null, true, DefCopyGridName) + "</span>";


        if (ngO.isTemplate) {
            str += "<BR><input type='checkbox' id='reconfigTemplate'>" +
                " Reconfigure new template copy";
        }


    }

    str += "</div>";
    var div1 = document.getElementById('selNameDiv');
    div1.innerHTML = str;

}


//----------------------------------------------------------------------------
// Put a check in checkbox for creating a grid copy
//----------------------------------------------------------------------------
function checkBox4CopyGrid() {
    var cb1 = document.getElementById('makeAcopy');
    cb1.checked = true;
}



//----------------------------------------------------------------------------
// Function called when the user selects an existing grid
// Note: Displays at 'CurHtmlGridId'
//----------------------------------------------------------------------------
function existSelDone() {

    var sO = CurStepObj;
    var fO = CurFuncObj;
    var mO = CurModObj;

    // Reads the selction value from the 'selWin' id
    //
    var sel1 = document.getElementById('selWin');

    // Get the selected index from the drop down box
    //
    var selind = sel1.selectedIndex;
    assert((selind < ExistingGridList.length), "invalid index");
    var selObj = ExistingGridList[selind];

    // Find the index of this grid in the function
    //
    var ind = findIndexOfGrid(fO, selObj, true);
    var gOsel = (ind >= 0) ? fO.allGrids[ind] : null;
    var template_grid = false;

    // STEP: 
    // if we did not find the grid within the current function scope, 
    // search templates 
    // TODO: Search templates of all modules
    //
    if (ind < 0) {

        //  TODO: Search templates of ALL modules (not just current module)
        //        because we can use templates from other modules (libraries)
        //
        // if we found the grid in templates, we always make a copy
        // of the template. Copying will be done below (with copy case)
        //
        var fOtemp = mO.allFuncs[FuncID.Template]; // template scope
        ind = findIndexOfGrid(fOtemp, selObj, true);
        //
        if (ind >= 0) { // if template grid found
            gOsel = fOtemp.allGrids[ind];
            template_grid = true;
            //
            var cb1 = document.getElementById('makeAcopy');
            assert(cb1,
                "templates allowed w/ copy only (output grids only)");
        }
    }


    // STEP:
    // if we did not find the grid in the current function OR templates,
    // search global scope
    //
    if (ind < 0) {

	// Find whether selObj grid is present in global scope
	//
	var gind = findIndexOfGrid(mO.allFuncs[FuncID.Global], selObj, true);

	if (gind >= 0) {
	    
	    // This is a global grid. So, create a reference (as we do 
	    // for a function call). Since the global scope is modelled 
	    // as a separate function, we need to create a reference. 
	    //
	    var gOglob = mO.allFuncs[FuncID.Global].allGrids[gind];
	    //
	    var newGId = fO.allGrids.length;
	    var ngO = new cloneGridObj(newGId, gOglob);
	    var niO = new IndexObj(ngO);               // new index obj

	    // Register the original global grid id as the source
	    // 
	    // NOTE: We CANNOT Point the .data field of cloned grid to 
	    // the .data field as follows because, the .data field of 
	    // gOglob is allocated later : 'ngO.data = gOglob.data;'
	    // So, we have to put this reference in the generated code.
	    // 
	    ngO.globalRefId = gind;     // record src grid id
	    ngO.isGridRef = true;       // this is a grid reference

	    // By default, mark a reference to a global grid as a constant grid
	    //
	    ngO.isConst = true;

	    // Add the cloned (reference) grid to the current
	    // function and current step AND draw it
	    //
	    CurFuncObj.allGrids.push(ngO);   // add clone to current function
	    CurStepObj.addGridId(newGId);    // add grid id to current step
	    CurStepObj.allIndObjs.push(niO); // add index object to step
	    resetColRowIndices(ngO, niO);    // set index var names
	    //
	    drawGrid(ngO, niO, CurHtmlGridId, false);    

	    logMsg("Made a new reference to a global grid");

	} else {
	    
	    assert(0, "Grid not found in template/globa/func. " + 
		   "TODO: Search in templates of other modules");
	}


    } else if (gOsel.inArgNum < 0) {          // if not an input arg (param)

	// This is not an incoming arg, so this is just an 
	// existing grid. There are two cases. 
	// NOTE: if this is source grid cb1 will be null.
	//
	var cb1 = document.getElementById('makeAcopy');
	//
	if ((cb1 && cb1.checked) || template_grid) {

	    // CASE 1: User requested a COPY of an existing grid. A 'copy'
	    //         is completely independent from the existing grid. So,
	    //         make a clone.
	    // NOTE: Templates are handled here because we have to make a copy
	    //       of a template.
	    //

	    // If a copy/template-copy is seletected, the copy must have
	    // a unique name
	    //
	    var cap1 = document.getElementById('captionEdit');

	    if (cap1.innerHTML == DefCopyGridName) {
		alert("Please edit the name of the new copy");
		return;
	    }

	    var newGId = fO.allGrids.length;
	    var ngO = new cloneGridObj(newGId, gOsel); // new grid obj
	    ngO.caption = cap1.innerHTML;
	    ngO.isTemplate = fO.isTemplateScope;       // is copy a tempalte
	    //
	    // Add the cloned grid to the current
	    // function and current step
	    //
	    CurFuncObj.allGrids.push(ngO);   // add clone to current function
	    CurStepObj.addGridId(newGId);    // add grid id to current step
	    //
	    // if this a template, see whether the user requested to 
	    // reconfig grid -- i.e., to mainly add dimensions
	    //
	    var cb2 = document.getElementById('reconfigTemplate');
	    //
	    if (template_grid && cb2.checked) {

		// We configure this grid similar to configuring a grid but
		// with only some re-configuring options. At the end of 
		// re-configure, we will add ngO to the step as we usually
		// do for a new output grid (in configDone()). There, we 
		// will create a new index object too (so don't do it here)
		//
		// TODO: Restrict reconfiguring options. 
		//
		NewGridObj = ngO;
		CurHtmlGridId = OutHtmlId; 
		//
		drawGrid(ngO, null, CurHtmlGridId, false);    

		ngO.numDimsOrig = ngO.numDims;    // set original num dims
		drawGridDimExtendMenu(ngO);
		//
	        return;

	    } else {     // not reconfiguring new template

		// Create an index object and add it to step
		//
		var niO = new IndexObj(ngO);      // new index obj
		CurStepObj.allIndObjs.push(niO); // add index object to step
		resetColRowIndices(ngO, niO);    // set index var names

		drawGrid(ngO, niO, CurHtmlGridId, false);    
	    }

	    logMsg("created a new copy of a grid");

	} else {

	    // CASE 2: User wants the same existing grid. Therefore, no clone
	    //         is made. However, we need a new index object because 
	    //         indices are always unique to a step
	    //         Note that we don't add gOsel to function becaue it
	    //         is already present in function (fO.allGrids[])

	    assert(gOsel, "Null selected grid"); 
	    var niO = new IndexObj(gOsel);               // new index obj
	    //
	    CurStepObj.addGridId(ind);       // add grid ID to step
	    CurStepObj.allIndObjs.push(niO); // add index object to step
	    resetColRowIndices(gOsel, niO);  // set index var names
	    //
	    drawGrid(gOsel, niO, CurHtmlGridId, false);    	    
	}


    } else {        // selected grid is an incoming arg (parameter)

	// This is an incoming arg. The arg grid is already present in the
	// function (fO.allGrids) -- added by changeFunc(). So, we make
	// reference to the existing parameter --  we do NOT 
	// add it again to the function. However, we still need to add the
	// incoming argument to the current step. So, do it below. 
	//
	var niO = new IndexObj(gOsel);               // new index obj
	//
	CurStepObj.addGridId(ind);       // add grid ID to step
	CurStepObj.allIndObjs.push(niO); // add index object to step
	resetColRowIndices(gOsel, niO);  // set index var names
	//
	drawGrid(gOsel, niO, CurHtmlGridId, false);    	    

    }

    // STEP:
    //
    // call the sequencer. If we are selecting an existing grid for the
    // output grid (when ConfigDone is not true), mark ConfigDone as true,
    // because we seletected an existing grid. 
    //
    if (CurStepObj.stageInStep < StageInStep.ConfigDone) {
        CurStepObj.stageInStep = StageInStep.ConfigDone;
    }
    //
    doNextStage();                     // call the sequencer to do next step

}


//----------------------------------------------------------------------------
// Function called when the user cancels an existing grid selection
// Note: Displays menu 'CurHtmlGridId'
//----------------------------------------------------------------------------
function existSelCancel() {

    doNextStage();       // call the sequencer to do next step
}


//----------------------------------------------------------------------------
// Grid configuration menu for creating a new grid
//----------------------------------------------------------------------------
function drawGridConfigMenu(gO) {

    var menuId = "GridConfMenu";                  // grid config menu
    
    var totCols = 5;                              // Total # of columns in menu

    var str = "<table class='gridconfig' id='" + menuId + "'>";
    
    var isMultiDim = (gO.numDims > 0);            // allow multi dims

    if (gO.numDims > 0) {

	// Draw Titles ......................................................
	//
	//MPI: additions (Extended)
	str += "<tr><th>Dimension</th>" //+ "<th>Name</th>" 
	    + "<th>Displayed Size</th> "
	    + "<th>Actual Size</th> <th>Titles?</th> <th>Data Types</th>"
	    /* TODO: Enable later for MPI support
	    + "<th>Extended?</th>"
	    */
	    + "</tr>";


	for (var d=0; d < gO.numDims; d++) {

	    var sz = " min=1 max=20 style='width:40px' ";
	    var onc = " onchange='updateDimSize(" + d + ",this.value,0)' ";
	    var disp = "<input type='number' " + sz + onc
		+ " value='" + gO.dimShowSize[d] + "'>";

	    sz = " style='width:80px' ";           // actual size
	    if (gO.dimHasTitles[d]) sz += " disabled ";
	    onc = " onchange='updateDimSize(" + d + ",this.value,1)' ";
	    var actsz = gO.dimDynSize[d] ? gO.dimDynSize[d] : gO.dimActSize[d];

	    var act = "<input type='text' " + sz + onc
		+ " value='" + actsz + "'>";
	
	    // Display the small 2D grid icon to select a dynamic actual
	    // size. Do this only when the dim does not have titles
	    //
	    if (!gO.dimHasTitles[d]) {
		act += " <img src='images/2D_array.jpg' "      // link to grid
		    + "width=15px height=20px onclick='pickActSz(" + d + ")'>";
	    } else {
		// empty image to keep the space. TODO: remove empty square
		//
		act += " <img width=15px height=20px style='border-width:0px'>";
	    }

	    onc = " onclick='changeDimHasTitles(" + d + ", this.checked)' ";  
	    if (gO.dimHasTitles[d]) onc += " checked ";
	    var titles = "<input type='checkbox'" + onc + ">";
	    
	    onc = " onclick='changeDimHasTypes(" + d + ", this.checked)' ";  
	    if (d == gO.typesInDim) onc += " checked ";
	    if (!gO.dimHasTitles[d]) onc = " hidden "; // disabled w/o types
	    var types = "<input type='checkbox'" + onc + ">";
   
	    //MPI: Checkbox enabled only for Global grids. 
	    onc = " onclick='changeDimIsExtended(" + d + ", this.checked)' ";  
	    if (gO.dimIsExtended[d]) onc += " checked ";
	    if (!gO.isGlobal) onc = " hidden ";
	    var dimIsExtended = "<input type='checkbox'" + onc + ">";



	    //MPI: additions (dimIsExtended)
	    str += "<tr> <td>" + (d+1) + "</td>"    // dimension number label
	    // + "<td>" + namestr + "</td>"         // dimension name text box
		+ "<td>" + disp + "</td>"           // displayed size number box
		+ "<td>" + act + "</td>"            // actual size text box
		+ "<td>" + titles + "</td>"         // has titles check box 
		+ "<td>" + types + "</td>"          // has types check box
		/* TODO: Enable later for MPI support
	 	+ "<td>" + dimIsExtended + "</td>"  // is Extended check box
		*/
	    ;
	}

	str += "</tr>";
    }

    // Display Done/Back BUTTONS .............................................
    //
    str += "<tr>";
    str += "<td class='lbutton'><input type='button' value='Back' " +
        "onclick='configBack()'></td>";

    if (isMultiDim) {

        // str += "<td class='rbutton'></td>";        // dim name

        str +=
            "<td class='rbutton'><input type='button' onclick='addDim()'" +
            "value='Add Dimension'></td>";

        var opt = (gO.numDims <= 2) ? " disabled " : "";
        str += "<td class='rbutton'><input type='button' " +
            "onclick='removeDim()' value='Remove Dimension'" + opt +
            "></td>";
    }


    str += "<td class='rbutton'></td>";

    str +=
        "<td class='rbutton'><input type='button' onclick='configDone()'" +
        "value='Done'></td>";
    str += "</tr>";

    // Vertical Space
    //
    str +=
        "<tr><td class='lbutton'><div style='height:10px'> </div></tr>";


    // STEP : --------------------------------------------------------------
    //      : Check boxes for the entire table
    //
    onc = " onclick='markInitData(this.checked)' ";
    if (gO.hasInitData) onc += " checked ";
    var idata = "<input type='checkbox'" + onc + ">" +
        "Enable manual entering of initial data";


    //NS: For reusing vars from existing modules
    onc = " onclick='markInExternalMod(this.checked)' ";
    if (gO.inExternalMod) onc += " checked ";
    var pap = "<input type='checkbox'" + onc + ">" +
	"Global variable exists in existing module";
    //onc = " onclick='addNameExternalMod()'  ";
    

    //NS2: For enabling common vars (in Fortran)
    // TODO: Takes away from the high-level generality
    //	     Use in an "advanced" mode only?
    onc = " onclick='markInCommon(this.checked)' ";
    if (gO.isCommon) onc += " checked ";
    var pap2 = "<input type='checkbox'" + onc + ">" +
	"Grid belongs in COMMON block";


    //DC3: For enabling a grid to "belong" to a struct/TYPE
    // TODO: Takes away from the high-level generality
    //	     Use in an "advanced" mode only?
    onc = " onclick='markStructCaption(this.checked)' ";
    if (gO.structCaption != null) onc += " checked ";
    var pap3 = "<input type='checkbox'" + onc + ">" +
	"Grid belongs to existing struct";


    /* TODO: Enable later for MPI support
    //MPI:
    onc = " onclick='markGlobal(this.checked)' ";
    if (gO.isGlobal) onc += " checked ";
    var iglob = "<input type='checkbox'" + onc + ">" +
        "Enable Global sharing across nodes (MPI)";

    //MPI:
    //TODO: Can be active ONLY if Global sharing is active
    onc = " onclick='markDistributed(this.checked)' ";
    if (gO.isDistributed) onc += " checked ";
    if (!gO.isGlobal) onc = " disabled ";
    var idistr = "<input type='checkbox'" + onc + ">" +
        "Enable distribution of data (MPI)";
    */

    str += "<tr><td  class='botcheck' colspan=" + totCols + ">" + idata +
        "</td></tr>";


    //DC4: Added flag for code integration mode.
    //NS: Only applies to the global function
    if (CurFuncObj.isGlobal && CodeIntegrationMode) {
    
	str += "<tr><td class='botcheck' colspan=" + totCols + ">" + pap +
	       "</td></tr>";

    }

    //DC4: Added flag for code integration mode.
    //DC3: Only applies if grid in the global function and existing module.
    if (CurFuncObj.isGlobal && gO.inExternalMod && CodeIntegrationMode) {
        str += "<tr><td  class='botcheck' colspan=" + totCols + ">" + pap3 +
               "</td></tr>";
    }

    //DC4: Added flag for code integration mode.
    //NS2: Only applies to the global function (for common blocks).
    if (CurFuncObj.isGlobal && CodeIntegrationMode) {

        str += "<tr><td  class='botcheck' colspan=" + totCols + ">" + pap2 +
               "</td></tr>";
    }


    /* TODO: Enable later for MPI support
    //MPI:
    str += "<tr><td  class='botcheck' colspan=" + totCols + ">" + iglob +
        "</td></tr>";
     str += "<tr><td  class='botcheck' colspan=" + totCols + ">" + idistr +
        "</td></tr>";
    */

    var msg = "Configure New Output Grid";
    //
    if (CurFuncObj.isTemplateScope)
        msg = "Configure New Template Grid";

    str += "<tr><td class='caption' colspan=6><BR>" + msg + "</td></tr>";

    var conf1 = document.getElementById(menuId);
    conf1.innerHTML = str + "</table>"; // update HTML of grid
    conf1.className = 'gridconfig';

    drawProgStructHead();
    showTip(TipId.EditTitles);
}


//----------------------------------------------------------------------------
// RECONFIG
// Grid configuration menu for *Reconfiguring an EXISTING grid*
//----------------------------------------------------------------------------
function drawGridReConfigMenu(gO) {

    var menuId = "GridConfMenu";                  // grid config menu
    
    var totCols = 5;                              // Total # of columns in menu

    var str = "<table class='gridconfig' id='" + menuId + "'>";
    
    var isMultiDim = (gO.numDims > 0);            // allow multi dims


    if (gO.numDims > 0) {

        // Draw Titles ......................................................
        //
        str += "<tr><th>Dimension</th>" //+ "<th>Name</th>" 
            + "<th>Displayed Size</th> "
            + "<th>Actual Size</th> <th>Titles?</th> <th>Data Types</th> " 
	    + "<th>Extended?</th>"
	    + "</tr>";

        for (var d = 0; d < gO.numDims; d++) {

	    // Note: Don't allow changing displayed size. Displayed size 
	    //       must be changed within the step (using 'arrow'). If we
	    //       allow this, then we have to adjust indices
	    // Note: When we disable, do NOT have an onchange event for it.
	    //       Some browsers do not like that.
	    //
            var sz = " min=1 max=20 style='width:40px' ";
            //var onc = " disabled ";
	    var onc = " onchange='updateDimSize(" + d + ",this.value,0)' ";
            var disp = "<input type='number' " + sz + onc + 
                " value='" + gO.dimShowSize[d] + "'>";

            sz = " style='width:60px' "; // actual size
            if (gO.dimHasTitles[d]) sz += " disabled ";
            onc = " onchange='updateDimSize(" + d + ",this.value,1)' ";
            var actsz = gO.dimDynSize[d] ? gO.dimDynSize[d] : gO.dimActSize[d];
            var act = "<input type='text' " + sz + onc + " value='" +
                actsz + "'>";
            act += " <img src='images/2D_array.jpg' " // link to grid
                + "width=15px height=20px onclick='pickActSz(" + d +
                ")'>";

	    // Don't allow changing types when reconfiguring
	    //
            onc = " disabled ";
            if (gO.dimHasTitles[d]) onc += " checked ";
            var titles = "<input type='checkbox'" + onc + ">";

            onc = " disabled ";
            if (d == gO.typesInDim) onc += " checked ";
            //if (!gO.dimHasTitles[d]) onc = " hidden "; // disabled w/o types
            var types = "<input type='checkbox'" + onc + ">";

	    //MPI:
	    onc = " disabled ";
            if (gO.dimIsExtended[d]) onc += " checked ";
            var dimIsExtended = "<input type='checkbox'" + onc + ">";


	    //MPI: Added dimIsExtended
            str += "<tr> <td>" + (d + 1) + "</td>" // dimension number label
                // + "<td>" + namestr + "</td>"    // dimension name text box
                + "<td>" + disp + "</td>" // displayed size number box
                + "<td>" + act + "</td>" // actual size text box
                + "<td>" + titles + "</td>" // has titles check box 
                + "<td>" + types + "</td>" // has types check box 
		+ "<td>" + dimIsExtended + "</td>" // has Extended check box 
            ;
        }

        str += "</tr>";
    }

    // Display Done/Back BUTTONS .............................................
    //
    str += "<tr>";
    str += "<td class='lbutton'><input type='button' value='Back' " +
        "onclick='reConfigBack()'></td>";


    str += "<td class='rbutton' colspan=3></td>";

    str +=
        "<td class='rbutton'><input type='button' onclick='reConfigDone()'" +
        "value='Done'></td>";
    str += "</tr>";

    // Vertical Space
    //
    str +=
        "<tr><td class='lbutton'><div style='height:10px'> </div></tr>";


    // STEP : --------------------------------------------------------------
    //      : Check boxes for the entire table
    //
    onc = " onclick='markInitData(this.checked)' ";
    if (gO.hasInitData) onc += " checked ";
    var idata = "<input type='checkbox'" + onc + ">" +
        "Enable manual entering of initial data";

    // TODO: Have to do for all other checkboxes (e.g., MPI)

    str += "<tr><td  class='botcheck' colspan=" + totCols + ">" + idata +
        "</td></tr>";

    var msg = "Reconfigure Grid";
    //
    if (CurFuncObj.isTemplateScope)
        msg = "Reconfigure Grid from Template";

    str += "<tr><td class='caption' colspan=6><BR>" + msg + "</td></tr>";

    var conf1 = document.getElementById(menuId);
    conf1.innerHTML = str + "</table>"; // update HTML of grid
    conf1.className = 'gridconfig';

    drawProgStructHead();
    showTip(TipId.ReConfigSizes);
}



//----------------------------------------------------------------------------
// For extending a template grid
//----------------------------------------------------------------------------
function drawGridDimExtendMenu(gO) {

    var menuId = "GridConfMenu";                  // grid config menu
    
    var totCols = 5;                              // Total # of columns in menu

    var str = "<table class='gridconfig' id='" + menuId + "'>";
    
    var isMultiDim = (gO.numDims > 0);            // allow multi dims

    if (gO.numDims > 0) {

	// Draw Titles ......................................................
	//
	str += "<tr><th>Dimension</th>" //+ "<th>Name</th>" 
	    + "<th>Displayed Size</th> "
	    + "<th>Actual Size</th> <th>Titles?</th> <th>Data Types</th> </tr>";


	for (var d=0; d < gO.numDims; d++) {


	    var sz = " min=1 max=20 style='width:40px' ";
	    var onc = " onchange='updateDimSize(" + d + ",this.value,0)' ";
	    var disp = "<input type='number' " + sz + onc
		+ " value='" + gO.dimShowSize[d] + "'>";

	    sz = " style='width:60px' ";           // actual size
	    if (gO.dimHasTitles[d]) sz += " disabled ";
	    onc = " onchange='updateDimSize(" + d + ",this.value,1)' ";
	    var actsz = gO.dimDynSize[d] ? gO.dimDynSize[d] : gO.dimActSize[d];
	    var act = "<input type='text' " + sz + onc
		+ " value='" + actsz + "'>";
	    act += " <img src='images/2D_array.jpg' "      // link to grid
		+ "width=15px height=20px onclick='pickActSz(" + d + ")'>";

	    onc = " onclick='changeDimHasTitles(" + d + ", this.checked)' ";  
	    if (gO.dimHasTitles[d]) onc += " checked ";
	    if (d < gO.numDimsOrig) onc += " disabled ";
	    var titles = "<input type='checkbox'" + onc + ">";
	    
	    onc = " onclick='changeDimHasTypes(" + d + ", this.checked)' ";  
	    if (d == gO.typesInDim) onc += " checked ";
	    if (d < gO.numDimsOrig) onc += " disabled ";
	    if (!gO.dimHasTitles[d]) onc = " hidden "; // disabled w/o types
	    var types = "<input type='checkbox'" + onc + ">";


	    str += "<tr> <td>" + (d+1) + "</td>"    // dimension number label
	    // + "<td>" + namestr + "</td>"         // dimension name text box
		+ "<td>" + disp + "</td>"           // displayed size number box
		+ "<td>" + act + "</td>"            // actual size text box
		+ "<td>" + titles + "</td>"         // has titles check box 
		+ "<td>" + types + "</td>"          // has types check box 
	    ;
	}

        str += "</tr>";
    }

    // Display Done/Back BUTTONS .............................................
    //
    str += "<tr>";
    str += "<td class='lbutton'><input type='button' value='Back' " +
        "onclick='configBack()'></td>";

    if (isMultiDim) {

        // str += "<td class='rbutton'></td>";        // dim name

        str +=
            "<td class='rbutton'><input type='button' onclick='addDim()'" +
            "value='Add Dimension'></td>";

        var opt = (gO.numDims <= gO.numDimsOrig) ? " disabled " : "";
        str += "<td class='rbutton'><input type='button' " +
            "onclick='removeDim()' value='Remove Dimension'" + opt +
            "></td>";
    }


    str += "<td class='rbutton'></td>";

    str +=
        "<td class='rbutton'><input type='button' onclick='configDone()'" +
        "value='Done'></td>";
    str += "</tr>";

    // Vertical Space
    //
    str +=
        "<tr><td class='lbutton'><div style='height:10px'> </div></tr>";


    // STEP : --------------------------------------------------------------
    //      : Check boxes for the entire table
    //
    onc = " onclick='markInitData(this.checked)' ";
    if (gO.hasInitData) onc += " checked ";
    var idata = "<input type='checkbox'" + onc + ">" +
        "Enable manual entering of initial data";

    str += "<tr><td  class='botcheck' colspan=" + totCols + ">" + idata +
        "</td></tr>";

    var msg = "Extend Template Copy";
    //
    if (CurFuncObj.isTemplateScope)
        msg = "Extend Template Copy";

    str += "<tr><td class='caption' colspan=6><BR>" + msg + "</td></tr>";

    var conf1 = document.getElementById(menuId);
    conf1.innerHTML = str + "</table>"; // update HTML of grid
    conf1.className = 'gridconfig';

    drawProgStructHead();
    showTip(TipId.EditTitles);
}





//----------------------------------------------------------------------------
// Pick a variable (scalar grid) as an actual size of a grid created
//----------------------------------------------------------------------------
function pickActSz(dim) {

    var fO = CurFuncObj;
    var str = "<table><tr><td><div id='" + PreviewHtmlGridId +
        "'> Select Actual Size of Grid </div></td></tr>";

    // Create the list of all grids for the drop down box
    //
    str += "<tr><td> Pick (Scalar) Value From: " +
        "<select id='selWin'> ";
    //+ "<select id='selWin' onclick='changeActSzSel(this.selectedIndex)'> ";

    //
    var grids_added = 0;

    for (var i = 0; i < fO.allGrids.length; i++) {

        // Show only scalar grids // TODO: Pick only integers
        //
        if (fO.allGrids[i].numDims < 1) {
            str += "<option value='" + i + "'";
            str += ">" + fO.allGrids[i].caption + "</option>";
            grids_added++;
        }
    }

    //
    str += "</select>";


    // Buttons for 'Done' and 'Cancel'
    //
    var opt = "";
    if (!grids_added) {
        showTip(TipId.NoScalarGrid);
        opt = " disabled ";
    }

    str += "<input type='button' value='Done' " + opt +
        " onclick='actSzSelDone(" + dim + ")'>";
    //
    str +=
        "<input type='button' value='Cancel' onclick='actSzSelCancel()'>";

    str += "<BR><BR><div id='selNameDiv'></div>";

    str += "</td></tr></table>";


    // change the innerHTML of the HTML object 
    //
    var htmlId = ActSzSelectHtmlId;
    var htmlElem = document.getElementById(htmlId);
    htmlElem.innerHTML = str;

}

//----------------------------------------------------------------------------
// Function called when user selects the actual size of a grid dimension
//----------------------------------------------------------------------------
function actSzSelDone(dim) {

    var fO = CurFuncObj;
    var gO = NewGridObj;

    // Get selected index from the drop down box
    //
    var selId = 'selWin';
    var selElem = document.getElementById(selId);
    var ind = selElem.selectedIndex;

    //alert(" ind: " + ind + " len:" + CurFuncObj.allGrids.length);
    assert(ind < CurFuncObj.allGrids.length);
    assert((dim < gO.numDims), "invalid dimension in new grid");

    // NOTE: The index 'ind' returned here is the index in the drop down 
    //       box. However, we insert only *scalar* grids into the drop down
    //       box. So, find the corresponding grid.
    //
    var scalars = 0,
        gInd = 0;

    for (var i = 0; i < fO.allGrids.length; i++) {

        // Count only scalar grids // TODO: Pick only integers
        //
        if (fO.allGrids[i].numDims < 1) {

            if (scalars == ind) {
                gInd = i;
                break;
            }

            scalars++;
        }
    }

    //alert("Grid ind in func selected:" + gInd);
    assert(gInd < CurFuncObj.allGrids.length);
    //
    var gObjSel = CurFuncObj.allGrids[gInd];
    assert((gObjSel.numDims < 1), "only scalar grids supported");

    var name = gObjSel.caption;

    // We record the scalar grid name in dimDynSize array. 
    //
    gO.dimDynSize[dim] = name;

    // The dimActSize array will initially contain the same size that we show
    // While we generate code, we will update dimActSize with the correct 
    // value
    //
    gO.dimActSize[dim] = gO.dimShowSize[dim];

    var htmlId = ActSzSelectHtmlId;
    var htmlElem = document.getElementById(htmlId);
    htmlElem.innerHTML = "<table></table>";

    reDrawConfig(); // redraw with updates
}

//----------------------------------------------------------------------------
// Called when user cancels actual grid size selection
//----------------------------------------------------------------------------
function actSzSelCancel() {

    var htmlId = ActSzSelectHtmlId;
    var htmlElem = document.getElementById(htmlId);
    htmlElem.innerHTML = "<table></table>";
}


//----------------------------------------------------------------------------
// If a grid has initial data, mark as such
//----------------------------------------------------------------------------
function markInitData(checked) {

    NewGridObj.hasInitData = checked;
    reDrawConfig();

}


//NS:
//----------------------------------------------------------------------------
// If a grid is existing in external module, mark as such
// Prompt the user to give name of existin module for which current exists
//----------------------------------------------------------------------------
function markInExternalMod(checked) {

    // Toggle accordingly
    NewGridObj.inExternalMod = checked;

    // If true, ask user for name
    if (checked) {

        var modname = prompt("Name of new function", "Existing module name");

        // TODO: Add checks! And warning to make sure.
        NewGridObj.nameExternalMod = modname;

    } else {

	NewGridObj.nameExternalMod = "";
	NewGridObj.structCaption = null; //DC3: Also removing struct name. This
					//     option is only available for code
					//     integration.

    }

    reDrawConfig();

}


//NS2:
//----------------------------------------------------------------------------
// If a grid is to belong in a COMMON block (Fortran) mark as such
// Prompt the user to give name of COMMON block in which current belongs
//----------------------------------------------------------------------------
function markInCommon(checked) {

    // Toggle accordingly
    NewGridObj.isCommon = checked;

    // If true, ask user for COMMON name
    if (checked) {

        var commonName = prompt("Name of new COMMON block", "common_block");

        // TODO: Add checks! And warning to make sure.
        NewGridObj.nameCommon = commonName;

    } else {

	NewGridObj.nameCommon = "";


    }

    reDrawConfig();

}


//----------------------------------------------------------------------------
//MPI: If a grid is marked as global, mark as such
//----------------------------------------------------------------------------
function markGlobal(checked) {

    NewGridObj.isGlobal = checked;

    if (!checked) {

	// If not Global, cannot be distributed, nor can its dimensions
	// be extended, so set to false.	    
        NewGridObj.isDistributed = false;
	for (var i = 0; i < NewGridObj.dimIsExtended.length; i++)
	    NewGridObj.dimIsExtended[i] = false;

    } else {

	NewGridObj.dimIsExtended[0] = true; // Default
        NewGridObj.isDistributed = true; // Default

    }
    reDrawConfig();

}


//DC3:
//----------------------------------------------------------------------------
// If a grid belongs to a struct, associate the grid to that struct name, so
// this is taken appropriately into account during code generation
//----------------------------------------------------------------------------
function markStructCaption(checked) {

    // Toggle accordingly
    //NewGridObj.structCaption = checked;

    // If true, ask user for struct name
    if (checked) {

        var structCaption = prompt("Name of existing struct variable in which grid belongs", "struct_name");

        // TODO: Add checks! And warning to make sure.
        NewGridObj.structCaption = structCaption;

    } else {

	NewGridObj.structCaption = null;

    }

    reDrawConfig();

}


//----------------------------------------------------------------------------
//MPI: If a grid is marked as distributed, mark as such
//----------------------------------------------------------------------------
function markDistributed(checked) {

    NewGridObj.isDistributed = checked;

    // If unchecking distributed, then uncheck all "Extended" checkboxes
    if(!checked) {

        for (var i = 0; i < NewGridObj.dimIsExtended.length; i++)
	    NewGridObj.dimIsExtended[i] = false;

    } else {

        NewGridObj.dimIsExtended[0] = true; // Default

    }
    reDrawConfig();

}



//----------------------------------------------------------------------------
// Genrate the default tab name 
//----------------------------------------------------------------------------
function getTabName(newdim, t) {

    if (newdim < 2)
        return (DefTitleRoots[newdim] + "" + t);
    else
        return (DefDimTitleRoot + (newdim - 2) + "" + t);

}


//----------------------------------------------------------------------------
// Add a dimension to a grid while configuring
//----------------------------------------------------------------------------
function addDim() {

    var gO = NewGridObj;            // currently configured grid

    var newdim = gO.numDims;
    gO.numDims++;
    if (gO.numDims == 2) gO.multiCol = true; //MPI: bug fix general
    gO.dimTitles[newdim] = new Array();
    gO.dimComments[newdim] = new Array();

    for (var t = 0; t < DefNumTabsInDim; t++) {
        //gO.dimTitles[newdim].push( "d" + (newdim+1) + DefDimTitleRoot + t);
        if (newdim > 1) //MPI: bug fix
	    gO.dimTitles[newdim].push(getTabName(newdim, t));
	else //MPI: bug, this can only be newdim = 1
	    gO.dimTitles[newdim].push(DefTitleRoots[ColDimId] + t);
	    
        gO.dimComments[newdim].push(null);
    }

    gO.dimHasTitles[newdim] = true;  // VERIFY -- do we need titles by default
    gO.dimHasIndices[newdim] = false; // VERIFY -- do we need indices by default

    gO.dimIsExtended[newdim] = false; //MPI:

    gO.dimShowSize[newdim] = DefNumTabsInDim;
    gO.dimActSize[newdim] = DefNumTabsInDim;
    gO.dimDynSize[newdim] = null;
    gO.dimShowStart[newdim] = 0;

    gO.dimSelectTabs[newdim] = 0;  // select first tab by default

    reDrawConfig(); // redraw with updates
}

//----------------------------------------------------------------------------
// Remove a dimension from a grid while configuring
//----------------------------------------------------------------------------
function removeDim() {

    var gO = NewGridObj;            // currently configured grid

    gO.numDims--;
    if (gO.numDims == 1) gO.multiCol = 0; //MPI: bug fix
    gO.dimTitles.pop();
    gO.dimComments.pop();
    gO.dimIsExtended.pop(); //MPI:

    // Note. Not updating other variables because they are not visible
    //
    reDrawConfig();               // redraw with updates
}


//----------------------------------------------------------------------------
// updates the dispalyed/actual size of a dimension
//
//----------------------------------------------------------------------------
function updateDimSize(dim, dim_size, isActual) {

    var gO = NewGridObj;
    var dimsz = parseInt(dim_size);

    if (!isActual) {                    // if we are updating display size

        gO.dimShowSize[dim] = dimsz;

	if (gO.dimHasTitles[dim])       // if this dim has titles
	    gO.dimActSize[dim] = dimsz; // actual = disp, if titles

        // If we increased the # of dims, push init values
        //
        for (var i = gO.dimTitles[dim].length; i < dimsz; i++) {

            //gO.dimTitles[dim].push( "d" + (dim+1) + DefDimTitleRoot + i ); 
            gO.dimTitles[dim].push(getTabName(dim, i));
            gO.dimComments[dim].push(null);
        }
        //
        // if we reduced the value, pop trailing entries
        //
        while (gO.dimTitles[dim].length > dimsz) {
            gO.dimTitles[dim].pop();
            gO.dimComments[dim].pop();
        }

	if (gO.dimSelectTabs[dim] >= dimsz)       // if selected tab is deleted
	    gO.dimSelectTabs[dim] = dimsz-1;      // set it to the last 

	if (gO.typesInDim == dim) {               // push data types
	    while (gO.dataTypes.length < dimsz) {
		gO.dataTypes.push(0);
	    }
	}

    } else {                           // updating actual size

        gO.dimActSize[dim] = dimsz;

	// if the user reduced the actual size to be smaller than displayed
	// size, call this function again to reduce displayed size as well
	//
	if (dimsz < gO.dimShowSize[dim])
	    updateDimSize(dim, dimsz, false);
    }



    reDrawConfig();               // redraw with updates

}


//----------------------------------------------------------------------------
// Change whether a dim has titles or not
//----------------------------------------------------------------------------
function changeDimHasTitles(dim, state) {

    var gO = NewGridObj;
    gO.dimHasTitles[dim] = state;

    // if the titles were removed AND if this dim has data types as well,
    // remove those data types
    //
    if (!state && (gO.typesInDim == dim))
        gO.typesInDim = -1;

    // Pick the opposite state for dimHasIndices -- i.e., 
    // (1) If dim has titles, it does not have indices
    // (2) if dim does not have titles, it has indices
    //
    gO.dimHasIndices[dim] = !state;

    // if has titles, select the first tab. Otherwise, select the middle tab
    // (because the middle tab contains the index)
    //
    var stab = (state) ? 0 : Math.floor(gO.dimShowSize[dim] / 2);
    gO.dimSelectTabs[dim] = stab;

    reDrawConfig();

    //alert("TODO: support titles/indices " + dim + " state:" + state);
}

//----------------------------------------------------------------------------
// Change whether a given dimension has data types associated with it
// Note: We let only one dimension to have data types.
//----------------------------------------------------------------------------
function changeDimHasTypes(dim, state) {

    var gO = NewGridObj;

    if (state)
        gO.typesInDim = dim;
    else {

        gO.typesInDim = -1;

    }

    reDrawConfig();

}


//----------------------------------------------------------------------------
//MPI: Change whether a dim is extended (to be used in MPI). 
//----------------------------------------------------------------------------
function changeDimIsExtended(dim, state) {

    var gO = NewGridObj;

    if (state)
        gO.dimIsExtended[dim] = true;
    else
        gO.dimIsExtended[dim] = false;

    reDrawConfig();

}

//----------------------------------------------------------------------------
// Redraw the config menu (after a change) and grid object
//----------------------------------------------------------------------------
function reDrawConfig(skip_table) {

    if (NewGridObj.beingReConfig) {                // if grid size re-config 
	drawGrid(NewGridObj, CurIndObj, CurHtmlGridId, true);
	drawGridReConfigMenu(NewGridObj);

    } else {

	drawGrid(NewGridObj, null, CurHtmlGridId, true);

	if (NewGridObj.numDimsOrig >= 0)         // extending template copy
	    drawGridDimExtendMenu(NewGridObj);	
	else                                          // for the 1st time
	    drawGridConfigMenu(NewGridObj);

    }

}

//----------------------------------------------------------------------------
// Done with grid configuration (when DONE button is pressed)
//----------------------------------------------------------------------------
function configDone() {

    var ngO = NewGridObj;
    var fO = CurFuncObj;
    var sO = CurStepObj;


    // don't accept default grid name -- user must change it
    //
    if ((ngO.caption == DefOutGridName) && CheckDefOutGridName) {
        alert("Please edit the default grid name: " + ngO.caption);
        return;
    }

    // don't accept a keywords as a grid name
    //
    if (isKeyword(ngO.caption)) {
        alert("Grid name cannot be a keyword: " + ngO.caption);
	return; //MPI: bug, allowed grid name to be a keyword
    }


    if (isExistingFuncName(CurModObj, ngO.caption)) {
        alert("Grid name cannot be an existing function name: " + ngO.caption);
	return; //MPI: bug, allowed a grid name to be existing function name
    }

    // TODO: Do other check on grid names

    // TODO: Don't accept any grid name starting with _ (underscore). It is
    //       reserved for internal variables only



    // Check for duplicate grid names in the current function
    //
    for (var g = 0; g < fO.allGrids.length; g++) {

        var gridO = fO.allGrids[g];

        if ((gridO.caption == ngO.caption) && (gridO != ngO)) {

            alert("Grid Name '" + ngO.caption + "' already exists." +
                " Please change grid name.");
            return;
        }
    }


    //MPI: Check if we have a global grid but not at least one extended dim
    //TODO: Here, we need to do further checks (e.g., all grids so far must
    //      be extended along the same dimensions
    if (ngO.isGlobal && ngO.isDistributed) {
	
	var count = 0;
        for (var i = 0; i < ngO.dimIsExtended.length; i++)
	    if (ngO.dimIsExtended[i]) count ++;

	if (count == 0) { 
		alert("A Global and Distributed grid needs at least one" + 
			"extended dimension. " +
			"Either add a dimension or uncheck " +
			"'Enable Global Sharing across nodes (MPI)'");
		return;
	}

    }
    //TODO: do an assert here to make sure that we have a valid state
    //i.e., if not global, cannot be distributed and have extended dims
    //	    if distributed, must be global
    //	    if we have extended dims, then must be global
    //	    etc.

    // Clear any tips from configuration
    //
    clearTip();

    // Mark config done
    //
    sO.stageInStep = StageInStep.ConfigDone;

    // If we are adding this to the template scope, nothing more
    // to be done. Show the 
    // TODO: Show a message saying the grid is added to the scope
    //
    if (CurFuncObj.isTemplateScope) {

	NewGridObj.isTemplate = true;        // this is template

        // Mark all stages done (trivially) for a template
        //
        NewGridObj.stageInStep = StageInStep.AllDone;

        newNextStep(CurFuncObj.curStepNum);

        return;
    }

    //MPI: TODO: Here need to check if we have extended along
    //all dimensions that have previously been extended in this
    //program's grids (we require the SAME all over the program).


    // Create the actual index object to be added to the step
    //
    var iO = new IndexObj(NewGridObj);
    CurStepObj.allIndObjs.push(iO);

    /*
    alert("New Grid obj row dims:" + iO.dimIndExprs[RowDimId].length + 
	  " showsz:" + NewGridObj.dimShowSize[RowDimId]);
    */

    // Add index variable names AND
    // Add 'row'/'col' etc. to the indices since we have done config
    //
    resetColRowIndices(NewGridObj, iO, true);

    // Clear config menu
    //
    var menuId = "GridConfMenu";                  // grid config menu
    var conf1 = document.getElementById(menuId);    

    conf1.innerHTML = "<table></table>";

    // Draw Out grid non-editable
    //
    drawGrid(NewGridObj, iO, CurHtmlGridId, false);

    // alert("Config done -- cur stage:" + CurStepObj.stageInStep);

    doNextStage();                         // call the sequencer function
}



//----------------------------------------------------------------------------
// Called when "Back" button is pressed while configuring an out grid
//----------------------------------------------------------------------------
function configBack() {

    popNewGrid();

    // Draw the new grid configuration menu to pick an option 
    // NOTE: We may not be able to call the following function always
    //       May need an if condition check to figure out which function
    //       to call after 'Done' is pressed (state machine)
    //
    drawNewGridMenu();

}

//----------------------------------------------------------------------------
// Pop a newly added grid (NewGridObj). This is called while we have started
// a new grid config (StageInStep.ConfigStarted) but have not finished it
//----------------------------------------------------------------------------
function popNewGrid() {

    // Clear config menu
    //
    var menuId = "GridConfMenu";                  // grid config menu
    var conf1 = document.getElementById(menuId);    
    conf1.innerHTML = "<table></table>";
   
    CurStepObj.allGridIds.pop();                 // pop added output grid 
    CurStepObj.allIndObjs.pop();

    CurFuncObj.allGrids.pop();                   // pop from function

    CurStepObj.stageInStep = StageInStep.New;
    clearTip();

}

//----------------------------------------------------------------------------
// Reconfig: User pressed cancel
//----------------------------------------------------------------------------
function reConfigBack() {

    logMsg("drawing current step");

    NewGridObj.beingReConfig = false;

    drawStep(CurStepObj);           // redraw entire step

}

//----------------------------------------------------------------------------
// Reconfig: User pressed Done
//----------------------------------------------------------------------------

function reConfigDone() {

    NewGridObj.beingReConfig = false;

    drawStep(CurStepObj);           // redraw entire step	
    console.log("re-config done");

    return;

}


// --------------------------------------------------------------------------
// Returns a dropdown menu string with all types supported
// NOTE: We don't need a dim because only one dim can have types (typesInDim)
//       ind = column/row index for which this string generated (or rowTypes)
//       selected_index = the already selected index 
// ---------------------------------------------------------------------------
function getTypeSelectStr(ind, selected_index, htmlId) {

        str = "<select onchange='changeType(this," + ind + ",\"" + htmlId +
            "\")'>";

	//DC5: Only show 'void' data type in function header
	// TODO: Also, only allow for return value, not the rest grids
	var typesCount = TypesArr.length - 1;
	if (CurStepObj.isHeader)
	    typesCount++;

	//DC5: Loop end value
        for (var i = 0; i < typesCount; i++) {
            str += "<option value='" + i + "' ";
            if (i == selected_index)
                str += "selected";
            str += ">" + TypesArr[i] + "</option>";
        }
        str += "</select>";

        return str;
    }

// -------------------------------------------------------------------------
// Change type in the NewGridObj based on user selection of a col/row type
// We don't need dim since only one dimension can have types (in typesInDim)
// --------------------------------------------------------------------------
function changeType(sel_obj, ind, htmlId) {

    if (!htmlId) { // while configuring a new grid

        NewGridObj.dataTypes[ind] = parseInt(sel_obj.value);

    } else {

	var gridId = getGridIdOfHtmlId(htmlId);   // find grid id from step
	var gO = CurFuncObj.allGrids[gridId];     // get grid obj from func
	gO.dataTypes[ind] = parseInt(sel_obj.value);
    }
}


//----------------------------------------------------------------------------
// Comments can be in serveral places. Those comment 'types' are
// defined in CommentType enumeration. The 'seqid' is the sequence number
// (or other sequence specifier) of the comment within that type.
//----------------------------------------------------------------------------
function getCommentHtmlId(type, seqid) {
    return "CommentId_" + type + "_" + seqid;
}

//----------------------------------------------------------------------------
// Retruns the comment string
// id = html id of the comment div/span
//----------------------------------------------------------------------------
function getCommentStr(tag, id, opt, msg) {

    // if there is no current 
    var msg1 = (msg) ? msg : "add comment";

    var commentDiv = "<" + tag + " id='" + id + "' " + opt +
        " onclick='getComment(this)'" + " title='" + msg1 + "'>" +
        "<img src='images/comment.png' height=14px width=14px>" + "</" +
        tag + ">";

    return commentDiv;
}

//----------------------------------------------------------------------------
// Get the last index we should show for a dim based on whether we are 
// showing data or not
//----------------------------------------------------------------------------
function getDimEnd(gO, d, start, showData) {

    // Usually, we show indices from start to start+dimShowSize[d]
    //
    var end = start + gO.dimShowSize[d];

    // However, if we are showing data, we have to make sure that we are
    // not showing beyond actual data size
    //
    if (showData && (gO.dimActSize[d] < end)) {
        end = gO.dimActSize[d];
    }
    return end;
}



//------------------- drawGrid (main Grid Drawing Method) --------------------
//
//----------------------------------------------------------------------------
// Draw a Grid (main function for drawing a Grid)
// ASSUMPTIONS:
// Side effects: gridObj.htmlId is set to htmlId. This is done to register
//               htmlId with gridObj within the current step as a temporary
//               measure.
//----------------------------------------------------------------------------
function drawGrid(gridObj, indObj, htmlId, edit) {

    // Create some shortcut names for commonly used members in gridObj
    //    
    var pO = CurProgObj;
    var gO = gridObj;                   // shortcut reference to gridObj
    var iO = indObj;

    var caption = gO.caption;
    //var hasRowTitles = gO.dimHasTitles[RowDimId];
    //var hasColTitles = gO.dimHasTitles[ColDimId];
    //var hasRowIndices = gO.dimHasIndices[RowDimId];
    //var hasColIndices = gO.dimHasIndices[ColDimId];
    var isHeaderStep = CurStepObj.isHeader;

    // We use the show data state of the current Step object by default. 
    // If it is necessary to override this, we can pass in an arg 
    // to this function
    //
    var showData = pO.showData;

    // Draw assignment arrows when we are drawing non-editable output grid.
    // Also, we don't draw arrows for templates
    //
    var drawArrows = (OutHtmlId == htmlId) && !edit && !gO.isTemplate;

    clearTip();

    // Register this htmlId with the grid Object. This assumes that the same
    // gridObj is not drawn on the screen twice -- i.e., we always clone
    //
    gO.htmlId = htmlId;
    //gO.showData = showData;             // record show data status

    var str = "";                       // html code holder



    // STEP A.1: ----------- Draw outermost container -----------------------  
    //
    // This is the outermost container table for all cases. It is also
    // used to draw the 'assignment arrow'. This has the 'htmllId' assigned.
    //
    var id = "id='" + htmlId + "'";          // html id for outermost table
    str += "<table class='outArrow' " + id + " >";

    if (gO.typesInDim < 0) {                     // global type in row1

	// We allow changing the data type while editing a grid OR 
	// for all grids w/ a global type (including return value) in a header
	//
	var type_sel = edit || CurStepObj.isHeader;	
	var tstr = (type_sel)? 
	    getTypeSelectStr(0, gO.dataTypes[0], htmlId ) : 
	    "(" + TypesArr[gO.dataTypes[0]] + ")";

        str += "<tr><td class='type'>" + tstr + "</td></tr>";
    }
    //
    //    
    str += "<tr><td>";                          // row2 for contents/arrows

    // STEP A.2: ----------- Draw Multi-Dimensional Panes --------------------
    //
    for (var d = gO.numDims - 1; d >= 2; d--) { // draw outer dimensions first

        // start an entire tabbed 2D pane
        //
        str += "<table class='tabPane' " + id + " >"; // 2D-pane

	var color = TabColors[(d-2) % TabColors.length]; // tab color & style
	var has_titles = gO.dimHasTitles[d];
	//
	var sel =" class=select ";                      // selected style
	var sty = " style='background-color:#" + color + "' ";
	sel += sty;              
	//
	// non-selected style
	//
	var nosel = " class=noselect style='background-color:#fefefe' ";

        // If the dimension has indices, draw it above titles (and types)
        //
        if (gO.dimHasIndices[d] && (iO || showData)) {
            str += "<tr>";

            // Select the background color of indices
            //
            var sty2 = "";
            if (gO.selIndDim == d) {

                // if this index range highligted
                //
                sty2 = " style='background-color:#dcffed' ";

            } else if (d < gO.numDims - 1) {

                // if not the outermost dim, use the background color of the
                // outer dim (d+1) -- not TabColors[0] is for dim 2
                //
                var color = TabColors[d - 2 + 1];
                sty2 = " style='background-color:#" + color + "' ";
            }

            // Find which tabs to show if we are showing data
            //
            var tab_start = (showData) ? gO.dimShowStart[d] : 0;
            var tab_end = getDimEnd(gO, d, tab_start, showData);
            //
            for (var t = tab_start; t < tab_end; t++) {
                var onc_args = "this, '" + htmlId + "'," + d + "," + t;
                onc = " onmousedown=\"gridIndMouseDown(" + onc_args +
                    ")\" ";
                onc += " onmouseup=\"gridIndMouseUp(" + onc_args + ")\" ";

                var istr = (showData) ? t : getIndExprStr(iO.dimIndExprs[d][t]);
                //
                // Draw left/right arrows for scrolling a larger matrix
                //
                if (showData && (t == tab_start) && (tab_start > 0)) {

                    var newstart =
                        gO.dimShowStart[d] - gO.dimShowSize[d];
                    if (newstart < 0) newstart = 0;
                    //
                    var args = "'" + htmlId + "'," + d + "," + newstart;
                    var click = " onclick=\"scrollData(" + args + ")\" ";
                    //
                    istr = "<img src='images/larrow1.png' " + click +
                        " width=16px height=16px>" + istr;


                } else if (showData &&
                    (t == (tab_end - 1)) && (tab_end < gO.dimActSize[d])) {

                    var args = "'" + htmlId + "'," + d + "," + tab_end;
                    var click = " onclick=\"scrollData(" + args + ")\" ";
                    //
                    istr += "<img src='images/rarrow1.png' " + click +
                        "width=16px height=16px>";
                }

                str += "<th class='index' " + sty2 + onc + ">" + istr +
                    "</th>";
            }
            str += "</tr>";
        }


        // draw tab TYPES, if the types are in this dimension
        //
        if (gO.typesInDim == d) {
            str += "<tr>";
            for (var t = 0; t < gO.dimShowSize[d]; t++) {
                var tstr = (edit) ?
                    getTypeSelectStr(t, gO.dataTypes[t], htmlId) : "";
                str += "<td class='type'>" + tstr + "</td>";
            }
            str += "</tr>";
        }

        // draw tab heads. If the current tab is selected, it is drawn 
        // differently than the other (un-selected) tabs.
        //
        str += "<tr>";
        for (var t = 0; t < gO.dimShowSize[d]; t++) {

            var prop = (t == gO.dimSelectTabs[d]) ? sel : nosel;

            // create the call for event handler. If we are editing, we 
            // can change the tab and edit the title. Otherwise, just change
            // the tab.
            //
            if (edit && has_titles) { // editable titles
                //
                var args = "this, '" + htmlId + "'," + d + "," + t + "," +
                    edit;
                prop += " contenteditable oninput=\"changeTabTitle(" +
                    args + ",0)\"" + " onclick=\"changeTabTitle(" + args +
                    ",1)\" "; + " onblur=\"changeTabTitle(" + args +
                    ",2)\" ";

            } else { // NO-tiltles OR non-editable titles - allow tab change

                var args = "this, '" + htmlId + "'," + d + "," + t + "," +
                    edit;
                prop += " onclick=\"selectTab(" + args + ")\" ";
            }
            //
            // if the tab has titles, use it. Otherwise, put an empty
            // cell - note we cannot put a div in 'cont' because it is picked up
            // by cell formula.
            //
            var cont = (has_titles) ? gO.dimTitles[d][t] : "";
            str += "<td " + prop + ">" + cont + "</td>";

        }
        str += "</tr>";

        // start the contents cell. We draw another 2D pane, or a 2D grid
        // in a 'content' cell
        //
        str += "<tr><td class='contents' " + sty + " colspan=" 
	    + gO.dimShowSize[d] + ">";
    }
    //
    // END STEP A: ----- End of Multi-Dimensional Panes -------------------



    // STEP ................................................................
    //
    // Draw the inner 2D grid of type 'gridTable' for each higher dimensional
    // tab. If we are showing data image, just get a canvas for drawing it --
    // the canvas will be filled at the end of this function.
    //
    if (showData == ShowDataState.DataImage)
        str += getCanvasFor2DGrid(gridObj, htmlId);
    else
        str += drawInner2DGrid(gridObj, indObj, htmlId, edit, showData);


    // STEP Z: --------------- Close Multi-Dimensional Pane(s) --------------
    //
    for (var d = 2; d < gO.numDims; d++) {
        str += "</td></tr></table>";
    }


    // STEP : ...............................................................
    //    
    // Close the outermost container table (of type 'outArrow') after
    // putting in the assign arrow image AND caption of table

    //
    var img = ""; // draw Assign Arrow
    if (drawArrows) {
        img = "<img src='images/bigarrow.jpg' width=60px height=60px>";
    }
    //
    //
    str += "</td><td>" + img + "</td></tr>"; // close row2 of outermost table

    // STEP: Row for config button / context menu
    //
    var configbut = (edit || showData) ? "" :
        getGridConfContextMenu(htmlId, gO.isConst);
    //
    str += "<tr><td class='gridconfbut'>" + configbut + "</td></tr>";


    // Draw the caption cell at the bottom row of Multi-Dimension Pane
    // First, create a contenteditable caption (in div) an add oninput function
    //
    var attrib = "";
    if (gO.inArgNum >= 0 && (gO.deleted == DeletedState.None)) {
        attrib += "<span class='captionParam'> (Parameter " + gO.inArgNum +
            ") </span>";
    }
    //
    var oninp = "";
    var indargs = "";         // index paramenters for grid cell reference
    //
    var isCapEdit = (edit || isHeaderStep) && !gO.beingReConfig;
    //
    if (isCapEdit) {          // if caption is editable
	var args = "this, '" + htmlId + "'";
	//var args = "this";
	//oninp = " oninput=\"changeCaption(" + args + ")\" contenteditable ";
	oninp = " onblur=\"changeCaption(" + args + ")\" contenteditable ";

    } else {                  // if caption is NOT editable

        // When the user clicks on a grid name, insert a grid reference
        // (mainly as a function arg) 
        //
        var args = "'" + htmlId + "'";
        oninp = " onclick=\"putGridRef(" + args + ")\" ";
        oninp += " style='cursor:pointer' ";

        //if (gO.isGridCellRef) {                   // TODO:
        //    indargs = getGridCellIndArgs(gO);
        //}

    }


    // Draw grid specific buttons at the bottom right corner of the grid
    //
    var configbut = ""; //"<img src='images/gear.jpg' width=15px height=15px " +
    " title='re-configure grid'" + ">";
    if (edit || showData) configbut = "";

    var buttons = "<div class='gridButDiv'>" + configbut + "</div>";

    /*
    //
    // Navaigation buttons for scrolling through data grids
    // TODO: complete these.
    // 
    var leftarr = "", rightarr = "";
    if (showData) {
	leftarr = "<span style='float:left'><img src='images/larrow.jpg'" +
	    " width=16px height=16px></span>";

	rightarr = "<span style='float:right'><img src='images/rarrow.jpg'" + 
	    " width=16px height=16px> </span>";
    }
    */

    str += "<tr><td class='caption'>" +
        "<span " + oninp + ">" + caption + "</span>" + indargs + "" +
        buttons + "</td></tr>" + "<tr><td>" + attrib + "</td></tr>";
    // 
    str += "</table>"; // close outermost table


    // STEP : update html object .............................................
    // Get the HTML object in the gridObj and update its innerHTML
    //
    var grid1 = document.getElementById(htmlId);
    grid1.innerHTML = str; // update HTML of grid
    grid1.className = 'outArrow';      // change class of htmlId



    var grid2d = document.getElementById(htmlId + "2dgrid");
    grid2d.className = 'gridTable';    // change class of 2d grid table


    // STEP
    // If we are showing the data image, draw the data on a canvas 
    //
    if (showData == ShowDataState.DataImage) {
        draw2DGridOnCanvas(gridObj, htmlId);
    }
}


//----------------------------------------------------------------------------
// Draw the data on the actual canvas for grid 
//----------------------------------------------------------------------------
function draw2DGridOnCanvas(gO, htmlId) {


    var canvas = document.getElementById(htmlId + "canvas2d");
    var canv = canvas.getContext("2d");

    // Go thru each row
    //
    var row_start = 0;
    var row_end = gO.dimActSize[RowDimId];
    //
    for (var r = row_start;
        (r < row_end); r++) {

        // Go thru each column
        //
        var col_start = 0;
        var col_end = gO.dimActSize[ColDimId];
        //
        for (var c = col_start; c < col_end; c++) {

            // Get the data value of the cell and its corresponding color
            //
            var val = getDataValOfCell(gO, r, c);
            val = (val === undefined) ? "" : val;
            //
            var hexcol = getHexColor(val, CurStepObj.maxDataVal);

            canv.fillStyle = "#" + hexcol;

            var scaleX = getImageScale4Grid(gO);
            var scaleY = scaleX;

            canv.fillRect(c * scaleX, r * scaleY, scaleX, scaleY);
        }

    }
}


//----------------------------------------------------------------------------
// Returns a scaling factor for a grid, for showing data image. The scale
// is based on the actual nubmer of data points in the grid
//----------------------------------------------------------------------------
function getImageScale4Grid(gO) {

    var MaxXsz = 200;         // this many pixels max in x direction
    var MaxYsz = 200;         // this many pixels max in x direction

    // Find X-scale
    //
    var cols = gO.dimActSize[ColDimId];
    var xscale = (MaxXsz > cols) ? Math.floor(MaxXsz / cols) : 1;

    // Fin Y-scale
    //
    var rows = gO.dimActSize[RowDimId];
    var yscale = (MaxYsz > rows) ? Math.floor(MaxYsz / rows) : 1;

    // Retrun minimum value out of the two scales
    //
    return ((yscale < xscale) ? yscale : xscale);

}

//----------------------------------------------------------------------------
// Returns a canvas for drawing the 'data image'
//----------------------------------------------------------------------------
function getCanvasFor2DGrid(gO, htmlId) {

    var str = "";

    // STEP : ...............................................................
    // table for the 2D grid (gridTable)
    //
    str += "<table class='gridTable' id='" + htmlId + "2dgrid'>";
    //
    str += "<tr><td>";

    // Determine the size of the canvas based on the actual number of data
    // points
    //
    var scale = getImageScale4Grid(gO);
    var width = scale * gO.dimActSize[ColDimId];
    var height = scale * gO.dimActSize[RowDimId];
    var can_name = htmlId + "canvas2d";

    // HTML5 code for canvas
    //
    str += "<canvas id='" + can_name + "' width='" + width + "' height='" +
        height + "' style='border:1px solid #d4e3e5;'>" + " </canvas>";


    str += "</td></tr>";
    str += "</table>"; // close grid table


    return str;

}


//----------------------------------------------------------------------------
// draw inner 2D grid (gridTable). We have to do this for each higher 
// dimensional tab. 
//----------------------------------------------------------------------------
function drawInner2DGrid(gO, iO, htmlId, edit, showData) {


    // Create some shortcut names for commonly used members in gridObj
    //    
    var pO = CurProgObj;

    var hasRowTitles = gO.dimHasTitles[RowDimId];
    var hasColTitles = gO.dimHasTitles[ColDimId];
    var hasRowIndices = gO.dimHasIndices[RowDimId];
    var hasColIndices = gO.dimHasIndices[ColDimId];
    var isHeaderStep = CurStepObj.isHeader;
    var isGlobal = gO.isGlobal; //MPI:
    var isDistributed = gO.isDistributed; //MPI:
    var dimIsExtended = gO.dimIsExtended; //MPI:
    //
    var colorize = (showData == ShowDataState.DataAndColor);
    //
    //
    var perColTy = (gO.typesInDim == ColDimId);
    var perRowTy = (gO.typesInDim == RowDimId);


    var str = "";

    // STEP : ...............................................................
    // table for the 2D grid (gridTable)
    //
    str += "<table class='gridTable' id='" + htmlId + "2dgrid'>";
    //
    // STEP 1: ...............................................................
    // Draw indices (or types) over columns.
    // Note: ColIndices/RowIndices contain indicies and/or types
    //       based on the type of the Grid
    //
    // start a new row and leave  columns for row labeles + indices/types
    //


    // STEP 1A: Draw indices .................................................
    //
    // Top Left Corner Cell always present -- in the same row as
    // column indices/titles AND same column as row indices/titles 
    //
    //var configbut = "<img src='images/gear.jpg' width=20px height=20px " +
    //	" title='re-configure grid'" + ">";


    var commentId = getCommentHtmlId(CommentType.Grid, htmlId);
    var commentSpan =
        getCommentStr('span', commentId, " style='margin:4px' ", gO.comment);
    var topleft = commentSpan;

    // STEP 2a: Draw column indices if required
    //
    var indstr = "";

    if (hasColIndices && (iO || showData)) {    // draw column indices 

        var col_start = (showData) ? gO.dimShowStart[ColDimId] : 0;
        var col_end = getDimEnd(gO, ColDimId, col_start, showData);

        // if showing data, draw left arrow to scroll. The left arrow is
        // drawn if and only if there are columns to the left to be shown
        // NOTE: The left arrow (when present) replaces table comment
        //
        if (showData && (col_start > 0)) {

            var newstart = gO.dimShowStart[ColDimId] - gO.dimShowSize[
                ColDimId];
            if (newstart < 0) newstart = 0;
            //
            var onc_args = "'" + htmlId + "'," + ColDimId + "," +
                newstart;
            var onc = " onclick=\"scrollData(" + onc_args + ")\" ";
            //
            topleft = "<img src='images/larrow1.png' " + onc +
                " width=16px height=16px>";
        }

        // skip cell for row types/titles
        //
        if (perRowTy) // if types present
            indstr += "<td class='topLeft'></td>";
        if (hasRowTitles && hasRowIndices) // if both titles/indices
            indstr += "<td class='topLeft'></td>";

        for (var c = col_start;
            (c < col_end) && gO.multiCol; c++) {

            // Draw column index cells
            //
            var onc = "";
            if (!edit) {
                var onc_args = "this, '" + htmlId + "'," + ColDimId + "," +
                    c;
                onc = " onmousedown=\"gridIndMouseDown(" + onc_args +
                    ")\" ";
                onc += " onmouseup=\"gridIndMouseUp(" + onc_args + ")\" ";
            }

            // whether to highlight col indices
            //
            var ishigh = (gO.selIndDim == ColDimId);
            var indclass = (ishigh) ? "colindexhigh" : "colindex";

            // if showing data, just use column number 
            //
            var indname = (showData) ? c :
                getIndExprStr(iO.dimIndExprs[ColDimId][c]);
            indstr += "<td class='" + indclass + "' " + onc + ">" +
                indname + "</td>";
	    //MPI: 'indname' shows 0, col, end1 (for columns in 2D)
	    //     if NOT showing data (in which case it takes them
	    //     from iO.dimIndExprs[][]. Otherwise shows col number
	    //     Num columns it gets from col_start/col_end that take
	    //     it from dimShowStart[] if showing data or 0, and
	    //     getDimEnd() for end respectively.

        }

        // Draw a right arrow to scroll to right. Draw the arrow if and only if
        // there is more data to show
        // 
        if (showData && (col_end < gO.dimActSize[ColDimId])) {

            var onc_args = "'" + htmlId + "'," + ColDimId + "," + col_end;
            var onc = " onclick=\"scrollData(" + onc_args + ")\" ";
            //
            indstr += "<td><img src='images/rarrow1.png' width=16px " +
                " height=16px " + onc + " ></td>";
        }
    }
    //
    str += "<tr> <td class='topLeft' >" + topleft + "</td>" + indstr +
        "</tr>";


    // STEP 2b: ...............................................................
    // Draw column titles (and types), if available
    //
    if (hasColTitles) { // draw column titles .................

        // Since we have column titles, the number of columns we show 
        // are fixed
        //
        var showCols = gO.dimShowSize[ColDimId];

        // Draw per-column data types, if needed .....................
        //
        // Note: Col Types can be present only when there are col titles
        // Draw column types if necessary
        //
        if (perColTy) { // if data type in every col

            // skip cells for row indices/types/titles
            //
            if (hasRowIndices)
                str += "<tr> <td class='topLeft'></td>";
            if (hasRowTitles)
                str += "<td class='topLeft'></td>";

            // put type over each column
            //
            for (var c = 0; c < showCols; c++) {
                var sel = "(" + TypesArr[gO.dataTypes[c]] + ")";
                if (edit) sel = getTypeSelectStr(c, gO.dataTypes[c],
                    htmlId);
                str += "<td class='colindex'>" + sel + "</td>";
            }

            str += "</tr>"; // close index/type row
        }


        // Now draw actual COLUMN TITLES
        //
        str += "<tr>"; // start col-titles row
        //
        if (hasRowIndices) str += "<td class='topLeft'></td>";
        if (perRowTy) str += "<td class='topLeft'></td>";
        if (hasRowTitles) str += "<td class='topLeft'></td>";

        for (var c = 0; c < showCols; c++) {

            var onc_args = "this, '" + htmlId + "'," + ColDimId + "," + c;
            var fun = (!edit) ? " onclick=\"titleClicked(" + onc_args +
                ")\"" :
                " oninput='changeColTitle(this," + c +
                ")' contenteditable";
            //
            var tip = (edit) ? " title='click to edit' " : "";
            var sty = " style='padding:4px' ";
            var divstr = "<div " + fun + tip + sty + ">" +
                gO.dimTitles[ColDimId][c];

            var commentId = getCommentHtmlId(CommentType.DimTitle +
                ColDimId, c);
            var cur_comment = gO.dimComments[ColDimId][c];
            var commentDiv = getCommentStr('div', commentId, "",
                cur_comment);

            str += "<td class='coltitle'>" + divstr + "</div>" +
                commentDiv + "</td>";
        }
        str += "</tr>"; // close column title row

    } // if there are column titles



    // STEP 3: ...............................................................
    // Draw each row (types/indices/titles/content-cells)
    //
    var row_start = (showData) ? gO.dimShowStart[RowDimId] : 0;
    var row_end = getDimEnd(gO, RowDimId, row_start, showData);

    for (var r = row_start;
        (r < row_end); r++) {

        str += "<tr>"; // start content row


        if (hasRowIndices && (iO || showData)) { // draw index in cell
            var onc = "";
            if (!edit) {
                var onc_args = "this, '" + htmlId + "'," + RowDimId + "," +
                    r;
                onc = " onmousedown=\"gridIndMouseDown(" + onc_args +
                    ")\" ";
                onc += " onmouseup=\"gridIndMouseUp(" + onc_args + ")\" ";
            }



            // whether to highlight row indices
            //
            var ishigh = (gO.selIndDim == RowDimId);
            //
            var indclass = (ishigh) ? "rowindexhigh" : "rowindex";

            // index string. If showing data, it is just the row number
            //
            var istr = "";
            //
            if (showData) {

                // Draw the up and down arrows, when we are showing data -- for
                // scrolling large matrices
                // Note: The arrows are shown if and only if there are more
                //       rows to be seen in the given direction
                //
                if ((r == row_start) && (row_start > 0)) {

                    var newstart =
                        gO.dimShowStart[RowDimId] - gO.dimShowSize[
                            RowDimId];
                    if (newstart < 0) newstart = 0;
                    //
                    var args = "'" + htmlId + "'," + RowDimId + "," +
                        newstart;
                    var onc = " onclick=\"scrollData(" + args + ")\" ";
                    //
                    istr += "<img src='images/uparrow1.png' " + onc +
                        " width=16px height=16px>";

                } else if (r == (row_end - 1) &&
                    (row_end < gO.dimActSize[RowDimId])) {

                    var args = "'" + htmlId + "'," + RowDimId + "," +
                        row_end;
                    var onc = " onclick=\"scrollData(" + args + ")\" ";
                    //
                    istr += "<img src='images/downarrow1.png' " + onc +
                        " width=16px height=16px>";
                }

                istr += "" + r; // row number whiles showing data

                //logMsg("rowIndices w/ showdata: " + hasRowIndices +" "+istr);


            } else if (gO.multiRow && iO) {
                istr = getIndExprStr(iO.dimIndExprs[RowDimId][r]);
		//MPI: 'istr' shows 0, row, end0 (if showing data, then
		//     it has endered the "if" not this "else if"...
            }

            var rind_str = "<td class='" + indclass + "' " + onc + ">" +
                istr + "</td>";

            // add row index only if multi-row
            //
            var scalar_str = "<td class='rowindex'></td>"; // empty cell
            str += (gO.multiRow) ? rind_str : scalar_str;

        } else if (hasRowIndices) {

            // We have row indices but no index object (e.g., for Header step)
            // Just draw an empty row-index cell.
            //
            str += "<td class='rowindex'></td>"; // empty cell

        }



        // Draw row types/titles, if present
        //
        if (hasRowTitles) { // draw TYPE in cell when row titles

            // if data type is in every row, draw it now 
            //
            if (perRowTy) { // data type in every row

                str += "<td class='rowindex'>";
                //
                var sel = "(" + TypesArr[gO.dataTypes[r]] + ")";
                if (edit) sel = getTypeSelectStr(r, gO.dataTypes[r],
                    htmlId);
                str += sel;
                //
                str += "</td>";
            }

            // Now Draw the actual title
            //
            var fun = (!edit) ? "" :
                " oninput='changeRowTitle(this," + r +
                ")' contenteditable";
            //
            var tip = (edit) ? " title='click to edit' " : "";
            var sty = " style='padding:4px' ";
            var tstr = " <span " + fun + tip + sty + ">" +
                gO.dimTitles[RowDimId][r];

            var comId = getCommentHtmlId(CommentType.DimTitle + RowDimId,
                r);
            var cur_comment = gO.dimComments[RowDimId][r];
            var comSpan = getCommentStr('span', comId,
                " style='margin:4px' ", cur_comment);

            str += "<td class='rowtitle'>" + comSpan + tstr + "</span>" +
                "</td>";

        } // if row titles (and types)


        // If this is a scalar grid, there are no row types/titles/indices
        // So, to move it right of the the topLeft comment, add a dummy
        // cell (beacause we assume there is always row titles/indices
        // will be present
        //
        if (gO.numDims == 0)
            str += "<td></td>";


        // Now draw CONTENT cells for all columns in this row
        //
        var col_start = (showData) ? gO.dimShowStart[ColDimId] : 0;
        var col_end = getDimEnd(gO, ColDimId, col_start, showData);
        //
        for (var c = col_start; c < col_end; c++) {

            // Select default cell class based on the row
            //
            var cell_class = (r % 2) ? " class='odd' " : " class='even' ";


	    //MPI: Show dotted cells type for extended dimensions' cells
	    //     Only when editing a grid
	    if (edit) {
	        if (gO.dimIsExtended[RowDimId] && 
		    !gO.dimIsExtended[ColDimId]) {

	            if (r < 2 || r >= row_end - 2)
		        cell_class = " class='extended' ";

	        } else if (gO.dimIsExtended[ColDimId] && 
		           !gO.dimIsExtended[RowDimId]) {

	 	    if (c < 2 || c >= col_end - 2)
		        cell_class = " class='extended' ";

	        } else if (gO.dimIsExtended[RowDimId] && 
		           gO.dimIsExtended[ColDimId]) {

	  	    if ( (r < 2 || r >= row_end -2) || 
		         ( (r >= 2 || r <= row_end - 2) && 
		           (c<2 || c >= col_end - 2)) )
		        cell_class = " class='extended' ";

	        }
	    }


            var tip = "",
                val = "",
                prop = "";
            if (!edit && !showData && !isHeaderStep && iO) {
                var cel = getCellExpr(gO, iO, r, c); // array notation string
                if (cel.length) { // if not empty
                    var args = "'" + htmlId + "'," + r + "," + c;
                    var onc = " onclick=\"putCellInd(" + args + ")\"";
                    tip = "title='" + cel + "' " + onc;
                }
            }

            // if initilizatio of data is required, make cells contentedible.
            //
            if (edit && gO.hasInitData) {

                /*
		val = getDataVal(gO.numDims-1, gO.data, r, c, gO.dimSelectTabs);
		val = (val === undefined) ? "" : val;
		*/

                prop += " contenteditable ";

            }


            // Highlight any selected cells.
            // We highlight only if an expression is selected in the 
            // code window. 
            //
            var ishigh = gO.hasSelection && !showData;
            var cell_high = false;
            //
            if ((gO.dimSelectTabs[RowDimId] == r) && ishigh &&
                (gO.dimSelectTabs[ColDimId] == c)) {
                cell_class = " class='select' ";
                cell_high = true;
            }
            //
            if (showData && !edit && gO.data) { // data to show

                val = getDataValOfCell(gO, r, c);

                val = (val === undefined) ? "" : val;
                if (colorize && !cell_high) {
                    var hexcol = getHexColor(val, CurStepObj.maxDataVal);
                    cell_class += " style='background-color:#" + hexcol +
                        "' ";
                }
            }
            //
            str += "<td " + cell_class + tip + prop + ">" + val + "</td>";
        }

        // Insert/Delete Row. 
        //
        if (!edit) {
            str += "<td  class='insrow' onclick='sliceClicked(this," +
                RowDimId + "," + r + ",\"" + htmlId + "\")'></td>";
        }
        //
        // close the row 
        //
        str += "</tr>";

    } // for all rows

    str += "</table>"; // close grid table


    return str;

}




//----------------------------------------------------------------------------
// Whenever a user selects a 'slice' (i.e., a row/col/tab) for the purpose
// of inserting/deleting that slice, we display the following menu.
// Note that we re-use the same span (gcMenuBody) used by grid context menu
// to display this menu too. 
//----------------------------------------------------------------------------
function sliceClicked(obj, dim, pos, htmlId) {

    var sO = CurStepObj;

    //
    // This is the menu body with options, formatted as a table
    //
    var str =
        "<span class='GCmenuBody' id='gcMenuBody'>" +
        "<table class='gcMenu'>"

    +"<tr><td onclick='insertSlice(\"" + htmlId + "\"," + dim + "," + pos +
        ")'>" + "Insert</td></tr>"

    + "<tr><td onclick='deleteSlice(\"" + htmlId + "\"," + dim + "," +
        pos + ")'>" + "Delete</td></tr>"


    + "</table>"; + "</span>";


    // Get the bounding rectangle of 'obj'
    // and use that bottom/left position to place the body of the menu
    //
    var rect = obj.getBoundingClientRect();

    // Menu body with options
    //
    var body1 = document.getElementById('gcMenuBody');

    body1.style.top = rect.bottom + 'px';
    body1.style.left = rect.left + 'px';

    // set the innerHTML of the body. Note that the head remains as is
    //
    body1.innerHTML = str;              // update HTML of menu body
    body1.className = 'GCmenuBody';     // change class of htmlId
   
}


//----------------------------------------------------------------------------
// Insert a row before the pos. 
//----------------------------------------------------------------------------
function insertSlice(htmlId, dim, pos) {

    var sO = CurStepObj;
    var fO = CurFuncObj;

    var gridIdInStep = getGridPosInStep4OfHtmlId(htmlId);
    var iO = sO.allIndObjs[gridIdInStep];
    var gO = fO.allGrids[sO.allGridIds[gridIdInStep]];

    // Insert an empty (null) index, if the dim has indices
    //
    if (gO.dimHasIndices[dim]) {

        if (pos == 0) {
            alert("Cannot insert BEFORE 0th entry");
            return;
        }

        // alert("adding entry @ pos " + pos + " for:" + gO.caption + " sz:" +
        //     (iO.dimIndExprs[dim]).length);


        //
        iO.dimIndExprs[dim].splice(pos, 0, null); // add 'null' index
    }


    // if the dim has titles
    //
    if (gO.dimHasTitles[dim]) {

        gO.dimTitles[dim].splice(pos, 0, DefTitleRoots[dim] + pos);
        gO.dimComments[dim].splice(pos, 0, null);
    }



    // increment the show size and redraw
    //
    gO.dimShowSize[dim] ++;
    drawGrid(gO, iO, gO.htmlId, false);
    drawCodeWindow(sO);

}


//----------------------------------------------------------------------------
// Insert a row before the pos. 
//----------------------------------------------------------------------------
function deleteSlice(htmlId, dim, pos) {

    var sO = CurStepObj;
    var fO = CurFuncObj;

    var gridIdInStep = getGridPosInStep4OfHtmlId(htmlId);
    var iO = sO.allIndObjs[gridIdInStep];
    var gridIdInFunc = sO.allGridIds[gridIdInStep];

    var gO = fO.allGrids[gridIdInFunc];


    // delete index at position if the dim has indices
    //
    if (gO.dimHasIndices[dim]) {

        if ((pos == 0) || (pos >= gO.dimShowSize[dim] - 1)) {
            alert("Cannot remove first/last enetry");
            return;
        }

        // mark all expressions that refer to this grid slice
        // with DELETED marker. 
        // Note: this grid can be present in
        // multiple steps, and each of them can have separate index
        // objects
        //
        markDeletedIndexExpr(gridIdInFunc, dim, pos);

        /*
	// Mark the current expression as deleted. 
	// TODO: Do this for ALL STEPS with this grid object

	var iexpr = iO.dimIndExprs[dim][pos];
	//
	if (iexpr) {

	    iexpr.deleted = true;
	}

	// alert("adding entry @ pos " + pos + " for:" + gO.caption + " sz:" +
	//     (iO.dimIndExprs[dim]).length);
	
	//
	iO.dimIndExprs[dim].splice(pos, 1); // delete one index

	*/

    }

    // if the dim has titles
    //
    if (gO.dimHasTitles[dim]) {

        gO.dimTitles[dim].splice(pos, 1);
        gO.dimComments[dim].splice(pos, 1);

    }


    // decrement the show size and redraw
    //
    gO.dimShowSize[dim] --;

    drawGrid(gO, iO, gO.htmlId, false);
    drawCodeWindow(sO);
}



//----------------------------------------------------------------------------
// When a slice (e.g., a row) of a grid is deleted, that grid can be present in
// multiple steps, and each of them can have separate index objects that
// need to be updated
// Mark all expressions that refer to gId slice given by <dim, pos>
// with DELETED marker.
//----------------------------------------------------------------------------
function markDeletedIndexExpr(gId, dim, pos) {


    var fO = CurFuncObj;

    // go thru each step in func
    //
    for (var s = 0; s < fO.allSteps.length; s++) {

        var stepO = fO.allSteps[s];

        // go thru each grid in step
        //
        for (var g = 0; g < stepO.allGridIds.length; g++) {

            var gridIdInFunc = stepO.allGridIds[g];

            if (gId == gridIdInFunc) { // if this is the same grid

                // index obj in step as the same position of required grid
                //
                var iO = stepO.allIndObjs[g];

                // Mark the current expression as deleted. 
                //
                var iexpr = iO.dimIndExprs[dim][pos];
                //
                if (iexpr) { // mark expr as deleted
                    iexpr.deleted = DeletedState.Deleted;
                }

                // Remove the index from index object 
                //
                iO.dimIndExprs[dim].splice(pos, 1); // delete one index   
            }
        }

    }

}


//----------------------------------------------------------------------------
// String for showing grid configuration context menu
//----------------------------------------------------------------------------
function getGridConfContextMenu(htmlId, gOconst) {

    var sO = CurStepObj;

    // if this is a constant grid, display a padlock 
    //
    var lock = "";
    if (gOconst) {
        var onc = " onclick='unlockGrid(\"" + htmlId + "\", " + gOconst +
            ")' ";
        //
        lock = "<img src='images/padlock.png' width=15px height=15px " +
            " title='grid is read-only'" + onc + ">";

    } else if (sO.isHeader) {

	var onc = " onclick='lockGrid(\"" + htmlId + "\", " + gOconst +
            ")' ";
        //
        lock = "<img src='images/unlocked.png' width=15px height=15px " +
            " title='grid can be modified'" + onc + ">";
    }

    // Put the grid config (gear wheel) button
    // 
    var onc = " onclick='getGridConfContxtMenuStr(this,\"" + htmlId +
        "\")' ";
    //
    var str = "<img src='images/gear.jpg' width=15px height=15px " +
        " title='re-configure grid'" + onc + ">" +
        "<span id='gcMenuBody'></span>"; // body span of the menu

    return lock + str;
}


//----------------------------------------------------------------------------
// Locks a grid in header
//----------------------------------------------------------------------------
function unlockGrid(htmlId, isConst) {

    var fO = CurFuncObj;
    var sO = CurStepObj;
    //
    var gridIdInF = getGridIdOfHtmlId(htmlId);       // find grid object
    var gO = fO.allGrids[gridIdInF];

    // if this is a constant grid, we allow changing it to a non-const grid
    // for a function argument (in the header)
    //
    if (isConst && sO.isHeader) {
        gO.isConst = false;
        // alert("gO made writable");
    }

    var gridIdInStep = getGridPosInStep4OfHtmlId(htmlId);
    var iO = sO.allIndObjs[gridIdInStep];

    drawGrid(gO, iO, gO.htmlId, false);
}

//----------------------------------------------------------------------------
// unlock a grid in header
//----------------------------------------------------------------------------
function lockGrid(htmlId, isConst) {

    var fO = CurFuncObj;
    var sO = CurStepObj;
    //
    var gridIdInF = getGridIdOfHtmlId(htmlId);       // find grid object
    var gO = fO.allGrids[gridIdInF];

    // if this is a non-constant grid, we allow changing it to a const grid
    // for a function argument (in the header)
    //
    if (!isConst && sO.isHeader) {
        gO.isConst = true;
        // alert("gO made writable");
    }

    var gridIdInStep = getGridPosInStep4OfHtmlId(htmlId);
    var iO = sO.allIndObjs[gridIdInStep];

    drawGrid(gO, iO, gO.htmlId, false);
}


//----------------------------------------------------------------------------
// Grid configuration context menu string
//----------------------------------------------------------------------------
function getGridConfContxtMenuStr(obj, htmlId) {

    var sO = CurStepObj;

    //
    // This is the menu body with options, formatted as a table
    //
    var str =
        "<span class='GCmenuBody' id='gcMenuBody'>" +
        "<table class='gcMenu'>"

    +"<tr><td onclick='gcMenuClicked(" + GCMenu.ShowSizes + ",\"" +
        htmlId + "\")'>" + "Change&nbsp;Sizes&nbsp;...</td></tr>"

    + "<tr><td onclick='gcMenuClicked(" + GCMenu.Delete + ",\"" + htmlId +
        "\")'>" + "Delete</td></tr>"

    + "</table>"; + "</span>";


    // Get the bounding rectangle of 'obj'
    // and use that bottom/left position to place the body of the menu
    //
    var rect = obj.getBoundingClientRect();

    // Menu body with options
    //
    var body1 = document.getElementById('gcMenuBody');

    body1.style.top = rect.bottom + 'px';
    body1.style.left = rect.left + 'px';

    // set the innerHTML of the body. Note that the head remains as is
    //
    body1.innerHTML = str;              // update HTML of menu body
    body1.className = 'GCmenuBody';      // change class of htmlId

}

//----------------------------------------------------------------------------
// Remove grid context menu
//----------------------------------------------------------------------------
function removeGridContextMenuBody() {

    var body1 = document.getElementById('gcMenuBody');
    if (body1) body1.innerHTML = "<span id='gcMenuBody'></span>";
}

//----------------------------------------------------------------------------
// Pick an action to execute when an option in the grid-configure context menu
// is clicked
//----------------------------------------------------------------------------
function gcMenuClicked(item, htmlId) {

    // alert("Clicked id:" + htmlId);

    removeGridContextMenuBody();

    switch (item) {

        case GCMenu.ShowSizes:
            changeGridSizes(htmlId);
            break;

        case GCMenu.Delete:
            deleteGrid(htmlId);
            break;
    };
}

//----------------------------------------------------------------------------
// Change grid sizes 
//----------------------------------------------------------------------------
function changeGridSizes(htmlId) {

    var sO = CurStepObj;
    var fO = CurFuncObj;

    var gridIdInStep = getGridPosInStep4OfHtmlId(htmlId);
    var iO = sO.allIndObjs[gridIdInStep];

    // For a source grid: Only the show sizes of the original grid can be
    // changed. Once changed, show sizes apply to all users of that grid

    // For a destination grid, we can change actual sizes as well. Can add
    // and delete titles/tabs as well. A change applies to all users of 
    // that grid.

    // Alternative: can only add titles/dims, etc. Deleting something is
    // just making it hidden.


    var gridIdInF = getGridIdOfHtmlId(htmlId);       // find grid object
    var gO = fO.allGrids[gridIdInF];



    // We are reconfiguring an existing grid object
    //
    gO.beingReConfig = true;
    initHtmlGridIds();
    CurHtmlGridId = OutHtmlId;

    // Configuring routines use  NewGridObj and CurIndObj
    // NOTE: Configure routines don't use an IndexObj. So, no changes to the
    //       IndexObj corresponding to this gO will be done. 
    //
    NewGridObj = gO;                    // point to existing gO 

    CurIndObj = iO;

    drawGrid(gO, CurIndObj, CurHtmlGridId, true);

    drawGridReConfigMenu(gO);
}

//----------------------------------------------------------------------------
// Deletes a grid
//----------------------------------------------------------------------------
function deleteGrid(htmlId) {

    var sO = CurStepObj;
    var fO = CurFuncObj;

    // Note: Since all grids (including global 'reference copies' and 
    // 'template copies' refer to a grid within this function. Therefore,
    // searching within this function is sufficient.
    //
    //var gridIdInF = getGridIdOfHtmlId(htmlId);       // find grid object
    //var gO = fO.allGrids[gridIdInF];


    if (OutHtmlId == htmlId) {

        alert("Cannot delete output grid. Please delete entire step");

    } else {

        // TODO: Check for any code box expression that is refering to this
        // grid. If one found, 2 optins:
        // (1) Ask the user to delete all dependent expressions first
        // (2) Delete all dependent expressions after warning the user
        // (3) Make dependent expressions refer to a grid named "DELETED" --
        //     this could lead to assertion failures if the user tries to
        //     click/manipulate DELETED expressions
        //

        // Remove the grid in the current step
        //
        var gridIdInStep = getGridPosInStep4OfHtmlId(htmlId);
        sO.allGridIds.splice(gridIdInStep, 1);

        // Redraw entire step
        //
        drawStep(sO);

    }

}


//----------------------------------------------------------------------------
// If an incoming grid cell parameter is drawn as a reference (rather than
// a scalar value), this method gives the default index names for incoming
// index argumnets. 
//----------------------------------------------------------------------------
function getGridCellIndArgs(gO) {

    var ret = "";

    var prop = " class='indexName' ";

    for (var d = 0; d < gO.numDims; d++) {

        if (d > 0) ret += ",";

        // TODO: write this func
        var onc = prop + " onclick='insertIncomingArgIndex()' ";
        //
        ret += "<span " + prop + ">In" + d + "</span>";
    }

    return "<span " + prop + ">[" + ret + "]</span>";
}


//----------------------------------------------------------------------------
// Get the caption for a incoming argument into a function. Usually incoming
// argument names (captions) can be changed 
//----------------------------------------------------------------------------
function getCaption4GridArg(argExpr, isEditable, caption) {


    var edit = (isEditable) ? " contenteditable " : "";
    var indices = "";

    var ret = "<span class='caption' id='captionEdit' " + edit + ">" +
        caption + "</span>" + indices;

    return ret;
}



//----------------------------------------------------------------------------
// Returns HTML string w/ onclick functions for dimension names used 
// for a grid. Note that an index name is for a dimension, and they
// appear in the dimension order. 
//----------------------------------------------------------------------------
function getIndNames4Step(sO) {

    var inames = "";

    for (var d=0; d < sO.dimNameExprs.length; d++) {   // for all dimensions

	var cl = " class='indexName' ";                // class name for index

        //var onC = " onclick=\"addIndNameExpr(" + d + ")\" ";
        //var onC = " onclick=\"indVarClicked(" + d + ")\" ";

        var onC = " onmousedown=\"indVarMouseDown(" + d + ")\" " +
            " onmouseup=\"indVarMouseUp(" + d + ")\" ";

        cl += " style='cursor:pointer' ";

        // if the user double clicked on the index names, make the index
        // names editable
        //
        if (sO.editDimNames) {
            var args = "this, " + d;
            //onC=" oninput=\"updateIndexEdit(" + args + ")\" contenteditable ";
            onC = " onblur=\"updateIndexEdit(" + args +
                ")\" contenteditable ";
            onC += "id='IndName" + d + "' ";
            onC += " onblur='doneIndexEdit()' ";
        }

        var dname = sO.dimNameExprs[d].str;
        var span = "<span " + cl + onC + ">" + dname + "</span>";
        var sep = (d > 0) ? "," : "";
        inames += sep + span;
    }

    var ret = "";

    // if there are index variables (i.e., not all grids are scalar)
    //
    if (sO.dimNameExprs.length) {
        ret = "<span style='font-size:10px'>Index Variables : </span>" +
            " <span class='indexNames'>[" + inames + "]</span>";
    }

    return ret;
}

//----------------------------------------------------------------------------
// Update index edit with edited value
//----------------------------------------------------------------------------
function updateIndexEdit(obj, d) {

    var sO = CurStepObj;
    var name = obj.innerHTML

    if (isExistingName(CurModObj, CurFuncObj, CurStepObj, name)) {

        alert("Name " + name + " already exists!");

    } else {
        sO.dimNameExprs[d].str = obj.innerHTML;
    }

    drawCodeWindow(sO);
}

//----------------------------------------------------------------------------
// Done index editing
//----------------------------------------------------------------------------
function doneIndexEdit() {

    var sO = CurStepObj;

    // change focus to the last box in code window and redraw entire step
    // TODO: We should record which box we were editing before we double
    //       clicked on index var
    //
    changeCWInputBox(sO.boxExprs.length - 1, true);
    redrawCurStep();
    //drawStep(sO, sO.showData);
}




//----------------------------------------------------------------------------
// Edit index name (index variable name) for a given dim
//----------------------------------------------------------------------------
function editIndNameExprs(dim) {

    var sO = CurStepObj;

    // STEP
    // Redraw code window without any focused box or any active
    // expression/space. This is important for the user so that the user
    // knows expression boxes are not where the input goes. 
    // We redraw with editDimNames true, so that dim names become editable
    //
    sO.focusBoxId = CodeBoxId.INVALID;
    sO.focusBoxExpr = null;
    //
    sO.activeParentPath = null;
    sO.activeChildPos = INVID;
    sO.isSpaceActive = false;
    sO.activeParentExpr = null;
    //
    sO.editDimNames = true;
    drawCodeWindow(sO);

    // STEP: After redrawing code window with editable dim names
    //       disable editable flag and set focus on the dim name the
    //       user clicked on 
    //
    sO.editDimNames = false;
    var name = "IndName" + dim;
    var sp1 = document.getElementById(name);
    //
    sp1.focus();
    //
    // disable our keyboard processing
    //
    sO.keyboardEnabled = false;

}


//----------------------------------------------------------------------------
// Mouse down event handler when clicked on an index variable
//----------------------------------------------------------------------------
function indVarMouseDown(dim) {

    if (!Timer) {

        // if there  is no timer set, set a new timer. If the timer
        // goes off, the user is still pressing down the mouse -- which
        // indicates the user intends to edit the name.
        // Note: on toucpads, the user has to click AND hold (i.e., click once
        //       and click again and hold) OR press the touchpad button
        //
        Timer = setTimeout(function() {
                Timer = null;
                editIndNameExprs(dim);
            },
            300);
    }


}

//----------------------------------------------------------------------------
// Mouse up event handler when an index variable name is clicked
//----------------------------------------------------------------------------
function indVarMouseUp(dim) {

    // We are here because the user released the mouse button.
    // If Timer is still set, it means, the user relased the button soon --
    // which indicates a single click. If the Timer is not set, that means
    // the timer has already expired and user has started editing -- so nothing
    // to do. 
    //
    if (Timer) {
        clearTimeout(Timer);
        Timer = null;
        addIndNameExpr(dim);
    }
}


//----------------------------------------------------------------------------
// This is the event handler for inserting a dimension name.
// This gets called when the user clicks on an index name at the bottom
// of a grid while developing a formula
//----------------------------------------------------------------------------
function addIndNameExpr(dim) {

    var sO = CurStepObj;
    var fO = CurFuncObj;

    if (sO.focusBoxId == CodeBoxId.Range) {       // append a new range
        
	// We are appending a whole new range variable
	// -- i.e., new range for dim with default output grid 
	//
	var gO = fO.allGrids[sO.allGridIds[0]];
	var rangeExpr = newRangeExpr(sO, dim, gO);     
	addGenExpr(rangeExpr);                    // add to range box

    } else {

        var iexpr = sO.dimNameExprs[dim];
        addGenExpr(iexpr);
    }
}


//----------------------------------------------------------------------------
// Called when a row/col/.. title is clicked
//----------------------------------------------------------------------------
function titleClicked(tdobj, htmlId, dim, ind) {

    var sO = CurStepObj;
    var gridId = getGridIdOfHtmlId(htmlId);       // find grid object
    var gO = CurFuncObj.allGrids[gridId];

    resetOnInput();

    // STEP:
    // If the user clicked on a grid title cell while a range field 
    // (e.g., start/end) is active, replace range field with index clicked on
    //
    if ((sO.focusBoxId == CodeBoxId.Range) && sO.activeParentExpr &&
        !sO.isSpaceActive) {

        var texpr = new ExprObj(true, ExprType.Literal, gO.dimTitles[dim]
            [ind]);
        texpr.gO = gO;

        var pos = sO.activeChildPos;
        //
        if (pos <= 2) { // if this is start or end fields

            // Replace the existing expression with title expression that
            // user clicked on
            //
            replaceOrAppendExpr(texpr);
            drawCodeWindow(sO); // redraw to update range in CW

            return;
        }
    } else {
        showTip(TipId.TitleClick);
    }


}


//----------------------------------------------------------------------------
// Scroll data in a dimension
// This method is called when the user clicks on an arrow to see more
// data (for a given dimension)
//----------------------------------------------------------------------------
function scrollData(htmlId, dim, new_ind) {

    var sO = CurStepObj;

    var gridId = getGridIdOfHtmlId(htmlId); // find grid object
    var gO = CurFuncObj.allGrids[gridId];

    // The caller of this routine should gurantee that new_ind is smaller
    // than the actual size of the dimension
    //
    assert((new_ind < gO.dimActSize[dim]) && (new_ind >= 0),
        "new ind out of dim size: caller must have checked");

    // Update the starting point for the dimension
    //
    gO.dimShowStart[dim] = new_ind;

    // Redraw the grid (since we are showing data, there is no indexObj)
    //
    drawGrid(gO, null, gO.htmlId, false);

}


//----------------------------------------------------------------------------
// Called when the user clicks on a grid index (e.g., row+1, end0, ...)
//----------------------------------------------------------------------------
function gridIndMouseDown(tdobj, htmlId, dim, ind) {

    // if the timer is not set, start the timer. If the timer expires before
    // the user releases the mouse button, that is click and hold -- we 
    // should edit in that case.
    //
    if (!Timer) {

        Timer = setTimeout(
            function() {
                Timer = null;
                gridIndEdit(tdobj, htmlId, dim, ind);
            },
            300);
    }
}

//----------------------------------------------------------------------------
// called when the user releases mouse button on a grid index (e.g., row+1)
//----------------------------------------------------------------------------
function gridIndMouseUp(tdobj, htmlId, dim, ind) {

    // if the user released the mouse button and timer is still set, 
    // that is a regular single click
    //
    if (Timer) {
        clearTimeout(Timer);
        Timer = null;
        gridIndAdd(tdobj, htmlId, dim, ind);
    }
}

//----------------------------------------------------------------------------
// Insert an index expression when user clicks on it
//----------------------------------------------------------------------------
function gridIndAdd(tdobj, htmlId, dim, ind) {

    var sO = CurStepObj;
    var gridId = getGridIdOfHtmlId(htmlId);       // find grid object
    var gO = CurFuncObj.allGrids[gridId];
    var iO = getIndObjOfHtmlId(htmlId);
    //
    var iexpr = iO.dimIndExprs[dim][ind];         // get current index expr

    // Don't allow inserting as a range
    //
    if (sO.activeParentExp && sO.activeParentExpr.isLoopStmt()) {
        warn("Cannot insert a grid cell into the Index Range");
        return;
    }

    // NOTE: if the user is currently editing the same index, we don't do 
    //       addGenExpr because that could lead to inifinite recursion.
    //
    if (iexpr && (iexpr != sO.focusBoxExpr)) {

        addGenExpr(iexpr);
    }
}




//----------------------------------------------------------------------------
// Start editing an index in grid -- when user clicks on a grid index.
// If the user has already selected an expression in a range, then 
// replace that expression. TODO: same for function arg
//----------------------------------------------------------------------------
function gridIndEdit(tdobj, htmlId, dim, ind) {

    var sO = CurStepObj;

    // We are here because the user clicked on an index cell (in a grid).
    // alert("grid index clicked:" + htmlId + " " + dim + " " + ind);


    var gridId = getGridIdOfHtmlId(htmlId);       // find grid object
    var gO = CurFuncObj.allGrids[gridId];
    var iO = getIndObjOfHtmlId(htmlId);
    //
    var iexpr = iO.dimIndExprs[dim][ind];         // get current index expr

    // if the user is currently editing the same index, just return because
    // this could lead to infinite recursion
    //
    if (iexpr && (iexpr == sO.focusBoxExpr))
        return;


    resetOnInput();

    // If user has not completed grid selction, don't process
    // 
    if (sO.stageInStep < StageInStep.GridSelectDone) {
        showTip(TipId.SelectGrids);
        return;
    }

    showTip(TipId.IndexEdit);

    // STEP:
    // If the user clicked on a grid index cell while a range field 
    // (e.g., start/end) is active, replace range field with index clicked on
    //
    if (iexpr && (sO.focusBoxId == CodeBoxId.Range) && sO.activeParentExpr &&
        !sO.isSpaceActive && expr2str(iexpr).length) {

        // NOTE: We can get empcty "Concat" expressions created below

        // TODO: iexpr gets here

        // alert("iexpr: " + iexpr);

        var pos = sO.activeChildPos;

	if (pos <= 1) {       // if this is start or end fields

            // var highexpr = sO.activeParentExpr.exprArr[pos];
            //alert("HighExpr: " + expr2str(highexpr) + " pos: " + pos);

	    // Replace the existing expression with index expression that
	    // user clicked on
	    //
	    replaceOrAppendExpr(iexpr);
	    drawCodeWindow(sO);              // redraw to update range in CW

            //alert("AfterRepl: "+expr2str(sO.activeParentExpr)+ " pos: "+ pos);

            return;
        }
    }

    // STEP: We are here because we want to edit the index cell

    // if we are editing the same index cell, do nothing here
    //
    if ((sO.focusBoxId == CodeBoxId.Index) && (sO.focusBoxExpr == iexpr))
        return;

    // if we are currently editing a different index, stop it first
    //
    if ((sO.focusBoxId == CodeBoxId.Index) && (sO.focusBoxExpr != iexpr)) {

        sO.activeParentPath = ""; // disable active expr/space
        redrawGridIndexExpr(sO); // redraw the index *cell*

        // Use the following if we want the cursor to disapear as well
        //
        // stopIndexEditing();
    }

    // if the user has a current highlighted expression in the code window
    // redraw code window
    //
    if (sO.focusBoxId != CodeBoxId.Index) {

        // set code box to INVALID because drawCodeWindow highlights boxes
        // based on this. 
        //
        //sO.focusBoxId = CodeBoxId.INVALID;
        sO.activeParentPath = ""; // disable active expr/space
        sO.activeChildPos = INVID;
        drawCodeWindow(sO); // redraw to remove any focus (glow)
    }


    
    sO.focusBoxId = CodeBoxId.Index;              // set focus to index box

    var first_time = false;

    //TODO: Use variables DefStartNameExt/DefEndNameExt (no hardcoded)
    //MPI: And bug fix
    var searchPattern = new RegExp("^(endLoc|end|startLoc)\\d+");
    

    // Create a root expression for index, for adding more
    //
    if (!iexpr /*|| (iexpr != ExprType.Concat)*/ ) {

        // if this is the first time this index is edited, add an index
        // expression of type 'Concat' -- which is a vanilla root expr
        // that will gather any child exprssions


        var fexpr = new ExprObj(false, ExprType.Concat, "");
        fexpr.gO = gO; // record gO in root expr

        sO.boxExprs[sO.focusBoxId] = fexpr; // record in boxExprs
        iO.dimIndExprs[dim][ind] = fexpr; // and grid index location

        // If we arleady have an index expr (e.g., row), addd it to the
        // 'concat' root expression
        //
        if (iexpr) fexpr.addSubExpr(iexpr);

    } else if (searchPattern.test(iexpr.str)) {

	//MPI: Bug fix: Was '==', should be "DefEndName"d, where d dimension
	//     Added DefEndNameExt for extended dimension end indices

        // We don't allow editing of last index name begenning with --"end"
        //
        alert("Cannot change last index name");
        //return; //MPI: Bug fix

    } else {

        assert((iexpr.type == ExprType.Concat), "Only concat supported");
    }

    // set focusBoxExpr and record the HTML table cell object -- for future
    // updates to the cell
    //
    sO.focusBoxExpr = iO.dimIndExprs[dim][ind];
    sO.focusHtmlObj = tdobj;

    // Make the last space active by default and draw the expression for
    // editing
    //
    setDefaultActiveSpace(sO);
    //
    redrawGridIndexExpr(sO);


}

//----------------------------------------------------------------------------
// When the user is editing an index expression, this method is called
// to update any edits (e.g., expression highlights, space clicked etc)
//----------------------------------------------------------------------------
function redrawGridIndexExpr(sO) {

    var iexpr = sO.focusBoxExpr;
    var tdobj = sO.focusHtmlObj;

    // Note: function getHtmlExprStmt() returns html str which contains
    // calls back exprClicked()  and spaceClicked() when an expression 
    // or space is clicked within 'istr'. 
    // NOTE: When the user is editing, we use a different expression type
    //       because 'Concat' type must be processed as a 'statement'
    //       to display editable spaces
    //
    if (iexpr.isConcat()) iexpr.type = ExprType.ConcatEdit;
    //
    var istr = getHtmlExprStmt(iexpr, sO, "");
    //
    if (iexpr.isConcatEdit()) iexpr.type = ExprType.Concat;


    var onB = " onblur='stopIndexEditing()' ";
    var str = "<span " + onB + ">" + istr + "</span>";


    tdobj.innerHTML = str;
}


//----------------------------------------------------------------------------
// Returns the plain string (non-editable) of an index expression 
//----------------------------------------------------------------------------
function getIndExprStr(expr) {
    return expr2str(expr);
}

//----------------------------------------------------------------------------
// Returns the 'expression' for a cell -- e.g., row[0][1]. 
//
// ind0 : first dimension index
// ind1 : second dimension index
//
// Note: If any of the dimensions has an empty title/index, the return
//       string will be the empty string

// VERIFY: must cordinate with putCellInd() for expression building
//----------------------------------------------------------------------------
function getCellExpr(gO, iO, row, col) {

    var ind0 = (!ColDimId) ? col : row; // if ColDimId==0, ind0 is col
    var ind1 = (RowDimId) ? row : col;  // if RowDimId==1, ind1 is row


    var str = ""; // gO.caption;
    var empty = false;

    if (gO.numDims > 0) {                           // first dimension

        var title = (!gO.dimHasTitles[0] && iO) ?
            getIndExprStr(iO.dimIndExprs[0][ind0]) : gO.dimTitles[0][ind0];

        str += "[" + title;
        empty = empty || (title.length == 0);
    }

    if (gO.numDims > 1) {                          // second dimension
	var title = (!gO.dimHasTitles[1] && iO) ? 
	     getIndExprStr(iO.dimIndExprs[1][ind1]) : gO.dimTitles[1][ind1] ;

        str += "," + title;
        empty = empty || (title.length == 0);
    }

    for (var d=2; d < gO.numDims; d++) {           // higher dimensions
	var ind = gO.dimSelectTabs[d];
	var title = (!gO.dimHasTitles[d] && iO) ? 
	     getIndExprStr(iO.dimIndExprs[d][ind]) : gO.dimTitles[d][ind] ;
	str += "," + title;
	empty = empty || (title.length == 0);
    }

    str += ']';
    if (empty) str = "";

    return str;
}


//----------------------------------------------------------------------------
// Return the index witin step of a given html ID
//----------------------------------------------------------------------------
function getGridPosInStep4OfHtmlId(htmlId) {

    for (var i = 0; i < AllHtmlGridIds.length; i++) {
        if (AllHtmlGridIds[i] == htmlId) {
            assert((i < CurStepObj.allGridIds.length), "Invalid Grid Id");
            return i;
        }
    }

    alert("Internal Bug: HtmlID for Grid not fuond for id:" + htmlId);
}

//----------------------------------------------------------------------------
// Return grid object associated with HtmlId
//----------------------------------------------------------------------------
function getGridIdOfHtmlId(htmlId) {

    for (var i = 0; i < AllHtmlGridIds.length; i++) {
        if (AllHtmlGridIds[i] == htmlId) {
            assert((i < CurStepObj.allGridIds.length), "Invalid Grid Id");
            return CurStepObj.allGridIds[i];
        }
    }

    alert("Internal Bug: HtmlID for Grid not fuond for id:" + htmlId);
}


//----------------------------------------------------------------------------
// Get index object currently associated with an html id
//----------------------------------------------------------------------------
function getIndObjOfHtmlId(htmlId) {

    for (var i = 0; i < AllHtmlGridIds.length; i++) {
        if (AllHtmlGridIds[i] == htmlId)
            return CurStepObj.allIndObjs[i];
    }

    alert("Internal Bug: HtmlID for Index not fuond for id:" + htmlId);
}


//----------------------------------------------------------------------------
// Called when the a grid is not being edited (onclick event handler). 
// So, just change the active tab.
//----------------------------------------------------------------------------
function selectTab(obj, htmlId, dim, tab, edit) {

    //alert(obj + " : " + htmlId + ":" + dim + ":" + tab);

    var gridId = getGridIdOfHtmlId(htmlId);
    var gridObj = CurFuncObj.allGrids[gridId];
    var iO = getIndObjOfHtmlId(htmlId);
    //
    gridObj.dimSelectTabs[dim] = tab;
    drawGrid(gridObj, iO, htmlId, edit);    // redraw grid
}

//----------------------------------------------------------------------------
// Called while the output grid is being edited. 
// The tab title is 'contenteditable'. When the user edits the title in 
// place, this method is called. This can be called by
// multiple events (onclick, oninput, onblur -- as indicated by eventid)
//----------------------------------------------------------------------------
function changeTabTitle(obj, htmlId, dim, tab, edit, eventid) {


    var gridId = getGridIdOfHtmlId(htmlId);
    var gridObj = CurFuncObj.allGrids[gridId];
    var iO = getIndObjOfHtmlId(htmlId);

    var curtab = gridObj.dimSelectTabs[dim];

    gridObj.dimSelectTabs[dim] = tab;

    if (gridObj.dimHasTitles[dim])
        gridObj.dimTitles[dim][tab] = obj.innerHTML;
    else
        assert(0, "updating tab that has no titles!");

    // Redraw the grid only when the focus is lost (onblur) OR if the user
    // clicked on an un-selected tab. Otherwise, the tab currently being 
    // edited lose focus
    //
    if ((eventid == 2) || (curtab != tab))
        drawGrid(gridObj, iO, htmlId, edit); // redraw grid
}


//----------------------------------------------------------------------------
// Called when clicked on an 'element' with comment (e.g., a div element)
// Records the 'element' in a global variable. Gets the comment from the
// user. The comment is assigned to the 'element' via stopComment.
//
// FIXME: we call stopComment() only on onchange. If there is no change
//        nothing is called. 
//----------------------------------------------------------------------------
function getComment(obj) {

    CommentObj = obj; // record object in global variable

    // Note: we set contenteditable true so that formula boxes will ignore
    //       the symbols/back-space we type. See myKeyPress() 
    //
    var prop = " style='background-color:#FFFFC2'  contenteditable='true' ";

    // string that goes into the comment box (multi-line text area)
    //
    var str = "<div class='comment' id='commentBox'>" +
        "<textarea title='add comment' " // onchange='stopComment(this)' " 
        + " onblur='stopComment(this)' id='commentText'  " + prop + ">" +
        obj.title + "</textarea></div>";

    // The commentBox object
    //
    var com1 = document.getElementById('commentBox');


    // Get the bounding rectangle of the 'obj' -- i.e., the comment marking div
    // and use that top/left position to place the comment box
    //
    var rect = obj.getBoundingClientRect();
    com1.style.top = rect.top + 'px';
    com1.style.left = rect.left + 'px';

    // set the innerHTML of the comment box
    //
    com1.innerHTML = str; // update HTML of comment
    com1.className = 'comment';      // change class of htmlId

    // Set focus inside the textarea. This is necessary to close the comment
    // box when the user clicks on somewhere other than the comment box
    //
    var comText = document.getElementById('commentText');
    comText.focus();

}


//----------------------------------------------------------------------------
// This method is called when the user changes a comment in the comment box.
// 
//----------------------------------------------------------------------------
function stopComment(inp1) {

    // Change the title of the current object /w comment
    // NOTE: CommentObj is the div/span element where we inserted the 
    //       baloon picture. Inp1 is the textarea.
    //
    CommentObj.title = inp1.value;
    updStr = inp1.value;

    // Remove the comment box
    //
    var com1 = document.getElementById('commentBox');
    com1.innerHTML = ""; // update HTML of grid
    com1.className = 'comment';      // change class of htmlId


    // NOTE: We have to go through ALL comment boxes in the step and 
    //       findout which comment div/span has CommentObj.id. Then update
    //       the corresponding Gridyly object
    //
    var sO = CurStepObj;
    //
    // STEP 1: check whether the user changed the "step comment"
    //
    var commentId = getCommentHtmlId(CommentType.StepTitle, 0);
    if (commentId == CommentObj.id) {
        sO.stepComment = updStr; //document.getElementById(commentId).title;  
        logMsg("Stop Comment for step title: " + sO.stepComment);
        return;
    }
    //
    //
    // STEP 2: check for a comment update to each formula box
    //
    for (var box = CodeBoxId.Range; box < sO.boxAttribs.length; box++) {

        // if the user updated comment belongs to 'box', change it now
        //
        var commentId = getCommentHtmlId(CommentType.StepBox, box);
        //
        if (commentId == CommentObj.id) {
            sO.boxAttribs[box].comment = updStr;
            logMsg("comment updated for formula box:" + box);
            return;
        }
    }
    //
    // STEP3: check for each grid comment update AND grid title updates
    //
    for (var gseq = 0; gseq < sO.allGridIds.length; gseq++) {

        // STEP A: Check whether the user edited the "grid comment"
        //
        var htmlId = AllHtmlGridIds[gseq];
        var commentId = getCommentHtmlId(CommentType.Grid, htmlId);
        //
        var gridId = getGridIdOfHtmlId(htmlId); // find grid object
        var gO = CurFuncObj.allGrids[gridId];

        // if the htmlIds of the CommentObj and the grid matches, update
        // the comment in that grid object
        //
        if (commentId == CommentObj.id) {

            gO.comment = updStr;
            logMsg("comment updated for grid: " + gO.caption);
            return;
        }

        // STEP B: Check whether the user edited a title comment
        //
        for (var d = 0; d < gO.numDims; d++) {

            if (gO.dimHasTitles[d]) { // only titles have comments

                for (var t = 0; t < gO.dimShowSize[d]; t++) { // for each title

                    var comId = getCommentHtmlId(CommentType.DimTitle + d,
                        t);
                    if (comId == CommentObj.id) {

                        gO.dimComments[d][t] = updStr;
                        logMsg("comment upd for title " + gO.dimTitles[d]
                            [t]);
                        return;
                    }
                }
            }

        } // for each dimension

    } // for each grid

}


//----------------------------------------------------------------------------
// When the user clicks on a grid name, this method is called to 
// insert a grid name as an argument in a function call
//----------------------------------------------------------------------------
function putGridRef(htmlId) {

    var sO = CurStepObj;
    var parExpr = sO.activeParentExpr;


    // STEP: 
    // Don't put grid ref for a scalar grid. 
    //
    var gridId = getGridIdOfHtmlId(htmlId);
    var gO = CurFuncObj.allGrids[gridId];

    // if user clicked on a scalar grid name
    // Note: We can put a scalar grid name in a range expression
    //
    if ((gO.numDims < 1) && parExpr && parExpr.isRange()) {

        showTip(TipId.ScalarGridRefClick);
        return;
    }

    // STEP: 
    //
    if (!parExpr) {         // no highlighted expression

        showTip(TipId.GridRefClick);

    } else if (parExpr.isRange() ||
        (parExpr.exprArr && parExpr.exprArr[sO.activeChildPos] &&
            parExpr.exprArr[sO.activeChildPos].isRange())) {

        // We are are here if the user has highlighted 
        // (i) a range (e.g., Out(....)) OR
        // (ii) the index variable name of the range (e.g., row=...)

        // Get the range expressio based on what the user was highlighting
        //
        var rangeExpr = parExpr.isRange() ? parExpr :
            parExpr.exprArr[sO.activeChildPos];

        assert(rangeExpr.isRange());
        assert(rangeExpr.exprArr[0].isLiteral(),
            "0th arg must be caption");

        // Save the selected dim of the existing range (we change it below)
        //
        var dim = rangeExpr.gO.selIndDim;

        // Remove the range higlight in the existing rangeExpr
        // WARN: Don't move above saving 'dim'
        // 
        rangeExpr.gO.hasSelection = false; // no highlihged cell in grid
        rangeExpr.gO.selIndDim = INVID; // no higlighted indices in any dim
        var iO = getIndObjOfHtmlId(rangeExpr.gO.htmlId);
        drawGrid(rangeExpr.gO, iO, rangeExpr.gO.htmlId, false);


        // STEP : Update the rangeExpr with new grid info
        //
        // The user is inserting a grid name into a range -- e.g., row=src(...)
        //
        var gridId = getGridIdOfHtmlId(htmlId);
        var gO = CurFuncObj.allGrids[gridId];

        // Use the new grid caption and point to gO.
        //
        rangeExpr.exprArr[0].str = gO.caption;
        rangeExpr.gO = gO;
        rangeExpr.gO.selIndDim = dim;
        //
        assert(rangeExpr.gO, "gO must be set for rangeExpr");

        // Redraw code window and new grid
        //
        drawCodeWindow(sO);
        var iO = getIndObjOfHtmlId(gO.htmlId);
        drawGrid(gO, iO, gO.htmlId, false);


        // Note: The user has to click on the index var name ( e.g., row= )
        // to show the highlight if the user was highlighting the grid name
        // before (as compared to entire range with index name) 


    } else if (parExpr.isFuncCall()) {

        // We are here because the user is editing function call args. So, 
        // go and insert grid reference
        //
        var gridId = getGridIdOfHtmlId(htmlId);
        var gO = CurFuncObj.allGrids[gridId];

        // Create grid reference expression and add it
        //
        var gridExpr = new ExprObj(true, ExprType.GridRef, gO.caption);
        gridExpr.gO = gO;
        //
        addGenExpr(gridExpr);

    } else {

        // Cannot insert a grid name anywhere else
        //
        showTip(TipId.GridRefClick);
    }


}



//----------------------------------------------------------------------------
// We are here because the user clicked on a cell in gO.
//
//----------------------------------------------------------------------------
function putCellInd(htmlId, row, col) {

    var gridId = getGridIdOfHtmlId(htmlId);
    var gO = CurFuncObj.allGrids[gridId];
    var iO = getIndObjOfHtmlId(htmlId);
    var sO = CurStepObj;


    // Reset (e.g., numberExpr) on new mouse input
    //
    resetOnInput();

    // If user has not completed grid selction, don't process
    // 
    if (sO.stageInStep < StageInStep.GridSelectDone) {
        showTip(TipId.SelectGrids);
        return;
    }

    // Don't allow inserting as a range
    //
    if (sO.activeParentExpr.isLoopStmt()) {
        warn("Cannot insert a grid index into Index Range. " +
            "Click on an Index Name variable instead to insert a new range"
        );
        return;
    }


    // We are here because the user clicked on a cell in gO. However, another
    // grid (e.g., a source grid) could have a cell highlighted. We have to 
    // remove that highlight before we insert this new grid expression and
    // highlight.
    //
    removeGridHighlight();

    // First, build a new sub-expression for the cell clicked -- e.g.,
    // out[row,col]. Note that we have to set gO.dimSelectTabs[] before we
    // call buildExpr().1
    //
    gO.dimSelectTabs[RowDimId] = row;
    gO.dimSelectTabs[ColDimId] = col;
    var gridExpr = buildExpr(gO, iO);  // build grid expr (e.g., out[row,col])

    // If this assertion fails, make sure focusBoxExpr is set properly
    // when drawing a step. By default, drawStep() does NOT set the focusExpr.
    //
    if (sO.focusBoxExpr == null) {
        assert(0,
            "Box Expr cannot be null. Must have been created before");
    }

    replaceOrAppendExpr(gridExpr);           // replace/append expr

    drawCodeWindow(sO);               // redraw code window

    // When we put a cell index -- if we replace the highlighted expr with
    // a different expr -- we need to update the grid object highlighting
    // NOTE: The following dependes on sO.activeParentExpr and hence
    //       must follow drawCodeWindow();
    //
    updateGridHighlight();


    // STEP
    //
    // Add default assignment operator for a formula after adding a grid
    // cell expression
    //
    if (sO.boxAttribs[sO.focusBoxId].isFormula() &&
        (sO.activeParentExpr.isStatement()) &&
        (sO.activeParentExpr.exprArr.length == 1)) {

        addOpExpr("=");
    }
}


//----------------------------------------------------------------------------
// By default, when no expression is highlighted, the last space in the
// focused expression is active (highlighted). 
//----------------------------------------------------------------------------
function setDefaultActiveSpace(sO) {

    // The entire box expression is the active expression (for setting space)
    // 
    sO.activeParentExpr = sO.focusBoxExpr;
    //
    // Parent path is the root of expression because we want to highlight
    // last space of parent expression
    // 
    if (sO.focusBoxExpr) {
        sO.activeParentPath = sO.focusBoxExpr.mySeq + ":";
        sO.activeChildPos = sO.activeParentExpr.exprArr.length; // last space
    } else {
        sO.activeParentPath = "";
        sO.activeChildPos = 0;
    }


    sO.isSpaceActive = true;                            // space active

    //alert("def space:" + sO.activeParentPath + " , " +  sO.activeParentPath); 
}

//----------------------------------------------------------------------------
// Reset keyboard/tip etc. when a new element is clicked
//----------------------------------------------------------------------------
function resetOnInput() {

    var sO = CurStepObj;
    sO.numberExpr = null;
    clearTip();
    sO.keyboardEnabled = true;
}


//----------------------------------------------------------------------------
// This method is called when the user clicks on an operator button
// like +, -, *, ... etc. In such a case, we just add the operator,
// redraw the code window (to reflect the change). 
//----------------------------------------------------------------------------
function addOpExpr(opstr) {
    var sO = CurStepObj;
    var opExpr = new ExprObj(true, ExprType.Operator, opstr);
    addGenExpr(opExpr);
}


//----------------------------------------------------------------------------
// Add a number expression. This is used to add a numbr expression to
// afunction call 
//----------------------------------------------------------------------------
function addNumExpr(opstr) {
    var sO = CurStepObj;
    var opExpr = new ExprObj(true, ExprType.Number, opstr);
    addGenExpr(opExpr);

    // This must be done after calling addGenExpr() above because it resets
    // sO.numberExpr
    //
    sO.numberExpr = opExpr;
}

//----------------------------------------------------------------------------
// Add a string expression
//----------------------------------------------------------------------------
function addStrExpr(opstr) {
    var sO = CurStepObj;
    var opExpr = new ExprObj(true, ExprType.String, opstr);
    addGenExpr(opExpr);
}

//----------------------------------------------------------------------------
// Add a string expression
// TODO: set ExprObj.grid = ... so that endname is associated with a grid
//----------------------------------------------------------------------------
function addEndExpr() {

    var endname = prompt("End value", "end0");

    var sO = CurStepObj;
    var opExpr = new ExprObj(true, ExprType.Literal, endname);

    // TODO: Set ExprObj.grid so that endname is associated with a grid
    //
    // opExpr.grid = ....

    addGenExpr(opExpr);
}




//----------------------------------------------------------------------------
// Delete an expression
//----------------------------------------------------------------------------
function delOpExpr() {

    var sO = CurStepObj;

    removeGridHighlight();
    resetOnInput();
    removeExpr();
    drawCodeWindow(sO);                 // redraw code window

}

//----------------------------------------------------------------------------
// Change conditional keyword in a mask box (e.g, if ==> else)
//----------------------------------------------------------------------------
function changeCondKeyword(word, type) {

    var sO = CurStepObj;

    if (!sO.isSpaceActive && (sO.activeChildPos == ROOTPOS)) {

        if (sO.activeParentExpr.isCondition()) {
            sO.activeParentExpr.str = word;
            sO.activeParentExpr.type = type;

        } else {
            // Tip: click on a condition (in Mask)
        }

    } else {

        // Tip: Select a keyword first to replace
    }

    drawCodeWindow(sO); // redraw code window
}


//----------------------------------------------------------------------------
// Add a given keyword for a formula box
//----------------------------------------------------------------------------
function addFormulaKeyword(word, exprtype) {

    var sO = CurStepObj;

    // Insert the keyword
    //
    if (sO.activeParentExpr.isExprStmt() && (sO.activeChildPos == 0)) {

        // A formula keyword is treated as a leaf because it can be 
        // deleted independently -- not like an if/elseif condition, which
        // appears by default (i.e., a mask box must have a keyword, unlike a
        // formula box). The arguments of the return follow the return
        // keyword expression (args are not children of return expr)
        //
        var keyExpr = new ExprObj(true, exprtype, word);
        //
        assert(keyExpr.isFormulaKeyword(),
            "must be a formula keyword type");
        //
        addGenExpr(keyExpr);

    } else {

        // Note: Currently, we can't get here since the button is displayed
        //       only for activeChildPos == 0
        //
        // Tip: let/return/break/continue accepted only as the first expression 
    }


    // Warn about using 'break' and 'return'. This must be done after
    // addGenExpr because it redraws window
    //
    if (exprtype == ExprType.Break) 
	showTip(TipId.AvoidBreak);
    else if (exprtype == ExprType.Return) 
	showTip(TipId.AvoidReturn);

}

//----------------------------------------------------------------------------
// Add a let expression for a formula box
//----------------------------------------------------------------------------
function addLetExpr() {

    var sO = CurStepObj;

    if (sO.activeParentExpr.isExprStmt() && (sO.activeChildPos == 0)) {

        var keyExpr = new ExprObj(false, ExprType.Let, '');
        var litExpr = new ExprObj(true, ExprType.LetName, 'name');
        keyExpr.addSubExpr(litExpr);
        //
        addGenExpr(keyExpr);
    }
}

//----------------------------------------------------------------------------
// Change a loop to a 'foreach' loop
//----------------------------------------------------------------------------
function changeLoopToForeach() {

    var sO = CurStepObj;

    if (sO.activeParentExpr.isLoopStmt() &&
        (sO.activeChildPos <= 0)) {

        sO.activeParentExpr.str = 'foreach';
        sO.activeParentExpr.type = ExprType.Foreach;
        sO.activeParentExpr.deleted = DeletedState.None;
    } else {
        showTip(TipId.SelectKeyword);
    }

    drawCodeWindow(sO);                 // redraw code window
}


//----------------------------------------------------------------------------
// Change a loop to a forever loop. 
//----------------------------------------------------------------------------
function changeLoopToForever() {

    var sO = CurStepObj;

    var expr = sO.activeParentExpr;

    if (expr.isLoopStmt() && (sO.activeChildPos <= 0)) {

        expr.str = 'forever';
        expr.type = ExprType.Forever;
        expr.deleted = DeletedState.None;
        //
        // Remove all index variable definitions
        //
        while (expr.exprArr.length)
            expr.exprArr.pop();


    } else {

        showTip(TipId.SelectKeyword);
        // Tip: click on a condition (in Mask)
    }

    drawCodeWindow(sO); // redraw code window
}


//----------------------------------------------------------------------------
// Add a 'generarl' expression to the box/positon that is currently active
//----------------------------------------------------------------------------
function addGenExpr(expr) {

    var sO = CurStepObj;

    // if any grid is currently highlighted, remove that highlight first
    // This is important if the user replaces a grid expression with an
    // operator
    //
    removeGridHighlight();
    resetOnInput();
    replaceOrAppendExpr(expr);
    drawCodeWindow(sO); // redraw code window
}





//----------------------------------------------------------------------------
// Called when the user clicks on 'insert NEW function' button
// This creates a function with fname and empty arg list. Args can be 
// added the same way we add expressions to a box
//----------------------------------------------------------------------------
function addNewFuncExpr() {

    // if any grid is currently highlighted, remove that highlight first
    // This is important if the user replaces a grid expression with an
    // operator
    //
    removeGridHighlight();


    // Get the new function name from the user and check for duplicate names
    // TODO: Check among grid names of function and let names of the step
    //
    var isdup = true;

    do {
        var fname = prompt("Name of new function", DefFuncName);

        // If user pressed cancel, just return
        //
        if (!fname)
            return;

        isdup = isExistingFuncName(CurModObj, fname);

        if (isdup) {
            alert("Function name " + fname + " already exists!");
        }

    } while (isdup);


    // main root expression for the function
    //
    var funcExpr = new ExprObj(false, ExprType.FuncCall, fname);

    // Create a new func obj and record in the current module
    //
    var fO = new FuncObj();
    fO.funcCallExpr = funcExpr;      // set reference to func expr

    CurModObj.allFuncs.push(fO);     // add function to current module

    assert((fO.argsAdded < 0), "Can't have any args yet");

    // STEP A: -------------------------------------------------- 
    //
    // Add return value (scalar grid).
    // NOTE: Return value is always a scalar. 
    //
    // Create a scalar grid and add it to the Function Grids/Header
    //
    var newGId = fO.allGrids.length;
    //
    var newgO = new GridObj(newGId, DefRetValName, 1, 1,
			    1, 1, false, false, false, false);
    newgO.isRetVal = true;
    //	
    // Record this grid in function header
    // Note: We add return value as the very first grid to function.
    //       This is necessary for having a unique grid ID for each 
    //       Grid (return value is a grid). This is useful in showing
    //       return *data* value. Also, user can directly assign to this.
    //
    fO.allGrids.push(newgO);
    fO.allSteps[FuncHeaderStepId].allGridIds.push(newGId);
    //
    fO.argsAdded = 0;     // indicates return value added

    // Add the data type to the arg grid. Return value is always integer
    //
    newgO.dataTypes.push(DataTypes.Integer);
    assert((newgO.dataTypes.length == 1), "must have only global type");
    newgO.typesInDim = -1;

    // Redraw step to reflect the addition of the function
    //
    drawProgStructHead();            // update program structre heading
    replaceOrAppendExpr(funcExpr);
    drawCodeWindow(CurStepObj);      // redraw code window
}


//----------------------------------------------------------------------------
// Add a libary function expression
//----------------------------------------------------------------------------
function addLibFuncExpr() {

    drawLibSelWin(0);
}

//----------------------------------------------------------------------------
// Draw the dialog for selecting (and inserting) a library function
//----------------------------------------------------------------------------
function drawLibSelWin(lib_ind) {

    var pO = CurProgObj;

    // STEP:
    // Create the list of all libraries to select
    //
    var str = "<table class='selectWin'>"
    str += "<tr><td> Select Library: <select id='selWin' " +
        " onchange='drawLibSelWin(this.selectedIndex)'> ";
    //
    for (var i = 0; i < pO.libModules.length; i++) {
        str += "<option value='" + i + "'";
        if (i == lib_ind)
            str += "selected";

        str += ">" + pO.libModules[i].name + "</option>";
    }

    //
    str += "</select>";
    str += "<BR><BR><div></div>";
    str += "</td></tr>"



    // STEP:
    // Create a list of all functions in the selected library
    // 
    str += "<tr><td> Select Function: <select id='selWin2'>";

    //	+ " onc='libFuncPicked("+ lib_ind + ",this.selectedIndex)'> ";
    //
    var libM = pO.libModules[lib_ind];

    for (var i = 0; i < libM.allFuncs.length; i++) {
        str += "<option value='" + i + "'";
        str += ">" + getFuncCallStr(libM.allFuncs[i].funcCallExpr) +
            "</option>";
    }
    //
    str += "</select>";


    str += "<BR><BR><div></div>";
    str +=
        "<input type='button' value='Cancel' onclick='cancelLibFunc()'>";
    str +=
        "<input type='button' value='Insert' onclick='insertLibFunc()'>";


    str += "<BR><BR><div></div>";
    str += "</td></tr>"


    str += "</table>";

    displayAtLastGrid(str);

    // change the innerHTML of the HTML object 
    //



}

//----------------------------------------------------------------------------
// Dspalay an html object (given by 'str') at the last grid position to be 
// added
//----------------------------------------------------------------------------
function displayAtLastGrid(str) {

    var gridId = CurStepObj.allGridIds.length;
    CurHtmlGridId = AllHtmlGridIds[gridId];
    var menuId = CurHtmlGridId;
    var menu1 = document.getElementById(menuId);
    menu1.innerHTML = str;
}


//----------------------------------------------------------------------------
// Get the function call string for a function call expression
//----------------------------------------------------------------------------
function getFuncCallStr(fexpr) {

    assert(fexpr.isFuncCall());

    // TODO: add args (with data types)
    //
    return (fexpr.str);
}

//----------------------------------------------------------------------------
// Cancel the library function selection
//----------------------------------------------------------------------------
function cancelLibFunc() {

    displayAtLastGrid("");
}

//----------------------------------------------------------------------------
// Inser a library function in the currently active grid
//----------------------------------------------------------------------------
function insertLibFunc() {

    var pO = CurProgObj;

    var sel1 = document.getElementById('selWin');
    var libind = sel1.selectedIndex;

    var sel2 = document.getElementById('selWin2');
    var funcind = sel2.selectedIndex;

    // alert("TODO: Lib ind:" + libind + " funcind:" + funcind);

    displayAtLastGrid("");

    // Create a function call expression and insert it
    //
    removeGridHighlight();



    // main root expression for the function
    //
    var libM = pO.libModules[libind];
    var libF = libM.allFuncs[funcind];
    var libFexpr = libF.funcCallExpr;
    var fname = libM.name + "." + libFexpr.str;
    var funcExpr = new ExprObj(false, ExprType.LibFuncCall, fname);

    // Insert the dummy args so that user knows the number/type of args
    // required
    //
    for (var i = 0; i < libFexpr.exprArr.length; i++) {

        var argE = new ExprObj(true,
            libFexpr.exprArr[i].type,
            libFexpr.exprArr[i].str);
        funcExpr.addSubExpr(argE);
    }

    replaceOrAppendExpr(funcExpr);
    drawCodeWindow(CurStepObj);      // redraw code window
}


//----------------------------------------------------------------------------
// Add a libary function expression
//----------------------------------------------------------------------------
function addExistFuncExpr() {

    drawExistFuncSelWin(0);
}

//----------------------------------------------------------------------------
// Draw the dialog for selecting (and inserting) a library function
//----------------------------------------------------------------------------
function drawExistFuncSelWin(mod_ind) {

    var pO = CurProgObj;

    // STEP:
    // Create the list of all libraries to select
    //
    var str = "<table class='selectWin'>"
    str += "<tr><td> Select Module: <select id='selWin' " +
        " onchange='drawExistFuncSelWin(this.selectedIndex)'> ";
    //
    for (var i = 0; i < pO.allModules.length; i++) {
        str += "<option value='" + i + "'";
        if (i == mod_ind)
            str += "selected";

        str += ">" + pO.allModules[i].name + "</option>";
    }

    //
    str += "</select>";
    str += "<BR><BR><div></div>";
    str += "</td></tr>"



    // STEP:
    // Create a list of all functions in the selected moduel
    // 
    str += "<tr><td> Select Function: <select id='selWin2'>";

    var modO = pO.allModules[mod_ind];

    for (var i = modO.FuncStartID; i < modO.allFuncs.length; i++) {
        str += "<option value='" + i + "'";
        str += ">" + getFuncCallStr(modO.allFuncs[i].funcCallExpr) +
            "</option>";
    }
    //
    str += "</select>";


    str += "<BR><BR><div></div>";
    str +=
        "<input type='button' value='Cancel' onclick='cancelLibFunc()'>";
    str +=
        "<input type='button' value='Insert' onclick='insertExistFunc()'>";


    str += "<BR><BR><div></div>";
    str += "</td></tr>"


    str += "</table>";

    displayAtLastGrid(str);

}

//----------------------------------------------------------------------------
// Inser a library function in the currently active grid
//----------------------------------------------------------------------------
function insertExistFunc() {

    var pO = CurProgObj;

    var sel1 = document.getElementById('selWin');
    var modind = sel1.selectedIndex;
    var modO = pO.allModules[modind];

    var sel2 = document.getElementById('selWin2');
    var funcind = sel2.selectedIndex + modO.FuncStartID;

    displayAtLastGrid("");


    // Create a function call expression and insert it
    //
    removeGridHighlight();

    // main root expression for the function
    //

    var existF = modO.allFuncs[funcind];
    var existFexpr = existF.funcCallExpr;
    var fname = existFexpr.str;

    // If this function is from a different module, add module name
    //
    if (modO != CurModObj)
        fname = modO.name + "." + fname

    var funcExpr = new ExprObj(false, ExprType.FuncCall, fname);

    // Insert the place hodler args so that user knows the number/type of args
    // required
    //
    for (var i = 0; i < existFexpr.exprArr.length; i++) {

        var argE = new ExprObj(true,
            existFexpr.exprArr[i].type,
            "arg" + i
            //existFexpr.exprArr[i].str
        );

        // Mark this argument as a place holder and add it to function call
        //
        argE.deleted = DeletedState.PlaceHolder;
        funcExpr.addSubExpr(argE);
    }

    replaceOrAppendExpr(funcExpr);      // insert func call expr 
    drawCodeWindow(CurStepObj);         // redraw code window
}


//----------------------------------------------------------------------------
// This method is called to replace an existing expression (with newExpr) OR
// to append the newExpr, based on whether an expression is currently 
// highlighted (active) or not.
//----------------------------------------------------------------------------
function replaceOrAppendExpr(newExpr) {

    var sO = CurStepObj;
    var pos;

    // if there is no active parent expression, nothing to replace. This
    // could happen when the focus is not on a code box
    //
    if (!sO.activeParentExpr)
        return;


    // If an expression is active, we replace that expression with
    // newExpr. If a space is active, we append newExpr (replace space)
    //
    if (!sO.isSpaceActive) {            // expression highlighted

        pos = sO.activeChildPos;

        if (pos != ROOTPOS) {

            if (sO.activeParentExpr.isUserFuncCall()) {

                // if we are replacing an arg in a function call, handle it
                //
                replaceFuncArg(sO.activeParentExpr, pos, newExpr);

            } else {

                // Replace existing expression
                //
                //sO.activeParentExpr.exprArr.splice(pos, 1, newExpr);

		//KK: Removed above and added three following ones, to
		//    make uniform indexing, now each index is a concat
		//    (type 12) and contains anything else as sub-expression
		//TODO: CAUTION: Make sure compatible with rest of the
		//      program (code-gen and parallelism detection)!

    		var concExpr = new ExprObj(false, ExprType.Concat, "");
    		sO.activeParentExpr.exprArr.splice(pos,1,concExpr);
    		sO.activeParentExpr.exprArr[pos].addSubExpr(newExpr);



                if (sO.activeParentExpr.isGridCell()) {

                    // Write invalid index to 0th dim
                    //
                    sO.activeParentExpr.dimIds[0] = -1;
                    showTip(TipId.CellIndReplace);
                    logMsg(
                        "Removed cell highlight dueto manual arr expr edit"
                    );
                }
            }

	} else {
	    showTip(TipId.RootEdit);    // can't replace root str (e.g., if)
	}

    } else {                            // if space highlighted
	
	pos = -1;                       // append at very end by default

	if (sO.activeChildPos >= 0) {
	    pos = sO.activeChildPos;    
	}

	// When a space is active, by default, we append the expression
	// at that space. However, if the space is next to an existing
	// single character operator, see whether we should combine them
	// to form a two char operator like +=, -=, ==, etc.
	// NOTE: A space and the *following* expression share the same
	//       child ID.
	//
	var exparr = sO.activeParentExpr.exprArr;

        if (newExpr.isAssignOp() && (pos > 0) &&
            exparr[pos - 1].isUniOperator()) {

            // if the new expression is an assignment operator (single = sign)
            // AND the preceding is a single character operator, combine
            // them to form a two char operator -- e.g., +=, ==, -=

            exparr[pos - 1].str += newExpr.str;

        } else if (newExpr.isUniOperator() && (pos < exparr.length) &&
            exparr[pos].isAssignOp()) {

            // new expression is an single operator (e.g., +/-/..) and the 
            // following is the assignement operator, combine them 
            // -- e.g., +=, -=, ==
            // NOTE: We add to the expression at 'pos' (not pos+1) because
            //       'pos' is the position of space. A space and the 
            //       following expression share the same child ID. 
            //
            exparr[pos].str = newExpr.str + exparr[pos].str;

        } else { // general case

            // Add new expression at the given position
            //
            exparr.splice(pos, 0, newExpr);
            //
            // If we are adding an arg to a function call, process argument
            // addition -- i.e., insert args into corresponding function 
            // header
            // 
            if (sO.activeParentExpr.isUserFuncCall()) {

                insertFuncArg(sO.activeParentExpr, pos);
            }

            sO.activeChildPos++; // move space to next since we appended
        }
    }
}

//----------------------------------------------------------------------------
// Remove an expression from the active position
//----------------------------------------------------------------------------
function removeExpr() {

    var sO = CurStepObj;
    var pos;

    // If an expression is active, we remove that expression AND activate
    // the space just before that removed expression (i.e., the sapce
    // id same as that of removed expression)
    //
    if (!sO.isSpaceActive) { // expression highlighted

        // console.log("no space");

        pos = sO.activeChildPos;

        if (pos != ROOTPOS) {

            //console.log("not root");

            // If we are deleting an arg of a function call, update the
            // header
            // 
            var removed = true;
            if (sO.activeParentExpr.isUserFuncCall()) {
                removed = removeFuncArg(sO.activeParentExpr, pos);
            }
	    //
            if (removed) {
                sO.activeParentExpr.exprArr.splice(pos, 1);
                sO.isSpaceActive = true; // activate space before removed expr
            }

        } else { // can't remove root str (e.g., if/else)


            if (sO.activeParentExpr.exprArr.length ||
                sO.activeParentExpr.isCondition()) {

                // Cannot delete non-empty 'foreach' OR any mask box 
                //
                showTip(TipId.RootEdit);

            } else {

                // if no children, mark as deleted (e.g., bare 'foreach')
                //
                sO.activeParentExpr.deleted = DeletedState.Deleted;
            }
        }

    } else { // if space highlighted

        // console.log("space @" + sO.activeChildPos);

        //pos = -1;                  // remove at very end by default

        if (sO.activeChildPos > 0) {
            pos = --sO.activeChildPos; // -- to move to previous space

            /*
	    if (pos < 0) {                 // if we moved to ROOTPS
		sO.isSpaceActive = false;  // space no longer active
	    }
	    */

            sO.activeParentExpr.exprArr.splice(pos, 1);
        }



        // var expr = sO.activeParentExpr.exprArr[pos];
        // if (expr.isDefinition()) expr.str = 'DELETED';



    }

}

//----------------------------------------------------------------------------
// update the 'let name' when user edits the name
//----------------------------------------------------------------------------
function updateLetName(obj) {

    // alert("Let updating ...");

    var sO = CurStepObj;
    //
    if (!sO.isSpaceActive) {

        var pos = sO.activeChildPos;

        // Note: Let name (identifier) is at the 0th sub-expression
        //
        var str = obj.innerHTML;

        if (isExistingName(CurModObj, CurFuncObj, CurStepObj, str)) {

            alert("Let name " + str +
                " already exists, or a reserved word!");

        } else {
            sO.activeParentExpr.exprArr[pos].exprArr[0].str = str;

        }

        // Redraw program structure heading to indicate new function name
        //
        drawProgStructHead(); // update program structre heading
        drawCodeWindow(sO);
    }
}

//----------------------------------------------------------------------------
// Update the string (name) of the active expression. Used to update
// function names.
//----------------------------------------------------------------------------
function updateFuncName(obj) {

    var sO = CurStepObj;
    //
    if (!sO.isSpaceActive) {
        var pos = sO.activeChildPos;
        var fname = obj.innerHTML;

        if (isExistingName(CurModObj, CurFuncObj, CurStepObj, fname)) {

            alert("Function name " + fname + " already exists!");

        } else {

            sO.activeParentExpr.exprArr[pos].str = obj.innerHTML;
        }

        // Redraw program structure heading to indicate new function name
        //
        drawProgStructHead(); // update program structre heading
        drawCodeWindow(sO);
    }
}

//----------------------------------------------------------------------------
// Update a number inserted by '123' button
//----------------------------------------------------------------------------
function updateNumber(obj) {

    var sO = CurStepObj;
    var pos = sO.activeChildPos;
    var numstr = obj.innerHTML;

    sO.activeParentExpr.exprArr[pos].str = numstr;

    // Update the function arg type, if this number is a function arg
    //
    if (sO.activeParentExpr.isUserFuncCall()) {
        updateFuncArgType(sO.activeParentExpr, pos, numstr);
    }

}

//----------------------------------------------------------------------------
// Update a string inserted by 'abc' button
//----------------------------------------------------------------------------
function updateString(obj) {

    var sO = CurStepObj;
    var pos = sO.activeChildPos;
    sO.activeParentExpr.exprArr[pos].str = obj.innerHTML;
}


//----------------------------------------------------------------------------
// PRE: The caller must ensure dimSelectTabs contain the correct selected
//      indices for EVERY dimension -- including dimension 0 and 1.
//----------------------------------------------------------------------------
function buildExpr(gO, iO) {

    // Create grid expression 
    //
    var gridExpr = new ExprObj(false, ExprType.GridCell, gO.caption);


    // go thru all dimensions and build sub-expressions for gridExpr
    //
    for (var d = 0; d < gO.numDims; d++) {

        var ind = gO.dimSelectTabs[d]; // selected index for a given dim

        if (gO.dimHasTitles[d]) {

            var expr = new ExprObj(true, ExprType.Title, gO.dimTitles[d][
                ind
            ]);
	   
            expr.gO = gO; // Added pointer to grid (to use in titles)
	    var concExpr = new ExprObj(false, ExprType.Concat, "");
     	    concExpr.addSubExpr(expr);
            gridExpr.addSubExpr(concExpr);
            gridExpr.dimIds.push(ind); // record selected index for dim

        } else {

            /*
	    var dname =  getIndExprStr(iO.dimIndExprs[d][ind]);
	    var expr = new ExprObj(true, ExprType.Index, dname);
	    */

            // Make a direct reference to the index expression
            //
            var expr = iO.dimIndExprs[d][ind];
            gridExpr.addSubExpr(expr);
            gridExpr.dimIds.push(ind); // record selected index for dim
        }
    }

    gridExpr.gO = gO;                    // record a reference to gO

    //gridExpr.str = gO.caption;           // record a reference to caption

    return gridExpr;
}



//----------------------------------------------------------------------------
// When re-drawing a grid object, reset row/col indices
//----------------------------------------------------------------------------
function resetColRowIndices(gO, iO) {

    // Add any index variable names for the step, if gO contains more
    // dimensions than previously noted for this step (using previously added
    // grids)
    //
    updateDimIndVarNames(CurStepObj, gO);

    for (var d = 0; d < gO.numDims; d++) { // add dim name in the middle
        var ind = Math.floor(gO.dimShowSize[d] / 2)
        var dexpr = getDefDimIndExpr(d);
        iO.dimIndExprs[d][ind] = dexpr;
        gO.dimSelectTabs[d] = ind;

	if (gO.dimIsExtended[d] == false) { //MPI: Also means gO.isDistributed 
					    //     == true && 
					    //     gO.isGlobal == true
            // Add 0 and 'end' index values 
            // Note: Add start (0) last -- in case the dim has only 1 entry
	    //       it should be labeled 0 (not endX)
	    //
            iO.dimIndExprs[d][gO.dimShowSize[d] - 1] = getDefIndEndExpr(d);
            iO.dimIndExprs[d][0] = getDefIndStartExpr();


	} else { //MPI: If dimension is extended use startX/endX

            // Note: Add start (0) last -- in case the dim has only 1 entry
	    //       it should be labeled 0 (not endX)
	    //
	    iO.dimIndExprs[d][gO.dimShowSize[d] - 1] = 
		getDefIndEndExprExt(gO, d);
	    iO.dimIndExprs[d][0] = getDefIndStartExprExt(gO, d); 
	}
    }
}



//---------------------------------------------------------------------------
// change caption of table (obj) when a contenteditable div element 
// is changed. Argument obj = html DOM object of the caller HTML element
// NOTE: Can accept additional parameters if needed
// We should allow changing caption while a grid is configure as well as
// while a grid is used (e.g., incoming func arg name). Therefore, we 
// cannot use NewGridObj but has to find the gO based on htmlId
//---------------------------------------------------------------------------
function changeCaption(obj, htmlId) {

    var gname = obj.innerHTML;

    if (isExistingFuncName(CurModObj, gname) ||
        isExistingGridNameInFunc(CurFuncObj, gname)) {

        alert("Name " + gname + " alrady exists!");
        return;
    }


    if (htmlId == PreviewHtmlGridId)  {      // while selecting existing grid

        NewGridObj.caption = gname;

    } else { // while within a step

	var sO = CurStepObj;
	var gridId = getGridIdOfHtmlId(htmlId);       // find grid object
	var gO = CurFuncObj.allGrids[gridId];
	gO.caption = gname;
    }
}


//---------------------------------------------------------------------------
// change column titles of the NewGridObj
//---------------------------------------------------------------------------
function changeColTitle(obj, col) {
    NewGridObj.dimTitles[ColDimId][col] = obj.innerHTML;
};


//---------------------------------------------------------------------------
// changes the row title
//---------------------------------------------------------------------------
function changeRowTitle(obj, row) {
    NewGridObj.dimTitles[RowDimId][row] = obj.innerHTML;
};



//----------------------------------------------------------------------------
// Updates the indent variable of sO.boxAttribs[box] for each box in the
// code winodw. This method must be called before drawing the codeWindow
// to update indent of each box.
// Returns max # of tabs (indents) seen in any of the boxes.
//----------------------------------------------------------------------------
function setCodeWindowIndentation(sO) {

    var indent = 1;           // indent to apply to next box (start from mask)
    var maxindent = 0;        // maximum indetation we saw

    // Go thru each box and set indentation. We start from Mask because
    // we never change indentation of Range (or Mask but we need to start
    // from Mask to set the next one). 
    //
    for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

        var expr = sO.boxExprs[box];
        var attrib = sO.boxAttribs[box];

        // if there is explicit indentation that the user has applied
        // apply that correction
        //
        indent += attrib.forceIndent;

        // If I am an else/elseif expression, automatically left shift by one
        //
        if (expr && (expr.isElse() || expr.isElseIf()))
            indent--;

        // If the forceIndent became went too left, cap it at 1. 
        // This could happen when the user moves a box, B, above to the left. 
        // However, we still do NOT fix forceIndent because, if the user 
        // moves box B again to the right, the current box should preserve
        // the forced indent.
        //
        if (indent < 1) indent = 1;


        // Record auto indentation in sO attribute
        //
        attrib.indent = indent;

        // Calculate the indent for NEXT box (also totoal columns) 
        //
        if (attrib.isMask() && expr) {

            if (expr.isIf() || expr.isElseIf() || expr.isElse())
                indent++;
        }

        if (indent > maxindent)
            maxindent = indent;

    }

    // Return total # of tabs (indents) seen in this code
    //
    return (maxindent);
}


//----------------------------------------------------------------------------
// Draws the code window based on the state of sO.boxExprs and sO.boxAttribs
//----------------------------------------------------------------------------
function drawCodeWindow(sO) {

    // STEP:
    // if we are currently editing an index expression in a grid -- e.g.,
    // row+4 -- we just need to redraw that expression -- we do not need
    // to redraw the entire code window.
    //
    if (sO.focusBoxId == CodeBoxId.Index) {
        redrawGridIndexExpr(sO);
        return;
    }

    // .....................................................................
    // STEP: Start drawing the code window table
    //
    var str = "<table class='codeWin'>";
    //
    // Small Vertical spaces between boxes
    //
    var vspace = "<tr><td><div style='height:4px'> </div></td></tr>";
    //
    // Find correct auto indentation AND total # of tab indents for table
    // Total columns is total indents + 1 + 1 (for 'let column')
    //
    var totcols = setCodeWindowIndentation(sO) + 1;


    // STEP: Draw Index names. Each index name is for a dimension.
    //
    var inames = getIndNames4Step(sO);
    //
    str += "<tr>" + "<td></td>" + "<td colspan=" + totcols +
        " style='text-align:center'>" + inames +
        "<div style='height:10px'></div></td>" + "</tr>" + vspace


    // STEP: Go thru each box and draw it
    //
    for (var box = CodeBoxId.Range; box < sO.boxAttribs.length; box++) {

        var expr = sO.boxExprs[box];
        var exprstr = "";
        var hasfocus = (sO.focusBoxId == box);
        //
        if (expr) {

            // If the box has focus always show expression. Otherwise, 
            // show the expression only if the statment is 'not empty' --
            // i.e., just 'if' is an empty statement.
            //
            var deleted = (expr.deleted == DeletedState.Deleted)

            if (!deleted && (hasfocus || !isEmptyStmt(expr))) {

                sO.curBox = box; // pass box # to getHtmlExprStmt
                //
                exprstr = getHtmlExprStmt(expr, sO, "");
            }
        }

        // Onclick function to change the focus to this box if the user
        // clicks on it. If the user has already clicked on this box (hasfocus)
        // then draw a glowing shadow.
        //
        var oncBox = " onclick='changeCWInputBox(" + box + ")' ";
        //
        var glow = (!hasfocus) ? "" :
            " style='box-shadow: -3px 3px 3px #dcffed' ";

        // Start a new row with label
        //
        var type = sO.boxAttribs[box].type;
        //
        var label = "Index Range";
        if (type == CodeBoxId.Mask) label = "Condition";
        if (type == CodeBoxId.Formula) label = "Formula";
        //
        str += "<tr>" + "<td>" + label + ":</td>";

        // print empty cells for proper indentation AND find column span
        // for the expression
        //
        for (var t = 0; t < sO.boxAttribs[box].indent; t++) {
            str += "<td></td>";
        }
        var colspan = totcols - sO.boxAttribs[box].indent;

        // FIX: Unique id needed
        //
        var commentId = getCommentHtmlId(CommentType.StepBox, box);
        var cur_comment = sO.boxAttribs[box].comment;
        var comment = getCommentStr('span', commentId, "", cur_comment);
        //

        var letstr = "";
        /*
	if (box == 3) {

	    letstr = "<span onclick='letClicked()'>let</span>";
	}
	*/

        // Draw the codebox containing expression
        //
        str += "<td class='formula' colspan=" + colspan + oncBox + glow +
            ">"

        + letstr
            + exprstr + "</td>" + "<td>" + comment + "</td>" // comment
            + "</tr>" + vspace;
    }


    // ......................................................................
    // STEP: Draw operator buttons at the BOTTOM of code window (same table)
    //
    var hsep = "<span> &nbsp </span>";

    // default input button description
    //
    var bstr = "<input type='button' class='opbutton' "; 
    var bstr2 =  "<button type='submit' class='opbutton' ";

    str += "<tr><td><div style='height:10px'> </div></td></tr>";
    str += "<tr>" + "<td></td>" // skip label cell

	+ "<td style='text-align:center' colspan=" + totcols + "> " 
	+ bstr + "value='+' onclick='addOpExpr(\"+\")'>" 
	+ bstr + "value='-' onclick='addOpExpr(\"-\")'>" 

	+ bstr + "value='*' onclick='addOpExpr(\"*\")'>"
	+ bstr + "value='/' onclick='addOpExpr(\"/\")'>"  
	+ hsep

        + bstr + "value='<' onclick='addOpExpr(\"<\")'>" 
        + bstr + "value='>' onclick='addOpExpr(\">\")'>" 
        + bstr + "value='<' onclick='addOpExpr(\"<\")'>" 
        + bstr + "value='=' onclick='addOpExpr(\"=\")'>" 
        + bstr + "value='!=' onclick='addOpExpr(\"!=\")'>" 
        + bstr + "value='<-' onclick='addOpExpr(\"<-\")'>"

	+ bstr + "value='OR' onclick='addOpExpr(\"OR\")'>" 
	+ bstr + "value='AND' onclick='addOpExpr(\"AND\")'>" 
	+ bstr + "value='NOT' onclick='addOpExpr(\"NOT\")'>"

	//DC4: TRUE/FALSE values (for boolean variables).
	+ bstr + "value='TRUE' onclick='addStrExpr(\".TRUE.\")'>" 
	+ bstr + "value='FALSE' onclick='addStrExpr(\".FALSE.\")'>" 
	+ hsep

        + bstr + "value='(' onclick='addOpExpr(\"(\")'>" 
        + bstr + "value=')' onclick='addOpExpr(\")\")'>" 
        + hsep
         
	// Buttons for functions. To create subscripts, we use 'bstr2'
	//
	+ bstr2 + "onclick='addNewFuncExpr()' title='new function'> " 
	+ "f<sub>new</sub></button>"
	//
	+ bstr2 + "onclick='addExistFuncExpr()' title='existing function'> " 
	+ "f<sub> </sub></button>"
	//
	+ bstr2 + "onclick='addLibFuncExpr()' title='library function'> " 
	+ "f<sub>lib</sub></button>"
	//
	+ hsep

	// Note: number expression is useful for adding a numeric func arg
	//  
        + bstr + " title='number' " 
	+  "value='123' onclick='addNumExpr(\"123\")'>" 

        + bstr + " title='string (characters)' " 
	+ "value='abc' onclick='addStrExpr(\"abc\")'>" 

        + bstr + " title='index end' " +  "value='end' onclick='addEndExpr()'>" 
        + hsep;

    str += bstr + "value='Delete' onclick='delOpExpr()'>" 
	+ "</td></tr>";


    // Does the Code Window has the focus (the CW may not have focus when
    // we are editing something else like an dim name
    //
    var cwFocus = (sO.focusBoxId != CodeBoxId.INVALID);

    // Add additional buttons for condition processing (if/else/...) if
    // the focus is on a mask box
    //
    if (cwFocus && sO.boxAttribs[sO.focusBoxId].isMask() &&
        (sO.activeChildPos == ROOTPOS)) {

        str += "<tr><td></td>" 
	    + "<td style='text-align:center' colspan=" + totcols + ">";

        var onc = " onclick='changeCondKeyword(\"if\"," + ExprType.If + ")'";
        str += bstr + "value='if' " + onc + ">";
        //
        var onc = " onclick='changeCondKeyword(\"else\"," + ExprType.Else +
            ")'";
        str += bstr + "value='else' " + onc + ">";
        //
        var onc = " onclick='changeCondKeyword(\"else if\"," +
            ExprType.ElseIf + ")'";
        str += bstr + "value='else if' " + onc + ">";
        //
        //var onc = " onclick='changeCondKeyword(\"break if\"," + 
        //    ExprType.BreakIf + ")'";
        //str += bstr + "value='break if' " + onc +  ">";

        str += "</td></tr>";
    }


    // Add additional buttons for formula processing (break/return) if
    // the focus is on a formula box AND we are at the start of the formula
    //
    if (cwFocus && sO.boxAttribs[sO.focusBoxId].isFormula() &&
        sO.activeParentExpr && sO.activeParentExpr.isExprStmt() &&
        (sO.activeChildPos == 0)) {

        str += "<tr><td></td>" +
            "<td style='text-align:center' colspan=" + totcols + ">";

        // Let
        //
        var onc = " onclick='addLetExpr()' ";
        str += bstr + "value='let' " + onc + ">";
        //
        // Continue
        //
        var onc = " onclick='addFormulaKeyword(\"continue\"," +
            ExprType.Continue + ")'";
        str += bstr + "value='continue' " + onc + ">";
        //
        // Return
        //
        var onc = " onclick='addFormulaKeyword(\"return\"," +
            ExprType.Return + ")'";
        str += bstr + "value='return' " + onc + ">";
        //
        // Break
        //
        var onc = " onclick='addFormulaKeyword(\"break\"," +
            ExprType.Break + ")'";
        str += bstr + "value='break' " + onc + ">";
        //
        str += "</td></tr>";
    }

    // Add additional buttons for keywords if the focus is on a range box
    //
    if (cwFocus && sO.boxAttribs[sO.focusBoxId].isRange() &&
        ((sO.activeChildPos == ROOTPOS) ||
            (sO.isSpaceActive && !sO.activeChildPos))) {

        str += "<tr><td></td>" +
            "<td style='text-align:center' colspan=" + totcols + ">";

        var onc = " onclick='changeLoopToForeach()' ";
        str += bstr + "value='foreach' " + onc + ">";
        //
        var onc = " onclick='changeLoopToForever()' ";
        str += bstr + "value='forever' " + onc + ">";

        str += "</td></tr>";
    }
    // STEP: Show/hide step options
    if(sO.showOptions) {

	// Show checkbox for user to specify enabling converting to OpenCL.
	// Example: for simple initializations the data transfer and OpenCL
	// overhead may outweigh the benefits (so don't transform step to
	// OpenCL kernel).
	var onc = "onclick='markOCLstep(this.checked)'";
        if (sO.potOCLstep) onc += " checked ";
	str += "<tr><td></td>" +
            "<td style='text-align:center' colspan=" + totcols + ">";
        str += "<input type='checkbox' " + onc + ">" +
	    "Enable OpenCL (if step parallelizable)" +
	    "</td></tr";

    }
    
    //
    str += "</table>";

    // Write to code window HTML element
    //
    var cw1 = document.getElementById('codeWindow');
    cw1.innerHTML = str;


    // .....................................................................
    // STEP: Draw Action Buttons to the right of the window
    //
    var but1 = "",
        but2 = "",
        but3 = "";

    // buttons appear after grid selection
    //
    var has_but = sO.stageInStep >= StageInStep.GridSelectDone;
    //
    if (has_but) {

        var onc = " onclick='addSource()' ";
        but1 = bstr + "value='Add Source Grid' " + onc;
        //
    }
    //
    var focusbox = sO.focusBoxId;
    var prop = " type='button' class='formulabut' "

    //
    str = "<table class='operatorWin'>"

    + "<tr><td>" + but1 + "</td></tr>"
        //+ "<tr><td><div style='height:10px'></div></td></tr>"

    + "<tr><td>" + "<input" + prop +
        "value='Add Formula' onclick='addFormulaBox()'>" + "</td></tr>"

    + "<tr><td>" + "<input" + prop +
        "value='Add Condition' onclick='addMaskBox()'>" + "</td></tr>";

    // Delete Box (don't allow deleting first 3 boxes -- TODO: relax this)
    //
    if (sO.focusBoxId > CodeBoxId.Range) {

        var val = (sO.boxAttribs[sO.focusBoxId].isMask()) ?
            "Delete Condition" : "Delete Formula";
        str += "<tr><td>" + "<input" + prop + "value='" + val +
            "' onclick='deleteBox()'>" + "</td></tr>";
    }


    str += "<tr><td>" + "<input" + prop +
        "value='<=' onclick='updateBoxIndent(-1)' ";
    var dis = isBoxIndentLegal(focusbox, -1) ? "" : " disabled "
    str += dis + ">"

    + "<input" + prop + "value='=>' onclick='updateBoxIndent(1)' ";
    var dis = isBoxIndentLegal(focusbox, 1) ? "" : " disabled "
    str += dis + "></td></tr>"

    /*
    str += "<tr><td>"
	+ "<img src='images/refresh.png' width=20px height=20px " 
	+ " title='refresh step' onclick='refreshCurStep()'>"
	+ "</td></tr>";
    */

    /* TODO: Enable later when incorporating OpenCL code-gen
    //OCL:
    + "<tr><td>" + "<input" + prop +
    "value='Options' onclick='showHideOptions()'>" + "</td></tr>"
    */

    + "</table>";

    var opw1 = document.getElementById('operatorWindow');
    opw1.innerHTML = str;

}

//----------------------------------------------------------------------------
// Show or Hide options for the step
//----------------------------------------------------------------------------
function showHideOptions() {

    // Toggle show options status in CurStepObj and draw code window
    // TODO: It is not necessary to draw the entire code window. Separate
    //       drawCodeWindow() function into 2 (or 3) functions, and call the
    //       the one responsible for drawing buttons.
    //
    CurStepObj.showOptions = !CurStepObj.showOptions;
    drawCodeWindow(CurStepObj);
}


// Refresh current step
//----------------------------------------------------------------------------
function refreshCurStep() {
    drawStep(CurStepObj);
}


//----------------------------------------------------------------------------
// Returns whether a given expression is 'empty'
//----------------------------------------------------------------------------

function isEmptyStmt(expr) {

    // for a if/elseif or a foreach statement, if only the root expression
    // is present without any children, then it is treated as an 
    // empty stmt. Note: Forever and Else don't need children.
    //
    if ((expr.isIf() || expr.isElseIf() || expr.isForeach()) &&
        (!expr.exprArr || !expr.exprArr.length)) {
        return true;
    }

    return false;
}





//----------------------------------------------------------------------------
// Retruns whether 'focusbox' can be indented by 'val' based on the current
// indent of the 'focusbox' stored in CurStepObj.
// This method is called to disable left/rigth indent buttons
//----------------------------------------------------------------------------
function isBoxIndentLegal(box, val) {


    var sO = CurStepObj;
    var attrib = sO.boxAttribs[box];

    // Do not allow any indentation of range 
    //
    if (box <= CodeBoxId.Range)
        return false;

    // Do not allow left shifting for a box that is already at left edge
    //
    if (attrib.indent + val < 1)
        return false;

    // don't allow right shifting beyond my predecessor
    //
    if ((attrib.indent + val) > sO.boxAttribs[box - 1].indent)
        return false;


    // if this is the formula right after the mask, do not allow any 
    // shifting. It is fully automatic. This is because there must be at
    // least one formula after a mask statement
    //
    if ((attrib.type == CodeBoxId.Formula) &&
        (sO.boxAttribs[box - 1].type == CodeBoxId.Mask))
        return false;


    // if this is a mask box with ELSE or ELSEIF expression, then it must
    // be at least one tab left to the previous block
    //
    var expr = sO.boxExprs[box];
    if ((attrib.type == CodeBoxId.Mask) &&
        expr && (expr.isElse() || expr.isElseIf()) &&
        (sO.boxAttribs[box].indent + val >= sO.boxAttribs[box - 1].indent)
    )
        return false;


    // alert("indent: " + attrib.indent + " val:" + val);

    // The required shifting in the direction of 'val' is allowed
    //
    return true;

}

//----------------------------------------------------------------------------
// update the indentation of a box in the forumula window
//----------------------------------------------------------------------------
function updateBoxIndent(val) {

    var sO = CurStepObj;
    var focusbox = sO.focusBoxId;
    var attrib = sO.boxAttribs[focusbox];


    attrib.forceIndent += val;

    // alert("new force indent for box " + sO.focusBoxId + " is: " +
    //	  sO.boxAttribs[sO.focusBoxId].forceIndent);

    drawCodeWindow(sO);

}



//----------------------------------------------------------------------------
// This function is called when the user clicks on an input box in the
// code window OR when the user clicks on an expression (The latter case
// is just ignored) 
// boxid: boxid to change to
// force: force changing to box (do not bail out early)
//----------------------------------------------------------------------------
function changeCWInputBox(boxid, force) {

    var pO = CurProgObj;
    var sO = CurStepObj;


    // Determine whether this event was called by clicking on and empty area
    // in a box (table cell in code window) OR an expression (which is a span
    // element). In the latter case, we just return after drawing the 
    // code window
    //
    var evt = window.event;
    if (!(evt.target instanceof HTMLTableCellElement) && !force) {

        // alert("evt.target:" + evt.target + 
        //    " edit?" + evt.target.contentEditable);
        // 

        if (evt.target.contentEditable == 'true') {

            // Do nothing here. This means a contenteditable entry like
            // let name OR function name. Note: the type of contentEditable
            // property is a string (not a boolean value)
            //

        } else {
            // contenteditable not true -- e.g., 'false', 'inherit', etc
            //
            // If currently editing index, stop it first because we change
            // focusBoxId below
            //
            if ((sO.focusBoxId == CodeBoxId.Index) &&
                (boxid != CodeBoxId.Index))
                stopIndexEditing();

            sO.focusBoxId = boxid; // focused box  
            sO.focusBoxExpr = CurStepObj.boxExprs[boxid]; // focused exprdd
            drawCodeWindow(CurStepObj); // redraw code window w/ new expr

            //console.log("not table cell");
        }

        return;
    }

    //console.log("table cell");

    // If the step is currently showing data, stop showing data (beause
    // user clicked on a code box to write code (or highlight)
    //
    if (pO.showData != ShowDataState.None) {
        showData(ShowDataState.None);
        return;
    }


    // If we are currently editing an index, stop it.
    // OR if there an active expression/highlight, remove it first
    //
    if (sO.focusBoxId == CodeBoxId.Index) {
        stopIndexEditing();
    } else {
        removeExprAndGridHighlight();
    }

    // Set the focus (active) expression and box AND set the default space
    // at the end of expression
    //
    sO.focusBoxExpr = CurStepObj.boxExprs[boxid];   // focused expr
    sO.focusBoxId = boxid;                          // focused box  
    //
    setDefaultActiveSpace(sO);                  // set default space
    //
    drawCodeWindow(CurStepObj); // redraw code window w/ new expr
    //
    // Note: drawCodeWindow() sets activeParentExpr by calling getHtmlExprStmt.
    //       Therefore, the following code must follow the call to
    //       drawCodeWindow()
    //
    updateGridHighlight();         // redraw grid with any changed highlights

}

//----------------------------------------------------------------------------
// Adds a formula box to the code window. The box is inserted just below
// the current box with focus.
//----------------------------------------------------------------------------
function addFormulaBox() {
    
    var sO = CurStepObj;
    var pos = sO.focusBoxId + 1;        // append pos
 
    assert((pos <= sO.boxExprs.length), "invalid formula box position");

    // Create a default expression statement for the new box and insert it
    //
    var newExpr = new ExprObj(false, ExprType.ExprStmt, "");
    //
    sO.boxExprs.splice(pos, 0, newExpr);     // add bin (empty root expression)

    // Add the corresponding BoxAtribs object for the new box
    //
    // Note: We use a dummy indentation of value 0. The correct indentation
    //       will be set when we draw the code window below
    //
    sO.boxAttribs.splice(pos, 0, new BoxAttribs(0, CodeBoxId.Formula));

    changeCWInputBox(pos, true); // change the input box and redraw code win
}


//----------------------------------------------------------------------------
// Adds a mask box to the code window. The box is inserted just below
// the current box with focus.
//----------------------------------------------------------------------------
function addMaskBox() {

    var sO = CurStepObj;
    var len = sO.boxExprs.length;
    var pos = sO.focusBoxId + 1;        // append pos

    // Add an "if" expression by default
    //
    var expr = new ExprObj(false, ExprType.If, "if");
    sO.boxExprs.splice(pos, 0, expr);

    // Note: We use a dummy indentation of value 0. The correct indentation
    //       will be set when we draw the code window below
    //
    sO.boxAttribs.splice(pos, 0, new BoxAttribs(0, CodeBoxId.Mask));

    changeCWInputBox(pos, true);       // change the input box and redraw
}

//----------------------------------------------------------------------------
// Delete a box (formula/mask) in the formula window
//----------------------------------------------------------------------------
function deleteBox() {

    var sO = CurStepObj;
    var len = sO.boxExprs.length;
    var pos = sO.focusBoxId;            // delete pos

    // Remove box expression and attributes
    //
    sO.boxExprs.splice(pos, 1);
    sO.boxAttribs.splice(pos, 1);

    // change the input box and redraw
    //
    changeCWInputBox(sO.focusBoxId - 1, true); 

}

//----------------------------------------------------------------------------
// This function is called when the user clicks on an expression 
// (a grid expression OR an operator) in the code window. 
//----------------------------------------------------------------------------
function exprClicked(parentPath, childpos) {

    var sO = CurStepObj;

    resetOnInput();

    // if we the user clicked on the same expression he is currently
    // editing, nothing to do
    //
    if ((sO.activeParentPath == parentPath) &&
        (sO.activeChildPos == childpos) &&
        (!sO.isSpaceActive)) {
        return;
    }

    if (sO.focusBoxId == CodeBoxId.Index) {

        // we are here because an expression within a grid index is 
        // clicked (while a grid index is being edited). 
        //
        //alert("todo: handle grid index editing here");

        sO.activeParentPath = parentPath;
        sO.activeChildPos = childpos; // set active child pos
        sO.isSpaceActive = false; // indicate space inactive (expr active)
        redrawGridIndexExpr(sO); // redraw expression
        return;

    }


    //alert("Expression Clicked :" + id );

    // We are here because the user clicked on an expression (grid/operator).
    // However, if the user has already clicked on a previous grid expression
    // we have to remove that grid highlight before we do a new highlight.
    //
    removeExprAndGridHighlight();

    sO.activeParentPath = parentPath;
    sO.activeChildPos = childpos;  // set active expression id
    sO.isSpaceActive = false;      // indicate space inactive (expr active)
    //
    drawCodeWindow(sO);   // redraw the code window   
    //
    // Note: drawCodeWindow() sets activeParentExpr by calling getHtmlExprStmt.
    //       Therefore, the following code must follow the call to
    //       drawCodeWindow()
    //
    updateGridHighlight();

}


//----------------------------------------------------------------------------
// Called when the user clicks on a keyword
//----------------------------------------------------------------------------
function keywordClicked(parentPath, childPos) {

    var sO = CurStepObj;

    resetOnInput();

    // Record parent path (path to the root node)
    //
    sO.activeParentPath = parentPath;
    sO.activeChildPos = childPos;  // ROOTPS used here as childPos
    sO.isSpaceActive = false;      // indicate space inactive (expr active)
    //
    drawCodeWindow(sO);   // redraw the code window   
    //
    // Note: drawCodeWindow() sets activeParentExpr by calling getHtmlExprStmt.
    //       Therefore, the following code must follow the call to
    //       drawCodeWindow()
    //
    updateGridHighlight();
}



//----------------------------------------------------------------------------
// Get the active (highlighted) ExprObj indicated by sO.activeParentExpr
//----------------------------------------------------------------------------
function getActiveExprObj(sO) {

    var act_expr = sO.activeParentExpr;      // currently active expression

    if (!sO.isSpaceActive && act_expr) {     // if space not active

        assert((sO.activeChildPos < act_expr.exprArr.length),
            "invalid child");

        expr = act_expr.exprArr[sO.activeChildPos];
        //
        if (expr) {
            return expr;
        }
    }

    return null;
}


//----------------------------------------------------------------------------
// NOTE: depends on CurStepObj.activeParentExpr (which is set by 
//       getHtmlExprStmt)
//       Therefore, this function **MUST** be called after a call to 
//       drawCodeWindow();
//----------------------------------------------------------------------------
function updateGridHighlight() {

    var sO = CurStepObj;
    var expr = getActiveExprObj(sO);
    //
    // whether the expr or any of its children are deleted/need-update
    //
    var deleted = isAnyExprDeleted(expr);

    if (expr && expr.isGridCell() && !deleted) {

        var gO = expr.gO;

	for (var d=0; d < gO.numDims; d++) {       // copy active dims
	    gO.dimSelectTabs[d] = expr.dimIds[d];
	}
	//
	gO.hasSelection = true;

        var iO = getIndObjOfHtmlId(gO.htmlId);

        drawGrid(gO, iO, gO.htmlId, false);
    }

}




//----------------------------------------------------------------------------
// Remove any highlighted cell in a grid. A cell in a grid is highlighted
// because an expression is selected in the code window.
// Note: This does NOT remove the currently selected (highlighted) expression
//----------------------------------------------------------------------------
function removeGridHighlight() {

    var expr = getActiveExprObj(CurStepObj);

    if (expr && expr.isGridCell() &&
        (expr.deleted == DeletedState.None) &&
        expr.gO.hasSelection) {

        var gO = expr.gO;

        gO.hasSelection = false; // remove highlighted cells
        gO.selIndDim = INVID; // remove any highlighed indices

        var iO = getIndObjOfHtmlId(gO.htmlId);

        drawGrid(gO, iO, gO.htmlId, false);
        //
        // Note: we are not removing active expression in the current step
        //       i.e., CurStepObj.activeParentExpr (or activeParentPath)
    }


    // if we are currently showing data, stop it as well
    //
    if (CurProgObj.showData != ShowDataState.None) {
        showData(ShowDataState.None);
    }

}


//----------------------------------------------------------------------------
// If the user is currently editing an index cell in a grid, and then clicks
// on the docuemnt body OR a formula etc., we have to stop index editing
// first by calling this method.
//----------------------------------------------------------------------------
function stopIndexEditing() {

    var sO = CurStepObj;

    // We are currently editing an index. Change the focus box to something
    // else and redraw the current step.
    // Note: Do NOT call changeCWInputBox() because it calls stopIndexEditing
    //       in turn -- which will lead to infinite recursion
    //
    var  boxid = sO.boxExprs.length -1;      // TODO: change to previous
    sO.focusBoxExpr = sO.boxExprs[boxid];    // focused expr
    sO.focusBoxId = boxid;                   // focused box  
    setDefaultActiveSpace(sO);                  // set default space

    // Redraw the current step    
    //
    redrawCurStep();

}


//----------------------------------------------------------------------------
// Remove the active (highlighted) expression in the code window AND
// the corresponding highlighted grid cell. A cell in a grid is highlighted
// because an expression is selected in the code window.
// Note: caller must redraw code window 
//----------------------------------------------------------------------------
function removeExprAndGridHighlight() {

    var sO = CurStepObj;

    // get active child expression
    //
    var expr = getActiveExprObj(sO);

    if (expr) {

        var gO = expr.gO;

        // if the grid is highlighted
        //
        var need_grid_refresh = (expr.isGridCell() &&
            (gO.hasSelection || (gO.selIndDim >= 0)));

        if (need_grid_refresh || expr.isRange()) {

	    gO.hasSelection = false;    // no highlihged cell in grid
	    gO.selIndDim = INVID;       // no higlighted indices in any dim

            var iO = getIndObjOfHtmlId(gO.htmlId);

	    drawGrid(gO, iO, gO.htmlId, false);	
	    //
	    sO.activeParentExpr = null; // remove expression
	    sO.activeParentPath = "";   // no active expression/space
	    sO.activeChildPos = INVID;  // remove sub expression Id
	}


    }

    // Remove any number that is actively being entered
    //
    resetOnInput();
}

//----------------------------------------------------------------------------
// This function is called when the user clicks on a space in the 
// code window.
//----------------------------------------------------------------------------
function spaceClicked(parentPath, childpos) {

    var sO = CurStepObj;

    resetOnInput();

    // is this a space in grid index (while editing index)
    //
    var isIndexSpace = (sO.focusBoxId == CodeBoxId.Index);

    //if (sO.focusBoxId == CodeBoxId.Index) return;

    // alert("Space Clicked :" + parentseq + "," + childpos );

    // we are here because the user clicked on a space. At this time, an
    // expresion is selected (expression and corresponding grid cell
    // highlighted) remove that selection and highlights.
    //
    if (!isIndexSpace)
        removeExprAndGridHighlight(); // remove any highlighed expression

    // Set the active space Id and redraw the code window
    //
    sO.activeParentPath = parentPath;
    sO.activeChildPos = childpos;
    sO.isSpaceActive = true;

    //
    if (!isIndexSpace)
        drawCodeWindow(sO);
    else
        redrawGridIndexExpr(sO);
}



//----------------------------------------------------------------------------
// User has clicked on a range
//----------------------------------------------------------------------------
function rangeClicked(parentPath, childpos) {

    resetOnInput();

    // if the actively highlighted range is clicked again, return
    //
    if ((CurStepObj.activeParentPath == parentPath) &&
        (CurStepObj.activeChildPos == childpos) &&
        (!CurStepObj.isSpaceActive)) {
        return;
    }

    // alert("range clicked:" +  parentseq);

    // We are here because the user clicked on an expression (grid/operator).
    // However, if the user has already clicked on a previous grid expression
    // we have to remove that grid highlight before we do a new highlight.
    //
    removeExprAndGridHighlight();

    // Set new paramenters for the range clicked
    //
    CurStepObj.activeParentPath = parentPath;     // active expression
    CurStepObj.activeChildPos = childpos;  // set active sub-expression id
    CurStepObj.isSpaceActive = false;      // indicate space inactive 
    //
    drawCodeWindow(CurStepObj);   // redraw the code window       

    // redraw the grid with index row/col/tab highlighted
    // Note: the call to drawCodeWindow sets CurStepObj.activeParentExpr
    //
    var expr = getActiveExprObj(CurStepObj);
    //
    if (expr && expr.isRange()) {
	expr.gO.selIndDim = expr.selDim;     // use recorded dim in ExprObj

        var iO = getIndObjOfHtmlId(expr.gO.htmlId);

        drawGrid(expr.gO, iO, expr.gO.htmlId, false);
    }

    // TODO: draw grid object too.
}


//----------------------------------------------------------------------------
// Adds a source grid to a current step
//----------------------------------------------------------------------------

function addSource() {

    // Re-start source selection when we press button
    //
    CurStepObj.stageInStep = StageInStep.SrcSelectStarted;
    doNextStage();
}




//----------------------------------------------------------------------------
// Returns the html strings for a given expression. 
// Args:
//  e          -- root ExprObj
//  sO         -- StepObject1 specifying <activeSeq, activePos> tuple
//  parentPath -- The 'path' to parent ExprObj, if there is a parent. The 
//                'path' is a string of sequence numbers, starting from root
//                to the parent in the following format:  e.g., "5:3:2:9:"
//                Note the trailing colon. 
//  myPos      -- position of 'e' in parents exprArr() (if there is a parent)
//
// This method is called recursively to traverse a given expression tree.
// When we are at a node, it is always that nodes responsibility to 
// highlight all its children AND put onclick functions for all spaces the
// node requires. If a node is a leaf node (or node has leafy components 
// like function/grid names), the node should put onclick function for the
// node itself (the onclick functions require parentSeq any myPos).
// Note: Any active node/space is uniquely identified using the tuple
// <parentSeq #, my postion in parent's exprArr -- or sibling #). 
//----------------------------------------------------------------------------
function getHtmlExprStmt(e, sO, parentPath, myPos) {


    var ret = "";                                 // returned html string

    var myPath = parentPath + e.mySeq + ":";      // path to this node

    // record active parent expression in sO. This is the way we obtain
    // an ExprObj that correspond to a path string.  
    //
    if (myPath == sO.activeParentPath) {
        sO.activeParentExpr = e;
    }


    if (e.deleted == DeletedState.Deleted) { // if expr is deleted

        ret = "<span style='color:red'>" + DefDeletedName + "</span>";

    } else if (e.deleted == DeletedState.NeedUpdate) { // if expr needs update

        var onc_args = "\"" + parentPath + "\"," + myPos;
        var oncE = " onclick='exprClicked(" + onc_args + ")' ";
        ret = "<span style='color:red' " + oncE + ">" + DefNeedUpdateName +
            "</span>";

    } else if (e.deleted == DeletedState.PlaceHolder) { // if place holder

        var onc_args = "\"" + parentPath + "\"," + myPos;
        var oncE = " onclick='exprClicked(" + onc_args + ")' ";
        ret = "<span style='color:orange' " + oncE + ">" + e.str +
            "</span>";

    } else if (e.isFuncCall()) {

	var args = "";                       // arguments within ( )
	var numargs = e.exprArr.length;

	for (var i=0; i <= numargs; i++) {   // note '<='

            // onclick and properties for spaces within the function call.
            // spaces are used as insertion points (similar to in a stmt)
            //
            var oncS_args = "\"" + myPath + "\"," + i;
            var oncS = " onclick='spaceClicked(" + oncS_args + ")' ";
            var sprop = oncS; // child space is clickable
            var cprop = ""; // child's properties

            if ((i > 0) && (i < numargs)) args += ",";

            // if this child is active (highlighted expr/space)
            //
            if ((myPath == sO.activeParentPath) && (sO.activeChildPos ==
                    i)) {

                if (!sO.isSpaceActive)
                    cprop = " style='background-color:#dcffed' ";
                else
                    sprop += " class='scursor' ";
            }

            // First add space, and then add spans for child expression
            //
            args += "<span " + sprop + ">&nbsp</span>";
            // 
            if (i < numargs) {

                var ccont = getHtmlExprStmt(e.exprArr[i], sO, myPath, i);
                args += "<span " + cprop + ">" + ccont + "</span>";
            }

        } // for all args


        // put onclick function and cursor changing for func name
        //
        var onc_args = "\"" + parentPath + "\"," + myPos;
        var oncE = " onclick='funcClicked(" + onc_args + ")' ";
        var fprop = oncE + " style='cursor:pointer' ";
        //	
        // If the function name is already highlighted, clicking on it again
        // makes the function name editable
        //
        if ((parentPath == sO.activeParentPath) && (sO.activeChildPos ==
                myPos)) {
            //var onin_args = " oninput=\"updateFuncName(this)\" "; 
            var onin_args = " onblur=\"updateFuncName(this)\" ";
            fprop += " contenteditable " + onin_args;
        }
        ret = "<span " + fprop + ">" + e.str + "</span>(" + args + ")";
        //+ "<span " + fprop + ">)</span>";



    } else if (e.isGridCell()) {

        // Note: it is my parent's (callers) repsonsibility to put a span 
        //       around me (whole grid), with highlight if necessary. My
        //       resposnsibility is to highlight a selected child. It's always
        //       child resposnbility to handle onclick. 
        // Note: There are no child spaces within a grid, so no space handling
        //       is necessary
        //
        var gcont = ""; // grid contents within [ ]

        for (var i = 0; i < e.exprArr.length; i++) {

            var cprop = "";
            if (i > 0) gcont += ",";

            // highlight my child (and all its children), if he has been
            // selected (clicked on) by the user
            //
            if ((myPath == sO.activeParentPath) && (sO.activeChildPos ==
                    i)) {

                cprop = " style='background-color:#dcffed' ";
            }

            var ccont = getHtmlExprStmt(e.exprArr[i], sO, myPath, i);
            gcont += "<span " + cprop + ">" + ccont + "</span>";

        }

        // put onclick function and cursor changing for grid name
        //
        var onc_args = "\"" + parentPath + "\"," + myPos;
        var oncE = " onclick='exprClicked(" + onc_args + ")' ";
        var gprop = oncE + " style='cursor:pointer' ";
        //	
        var brack1 = (e.exprArr && e.exprArr.length) ? "[" : "";
        var brack2 = (e.exprArr && e.exprArr.length) ? "]" : "";
        //
        var cap = e.gO.caption; // if gO.caption updated, use that
        //
        ret = "<span " + gprop + ">" + cap + brack1 + "</span>" + gcont +
            "<span " + gprop + ">" + brack2 + "</span>";


    } else if (e.isStatement()) { // an assignment/if statement (expr statement)

        // Just go thru the exprArr in sequence and collect (print) all
        // the html values
        // Note: This is the case for handling the root node
        // Note: i includes exprArr.length, to handle the trailing space
        //
        for (var i = 0; i <= e.exprArr.length; i++) {

            var oncS_args = "\"" + myPath + "\"," + i;
            var oncS = " onclick='spaceClicked(" + oncS_args + ")' ";

            // Every child is always 'clickable' individually
            //
            var cprop = "";
            var sprop = oncS; // child space is clickable

            //alert("mypath:" + myPath + " cpos:" + i);

            // if this child is active (highlighted)
            //
            if ((myPath == sO.activeParentPath) && (sO.activeChildPos ==
                    i)) {

                // alert("last space active");

                // this child exprssion/space is selected. A selected
                // child is highlighted 
                //
                if (!sO.isSpaceActive)
                    cprop = " style='background-color:#dcffed' ";
                else
                    sprop += " class='scursor' ";


            } else {
                // For showing where the spaces are
                // sprop += " style='background-color:#fafafa' ";
            }

            // First add space, and then add spans for child expression
            // We insert insertion spaces only if a statement is editable
            //
            ret += "<span " + sprop + ">&nbsp</span>";
            // 
            if (i < e.exprArr.length) {

                var ccont = getHtmlExprStmt(e.exprArr[i], sO, myPath, i);
                ret += "<span " + cprop + ">" + ccont + "</span>";

            }
        }

        // STEP: 
        // Handle the leading *keyword* like if/else/elseif/foreach/...
        // NOTE: These keywords are NOT childrend of expression 'e' but
        //       keywords are stroted in e.str which is the node itself.
        //       in such cases, ROOTPOS is used as the child pos w/ 
        //       myPath (not parentPath + childPos)
        //
        if (e.isCondition() || e.isLoopStmt()) {

            // put onclick function and cursor changing
            // Note: since this is the root node property, we use myPath
            //       instead of parentPath (plus ROOTPOS)
            //
            var onc_args = "\"" + myPath + "\"," + ROOTPOS;
            var oncE = " onclick='exprClicked(" + onc_args + ")' ";
            var gprop = oncE;
            //	
            // if the keyword (at ROOTPS) is selected, highlight it
            // NOTE: keyword is in e.str and hence we use ROOTPOS as childPos
            //
            if ((sO.activeParentPath == myPath) &&
                (sO.activeChildPos == ROOTPOS)) {

                gprop += " style='background-color:#dcffed'";

            } else {
                gprop += " style='cursor:pointer' ";
            }
            //
            if (!(ret == "") && (e.isIf() || e.isElseIf() || e.isBreakIf()))
                ret = " (" + ret + ")";
            //
            ret = "&nbsp;<span " + gprop + ">" + e.str + "</span>" + ret;

        }


    } else if (e.isRange()) {

        // Note: There are no child spaces within a range, so no space handling
        //       is necessary
        //
        var rcont = ""; // range contents within ( )

        // whether the range name 'e.g., row, col' is highlighted by my parent
        //
        var amIhigh = (parentPath == sO.activeParentPath) &&
            (myPos == sO.activeChildPos);

        // if my parent has highlighted me OR if I am highligting a child of
        // mine like start/end/step, only then print start/step/end
        //
        if (amIhigh || (myPath == sO.activeParentPath)) {

            // Tool tips for each of he fields in the range
            //
            var rtips = ['grid name', 'start', 'end', 'step'];

	    // Note. In exprArr, expressions are stored as 
	    //       grid-caption/start/end/step
	    //
            // For producing caption/start/end/step. The output should
            // look like (start:end:step)
            //
            for (var i = 0; i < e.exprArr.length; i++) {

                // Add tool tip
                //
                var cprop = " title='" + rtips[i] + "' ";

                if (i == 1) rcont += "("; // after grid caption
                if (i > 1) rcont += " : "; // after start/end

                // if this child is active (highlighted)
                //
                if ((myPath == sO.activeParentPath) && (sO.activeChildPos ==
                        i)) {

                    cprop += " style='background-color:#dcffed' ";
                }

                var ccont = getHtmlExprStmt(e.exprArr[i], sO, myPath, i);

                // alert("cont: " + ccont);

                rcont += "<span " + cprop + ">" + ccont + "</span>";
            }

            // put = ) around the  'caption(start:end:step' produced above
            //
            rcont = "=" + rcont + ")";
        }

        // put onclick function and cursor changing for grid name
        //
        var onc_args = "\"" + parentPath + "\"," + myPos;
        var oncE = " onclick='rangeClicked(" + onc_args + ")' ";
        var rprop = oncE + " style='cursor:pointer' ";
        rprop += " title='index variable name' ";
        //	
        ret = "<span " + rprop + ">" + e.labelExpr.str + "</span>" +
            rcont;


    } else if (e.isLet()) {


        // This is a let definition (i.e., at position 0). Non definition
        // let names are handled as a regular leaf case

        // put onclick function for 'let' keyword
        //
        var onc_args = "\"" + parentPath + "\"," + myPos + "," + sO.curBox
        var oncLet = " onclick='letClicked(" + onc_args + ")' ";


        // Use mouse down/up functions on the let 'name'. Click and press to
        // edit. 
        //
        var oncName = " onmousedown='letNameMouseDown(" + onc_args +
            ",this)' " + " onmouseup='letNameMouseUp(" + onc_args +
            ",this)' ";

        var namestr = "";

        // the identifier for let is always at the 0th position
        //
        var ident = e.exprArr[0].str;

        // if this let definition is already selected, enable editing
        // of the let name. Otherwise, change cursor and font color to 
        // show it is a clickable definition
        //
        if ((parentPath == sO.activeParentPath) && (sO.activeChildPos ==
                myPos)) {
            //var onin_args = " oninput=\"updateLetName(this)\" "; 
            var onin_args = " onblur=\"updateLetName(this)\" ";
            oncName = " contenteditable " + onin_args;

            // NOTE: For some odd reason, we cannot put a space just before
            //       the equal sign -- after </span> with contenteditable
            //
            namestr = "<span " + oncName + ">" + ident + "</span>=";


        } else {

            // this let definition is not active. So, just draw it
            //
            oncName += " style='cursor:pointer;color:brown' ";
            namestr = "<span " + oncName + ">" + ident + "</span> =";

        }

        // Put let keyword and name
        //
        ret = "<span " + oncLet + ">let </span>" + namestr;


    } else if (e.isLeaf()) { // any other leaf expr not handled above

        // Note: leaves like operators, indices are handled here
        //
        // Note: put leaf processing at the end because there are other
        //       leaf expressions handled above which require special 
        //       handling
        // Note: Since this is a leaf, no highlighting is involved. 
        //       Highlighting  is always parent's responsibility. However,
        //       onclick handling for an expr is always child's responsibility.
        //       For onclick, we need parent's seq# and my position --
        //       position of e -- in parents exprArr().

        // if the leaf is selected (highlighted), in some cases we need to 
        // allow editing of the leaf (e.g., for a number/string constant)
        //
        var prop = "";
        //
        if ((parentPath == sO.activeParentPath) && (sO.activeChildPos ==
                myPos)) {

            if (e.isNumber()) {
                prop = " contenteditable onblur=\"updateNumber(this)\" ";
            } else if (e.isString()) {
                prop = " contenteditable onblur=\"updateString(this)\" ";
            }
        }
        //
        var onc_args = "\"" + parentPath + "\"," + myPos;
        var oncE = " onclick='exprClicked(" + onc_args + ")' ";
        var quote = (e.isString()) ? "'" : "";
        //
        ret = quote + "<span " + oncE + prop + ">" + e.str + "</span>" +
            quote;

    } else if (e.isConcat()) {

        //alert("Concat");

        // if this is an index concatanation -- e.g., col+4
        //
        var cont = ""; // contents of the concatenated index expr
        //
        for (var i = 0; i < e.exprArr.length; i++) {

            // No highlighting of children in index conacatenation
            //
            cont += getHtmlExprStmt(e.exprArr[i], sO, myPath, i);
        }

        // Put a span and onclick function around this concatenated 
        // expression (since the entire concatenated expression is treate
        // as a leaf)
        //
        var onc_args = "\"" + parentPath + "\"," + myPos;
        var oncE = " onclick='exprClicked(" + onc_args + ")' ";
        ret = "<span " + oncE + ">" + cont + "</span>";


    } else {

        alert("TODO: Handle expression not handled above");
    }


    return ret;

}


//----------------------------------------------------------------------------
// If the user single clicks on a let name, insert it to the active
// position of the code box
//----------------------------------------------------------------------------
function addLetNameExpr(parentPath, childPos, box, obj) {


    var sO = CurStepObj;

    // if the cursor is in the box where the let is clicked
    // treat it as an attempt to edit the let name
    //
    if (sO.focusBoxId == box) {

        editLetName(parentPath, childPos, box, obj);
        //alert("Cannot insert let name to the same box it is defined");
        return;
    }


    // If the user tries to insert a let name as the very first expr in 
    // a box, don't allow
    //
    if (!sO.focusBoxExpr || !sO.focusBoxExpr.exprArr ||
        !sO.focusBoxExpr.exprArr.length) {

        if ((sO.focusBoxId != CodeBoxId.Index) &&           // index OK
	    (!sO.boxAttribs[sO.focusBoxId].isMask())) {     // condition OK

            alert("Let name can be used only as a source");
            return;
        }

    }
    /*
    if ((sO.activeChildPos == 0) && !sO.activeParentExpr) {
	alert("Let name can be used only as a source");
	return;
	}
    */



    // alert("let name insert using box: " +  box + " expr:" + lexpr.str );

    // Find the let expression and add it 
    // let expression is always the first expression in the box AND
    // the let identifier is the at the 0th sub-expression
    //1


    var lexpr = sO.boxExprs[box].exprArr[0].exprArr[0];
    //
    addGenExpr(lexpr);




    // Disable code box focus change
    // TODO: May not be applicable in all cases
    //
    //sO.letClicked = true;


}

//----------------------------------------------------------------------------
// When the timer expires (user keeps pressing the mouse button), we 
// enable editing -- just by highlingting the entire expression, which
// makes the name contenteditable. 
// TODO: When we highlight, the cursor is not placed on let name. 
//----------------------------------------------------------------------------
function editLetName(a1, a2, box, obj) {

    var sO = CurStepObj;
    exprClicked(a1, a2);           // call exprClick to process as usual
    sO.keyboardEnabled = false;    // disable explicit keyboard processing

    // TODO: Place the caret inside the editable box
    // 
    // E.g., get 'obj' span, and write its innerHTML with content editable
    // and then place caret in it. 

    // obj.setSelectionRange(1,1);
    //obj.focus();
}

//----------------------------------------------------------------------------
// When we detect the mouse down, start a timer to decide whether it is
// single click or a click and press. If the timer expires, we start editing
// the let name 
// Note: see IndVarMouseDown/Up for description
//----------------------------------------------------------------------------
function letNameMouseDown(a1, a2, box, obj) {

    if (!Timer) {
        Timer = setTimeout(function() {
                Timer = null;
                editLetName(a1, a2, box, obj);
            },
            300);
    }
}

//----------------------------------------------------------------------------
// When the mouse is up, add the let name to current active expr/space
// if the timer has not expired (i.e., single click)
//----------------------------------------------------------------------------
function letNameMouseUp(a1, a2, box, obj) {

    if (Timer) {
        clearTimeout(Timer);
        Timer = null;
        addLetNameExpr(a1, a2, box, obj);
    }
}


//----------------------------------------------------------------------------
// The let keyword is clicked. Then we highlight the entire let keyword
// and the name -- then the name becoems editable
//----------------------------------------------------------------------------
function letClicked(arg1, arg2) {

    var sO = CurStepObj;

    exprClicked(arg1, arg2);       // call exprClick to process as usual
    sO.keyboardEnabled = false;    // disable explicit keyboard processing

}


//----------------------------------------------------------------------------
// This method is used to disable explicit processing on keyboard input
// when a function name is clicked. A function name is 'contenteditable' and
// hence we should not process keyboard input explicitly while the user
// is editing the function name.
//----------------------------------------------------------------------------
function funcClicked(arg1, arg2) {

    var sO = CurStepObj;

    exprClicked(arg1, arg2);       // call exprClick to process as usual
    sO.keyboardEnabled = false;    // disable explicit keyboard processing

}


//----------------------------------------------------------------------------
// Identifiers are not always acceptable to JavaScript -- e.g.,
// 'in' is a keyword in JS and cannot be used as a name of an incoming arg,
// grid, etc. One strategy is to prefix every identifier with "_". We can
// disable this for easy JS debugging. 
//----------------------------------------------------------------------------
function var2JS(str) {

    return "_" + str;
}

//----------------------------------------------------------------------------
// Check expression syntax
//----------------------------------------------------------------------------
function checkExprSyntax(e) {

    //logMsg("syncheck " + e.str);

    var ret = SynErr.None;               // return value from this func

    // Shortcut name
    //
    var expArr = e.exprArr;


    if (e.isStatement() || e.isConcat()) {


        // Nothing to do for an empty expr array
        //
        if (!expArr || (expArr.length < 1))
            return SynErr.None;


        // This a concatenated statement. 
        // For each child of e, called curE, 
        //   (i) we examine curE by calling this function recursively in case
        //       curE contains other conctagenated expressions).
        //   (ii)we examine curE with its predecessor and successor to see
        //       whether it is well formed
        //
        //
        for (var i = 0; i < expArr.length; i++) {

            var curE = expArr[i];

            // Check any children of curE
            //
            if (curE.exprArr && (curE.exprArr.length > 1)) {
                ret = checkExprSyntax(curE);
            }

            if (ret != SynErr.None)
                return ret;

            //logMsg("curE = " + curE.str);

            // Check curE w.r.t. its predecessor and successor
            //
            var prevE = (i > 0) ? expArr[i - 1] : null;
            var nextE = (i < expArr.length - 1) ? expArr[i + 1] : null;
            //
            var synerr = check3ExprSyntax(prevE, curE, nextE);

            if (synerr != SynErr.None) {

                alert("Syntax Error at expression: " + e.str + " code:" +
                    synerr + " func:" + SynErrLoc.func + " step:" +
                    SynErrLoc.step + " box: " + SynErrLoc.box +
                    " token: " + curE.str);

                return synerr;

                // CurE.err = ErrType.SynError;
            }
        }

    } else if (e.isFuncCall() || e.isGridCell()) {

        // check each individual arg as a separate expression
        //
        for (var i = 0; expArr && (i < expArr.length); i++) {

            //logMsg("check fcall/cell: " + e.str);
            ret = checkExprSyntax(expArr[i]);

            if (ret != SynErr.None) return ret;
        }


    } else if (e.isLeaf()) {

        // Nothing to check for a leaf
        //
        //logMsg("skipping leaf: " + e.str);


    } else if (e.isRange()) {

        // Skip range checking for now


    } else {
        assert(0, "Unknow expression type in syntax checking: " + e.str);
    }


    return SynErr.None;

}

//----------------------------------------------------------------------------
// Return values:
//  true  : there is nothing wrong with curE.
//  false : curE is not acceptable
//----------------------------------------------------------------------------
function check3ExprSyntax(prevE, curE, nextE) {

    // STEP 1: Check curE with PrevE

    if (!prevE) {

        // there is no previous expression. 
        // We cannot have a binary operator as curE
        //

        if (curE.isOperator() &&
            !(curE.isUnaryOperator() || curE.isOpenParen())) {
            return SynErr.MisplacedOp;
        }

    } else {

        // There is a previous expresson
        //

        // Two identifiers/numbers cannot follow each other. Similarly,
        // there cannot be identifier just after a close parenthesis
        //
        if ((prevE.isIdentOrNum() || prevE.isCloseParen()) &&
            curE.isIdentOrNum()) {

            return SynErr.TwoIdents; // operator missing between idents
        }

        // Two operators cannot follow each other except when we have open
        // parenthesis

        if (prevE.isOperator()) {

            if (curE.isOpenParen()) {
                //
            } else if (prevE.isOperator() && curE.isOperator() &&
                !curE.isUnaryOperator() && !prevE.isCloseParen()) { //KK:
                return SynErr.TwoOps; // two operators followin each other
            }
        }

    }


    // STEP 2: Check curE with NextE
    //
    if (nextE) {

        // There is a next expression

        // There cannot be anything following a grid reference
        //
        if (curE.isGridRef()) {
            return SynErr.GridRefMixed;
        }

    } else {

        // There is no next expression


        // Cannot end an expression with an operator
        //
        if (curE.isOperator() && !curE.isCloseParen()) {

            return SynErr.OpAtEnd; // operator at the end
        }
    }

    // There is nothing wrong with curE.
    //
    return SynErr.None;

}

/*
//----------------------------------------------------------------------------
// Check the data types of an expression to make sure that data types are
// compatible
//   dtype : required data type for this expression OR null (initially)
//   ret   : negative if type error, 
//         : LargeValue for a GridReference
//         : otherwise data type of e 

// TODO: Let cannot have a gridReference on its right hand side. Let can
//      be only a scalar expression. 

//----------------------------------------------------------------------------
function checkDataTypes(e, dtype) {

    if (!e) {

    } else if (e.isOperator())

	return dtype;                   // no change w/ an operator

    } else if (e.isLeaf) {
	
	if (dtype && (dtype != getDtype(e)) {
	    e.err = ExprErrTy.TypeErr;
	    return (-1);               // error detected at this leaf 
	} else if (!dtype) {
	    return getType(e);
	}
	   
    } else if (e.isFuncCall()) {

	// check func call args against header
	
	var retType = ; // get ret type from function header
	return retType;

    } else if (e.isGridCell()) {

	// grid cell indices must be int/uniqInd

	// we return the type of grid cell
	return findDataTypeOfGridCellExpr(e)

    } else if (e.isGridRef()) {


	// TODO: What do we do about GridReference

	return 100; 

    } else if (e.isLet()) {
	
	// let definition does not introduce a new type
	// TODO: Introduce another ID to ignore (e.g. 200)

    } else {

	// this is a compound statement. Go thru each expression

	var cur_dtype = null;  // TODO: start with 0

	for (var i=0; i < e.exprArr.length; i++) {

	    var expr_dtype = checkDataTypes(e.exprArr[i], cur_dtype);

	    if (expr_dtype < 0) {

		// type error detected at child expression
		// Child has set the error position in e
		//
		return null;

	    } else  if (expr_dtype != cur_dtype) {

		if (!cur_dtype) {

		    cur_dtype = expr_dtype;       // set type for the 1st time

		} else {
		    
		    // This cannot happen. It is always childs responsibility
		    // to check and return null AND set error

		}
	    }

	}

    }

}
*/


//----------------------------------------------------------------------------
// Method called to get a java script code for an expression.
// This method is identical to expr2str except for the way grid indices
// are printed. Java script requires row-major addressing and hence
// NOTE: Grid/Function/index names are made compatible with JavaScript
//       variable names by calling var2JS
//----------------------------------------------------------------------------
function expr2JSstr(e) {

    var ret = "";
    var pre = "",
        post = "",
        sep = "";

    if (!e) {            // empty string

    } else if ((e.isOperator() || e.isNumber() || e.isFormulaKeyword()) &&
        !e.isLetName()) {

        var opstr = e.str;
        //
        if (opstr == 'AND') opstr = "&&"; // handle special opstr's
        else if (opstr == 'OR') opstr = "||";
        else if (opstr == 'NOT') opstr = "!";
        //
        ret = opstr;

    } else if (e.isString()) {

        // To take care of 'abc', otherwise it is treated by isLeaf() 
        //
        ret = "\"" + e.str + "\"";

    } else if (e.isLeaf()) { // other type of leaf

        ret = var2JS(e.str);

    } else if (e.isLet()) { // let expression (let name at exprArr[0])

        ret = "var " + var2JS(e.exprArr[0].str) + " = ";

    } else if (e.isGridCell() && (e.exprArr.length == 0)) { // scalar grid

        pre = var2JS(e.gO.caption);
        ret = ".data[0]"

    } else if (e.isGridCell()) { // any non-scalar grid

        // read catption instead of e.str because caption can be changed
        // 
        pre = var2JS(e.gO.caption + ".data");

        // last dimension. For a scalar, lastd = -1 so none of the statments
        // below get executed -- i.e., ret = "";
        //
        var lastd = e.exprArr.length - 1;

        // Note: JavaScript (like C) has row-major array addressing
        //       --e.g., out[dim4][dim3][row][col], with cols changing fastest
        // print in reverse order for higher dimensions -- 
        // C-style array indexing. Therefore, they must be printed in the
        // following order: highest dim, ..., col, row.
        //
        for (var i = lastd; i >= 2; i--) { // print higher dims
            ret += "[" + expr2JSstr(e.exprArr[i]) + "]";
        }

        // Print row expression because C/JS syntax requires [row][col]
        //
        if (lastd >= 0) // print row expr
            ret += "[" + expr2JSstr(e.exprArr[RowDimId]) + "]";

        // Print col expressoin last because C/JS syntax requires [][][col]
        //
        if (lastd >= 1) // print col expr
            ret += "[" + expr2JSstr(e.exprArr[ColDimId]) + "]";


    } else if (e.str == "FileInput.loadCSVFile" ||
                    e.str == "FileOutput.saveCSVFile") {
      
	var modname = e.str.split(".");
	pre = var2JS(modname[0]) + "." + modname[1];
	pre += "(";
	ret += var2JS(e.exprArr[0].str) + ", " + "\"" + e.exprArr[1].str + "\"";
	post = ")";

    } else { // compound statement or function

        if (e.isFuncCall()) {

            // Default case: For a function in this module
            //
            pre = "this." + var2JS(e.str);

            // Case 1: If a module name is specified, use it
            //
            var modname = e.str.split("."); // get the module name
            if (modname.length > 1)
                pre = var2JS(modname[0]) + "." + var2JS(modname[1]);

            // Case 2:
            // if this is a library function with the same JavaScript library
            // name (e.g, Math), then don't translate e.str
            //
            if (e.isLibFuncCall()) {

                var modname = e.str.split("."); // get the module name

                if (isJSLibModule(modname[0])) { // if JS module

                    // For modulo there is NO JS function. Mod is an operator
                    // -- always with two args
                    // TODO: Think of a more general way to handle this
                    //
                    
                    if (modname[1] == "mod") {
                        //
                        
			//alert(
                        //    "TODO: Think of a more genral way to do this"
                        //);
                        return "(" + expr2JSstr(e.exprArr[0]) + " % " +
                            expr2JSstr(e.exprArr[1]) + ")";

                    } else {
                        pre = e.str;
                    }

                } else {

                    pre = var2JS(modname[0]) + "." + modname[1];

                }
            }

            pre += "(";
            post = ")";
            sep = ",";

        } else {
            sep = " ";
        }

        for (var i = 0; i < e.exprArr.length; i++) {

            if (i > 0) ret += sep;

            ret += expr2JSstr(e.exprArr[i]);

        }
    }

    return pre + ret + post;
}



//----------------------------------------------------------------------------
// For printing the 'str' value of each expression in an expression tree. 
//----------------------------------------------------------------------------
function expr2str(e) {

    var ret = "";
    var pre = "",
        post = "",
        sep = "";

    if (!e) { // empty expression

    } else if (e.deleted == DeletedState.Deleted) {

        ret = DefDeletedName;

    } else if (e.deleted == DeletedState.NeedUpdate) {

        ret = DefNeedUpdateName;

    } else if (e.isLeaf()) { // leaf

        ret = e.str;

    } else { // any no leaf

        if (e.isGridCell()) {

            // Brackets are not present for scalar grids
            //
            var brac1 = (e.exprArr && e.exprArr.length) ? "[" : "";
            var brac2 = (e.exprArr && e.exprArr.length) ? "]" : "";

            pre = e.str + brac1;
            post = brac2;
            sep = ",";
        }

        if (e.isFuncCall()) {
            pre = e.str + "(";
            post = ")";
            sep = ",";
        }

        for (var i = 0; i < e.exprArr.length; i++) {
            if (i > 0) ret += sep;
            ret += expr2str(e.exprArr[i]);
        }
    }

    return pre + ret + post;

}

//----------------------------------------------------------------------------
// If e or any of its sub-expressions have been deleted
//----------------------------------------------------------------------------
function isAnyExprDeleted(e) {

    if (!e)
        return false;

    if (e.deleted != DeletedState.None)
        return true;

    if (e.exprArr) {
        for (var i = 0; i < e.exprArr.length; i++) {
            if (isAnyExprDeleted(e.exprArr[i]))
                return true;
        }
    }

    return false;
}


//----------------------------------------------------------------------------
// Create a default range expression (with default start/end/step values)
//----------------------------------------------------------------------------
function newRangeExpr(sO, dim, gO) {

    var dname = sO.dimNameExprs[dim].str;
    var rangeExpr = new ExprObj(false, ExprType.Range, dname);
    //
    rangeExpr.selDim = dim;     // record dim this reprsents
    rangeExpr.gO = gO;
    rangeExpr.labelExpr = sO.dimNameExprs[dim];

    // First add caption of the grid as the first expr
    //
    var gridcap = new ExprObj(true, ExprType.Literal, gO.caption);
    rangeExpr.addSubExpr(gridcap);

    // Add default start/end/step expressions
    //
    if (gO.dimIsExtended[dim] == false) { //MPI: Also means gO.isDistributed

        var start = new ExprObj(true, ExprType.Number, 0);
	var concExprStart = new ExprObj(false, ExprType.Concat, "");
	concExprStart.addSubExpr(start);
        rangeExpr.addSubExpr(concExprStart);
        
	//MPI: TODO: CAUTION: Need to append gO.caption, to distinguish
	//     when end0, etc. used as index (different value between
	//     different grids.
        var end = new ExprObj(true, ExprType.Literal, DefEndName + dim);
	var concExprEnd = new ExprObj(false, ExprType.Concat, "");
	concExprEnd.addSubExpr(end);
        rangeExpr.addSubExpr(concExprEnd);
        
    } else { //MPI: If dim is extended use DefStartNameExt/DefEndNameExt
	     //     Need to append  

	var start = new ExprObj(true, ExprType.Literal, 
			DefStartNameExt + dim + "_" + gO.caption);
	var concExprStart = new ExprObj(false, ExprType.Concat, "");
	concExprStart.addSubExpr(start);
        rangeExpr.addSubExpr(concExprStart);;
        
        var end = new ExprObj(true, ExprType.Literal, 
			DefEndNameExt + dim + "_" + gO.caption);
	var concExprEnd = new ExprObj(false, ExprType.Concat, "");
	concExprEnd.addSubExpr(end);
        rangeExpr.addSubExpr(concExprEnd);

    }
    var step = new ExprObj(true, ExprType.Number, "1");
    var concExprStep = new ExprObj(false, ExprType.Concat, "");
    concExprStep.addSubExpr(step);
    rangeExpr.addSubExpr(concExprStep);

    return rangeExpr;

}



// =========================== CODE GEN ======================================

// NOTE: Grid/Function/index/var names are made compatible with JavaScript
//       variable names by calling var2JS
// TODO: Process Titles (e.g., to remove spaces etc)



//----------------------------------------------------------------------------
// Main method called to show data (or stop showing data) for a step
//----------------------------------------------------------------------------
function showData(state) {

    var pO = CurProgObj;

    if (state) {                          // if show data OR colorize
	//changeFunc(DefMainFuncInd);     // change to function main() & step
	pO.showData = state;
	calcData();
    } else {
	pO.showData = state;
	drawStep(CurStepObj);           // disable showing data
    }

}


//----------------------------------------------------------------------------
// method for calculating data using JavaScript generated -- for showing
// data
//----------------------------------------------------------------------------
function calcData() {

    var pO = CurProgObj;                 // needed for eval()

    var mO = CurModObj;
    var sO = CurStepObj;
    var fO = CurFuncObj;

    var oGId = sO.allGridIds[0];
    var ogO = fO.allGrids[oGId];         // output grid


    // Create the  grid for starup args. Function Main() referes to this
    // startup arg array (as a parameter)
    // IMPORTANT: The name of the following variable MUST be same as
    //            the DefStartupGridName
    //
    var StartUpGrid = new GridObj(0, DefStartupGridName, 1, 10,
        1, 4, false, false, false, true);
    createArrays(StartUpGrid);


    if (CurStepObj.stageInStep >= StageInStep.GridSelectDone) {

        updateStepObj(CurStepObj); // record any updated formula etc.

        // create data arrays for all grids in the prog (except for in args)
        //
        allocOrClearDataArrays4Prog();

    } else {
        showTip(TipId.SelectGrids);
        return;
    }


    // Get the java script code corresponding to the step
    //
    var code = getJavaScriptStr();


    // evaluate java script code using 'eval()'. This will put proper
    // values in to 'data' arrays in output grid object.
    //
    try {
        eval(code);
    } catch (ex) {

        if (ex instanceof SyntaxError) {
            alert("Syntax Error: " + ex.message);
        } else {

            // We reached the breakpoint that we inserted manually for the
            // current step. Nothing else to be done
            //
            if (ex.message) {
                alert("Error: " + ex.message);
            }

            //var msg = (ex.message)? "Msg: " + ex.message : "";
            //alert("Breakpoint for current step reached " +  msg);
        }
    }


    // set the maximum data value and draw the grids again
    //
    sO.maxDataVal = getMaxDataVal(ogO.numDims - 1, ogO.data, MININT);
    //
    drawStep(CurStepObj);

}



//----------------------------------------------------------------------------
// Show the java script string generated for the step
//----------------------------------------------------------------------------
function showJavaScriptStr() {

    var code = getJavaScriptStr();
    alert("code:\n" + code);

}

//----------------------------------------------------------------------------
// If a library module has an corresponding JavaScript implementation with
// the same name (e.g., Math), return true
//----------------------------------------------------------------------------
function isJSLibModule(libname) {

    var JSLibNames = ['Math'];

    // See whether this lib has a JS implementation (e.g., Math, String)
    //
    var is_js_lib = false;
    for (var i = 0; i < JSLibNames.length; i++) {
        if (JSLibNames[i] == libname)
            is_js_lib = true;
    }

    return is_js_lib;
}



//----------------------------------------------------------------------------
// Returns javascript for the *current* step in *current* funtion that the
// user is currently working on
//----------------------------------------------------------------------------
function getJavaScriptStr() {


    // Create SynErrLoc to store location of errors we may encounter
    //
    if (!SynErrLoc)
        SynErrLoc = new CodeLocationObj;


    // var sO = CurStepObj;
    // var fO = CurFuncObj;

    var pO = CurProgObj;


    // STEP:
    // Add library names to refer to correct library moduels. E.g., 
    // to execute FileInput.readCSV() routine, we need to have FileInput 
    // initialized.  
    //
    var lib_decl = "";
    //
    for (var lib = 0; lib < pO.libModules.length; lib++) {

        var libname = pO.libModules[lib].name;
        //
        // If the lib module has a corresponding JavaScript libary, no
        // need to list it (e.g., Math)
        //
        if (!isJSLibModule(libname)) {
            lib_decl += "var " + var2JS(libname) + " = pO.libModules[" +
                lib + "].libObj;\n";
        }
    }
    lib_decl += "\n";


    // STEP:
    // Create references for user defined modules because they are 
    // accessible from any other module. Create entries 
    // of the form: 'var new _Modname = new _ModnameObj();'
    //
    var mod_decl = "";
    //
    for (var mod = 0; mod < pO.allModules.length; mod++) {
        var modname = pO.allModules[mod].name;
        mod_decl += "var " + var2JS(modname) + " = new " + var2JS(modname) +
            "Obj()" + ";\n";
    }
    mod_decl += "\n";

    // STEP:
    // get code for each module. We get a javascript object ("class") 
    // representing each module.
    //
    var mod_code = "";
    //
    for (var mod = 0; mod < pO.allModules.length; mod++) {
        mod_code += getJavaScript4Module(pO, mod);
    }


    // STEP:
    // 
    // Insert call for the main method to start the ball rollin
    // call Main method
    //
    var modname = var2JS(DefStartModName);
    //
    var main_call = "\n" + modname + "." + var2JS(DefMainFuncName) + "(" +
        DefStartupGridName + "); /* call main */\n";
    //
    // Also insert a call to the global method -- e.g., to initialize any
    // global grids
    //
    var global_call = "\n" + modname + "." + var2JS(DefGlobalScopeName) +
        "(); /* init globals */ \n";

    return (lib_decl + mod_decl + mod_code + global_call + main_call);
}


//----------------------------------------------------------------------------
// Genrate javascript code for a module.
// Note: We create a java script object (class) to represent a module
//       because there are multiple modules in a program. E.g.,
// 
//  function myModule1Obj() {
//     this.Main = function Main() { ... }
//     this.func = function func() { ... }
//       :       :     :      :       :
//  } 
// 
//----------------------------------------------------------------------------
function getJavaScript4Module(pO, m) {

    var mO = pO.allModules[m];

    // Module name (prefixed with underscore)
    //
    var modname = var2JS(mO.name);


    SynErrLoc.module = mO.name;   // note location for errors

    // Generate JS object of the form:  'function moduleObj() {'
    //
    var modobj_decl = "function " + modname + "Obj() {\n";

    // Insert member name 'mO' into the JS ojbect for this module
    // This is mainly a shorthand for members of this module to use
    //
    var modvar_decl = "this.mO = pO.allModules[" + m + "];\n";

    // STEP 1: 
    //
    // First, generate code for all functions, including 'main()' in this
    // module
    //
    var func_code = "";

    for (var f = mO.GlobalScopeID; f < mO.allFuncs.length; f++) {

        // Genererate entries of the form: 'this.func = function func(){ ... }' 
        //
        var fO = mO.allFuncs[f];
        var fname = var2JS(fO.funcCallExpr.str);
        //
        func_code += "\nthis." + fname + " = " + getJavaScriptStr4Func(mO,
            f);
    }

    // Close the moduleObj (i.e., JS object (class))
    //
    var modobj_decl_end = "} /* moduleObj */\n\n\n";


    return (modobj_decl + modvar_decl + func_code + modobj_decl_end);

}



//----------------------------------------------------------------------------
// Returns JavaScript code for a single function
//----------------------------------------------------------------------------
function getJavaScriptStr4Func(mO, f) {

    var fO = mO.allFuncs[f];

    SynErrLoc.func = fO.funcCallExpr.str;   // note location for errors

    // STEP: Varaibles of the function
    //
    var func_vars = "";
    //
    func_vars += "var fO = this.mO.allFuncs[" + f + "];\n"; // fO reference 
    //
    func_vars += "var " + var2JS(DefRetValName)        // Ref for ReturnValue
	+ " = " + "fO.allGrids[0];\n";


    // STEP 
    // construnct Function header AND initialize incoming args
    //
    var func_head = "function " + var2JS(fO.funcCallExpr.str) + "(";
    //
    // Add argument list to function header. To do that, go thru ALL
    // grids in the function and see whether each is an incoming arg
    //
    for (var g = 0; g < fO.allGrids.length; g++) {

        var gO = fO.allGrids[g]; // 

        if (gO.inArgNum >= 0) { // this grid is an incoming arg

            if (gO.inArgNum > 0) // if this is later than first arg
                func_head += ","; // add arg separator

            // STEP: 
            // Process scalar (including number) arguments differently 
            // since Grids are not passed in for sclars (all scalars
            // are passed by VALUE)
            //
            // Get the expression corresponding to this arg from func call
            //
            var argExpr = fO.funcCallExpr.exprArr[gO.inArgNum];
            //
            // If the incoming argument is a number, there is NO grid passed
            // in as an argument (just a java script variable).  Same is true 
            // for a scalar grid cell (passed by value). So, assign
            // the value of that variable to .data[0]. Otherwise, a Grid
            // is passed in as the arg. So, point to its data array
            //
            // NOTE: We MUST NOT check for number of dimensions to determine
            //       whether an arg is a scalar. It is possible to pass
            //       a any grid (including a scalar grid) as a grid reference. 
            //       We may check for argExpr.isGridRef() or gO.isGridRef 
            //       if needed
            //
            if (argExpr.isScalarExpr() || argExpr.isGridCell()) {

                assert(!gO.isGridRef, "Must not be a grid reference");

                // Create a dummy arg name for the number and add it to header
                //
                var scalarArgName = "_scalarArg" + gO.inArgNum;
                func_head += scalarArgName;

                // Copy the value of the number to the argument grid that
                // is representing the number for this function
                //
                func_vars += "fO.allGrids[" + g + "].data[0] = " +
                    scalarArgName + ";\n";

                // Declear grid name and point it to argument grid. 
                // So, that any modification
                // to grid (scalar variable) will show up in the data field
                // of the grid
                //
                func_vars += "var " + var2JS(gO.caption) +
                    " = fO.allGrids[" + g + "];\n";

                // logMsg("Number/GridCell as arg");

            } else { // incoming arg is not passed by value

                // First add arg name to the function header
                //
                func_head += var2JS(gO.caption);

                // No variable definitions to add here. Because, this is
                // an argument that is passed by referece (i.e., a grid name)
                // Therefore, any grid expression will refer to the 
                // .data filed of this arg (see expr2JSsstr). 
                // However, for the purpose of displaying data in a 
                // callee function data field of any incoming grid ref arg 
                // should point to the caller's grid
                //
                func_vars += "fO.allGrids[" + g + "].data = " + var2JS(gO
                    .caption) + ".data;\n";

            }

        } /* if grid is an incoming arg */

    }
    //
    func_head += ") {\n";



    // STEP: Code for each step in the function
    //
    var step_code = "";
    //
    var stepStart = 1;        // note: prototype at step 0. 
    //
    for (var s = stepStart; s < fO.allSteps.length; s++) {

        SynErrLoc.step = s; // note location for locating any errors
        //
        step_code += getJavaScriptStr4Step(mO, fO, fO.allSteps[s]) + "\n";

        // Insert a manual breakpoint after the current step. This is 
        // done in order to stop showing data of a given step
        //
        if (fO.allSteps[s] == CurStepObj) {
	    //
            step_code +=
                "throw 'Breakpoint for current step reached' ;\n";;
        }

    }

    // Insert a default return statement for the return value at the very
    // end of the function. If there is a prior return statement that user
    // inserted, this becomes just dead code
    //
    var def_ret = "";
    //
    if (fO.isRegularFunc())
	def_ret = "return " + var2JS(DefRetValName) + ".data[0];\n";

    // Return function definition1
    // 
    return func_head + func_vars + step_code + def_ret + "} /* func */\n";

}



//----------------------------------------------------------------------------
// if the output grid in this step must be allocated (resized) dynamically
// (based on a scalar grid value), do so here
//----------------------------------------------------------------------------
function allocAnyDynamicGrid(mO, fO, sO) {

    var out_id = 0;                     // grid id of output grid

    var gId = sO.allGridIds[out_id];
    var gO = fO.allGrids[gId];

    // if this step doesn't have a new out grid OR the out grid does not
    // have any dynamic sizes, then nothing to do
    //
    if (!sO.isOutGridNew || !(gO && hasDynSize(gO)))
        return "";


    logMsg("Out grid " + gO.caption + " of step has a dynmic size");

    // Go thru each dimension and if any dim is dynamically sized (based on 
    // the value of scalar grid), insert code to calculate the value and 
    // reallcate grid data
    //
    var ret = "/* dynamic grid allocation */\n";

    var outname = "fO.allGrids[" + gId + "]";
    //
    for (var d = 0; d < gO.numDims; d++) {

        if (gO.dimDynSize[d]) {

            // TODO: If scalar grid name does not exist, throw an error
            //
            // insert the line to copy the value of scalar grid to 
            // dimActSize[d] 
            //
            ret += outname + ".dimActSize[" + d + "] = " + 
		var2JS(gO.dimDynSize[d]) + ".data[0];\n";

	    // ret += "alert(" + var2JS(gO.dimDynSize[d]) + 
	    //	".data[0]);\n";

            logMsg(" -- inserted code to dynamically allocate for dim: " + d);
        }
    }

    // STEP:
    // Now, insert code to dynamically allocate the output grid at this
    // point
    //
    ret += "createArrays(" + outname + ", true);\n\n";

    return ret;

}


//----------------------------------------------------------------------------
// Returns JavaScript code for a given step in a given function
//----------------------------------------------------------------------------
function getJavaScriptStr4Step(mO, fO, sO) {

    // if the output grid in this step must be allocated (resized) dynamically
    // (based on a scalar grid value), do so now
    //
    var dyn_alloc = allocAnyDynamicGrid(mO, fO, sO);

    // STEP: set grid names to point to .data arrays
    //
    var grids = "";
    var titleDefs = "";

    // Go thru all grids in the step and create references to the data 
    // fields

    for (var g = 0; g < sO.allGridIds.length; g++) {

        var gId = sO.allGridIds[g];
        var gO = fO.allGrids[gId];


        if ((gO.inArgNum < 0) && (!gO.isRetVal)) { // if arg nor retval

            if (gO.numDims > 0) { // non-scalar grid

                if (gO.globalRefId >= 0) {

                    // if this is a global grid (which is in fact a reference)
                    // add that reference here -- i.e., the .data field of
                    // this grid should point to the .data field of 
                    //		
                    grids += "fO.allGrids[" + gId + "].data = " +
                        " this.mO.allFuncs[" + FuncID.Global + "]." +
                        "allGrids[" + gO.globalRefId +
                        "].data;/* globalRef */\n";
                }

                // Produce references to gO.data[] array as necessary
                // This is a grid AND is NOT an incoming arg. 
                //
                grids += "var " + var2JS(gO.caption) + " = " +
                    "fO.allGrids[" + gId + "];\n";

            } else { // scalar grid

                grids += "var " + var2JS(gO.caption) + " = " +
                    "fO.allGrids[" + gId + "];\n";
            }
        }


        // STEP: get var defs for titles 
        // TODO: FIX: If two title names are the same AND have different
        //            values in two grids, then the following is incorrect.
        //            To avoid this prefix each title name with Grid name --
        //            e.g., out_Title1.
        //
        titleDefs += getTitleDefsOfGrid(gO);
    }


    // STEP: create for loops (a loop for each index var)
    // Note: rangeExpr.exprArr[] contains root range expressions
    //
    var rangeExpr = sO.boxExprs[CodeBoxId.Range];
    var loop_close = "";
    var forstr = "";

    // if there are index variables
    //
    if (rangeExpr && rangeExpr.exprArr && rangeExpr.isForeach()) {

        var num_index_vars = rangeExpr.exprArr.length;

        for (var iv = 0; iv < num_index_vars; iv++) {

            // STEP: get code for foreach loop 
            //
            var rexpr = rangeExpr.exprArr[iv];

            assert(rexpr.gO, "Range expr must have a gO");

            // Define the value of literal 'end'
            //
            var endv = var2JS(DefEndName + rexpr.selDim);

            // Pick the end value based on whether the size of the dim is
            // variable (dynmically allocated) or not
            //
            //var endval = rexpr.gO.dimActSize[rexpr.selDim];
            var dynendval = rexpr.gO.dimDynSize[rexpr.selDim];
            //var endval = (dynendval) ? var2JS(dynendval) + ".data[0]" : 
            //actendval;
            //

            var outGrid = fO.allGrids[sO.allGridIds[0]]; //Output grid

	    // NOTE: The dimActSize[dim] should be set when a grid is 
	    //       created. If the there dim has a dynamic size,
	    //       dimActSize[dim] must be evaluated there -- we already
	    //       do this in allocAnyDynamicGrid() -- called at the top
	    //       of this function.  Doing it here for a range expression 
	    //       is WRONG. Range must just use the value already set by
	    //       allocAnyDynamicGrid.

	    /*
            if (sO.isOutGridNew && (iv < outGrid.numDims) &&
                outGrid.dimDynSize[iv]) {

		// NOTE: The following is WRONG. We must NOT write to the 
		// something like "dimActSize[_row]",  
		//
                //forstr += var2JS(outGrid.caption) + ".dimActSize[" +
                //    var2JS(rexpr.labelExpr.str) + "] = " + var2JS(
                //        dynendval) + ".data[0]" + ";\n";

	     }
	    */

	    
            var endval = rexpr.gO.dimActSize[rexpr.selDim];
            forstr += "var " + endv + " = " + var2JS(rexpr.gO.caption) +
                ".dimActSize[" + iv + "]" + "-1;\n";

            //alert(forstr);
            //forstr += "var " + endv + " = " + endval + "-1;\n";

            var ivar = var2JS(rexpr.labelExpr.str); // TODO: FIX FIX FIX

            // Start/End/Step expressions
            //
            var start = expr2JSstr(rexpr.exprArr[RangeFields.Start]);
            var end = expr2JSstr(rexpr.exprArr[RangeFields.End]);
            var step = expr2JSstr(rexpr.exprArr[RangeFields.Step]);

            forstr += "for (var " + ivar + "=" + start + "; " + ivar +
                " <= " +
                end + "; " + ivar + "+=" +
                step + ") {\n";

            loop_close += "} /* foreach */\n";
        }

    } else if (rangeExpr && rangeExpr.isForever()) {

        forstr += "while (true) {";
        loop_close = "} /* forever */\n";
    }


    // STEP: Go thru all the boxes and create mask/formula
    //
    var stmt = "";
    var prev_indent = 1; // first statement always has 1 tab indentation

    for (var box = CodeBoxId.Range + 1; box < sO.boxAttribs.length; box++) {

        // Step: close braces -- if there is a reduction in indent level
        //       from previous, close braces
        //
        var indent = sO.boxAttribs[box].indent;
        //
        for (var i = indent; i < prev_indent; i++) {
            stmt += "} /* block */\n";
        }
        prev_indent = indent;


        var boxexpr = sO.boxExprs[box];

        // Step:
        //
        SynErrLoc.box = box;
        checkExprSyntax(boxexpr);


        // Step: handle mask statment if/else/elseif/breakif
        //
        if (sO.boxAttribs[box].isMask()) {

            if (boxexpr.isElse()) { // 'else' has no expression between ()

                stmt += "else {\n";

            } else if (boxexpr && boxexpr.exprArr && boxexpr.exprArr.length) {

                // condition with child expression  -- if/elseif/breakif
                //
                stmt += boxexpr.str + "(" + expr2JSstr(boxexpr) + ") {\n";

            } else {

                // since we depend on indentation to figure out braces,
                // if there is a mask box, even when it is empty, we have
                // to open a new block
                //
                stmt += "{\n";
            }


        } else {

            // Step: process a formula statement
            //
            stmt += expr2JSstr(boxexpr) + ";\n";

        }

    }


    // Step: close end braces -- if there is a reduction in indent level
    //       from previous, close braces. Note: Indentation must end at 
    //       with just 1 tab. 
    //
    for (var i = 1; i < prev_indent; i++) {
        stmt += "} /* block */\n";
    }


    var post_loop = "";

    /*
    // STEP: Write values of scalar variables back to grid.data[0]
    // FIX: If the user inserts a return statement, this statement is 
    //      not executed. However, we don't show data for such calle funcs???
    //      In that case, this is just dead code.
    //

    //
    for (var g=0; g < sO.allGridIds.length; g++) {

	var gId = sO.allGridIds[g];
	var gO = fO.allGrids[gId];

	if (gO.inArgNum < 0) {          // if gO is not incoming arg
	  
	    if (gO.numDims == 0) {

	
		// copy the value of a scalar variable into the data array
		// of the grid. Since we always allocate a data array for
		// a scalar variable, data[0] alwyas exists. 
		//
		post_loop = "fO.allGrids[" + gId + "]" + ".data[0] = " +
		     var2JS(gO.caption) + ";\n";

		
		// copy the value of a scalar variable into the data array
		// of the grid, if it exists
		//
		//var darr = "var darr = fO.allGrids[" + gId + "]" + ".data";
		//
		//post_loop = darr + ";\n" + 
		//    "if (darr) darr[0] = " + var2JS(gO.caption) + ";\n";
		

	    }
	}
    }
    */


    // STEP: Combine all the above components together
    //
    var code = dyn_alloc + grids + titleDefs + forstr + stmt + loop_close +
        post_loop;

    // alert("code:\n" + code);

    return (code);

}

//----------------------------------------------------------------------------
// returns the var definition for titles of a grid
//
// TODO: ASSUMPTION: Assumes there are no conflicting title names. Enforce.
///----------------------------------------------------------------------------
function getTitleDefsOfGrid(gO) {

    var str = "";

    for (var d = 0; d < gO.numDims; d++) { // for each dim

        if (gO.dimHasTitles[d]) { // if titles

            for (var i = 0; i < gO.dimShowSize[d]; i++) {

                if (str.length) str += ", "; // separator
                str += var2JS(gO.dimTitles[d][i]) + "=" + i; // var def
            }
        }

    }

    if (str.length) // if there are titles  
        str = "var " + str + ";\n";

    return str;
}



//----------------------------------------------------------------------------
// Demo of using eval() to evaluate 
//----------------------------------------------------------------------------

function calcValues() {

    var arr1 = [1, 2, 3, 4, 5];
    var arr2 = [1, 2, 3, 4, 5];
    var resA = new Array();

    var ind1 = 3;
    var ind2 = 2;

    var res = 0;

    // We can use ANY of the following options. Last method would be 
    // preferable for calculating entire arrays. 
    //
    //
    var str = " arr1[ind1] + arr2[ind2] ";
    alert("result : " + eval(str));
    //
    var str2 = " res = arr1[ind1] + arr2[ind2] ";
    eval(str2);
    alert("result : " + res);
    //

    var str3 = " for (i=0; i < 4; i++) resA[i] =  arr1[i] + arr2[i] ";
    eval(str3);
    alert("res arr: " + resA);

}



/////////////////////////////////// SAVE / LOAD ////////////////////////////

// Saving files using JSON
// -----------------------
//
// See ttp://www.json.org/js.html for JSON introduction
//
// Note: that JSON does not stringify *cyclic* objets by default. Recursive
//       expression tree seems to be OK -- see code below.
//


//----------------------------------------------------------------------------
// Note: JSON follows object references when it stringify. So, when we 
//       stringify an ExprObj, then it follows the gO fields unless we filter
//       it out. 
//
// TODO: In an Expr, convert gO refernces to some index before saving. E.g.,
//       index into allGrids[]. When restoring, the proper references must
//       be restored. 
//
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Main entry point for saving a program
//----------------------------------------------------------------------------
function saveProg() {

    // Get a new 'save image' of the entire program
    //
    var pOsav = new ProgObjSav(CurProgObj);

    // Get the JSON string for this 'save image' for the entire program
    //
    var jstr = JSON.stringify(pOsav);

    download2Save(jstr, CurProgObj.fileName);

}

//----------------------------------------------------------------------------
// Save current program as ...
//----------------------------------------------------------------------------
function saveProgAs() {

    // Get a new 'save image' of the entire program
    //
    var pOsav = new ProgObjSav(CurProgObj);

    var filename = prompt("Please enter new file name:", "myprogram.txt");
    //
    if (filename) {
        pOsav.fileName = filename;
    }

    // Get the JSON string for this 'save image' for the entire program
    //
    var jstr = JSON.stringify(pOsav);

    if (filename) {

        download2Save(jstr, filename);

    } else {
        alert("Please enter a valid file name to save");
    }
}




//----------------------------------------------------------------------------
// Main routine for loading a program
//----------------------------------------------------------------------------
function loadProg() {

    uploadProg();
}

//----------------------------------------------------------------------------
// After the file loading is complete, recreate the program state
//----------------------------------------------------------------------------
function loadProgContd(jstr) {


    // load 'save image' of the entire program using Json str
    //
    var pOload;
    try {
        pOload = JSON.parse(jstr);
    } catch (e) {
        alert("JSON program parse error", e);
    }

    // Recreate a new program object using loaded image of the program
    //
    CurProgObj = loadProgObj(pOload, ProgFileName);

    alert("New Program Loaded");

    CurModObj = CurProgObj.allModules[CurProgObj.curModNum];
    CurFuncObj = CurModObj.allFuncs[CurModObj.curFuncNum];
    //
    assert(CurFuncObj.allSteps.length > CurFuncObj.curStepNum);
    CurStepObj = CurFuncObj.allSteps[CurFuncObj.curStepNum];

    // Reset any global variables to get a fresh start
    //
    resetGlobalVars();

    // Draw the step. This will also set the focusBoxExpr for the current
    // step.
    //
    drawStep(CurStepObj);
}



//----------------------------------------------------------------------------
// Implementation of the file input libarary
//----------------------------------------------------------------------------
function FileInputObject() {

    // Thie method is called when asynchronous file reading is complete
    //
    this.loadCSVFileDone = function(str, gO) {

        var lines = str.split("\n");

        var elems_seen = -1;
        var lines_seen = 0;

        // Go thru each line and assign data
        //
        for (var l = 0; l < lines.length - 1; l++) {

            var line = lines[l];
            var elems = line.split(",");

            if (elems.length == 0) // skip empty lines
                continue;

            // Check whether we see the same number of elements in each row
            //
            if (elems_seen < 0) {
                elems_seen = elems.length;
            } else if (elems_seen != elems.length) {

                alert("Not same number of elements in all rows");
            }

	    if(gO.numDims!=1){
            	for (var e = 0; e < elems.length; e++) {
                    gO.data[lines_seen][e] = parseInt(elems[e]);
            	}
	    } else {
	    	for (var e = 0; e < elems.length; e++) {
		    gO.data[e] = parseInt(elems[e]);
		}
	    }

            lines_seen++;

            console.log("line " + l + ":" + lines[l] + " elems:" +
                elems.length);

        }

        // Set the actual nubmer of rows/columns
        //
	if(gO.numDims != 1) {
            gO.dimActSize[RowDimId] = lines_seen;
            gO.dimActSize[ColDimId] = elems_seen;
	} else {
	    gO.dimActSize[RowDimId] = elems_seen;
	    gO.dimActSize[ColDimId] = lines_seen;
	}

        // Redraw step to show the data read
        //
        drawStep(CurStepObj);

        console.log("CSV file reading done. Writing to gO: " + gO.caption);

        //alert("Contents:\n" + str);	
    }

    // Method called when 'libFileRead' element is clicked. This prompts
    // user with a file selection dialog, any the launches an asynchronous
    // file read opeartion
    //
    this.loadCSVFileContd = function(fiO, filename, gO) {

        var x_rem_obj;
        var readText = "";

        x_rem_obj = new XMLHttpRequest();
        x_rem_obj.onreadystatechange = handleStateChange;
        x_rem_obj.open("GET", filename, false);
        x_rem_obj.send();

        function handleStateChange() {
            if (x_rem_obj.readyState == 4) {
                readText = (x_rem_obj.status == 200 ? x_rem_obj.responseText :
                    null);
                fiO.loadCSVFileDone(readText, gO);
            }
        }

    }


    // Main entry point to read a CSV file
    //
    this.loadCSVFile = function(gO, filename) {

        // A CSV file can be read only into a 1D or 2D grid
        //
        if (gO.numDims > 2) {
            alert("Reading CSV file into " + gO.caption +
                ": Must be 1D or 2D grid");
            return;
        }

        //var elem = document.getElementById('libFileRead');

        var fiO = this;

        fiO.loadCSVFileContd(fiO, filename, gO);

    }

}



//----------------------------------------------------------------------------
// Implementation of the file output libarary
//----------------------------------------------------------------------------
function FileOutputObject() {

    // Main entry point to save a CSV file
    //
    this.saveCSVFile = function(gO) {

        // Only a 2D grid can be saved as a CSV file
        //
        if (gO.numDims != 2) {
            alert("Saving " + gO.caption +
                " as CSV file: Must be 2D grid");
            return;
        }

        // Write the data values into a comma separated string
        //
        var str = "";
        for (var row = 0; row < gO.dimActSize[RowDimId]; row++) {
            for (var col = 0; col < gO.dimActSize[ColDimId]; col++) {

                if (col > 0) str += ",";
                str += gO.data[row][col];
            }
            str += "\n";
        }

        // Write the string to a file (as download)
        //
        var elem = document.getElementById('libFileSave');

        str = encodeURIComponent(str);
        elem.setAttribute('href', 'data:text/octet-stream,' + str);
        elem.download = "myfile.txt"; // filename; 
        //
        // auto click the link so that it will start downloading (saving)
        //
        elem.click();
    }
}


//----------------------------------------------------------------------------
// System object
//----------------------------------------------------------------------------
function SystemObject() {

    // Note: We cannot run a system command on the client. However, if this
    // is connected with a web server, we can run system commands -- e.g., 
    // using Node.js
    // Note: A command excuted via the system command should produce a file
    //       e.g., a csv file, which a program can read into a grid.
    //
    this.runCommand = function(command) {
        alert(
            "System.runCommand: System functions can run only w/ backend"
        );
    }
}


//----------------------------------------------------------------------------
// Matrix object to perform common matrix functions. A back-end implementation
// (e.g., Fortran) can call MKL directly for these. 
//----------------------------------------------------------------------------
function MatrixObject() {

    // Matrix Add
    //
    this.add = function(gOd, gOs1, gOs2) {

        // First check whether dimensions are equal
        //
        var dimeq1 = (gOd.dimActSize[RowDimId] == gOs1.dimActSize[
                RowDimId]) &&
            (gOd.dimActSize[RowDimId] == gOs2.dimActSize[RowDimId]);
        //
        var dimeq2 = (gOd.dimActSize[ColDimId] == gOs1.dimActSize[
                ColDimId]) &&
            (gOd.dimActSize[ColDimId] == gOs2.dimActSize[ColDimId]);
        //
        if (!dimeq1 || !dimeq2) {
            alert("Matrix.Add: Dimension sizes not equal. Aborting.");
            return;
        }

        // Do the actual addition
        //
        for (var r = 0; r < gOd.dimActSize[RowDimId]; r++) {

            for (var c = 0; c < gOd.dimActSize[ColDimId]; c++) {

                gOd.data[r][c] = gOs1.data[r][c] + gOs2.data[r][c];
            }
        }
    }
}



//----------------------------------------------------------------------------
// Download a file to save
//----------------------------------------------------------------------------
function download2Save(str, filename) {


    // Get the anchor element reserved for saving and change its attributs
    //    
    var elem = document.getElementById('saveLink');
    elem.setAttribute('href', 'data:text/plain;charset=utf-8,' + str);
    elem.download = "myfile.txt"; // filename; 
    //
    // Clic the link so that it will start downloading (saving)
    //
    elem.click();
}


//----------------------------------------------------------------------------
// Temp: Function that can save text with newlines/tabs
//----------------------------------------------------------------------------
function tmpDownload(str) {


    str = encodeURIComponent(str);

    elem.setAttribute('href', 'data:text/octet-stream,' + str);

    elem.download = "myfile.txt";
    //
    // Clic the link so that it will start downloading (saving)
    //
    elem.click();
}


//----------------------------------------------------------------------------
// Upload a program file. This just draws a dialog to upload files
// TODO: Draw a pop-up window
//----------------------------------------------------------------------------
function uploadProg() {

    // The fileUpload input box (hidden) declared in grid_main.html
    // is used to select the file. We just click the 'chose file' button
    // contained in that input element. When we automatically click the 
    // input element below, we execute uploadFileContents() routine.
    //
    var upelem = document.getElementById('fileUpload');
    upelem.click();
}

//----------------------------------------------------------------------------
// Do the actual uploading of the file contents
//----------------------------------------------------------------------------
function uploadFileContents(filelist) {

    var fileObj = filelist[0];

    ProgFileName = fileObj.name; // set global variable of file name
    //
    //alert("file:" + fileObj.name);

    var freader = new FileReader();

    // Onload function that gets executed when the file reading completes
    //
    freader.onload =
        function() {
            // alert("File reading ended");
            // alert("File contents:" + this.result);
            //
            loadProgContd(this.result);
        };

    // Start reading the file asynchronously. When the file reading completes
    // freader.onload function will be called
    //
    freader.readAsText(fileObj);

    /*
    var elem = document.getElementById('dialogBox');
    elem.innerHTML = "<div id='dialog'></div>";
    elem.className = 'dialog';
    */

}
