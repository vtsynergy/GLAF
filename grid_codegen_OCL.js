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
//Purpose: Grid Language OpenCL Code Generation (work in progress)
//Author : Konstantinos Krommydas
//Started: November 11, 2014
//Updated: December 18, 2015
//----------------------------------------------------------------------------



//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// PART A: Global Variables.
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------


// This is used to define the target device (code generation paths are
// different for FPGA (and in the future specific optimizations will target
// each different type of device).
// Another option would be to generate code that would include all cases, and
// what would run would depend on user selection at run-time.
// TODO: Line 566 from grid_codegen.js (from auto-tuning code generation for
//       the OpenCL case, depending on user's choice of platform). At that
//       point substitute this variable on main code_generation function for
//       OpenCL and delete this from here.
//       OR: change the saveOCLstring() and pass parameter there accordingly.
var Device_type;


// If DataTran == 1, generate the general OpenCL code with data transfers
// using clEnqueueReadBuffer, etc.
// If DataTran == 0, generate the OpenCL code using SVM.
var DataTran;


var DevT = {

    CPU: 1,
    GPU: 2,
    MIC: 3,
    FPGA: 4,

}


// This saves the cl_mem pointers passed to host functions when needed.
// The reason we are saving this information is so that when building
// clSetKernelArgs() for passing arguments to parallel steps within a
// functions we know to pass the pointer as (void *) arg, instead of
// (void *) &arg, which would be the case for a non-pointer cl_mem var.
// Reinitialized per function.
var CLMEMSinFunc;


// Keeps track of functions called from step Used for renaming/
// identifying __device functions, for the case when functions
// are called from within the context of a __kernel.
var CalledFuncsFromStep;


// Holds ALL functions that have been called at least one
// from within a kernel.
var CalledFuncsFromKernels; 


// Records prototypes of auxiliary device functions (inlined) in ocl file.
var DeviceAuxPrototypes;


// Variable to save the strings for all function prototypes.
var Func_prototypes;


// Used to save grid objects used in current function. Reinitialized per func.
var GridsInFuncOCL;


// Array of OCL_step_grids_info objects. Re-initialized per function.
var OCL_steps_dataTran;


var CL_Funcs; // TO save OpenCL kernels in .cl file


var Func_code_ser; // To save serial device code to be in .cl file


// OpenCL global variables, to be prepended in each OpenCL program's C file.
// They are used in all OpenCL-related functions.
// index_data: used to hold the information for start/step of each of the 3
// (max) global dimensions - used within the kernel to identify which
// array element to access (e.g., if row=grid(start:end0:step) in host 
// side), then the global dimension will have ceil(end-start+1)/step) 
// work-items, and accessing each in the kernel will be by 
// row=start+get_global_id(0)*step.
var OCL_glob_vars = "cl_platform_id plat_id;\n" +
		    "cl_device_id dev_id;\n" +
		    "cl_context context;\n" +
		    "cl_command_queue commands;\n" +
		    "cl_kernel kernel_cl;\n" +
		    "cl_int error;\n" +
		    "cl_program program;\n" +
		    "int *index_data;\n";


// Sets up the OpenCL environment (device, queues, etc.)
// To be called from the main function of an OpenCL program's C file.
// TODO: Right now hardcoded device type (e.g., CPU/GPU/MIC...)
var OCL_setup_dev_code = "void setup_OCL_dev() {\n" + addIndentation(0) +
	"error = clGetPlatformIDs(1, &plat_id, NULL);\n" + addIndentation(0) +
	"assert(error == CL_SUCCESS);\n" + addIndentation(0) +
	"error = clGetDeviceIDs(plat_id, __DEV_TYP__, 1, &dev_id, NULL);\n" + 
	addIndentation(0) +
	"assert(error == CL_SUCCESS);\n" + addIndentation(0) +
	"context = clCreateContext(NULL, 1, &dev_id, NULL, NULL, &error);\n" +
	addIndentation(0) +
	"assert(error == CL_SUCCESS);\n" + addIndentation(0) +
	"commands = clCreateCommandQueue(context, dev_id, 0, &error);\n" +
	addIndentation(0) +
	"assert(error == CL_SUCCESS);\n" + addIndentation(0) +
	"FILE* kernel_fp = fopen(\"ocl_kernels.cl\", \"r\");\n" +
	addIndentation(0) +
	"fseek(kernel_fp, 0, SEEK_END);\n" + addIndentation(0) +
	"size_t kernel_size = (size_t)ftell(kernel_fp);\n" + 
	addIndentation(0) +
	"rewind(kernel_fp);\n" + addIndentation(0) +
	"__KERN_ALLOC__" +	
	addIndentation(0) +
	"fread((void* )kernel_source, kernel_size, 1, kernel_fp);\n" +
	addIndentation(0) +
	"fclose(kernel_fp);\n" + addIndentation(0) +
	"__CREATE_PROG__" +
	addIndentation(0) +
	"assert(error == CL_SUCCESS);\n" + addIndentation(0) +
	"error = clBuildProgram(program, 1, &dev_id, NULL, NULL, NULL);\n" +
	addIndentation(0) +
	"if (error == CL_BUILD_PROGRAM_FAILURE) {\n" + addIndentation(1) +
	"char *logTxt;\n" + addIndentation(1) +
	"size_t logSize;\n" + addIndentation(1) +
	"clGetProgramBuildInfo(program, dev_id, CL_PROGRAM_BUILD_LOG, 0," +
        " NULL, &logSize);\n" + addIndentation(1) +
	"logTxt = (char* )malloc(sizeof(char)*logSize);\n" + 
	addIndentation(1) +
	"clGetProgramBuildInfo(program, dev_id, CL_PROGRAM_BUILD_LOG, " + 
	"logSize, (void* )logTxt, NULL);\n" + addIndentation(1) +
	"fprintf(stderr, \"Build Error Log:\\n%s\", logTxt);\n" + 
	addIndentation(0) +
	"}\n" + addIndentation(0) + 
	"assert(error == CL_SUCCESS);\n" +
	"}\n\n";


var OCL_checkSVM = "int checkSVMAvailability (cl_device_id device) {\n" + 
        addIndentation(0) +
    	"cl_device_svm_capabilities caps;\n" + addIndentation(0) +
	"int gran;\n" + addIndentation(0) +
    	"cl_int err = clGetDeviceInfo(device, CL_DEVICE_SVM_CAPABILITIES, " +
	"sizeof(cl_device_svm_capabilities), &caps, 0);\n" + 
	addIndentation(0) +
	"gran = (err == CL_SUCCESS && (caps & " +
	"CL_DEVICE_SVM_FINE_GRAIN_BUFFER));\n" + addIndentation(0) +
	"if (gran == 1) return 1;\n" + addIndentation(0) +
	"gran = (err == CL_SUCCESS && (caps & " +
	"CL_DEVICE_SVM_COARSE_GRAIN_BUFFER));\n" + addIndentation(0) +
	"if (gran == 1) return 2;\n" + addIndentation(0) +
	"return 0;\n}\n\n";


var OCL_alignedMem = "#define AOCL_ALIGNMENT 1024\n" +
	"void *alignedMalloc(size_t size) {\n" + addIndentation(0) +
	"void *result = NULL;\n" + addIndentation(0) +
	"posix_memalign (&result, AOCL_ALIGNMENT, size);\n" + 
	addIndentation(0) +
	"return result;\n}\n\n" +
	"void alignedFree(void *ptr) {\n" + addIndentation(0) +
	"free(ptr);\n}\n\n";


// Finalizes OpenCL environment.
// To be called at the end of main function of OpenCL program's C file.
var OCL_finalize_code = "void finalize_OCL_dev() {\n" + 
			addIndentation(0) +
			"clReleaseKernel(kernel_cl);\n" +
			addIndentation(0) +
			"clReleaseProgram(program);\n" +
			addIndentation(0) +
			"clReleaseCommandQueue(commands);\n" + 
			addIndentation(0)+
			"clReleaseContext(context);\n" +
			"}\n\n";


var SVM_util_code = "#define N 40\n\n" +
	"//Contains the level of SVM support on device\n" +
	"//0=no SVM, 1=coarse-grained buffer, 2=fine-grained buffer, " +
	"3=fine-grained system\n" +
	"int SVM_Support;\n\n" +
	"//Marks next available entry on AddressInfo/MapInfo arrays\n"  +
	"//Initialized in Main on program start\n" +
	"int NextAvail;\n\n" +
	"//Contains address information for SVM pointers\n" +
	"void* AddressInfo[N];\n\n" +
	"//Contains info about whether a given SVM pointer\n" +
	"//is mapped on the host side\n" +
	"int MapInfo[N];\n\n" +
	"void addNewAddress(void* address) {\n\n" + addIndentation(0) +
	"//Set address\n" + addIndentation(0) +
	"AddressInfo[NextAvail] = address;\n" + addIndentation(0) +
	"//Initialize MapInfo[] to zero\n" + addIndentation(0) +
	"MapInfo[NextAvail++] = 0;\n\n" +
	"}\n\n" +
	"int findAddress(void* address) {\n\n" + addIndentation(0) +
	"int i;\n\n" + addIndentation(0) +
	"for (i = 0; i < N; i++) {\n" + addIndentation(1) +
	"if (AddressInfo[i] == address)\n" + addIndentation(1) +
	"return i;\n" + addIndentation(0) +
	"}\n\n" + addIndentation(0) +
	"return -1; //Should not happen\n\n" +
	"}\n\n" +
	"int checkMapped(void* address) {\n\n" + addIndentation(0) +
	"return MapInfo[findAddress(address)];\n\n" +
	"}\n\n" +
	"void SVM_Map(void * address, int size) {\n\n" + addIndentation(0) +
	"if (!checkMapped(address)) {\n" + addIndentation(1) +
	"clEnqueueSVMMap(commands, CL_TRUE, CL_MAP_READ | CL_MAP_WRITE, " +
	"address, size, 0, 0, 0);\n" +
	addIndentation(1) +
	"MapInfo[findAddress((void*)address)] = 1;\n" + addIndentation(0) +
	"}\n\n" +
	"}\n\n" +
	"void SVM_Unmap(void * address) {\n\n" + addIndentation(0) +
	"if (checkMapped(address)) {\n" + addIndentation(1) +
	"clEnqueueSVMUnmap(commands, address, 0, 0, 0);\n" +
	addIndentation(1) +
	"MapInfo[findAddress((void*)address)] = 0;\n" + addIndentation(0) +
	"}\n\n" +
	"}\n\n";


var DataTran_util_code = "#define N 40\n\n" +
	"//Marks next available entry on AddressInfo/MapInfo arrays\n"  +
	"//Initialized in Main on program start\n" +
	"int NextAvail;\n\n" +
	"//Contains address information for malloc'd pointers\n" +
	"void* AddressInfo[N];\n\n" +
	"//Contains address information for cl_mem addresses\n" +
	"cl_mem* ClMemAddressInfo[N];\n\n" +
	"//Contains info about whether a given SVM pointer\n" +
	"//is mapped on the host side\n" +
	"int MapInfo[N];\n\n" +
	"void addNewAddress(void* address, cl_mem* clMemAddress) {\n\n" + 
	addIndentation(0) +
	"//Set address\n" + addIndentation(0) +
	"AddressInfo[NextAvail] = address;\n" + addIndentation(0) +
	"ClMemAddressInfo[NextAvail] = clMemAddress;\n" + addIndentation(0) +
	"//Initialize MapInfo[] to zero\n" + addIndentation(0) +
	"MapInfo[NextAvail++] = 0;\n\n" +
	"}\n\n" +
	"int findAddress(void* address) {\n\n" + addIndentation(0) +
	"int i;\n\n" + addIndentation(0) +
	"for (i = 0; i < N; i++) {\n" + addIndentation(1) +
	"if (AddressInfo[i] == address)\n" + addIndentation(1) +
	"return i;\n" + addIndentation(0) +
	"}\n\n" + addIndentation(0) +
	"return -1; //Should not happen\n\n" +
	"}\n\n" +
	"int checkMapped(void* address) {\n\n" + addIndentation(0) +
	"return MapInfo[findAddress(address)];\n\n" +
	"}\n\n" +
	"void d2h_tran(void * address, int size) {\n\n" + addIndentation(0) +
	"if (!checkMapped(address)) {\n" + addIndentation(1) +
	"cl_mem* tmp_buff = ClMemAddressInfo[findAddress((void*)address)];\n" +
	addIndentation(1) +
	"if (checkMapped(address) != -1)\n" + addIndentation(2) +
	"clEnqueueReadBuffer(commands, *tmp_buff, CL_TRUE, 0, size, " + 
	"address, 0, NULL, NULL);\n" + addIndentation(1) +
	"MapInfo[findAddress((void*)address)] = 1;\n" + addIndentation(0) +
	"}\n\n" +
	"}\n\n" +
	"void h2d_tran(void * address, int size) {\n\n" + addIndentation(0) +
	"if (checkMapped(address)) {\n" + addIndentation(1) +
	"cl_mem* tmp_buff = ClMemAddressInfo[findAddress((void*)address)];\n" +
	addIndentation(1) +
	"clEnqueueWriteBuffer(commands, *tmp_buff, CL_TRUE, 0, size, " + 
	"address, 0, NULL, NULL);\n" + addIndentation(1) +
	"MapInfo[findAddress((void*)address)] = 0;\n" + addIndentation(0) +
	"}\n\n" +
	"}\n\n";


//----------------------------------------------------------------------------
// Contains information for a grid for a *single* step needed so that we know
// how to construct the H2D copies (clEnqueueWriteBuffer) and D2H copies
// (clEnqueueReadBuffer) before and after a kernel call. The latter is needed
// only when isWritten is 1 for a grid.
// TODO: Superseded by new method.
// Info used for constructing setting kernel arguments (depending on whether
// they are cl_mem objects -true for nonscalar grids- in which case we must
// call the setKernelArgs OCL function with sizeof(cl_mem) or if scalar with
// sizeof(TYPE).
// CAUTION: The code for isNonScalar starts from tmp_length+1 and 
// goes down to 2 for STRUCTS. This is done so that in setting the kernel
// arguments (getOCLargCalls()) we know when to add dynamic size - last
// (IF dynamic size is used for dimX's). General simple non-scalar 
// grids have isNonScalar == 1 and scalar ones have isNonScalar == 0. 
//----------------------------------------------------------------------------
function OCL_grid_info(gO, name, size) {

    this.gO = gO;
    this.name = name; // Does not contain 'ft_' prefix.
    this.size = size;
    this.isWritten = 0;
    this.isNonScalar = 0;

}


//----------------------------------------------------------------------------
// Object type that will form an array for all steps of a function.
// Info will be used to identify whether H2D/D2H data transfers are needed
// (essentially it keeps track of the validity of data across host/device).
// gridsInStep is an *ASSOCIATIVE* array: indexed by the NAME of the (non-
// scalar) grid and it's value is 0:not written in step, 1:written in step.
//----------------------------------------------------------------------------
function OCL_step_grids_info() {

    this.typeOfStep; // 0=Host(serial), 1=Device(parallel)
    this.gridsInStep = new Array();
    this.gridsSizes = new Array();

}




//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// PART B: OpenCL Code Generation Workflow Core Code
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------


//----------------------------------------------------------------------------
// Save the OpenCL/C string generated for the program
// parallel: save parallel version if 1. Serial if 0.
// show: To be passed in showOCLstr to alert(code) if 1.
// 	 Else, just to return the code string to caller.
//----------------------------------------------------------------------------
function saveOCLstr(parallel, show) {

    // TODO: In saveOCL() we'll pass the Device_type as given in the 
    //       auto-tuning menu. For now, we set manually for testing.
    Device_type = DevT.FPGA; 

    // TODO: Choose this from the auto-tuning menu as an option.
    DataTran = 1;

    // Get the JSON string
    //Regenerate code every time, otherwise may save old one if updated
    TypesAllFuncs = new Array();
    NamesAllFuncs = new Array();
    var str = encodeURIComponent(showOCLstr(1, parallel, show)); 
    //TODO: Is SoA by default (hence '1' as first argument)

    download2SaveFortran(str, CurProgObj.fileName);

    str = "";

    for (var i = 1; i < DeviceAuxPrototypes.length; i++) {

	str += DeviceAuxPrototypes[i] + "\n";

    }
    str += "\n";

    // (start from 3 - i.e., AFTER main function)
    for (var i = 3; i < Func_code_ser.length; i++) {

	var t_func = CurModObj.allFuncs[i].funcCallExpr.str;

    	if (CalledFuncsFromKernels.indexOf(t_func) != -1)
			str += "inline " + Func_code_ser[i];

    }

    for (var i = 0; i < CL_Funcs.length; i++) {

	str += CL_Funcs[i] + "\n\n";

    }

    str = encodeURIComponent(str);

    download2SaveFortran(str, "ocl_kernels.cl");


    // Adding helper functions
    str = OCL_glob_vars;
 
    if (DataTran == 1) str += "cl_mem index_data_dev;\n";
    str += "\n";

    if (Device_type != DevT.FPGA) {
    
        var tmp = OCL_setup_dev_code.replace("__CREATE_PROG__", "program = " +
		  "clCreateProgramWithSource(context, 1, (const char **)" +
	   	  "&kernel_source, &kernel_size, &error);\n");
	tmp = tmp.replace("__KERN_ALLOC__", "char* " + 
	      "kernel_source = (char *)malloc(sizeof(char)*kernel_size);\n");


	str += tmp.replace("__DEV_TYP__", "CL_DEVICE_TYPE_GPU");

    } else {

       var tmp = OCL_setup_dev_code.replace("__CREATE_PROG__", "program = " +
	 	 "clCreateProgramWithBinary(context, 1, &dev_id, " + 
	   	 "&kernel_size, (const unsigned char **)&kernel_source, " +
		 "NULL, " + 
	   	 "&error);\n"); 

       tmp = tmp.replace("ocl_kernels.cl", "ocl_kernels.aocx");
       
       tmp = tmp.replace("__KERN_ALLOC__", "unsigned char* " +
		"kernel_source = (unsigned char *)malloc(sizeof(unsigned " +
		"char)*kernel_size);\n"); 

       str += tmp.replace("__DEV_TYP__", "CL_DEVICE_TYPE_ACCELERATOR");

    }

    str += OCL_finalize_code;
   

    if (DataTran == 0) {

	if (Device_type != DevT.FPGA) {

            str += OCL_checkSVM + SVM_util_code;

        } else {

	    str += OCL_alignedMem;

        }


    } else {

	    str += DataTran_util_code;

    }
    
    str = encodeURIComponent(str);

    download2SaveFortran(str, "ocl_util.h");

}


//----------------------------------------------------------------------------
// Show the OpenCL/C string generated for the program
// strOfArr: Use structures of arrays (SoA) if 1. Arrays of structures (AoS)
// 	     if 0.
// show: Show code in JS (using alert) if 1. Just return code string if 0.
//----------------------------------------------------------------------------
function showOCLstr(strOfArr, parallel, show) {

    Soa = strOfArr;
    Soa = 1; //TODO: CAUTION. For OpenCL we cannot pass structs.
    ShowParallel = parallel;

    // If parallel code generation selected first we need to analyze.
    if (ShowParallel) {

        var mO = CurModObj; // TODO: Generalize for multiple modules.
        var fO = mO.allFuncs[getFuncIdByName(mO, "Main")];
        analyzeParallelismAll(fO, 0);
    
    }

    TypesAllFuncs = new Array();
    NamesAllFuncs = new Array();
    CL_Funcs = new Array(); // Save OpenCL code for .cl file.
    Func_code_ser = new Array(); // Contains SERIAL (for __device)
    CalledFuncsFromKernels = new Array();
    DeviceAuxPrototypes = new Array();

    var code = getOCLstr();
    if (show) {

        alert(code);

    } else {

        return code;

    }

}


//----------------------------------------------------------------------------
// Returns OpenCL/C for the current step in current funtion that the
// user is currently working on
//----------------------------------------------------------------------------
function getOCLstr() {

    var mO = CurModObj;

    // First, generate code for all functions, including 'main()'
    // Will be elements of the func_code array.
    var func_code = new Array(); // Used to store code for EACH function.
    
    // Variable to store all include statements needed.
    var inclStmts = "#include <stdio.h>\n" +
	    	    "#include <stdlib.h>\n" +
		    "#include <assert.h>\n";
    if (ShowParallel) {

	if (Device_type != DevT.FPGA) {

            inclStmts += "#include <CL/cl.h>\n";

	} else {

	    inclStmts += "#include \"CL/opencl.h\"\n";

	}

	inclStmts += "#include \"ocl_util.h\"\n";

    }

    TypeStr = ""; // Used to store TYPEs (i.e., structures).
    Func_prototypes = new Array();
   
    // A single string that contains all function code.
    var func_code_all = "";

    // TODO: Be careful with global variables. If not initialized every time, 
    // they'll hold the value, until exiting program!
    GID_function = 0;

    for (var f = mO.FuncStartID; f < mO.allFuncs.length; f++) {

	var func_code_tmp = getOCLstr4Func(mO, f);
	Func_code_ser[f] = func_code_tmp[0].replace(/^.*?#pragma omp.*\n?/mg, 
			   "");
        func_code[f] = func_code_tmp[1];
        
	// Note: Be careful where TypesAllFuncs starts != mO.allFuncs[start].
	func_code_all += TypesAllFuncs[f - mO.FuncStartID] + " " +
		 	 func_code[f];

    }


    // Code generation generates memory allocations with clSVMAlloc().
    // For the FPGA case, where fine-grained system SVM is supported
    // we don't need clSVMAlloc(), but *aligned* regular memory allocation.
    // So, we substitute all instances accordingly.
    // Last, we need to substitute clSetKernelArgSVMPointer() to 
    // clSetKernelArgSVMPointerAltera()
    if (Device_type == DevT.FPGA && DataTran == 0) {

        func_code_all = func_code_all.replace(/clSetKernelArgSVMPointer/g, 
			"clSetKernelArgSVMPointerAltera");

	// Also, replace clSVMAlloc() with alignedMalloc()
	// clSVMAlloc(context, CL_MEM_READ_WRITE, <SIZE>, 0);\n
	// alignedMalloc(<SIZE>);\n
	func_code_all = func_code_all.replace(
		     /clSVMAlloc\(context, CL\_MEM\_READ\_WRITE, (.+?), 0\)/g,
		     "alignedMalloc($1)");

	// Also, replace clSVMFree() with alignedFree().
	func_code_all = func_code_all.replace(
			/clSVMFree\(context, (.+)\)/g,
			"alignedFree($1)");

    }


    // Generate code for main method.
    var main_call = "int main(int argc, char *argv[]) {\n";

    //TODO:C: Use startup arguments grid as *argv[]. Add more flexibility.
    var t_mainFunc = CurModObj.allFuncs[DefMainFuncInd];
    main_call += addIndentation(0) + "char *" +
        	 var2OCL(t_mainFunc.allGrids[1].caption)+"[4];\n";

    main_call += addIndentation(0) + "int " +
        	 var2OCL(t_mainFunc.allGrids[0].caption) + ";\n";

    // If generating parallel implementation, setting nested parallelism off
    // by default.
    // TODO: This may be an option for the auto-tuner.
    // 	     If no parallel step in program, no need.
    if (ShowParallel) {
	    
        main_call += addIndentation(0) + "setup_OCL_dev();\n";

    }

    if (DataTran == 0) {

        main_call += addIndentation(0) + "index_data = (int *)clSVMAlloc(" +
	     	     "context, CL_MEM_READ_WRITE, sizeof(int)*9, 0);\n";

    } else {

        main_call += addIndentation(0) + "index_data = (int *)malloc(" +
	     	     "sizeof(int)*9);\n";

        main_call += addIndentation(0) + "index_data_dev = clCreateBuffer(" +
	    	     "context, CL_MEM_READ_WRITE, " +
		     "sizeof(int)*9, NULL, &error);\n";

    }

    // If device type is not FPGA (i.e., is CPU/GPU/MIC, then query the 
    // device for degree of SVM_SUPPORT.
    // NOTE: We only support OpenCL on devices that support OpenCL 2.0
    if (Device_type != DevT.FPGA && DataTran == 0) {

        main_call += addIndentation(0) + "SVM_Support = " +
		     "checkSVMAvailability(dev_id);\n";

 	main_call += addIndentation(0) + "if (SVM_Support==2) {\n" +
		     addIndentation(1) + "addNewAddress(index_data);\n" + 
		     addIndentation(0) + "}\n";

    } else if (DataTran == 1) {

	main_call += addIndentation(0) + 
		     "addNewAddress(index_data, &index_data_dev);\n";

    } 
    // Else, if FPGA, do nothing (any required changes are done cumulatively
    // at the end of code generation.

    main_call += addIndentation(0) +
        var2OCL(CurModObj.allFuncs[DefMainFuncInd].allGrids[0].caption) +
        " = " + var2OCL(DefMainFuncName) +
        "(" + 
	var2OCL(CurModObj.allFuncs[DefMainFuncInd].allGrids[1].caption) +
        ");\n";

    main_call += addIndentation(0) + "finalize_OCL_dev();\n";

    if (DataTran == 0) {

        main_call += addIndentation(0) + "clSVMFree(context, index_data);\n}\n";

    } else {

        main_call += addIndentation(0) + "free(index_data);\n";
        main_call += addIndentation(0) + 
		     "clReleaseMemObject(index_data_dev);\n}\n";

    }

    if (Device_type == DevT.FPGA) {


	main_call = main_call.replace(
		    /clSVMAlloc\(context, CL\_MEM\_READ\_WRITE, (.+?), 0\)/g,
		    "alignedMalloc($1)");

	main_call = main_call.replace(
			/clSVMFree\(context, (.+)\)/g,
			"alignedFree($1)");

    }

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

	//TODO: What about serial/parallel version if both abailable?

    }

    func_protos += "\n\n";
  
    // Final generated code will contain the TYPEs code, the library functions
    // code (e.g., for read/write CSV), the functions' code (that contains all
    // functions, including ft_Main), and the PROGRAM "int main" that calls
    // Main function and subsequently any other functions.
    var returnedCode = inclStmts + TypeStr + func_protos +
	               func_code_all + "\n" + main_call;

    returnedCode = returnedCode.replace(/UNIQUEIND/g, "int"); //TT

    return returnedCode;

}


//----------------------------------------------------------------------------
// Returns OpenCL/C code for a single function
//----------------------------------------------------------------------------
function getOCLstr4Func(mO, f) {

    var fO = mO.allFuncs[f];
    
    // This is the GID_function id of current function corresponding to 
    // TypesAllFuncs and NamesAllFuncs.
    var gID_f = 0;
    
    // Initialize current step numbering for current function to zero.
    CurStep = 0;
    
    // Initialize GridsInFuncOCL.
    GridsInFuncOCL = new Array();
    Loop_var_per_dim = new Array();
    Index_end_per_dim = new Array();
    OCL_steps_dataTran = new Array();
    CLMEMSinFunc = new Array();


    if (var2OCL(fO.funcCallExpr.str) == "ft_Main") {
	    
        // Add an INTEGER as main's return value (TypesAllFuncs[0])
        TypesAllFuncs.push("int");
        //gID_f = 1;	//NEXT one
        GID_function = 1; //NEXT one

    } else {
        
        for (var i = 0; i < TypesAllFuncs.length; i++) {

            if (NamesAllFuncs[i] == var2OCL(fO.funcCallExpr.str)) {
                
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
    Index_end_decl = "";
    Grids_new_decl = "";
    TitleDefs_decl = "";
    AllocFreePerFunc = "";

    // Create function header (function type plus name). Arguments to be
    // added in subsequent steps.
    // TODO:C: Commented out.
    var func_name =  var2OCL(fO.funcCallExpr.str);
	

    var func_head = "(";

    // Head part for OCL device inline (auxiliary functions (where pointers 
    // will be __global).
    var func_head_OCL_dev = "(";
				
    // In func_vars we declare the type and name of any grids that were passed
    // as parameters in the current function for which we are generating code.
    var func_vars = "";

    var func_val_init = "";

    var arr_dynValues = new Array();
	
    // Add argument list to function header. To do that, go through ALL grids 
    // in the function and add those who are incoming args.
    for (var g = 0; g < fO.allGrids.length; g++) {

        var gO = fO.allGrids[g]; 

        // TODO: If a grid with specific indices e.g. array[3][1] treat as 
        // passed by value!
        if (gO.inArgNum >= 0) { // This grid is an incoming arg.


            if (gO.numDims > 1 && gO.typesInDim != -1) {

		if (gO.inArgNum > 0) {

		    func_head += ", "; // arg separator
		    func_head_OCL_dev += ", "; // arg separator

	    	}

		if (!Soa) func_head += getDataTypeString_OCL(gO) + " *";

                //TODO: CAUTION: This is for the case of TYPE variable
                //	passed using its name (i.e., no specific element).
                func_head += expandStruct(gO, 1); //"typvar_" + gO.caption;
		func_head_OCL_dev += expandStruct(gO, 2);


            } else {

		if (gO.numDims >= 1) { 
		      
		    if (gO.inArgNum > 0) {

			func_head += ", ";         // arg separator
			func_head_OCL_dev += ", "; // arg separator

	    	    }

		    // If called function contains at least a parallel step, we
		    // will need a cl_mem pointer that will be used if need be.
		    // TODO: Can be optimized in case this variable is NOT used
		    // in the parallel step of called function (i.e., not pass).
		    if (DataTran == 1) {

		        if (funcContainsParStep(mO,f) && 
			    func_name != "ft_Main") {
		     
		            func_head += "cl_mem *" + var2OCL(gO.caption) + 
				         "_dev, ";
		    	    CLMEMSinFunc.push(gO.caption);

		        } 

		    }

                    func_head += getDataTypeString_OCL(gO) + " ";

		    func_head += "*" + var2OCL(gO.caption);

		    func_head_OCL_dev += "__global " + 
			                 getDataTypeString_OCL(gO) + 
					 "* " + var2OCL(gO.caption); 
		    // Grid caption as arg name 

	    	} else {

		    // Ensure has not been implicitly declared via a
		    // dynamically sized non-scalar grid (see below).
		    var regex = new RegExp(var2OCL(gO.caption + "[, )]")); 
		    if (func_head.search(regex) == -1) {

			if (gO.inArgNum > 0) {

		            func_head += ", ";         // arg separator
			    func_head_OCL_dev += ", "; // arg separator

	    		}    

		    	func_head += getDataTypeString_OCL(gO) + " " + 
			             var2OCL(gO.caption);
		        func_head_OCL_dev += getDataTypeString_OCL(gO) + " " + 
					     var2OCL(gO.caption);

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

		    var t_sz = gO.dimDynSize[i];
		    if (t_sz != null) {

			// The second check takes care of scalars that may have
			// been passed as parameters (i.e., explicitly), while
			// the first is for implicit passing (scalar grids used
			// for dynamic size of non-scalar grids).
		        if(arr_dynValues.indexOf(var2OCL(t_sz))==-1 &&
			   func_head.indexOf(var2OCL(t_sz))==-1) {
			    
			    arr_dynValues.push(var2OCL(t_sz));
			    dynVals += "int "+var2OCL(t_sz) + ",";
		            
			}
			    
		    }

		}
	       
 		if (dynVals != ", ") {

		    func_head += dynVals.replace(/,+$/, "");
		    func_head_OCL_dev += dynVals.replace(/,+$/, "");
		    
		}

	    } else {
		
		// For scalar-grids, we have to copy the src value into a temp
	   	// variable called fun_<src_var_name>.  
		// TODO: Can fuse with earlier similar loop for function 
		// header.  
		func_vars += addIndentation(0) + getDataTypeString_OCL(gO,
                             null) + " fun_" + gO.caption + ";\n";

                func_val_init += addIndentation(0) + "fun_" + gO.caption +
                                 " = " + var2OCL(gO.caption) + ";\n";

	    }

        }

    }
    
    func_head += ")";

    Func_prototypes.push(func_name + func_head + ";\n");

    func_head += " {\n";

    var func_ret_type = getDataTypeString_OCL(fO.allGrids[0], null);
    
    // TODO: Only add if called from a __kernel function in the .cl file.
    DeviceAuxPrototypes.push(func_ret_type + " " + func_name + "_device" + 
		            func_head_OCL_dev + ");");

    func_head_OCL_dev += ") {\n";

    // At this point we have completed in func_head the function header
    // that contains the type of function, the function name, and its
    // arguments contained in parentheses.

    // Used for declaration of the ret value declaration within the func.
    // This is always in position 0 in fO.allGrids[].
    func_vars += addIndentation(0) + getDataTypeString_OCL(fO.allGrids[0],
                 null) + " " + var2OCL(fO.allGrids[0].caption) + ";\n";

    // STEP: Code for each step in the function.
    // Two positions ([0] is serial for __device, [1] is for parallel).
    var step_code = new Array();
    step_code[0] = "";
    step_code[1] = "";
    
    var stepStart = 1; // Note: Function prototype at step 0. 
    
    for (var s = stepStart; s < fO.allSteps.length; s++) {

	OCL_steps_dataTran.push(new OCL_step_grids_info());
        
        var step_code_tmp = getOCLstr4Step(fO, fO.allSteps[s], mO);
	step_code[0] += step_code_tmp[0]; // Serial (_device).
	step_code[1] += step_code_tmp[1]; // Parallel (if available).
	
	// For CPU/GPU/MIC (i.e., except FPGA) we need to add SVM calls for 
	// Map/Unmap where necessary)
	if (Device_type != DevT.FPGA && DataTran == 0) {

	    step_code[1] = addSVMcalls(fO, s, step_code[1]);
	    
	} else if (DataTran == 1) {
	       
	    // Check if there is a need for D2H transfers for non-scalar
	    // grids that were previously altered in a device step.
	    // Check if there is a need for H2D transfer for non-scalar
	    // grids that were previously altered in a host step.
	    step_code[1] = addDataTranCalls(fO, s, step_code[1]);
	
	} else {
	    step_code[1] = step_code[1].replace("__SVM__", "");
	}
    }


    //printOCL_steps_dataTran(); // For debugging purposes.

    if (AllocFreePerFunc != "") {

	AllocFreePerFunc = "\n" + addIndentation(0) + 
		            "//Freeing up memory buffers.\n" + 
			    AllocFreePerFunc;

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
    var function_string = new Array();
    
    function_string[0] = func_ret_type + " " + func_name + "_device" + 
	    		 func_head_OCL_dev;

    function_string[1] = func_name + func_head;
    
    function_string[0] += func_vars + Row_col_decl + Index_end_decl + 
	    		  TitleDefs_decl + Grids_new_decl + func_val_init + 
			  step_code[0] + AllocFreePerFunc;

    function_string[1] += func_vars + Row_col_decl + Index_end_decl + 
	    		  TitleDefs_decl + Grids_new_decl + func_val_init + 
			  addIndentation(0) + "int dev_alloc_size;\n" +
        		  step_code[1] + AllocFreePerFunc;

    // If no return statement added (e.g., when "void"), we return 1 (since 
    // ALL Grid Language functions need a return type).
    //if (function_string.indexOf("return") == -1) {

    //	function_string += addIndentation(0) + "return 1;\n";

    //}


    // TODO: CAUTION: Copy back to host anything written on device and
    //       not yet updated on host.
    
    function_string[0] += "}\n\n";
    function_string[1] += "}\n\n";

    function_string[1] = replaceClArgsInFuncCall(func_head, function_string[1]);

    return function_string;

}


//----------------------------------------------------------------------------
// Returns OpenCL/C code for a given step in a given function
//----------------------------------------------------------------------------
function getOCLstr4Step(fO, sO, mO) {


    var kernelHead = "";         // For saving current step/kernel head.
    var kernelHeadArgs = "";     // For saving current step/kernel head's 
    			         // arguments.
    var ivars_all = "";          // For saving all row/col/indX declarations for
    			         //step/kernel.
    var ivars_get_glob = "";     // For saving assignment of get_global_id() to
    			         // the index variables that are parallelizable.
    var cur_glob = 0;            // Keeps track of current get_glob_id(X)
    var allocatablesOCL = "";    // Keeps track of OpenCL buffer creation.
    var titleDefs_per_step = ""; // For title definitions per step.
    var func_ret_decl = "";      // For declaring return values for functions
			         // called from within a step/kernel function.
    var glob_work_size = "\n" + addIndentation(0) + 
	    		 "//Converting parallel loop in " +
	   		 "NDRange (loop indices converted to NDRange " +
			 "dimensions).\n"; 
    glob_work_size += addIndentation(0) + "size_t globalWorkSize_s" + 
	    		 CurStep + "[] = {"; 

    var ocl_step_grid_info = new Array();

    // Contains the generated code that calculates index_data[] values
    // Where index_data is generated code, too and passed as such to
    // the kernel.For example:index_data_set = "ft_end0 - 2", where
    // this string is assigned in generated code for index_data[0]
    // (i.e., "index_data[0] = ft_end0 - 2"). This is then 
    // calculated in the code when executed, and "index_data" is
    // passed to the kernel to be used there as needed.
    var index_data_set = "";

    // Both below variables are used to help generate code that is
    // non-redundant (e.g., instead of "ft_end - 0", code generated
    // would only be "ft_end").
    var idat_ctr = 0;
    var index_data_int = new Array();
    for (var i = 0; i < 9; i++) index_data_int[i] = 0;

    var grids = "";
    var let_grids = "";
    var titleDefs = "";
    Struct_el_decl = ""; 

    // Used to keep track of which DO loops are parallel, so we
    // can close them appropriately in the reverse order.
    var stepOmpDoStack = new Array();

    // Increase step ID (within a function- across functions this is 
    // re-initialized to zero)
    CurStep++;

    var allocatablesOfStep = "";

    var funcID = getFuncIdByName(mO, fO.funcCallExpr.str)

    CalledFuncsFromStep = new Array();            // Re-initialize per step.

    OCL_steps_dataTran[CurStep-1].typeOfStep = 0; // (default)

    var arr_dynValues = new Array();

    // Go through all grids in the step and declare as needed fields.
    for (var g = 0; g < sO.allGridIds.length; g++) {

        var gId = sO.allGridIds[g];
        var gO = fO.allGrids[gId];

	var newgrid = createTypeString_OCL(gO); 
	// Get the declaration of current grid

	// Get what is inside the parentheses (i.e., the dimensions).
	// Can be null if it is a scalar grid.
        var dimensions = newgrid.match(/\[(.+)\]/);
	
	var gridNam;
	var devalloc;
	var tmp_type;
	var tmp_dim_alloc;

	if (dimensions != null) {

	    newgrid = newgrid.replace(dimensions[0], "");
	    
	    var split_dims = dimensions[1].replace(/ /g, "");
            split_dims = split_dims.split("][");

            for (var k = 0; k < split_dims.length; k++) {

	        // If dimension is not a number, this means we may be using
		// a variable as a dimension.
                if (isNaN(split_dims[k])) {

                    // Only exception if we have a TYPE declared which
		    // is NOT allocatable.
                    // TODO: If we decide to allow tables of structs 
		    // later this will need to be reconsidered.
                    if (split_dims[k].indexOf("TYP_") == -1 &&
                        split_dims[k].indexOf("DIMENSION") == -1) {

                        //split_dims[k] = var2OCL(split_dims[k]);

                    }

                }

             }

	     // C pointer to be malloc'd and do actual allocation by calling 
	     // "malloc".

	     if(gO.numDims > 1 && gO.typesInDim != -1)
                 tmp_type = "struct TYP_" + gO.caption;
	     else
	         tmp_type = getDataTypeString_OCL(gO, null);

	     tmp_dim_alloc = "";

	     for(var i = 0; i < split_dims.length - 1; i++)
	         tmp_dim_alloc += split_dims[i] + "*";
			
	     tmp_dim_alloc += split_dims[split_dims.length - 1];
		
	     if(gO.typesInDim == -1)
		gridNam = var2OCL(gO.caption);
	     else
		gridNam = "typvar_" + gO.caption;
		
	     devalloc = addIndentation(0) + "dev_alloc_size = sizeof(" + 
		        tmp_type + ")*" + tmp_dim_alloc + ";\n";

	     ocl_step_grid_info.push(new OCL_grid_info(gO, gO.caption, 
			             "sizeof(" + tmp_type + ")*" +
			             tmp_dim_alloc));

	     ocl_step_grid_info[ocl_step_grid_info.length-1].isNonScalar = 1;
	     	    
	     var tmpArg = newgrid.replace("*","* restrict ");
	     kernelHeadArgs += "__global " + tmpArg.replace(";\n", "");
	     // TODO: sanity checks.
	     //
	     kernelHeadArgs += getDynValArgs(kernelHeadArgs, gO, arr_dynValues);	     
	     kernelHeadArgs +=", ";

	} else {

	    // Two cases: either a scalar grid 
	    // or a series of declarations that correspond to a struct
	    // i.e., grid with titles and different data types per dim.
	    // In the latter case we need to add all of them to 
	    // ocl_step_grid_info and add appropriately to kernelHeadArgs, as 
	    // if it were non-scalar grids.
	    if (gO.numDims == 0) {	
	    
		    
	        // Ensure has not been implicitly declared via a
		// dynamically sized non-scalar grid (see below).
		var regex = new RegExp(var2OCL(gO.caption + "[, )]")); 
		if (kernelHeadArgs.search(regex) == -1) {

		    ocl_step_grid_info.push(new OCL_grid_info(gO, gO.caption, 
			"sizeof(" + getDataTypeString_OCL(gO, null) + ")"));

		     kernelHeadArgs += newgrid.replace(";\n","") + ", ";

		 }
		 // else do not declare at all.
		    
	         //ocl_step_grid_info.push(new OCL_grid_info(gO, gO.caption,
		 //	               "sizeof(" + 
		 //		       getDataTypeString_OCL(gO, null) + 
		 //		       ")"));

	         //kernelHeadArgs += newgrid.replace(";\n","") + ", "; 

	    } else if (gO.numDims > 1 && gO.typesInDim != -1) {

		kernelHeadArgs += expandKernelHeadStruct(ocl_step_grid_info, gO);
		// Since all elements of a struct have the same dimensions
		// (if dynamic) this only needs to run once per struct.
		kernelHeadArgs += 
			getDynValArgs(kernelHeadArgs,gO, arr_dynValues) + ", ";

	    } else {

		alert("Unhandled");

	    }

	}


        if ((gO.inArgNum < 0) && (!gO.isRetVal)) {

            // If grid has been already declared within THIS function, 
            // do not re-declare.
            
            // Get what is inside the parentheses (i.e., the dimensions).
	    // Can be null if it is a scalar grid.

            if (dimensions != null) {

                // Check all dims. If EVEN one dynamic is found, we need
	    	// to address the array as a 1D dynamically allocated.
	    	// dimActSize length always has length equal to the 
		// dimensions, even if their value is not the one used (but 
		// rather a dynamic size if applicable).
	    	// TODO:C: Doesn't cover STRUCTS with dynamic num of elements.
	        	
		if (GridsInFuncOCL.indexOf(gO) == -1) {

		    if (DataTran == 0) {


			// No need for separate allocatables for SVM.
			/*
		        allocatablesOCL += devalloc + addIndentation(0) +
	   			       gridNam + " = " + "(" + tmp_type + 
				       " *)" +  
				       "clSVMAlloc(context, " +
				       "CL_MEM_READ_WRITE, dev_alloc_size" +
	    			       ", 0);\n";
			*/

		        allocatablesOfStep += devalloc + addIndentation(0) +
	   				  gridNam + " = " + "(" + tmp_type + 
					  " *)" +  
					  "clSVMAlloc(context, " +
					  "CL_MEM_READ_WRITE, " +
	    				  "dev_alloc_size, 0);\n";

		    } else {

			// Initializing OpenCL buffer.
		        allocatablesOCL += addIndentation(0) + gridNam + 
	    				   "_dev = clCreateBuffer(" +
	    				    "context, CL_MEM_READ_WRITE, " +
					    "dev_alloc_size, NULL, &error);\n";


                        allocatablesOfStep += devalloc + addIndentation(0) +
                            		      gridNam + " = " + "(" +  
					      tmp_type + 
					      " *)malloc(dev_alloc_size);\n";

		    }


		    if (Device_type != DevT.FPGA && DataTran == 0) {

		        // Only do if course-grained buffer SVM is the level 
			// supported.
			// Otherwise, SVM_Support will be 2 (for fine-grained 
			// buffer) and maps/unmaps are not needed (nor keeping 
			// track of addresses accordingly).
		        allocatablesOCL += addIndentation(0) + 
				           "if (SVM_Support==2) {\n" +
		    		           addIndentation(1) + 
					   "addNewAddress(" + gridNam + 
					   ");\n" + addIndentation(0) +
				           "}\n";

		        allocatablesOfStep += addIndentation(0) + 
				              "if (SVM_Support==2) {\n" +
		    		              addIndentation(1) + 
					      "addNewAddress(" + gridNam + 
					      ");\n" + addIndentation(0) +
				       	      "}\n";

		    } else if (DataTran == 1) {

		        allocatablesOCL += addIndentation(0) + 
					   "addNewAddress(" + gridNam +
					   ", &" + gridNam + "_dev" +
					   ");\n";

		        //allocatablesOfStep += addIndentation(0) +  
			//		      "addNewAddress(" + gridNam +
			//		      ", &" + gridNam + "_dev" +
			//		      ");\n";
			
		    }
		    // Else SVM fine-grained system for FPGA.

		    grids += addIndentation(0) + newgrid;	
		   
		    if (DataTran == 1) {

		        // Not needed for *scalar* grids. Passed by value 
			// in OpenCL.
	   	        grids += addIndentation(0) + "cl_mem " +
	 			 var2OCL(gO.caption) + "_dev;" + "\n";

		    }		    

		    // Push into list of grids that have been declared (in the 
                    // context of the current function being parsed).
		    GridsInFuncOCL.push(gO);

		}

            } else { // Corresponds to dimensions == null, i.e., scalar var
		     // or struct (represented as <type0>* <name>_dim0, etc.).

		if (GridsInFuncOCL.indexOf(gO) == -1) {


		    // If no SVM implementation, we need to declare the
		    // corresponding cl_mem objects for the struct elements.
		    if (DataTran == 1) {

			// For each <type>* <name> element, declare a 
			// cl_mem <name> memory object.
			var cldev = "";
			if (gO.numDims != 0) {

				cldev = newgrid.replace(/.+\*(.+);$/mg, 
					addIndentation(0) + "cl_mem $1_dev;");	

		    	}

			newgrid += cldev;

		    }

		    if (gO.numDims != 0)
		    	grids += newgrid;
		    else
			grids += addIndentation(0) + newgrid;

		    // Push into list of grids that have been declared (in the 
                    // context of the current function being parsed).
                    GridsInFuncOCL.push(gO);

		}

            }	

        }

        // STEP: Get var defs for titles.
        
        // Get titleDefs (e.g., var _d3Tab1,_d3Tab2...).
        var titleDefTmp = getTitleDefsOfGrid(gO);

	// If it is a struct, we don't need to take care of titles
	// because we use the _dimX representation.
	// TODO: Doesn't take into account structs with >1D elements.
        if (titleDefTmp != "") {

            // Remove "var " and trailing ';' (e.g., _d3Tab1,_d3Tab2...)
            // and prepend the grid name before the title, to discern
	    // for the cases when a (same) title is used for different
	    // grids that corresponds to different values.
	    titleDefTmp = titleDefTmp.replace("var ", "");
            //titleDefTmp = titleDefTmp.replace(";", "");
            titleDefTmp = titleDefTmp.replace(/_/g, gO.caption + "_");
            titleDefTmp = "int " + titleDefTmp;

	    if (gO.typesInDim == -1)
	        titleDefs_per_step += addIndentation(0) + titleDefTmp;

	    // If it has been declared already, do not re-declare.
            if (TitleDefs_decl.indexOf(titleDefTmp) == -1) {

                TitleDefs_decl += addIndentation(0) + titleDefTmp;

            }

        }

    }

    // STEP: Create for loops (a loop for each index var)
    // Note: rangeExpr.exprArr[] contains root range expressions
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

    var num_index_vars = 0;

    // if there are index variables
    if (rangeExpr && rangeExpr.exprArr && rangeExpr.isForeach()) {

        num_index_vars = rangeExpr.exprArr.length;
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
            var ivar = var2OCL(rexpr.labelExpr.str);

            // Define the value of literal 'end'
            var endv = var2OCL(DefEndName + rexpr.selDim);

            //TODO: Why did I change the below to the above?			
            //var endv = expr2OCLstring(rexpr.exprArr[RangeFields.End]);

            // IMPORTANT NOTE: semasiology is SAVE if assignment 
            // is at the time of declaration. Saved between function calls. 
            // That's why here we separate declaration and initialization.

	    // We do not allow re-declaration of the same end variable across 
	    // steps.
            if (Index_end_per_dim.indexOf(endv) == -1) {

                Index_end_decl += addIndentation(0) + "int " + endv + ";\n";
                Index_end_per_dim.push(endv);

            }


            // Pick the end value based on whether the size of the dim is
            // variable (dynamically allocated) or not.
            //
            var actendval = rexpr.gO.dimActSize[rexpr.selDim];
            var dynendval = rexpr.gO.dimDynSize[rexpr.selDim];
            var endval = (dynendval) ? var2OCL(dynendval) : actendval;

	    // Step: Start/End/Step expressions
            var start = expr2OCLstring(rexpr.exprArr[RangeFields.Start]);
            var end = expr2OCLstring(rexpr.exprArr[RangeFields.End]);
            var step = expr2OCLstring(rexpr.exprArr[RangeFields.Step]);


	    // TODO: Map/Unmap if SVM coarse grain
	    if (start != "0") {

	        index_data_set += addIndentation(0) + "index_data[" + idat_ctr +
				  "] = " + start + ";\n";
	    	index_data_int[iv*3] = 1;

	    }

	    idat_ctr++;

	    if (step != "1") {

	        index_data_set += addIndentation(0) + "index_data[" + idat_ctr +
		    	      "] = " + step + ";\n";
	        index_data_int[iv * 3 + 1] = 1;

	    }

	    if (index_data_int[iv*3] || index_data_int[iv*3+1])
		    index_data_int[iv*3+2] = 1;
	    
	    idat_ctr++;


	    // In parallel code generation, if loop is parallel, we append the
	    // endval to the collapsed loop vars, else we declare it normally
	    // at its normal position (whereas collapsed loop vars appear 
	    // BEFORE the OMP PARALLEL DO COLLAPSED(X) directive).
            if (ShowParallel) {

                if (Pragma_str[funcID][CurStep].indexOf(ivar) != -1) {

                    collapsed_loop_vars += addIndentation(0) + endv + " = " + 
			    		   endval + "-1;\n"; 

		    // If step != 1 then need CEIL().
		    // In general case: ceil(end-start+1)/step).
		    var tmp_glob_work_size = end; 
		    if (start != "0")	    
		        tmp_glob_work_size += " - " + start;
		    tmp_glob_work_size += " + 1";
		    if (step != "1") { 

		        tmp_glob_work_size = "ceil((" + tmp_glob_work_size + 
				             ")/" + step + ")";
		    	InclMath = 1; // To include math.h

		    }
		    glob_work_size += tmp_glob_work_size + ", ";

                } else {

                    forstr2 += addIndentation(iv) + endv + " = " + endval +
                               "-1;\n";

                }

            } else {

                forstr += addIndentation(iv) + endv + " = " + endval + 
			  "-1;\n";

            }

            if (Loop_var_per_dim.indexOf(ivar) == -1) {

                // Adding the names of index variables to the declarations 
                // string if it has NOT been declared for this dimension in 
                // THIS function and push it in the array denoting it has 
                // been now declared.
                Row_col_decl += addIndentation(0) + "int " + ivar + ";\n";
                Loop_var_per_dim.push(ivar);

            }


           

            if (ShowParallel) { 

	        // ShowParallel check is implied 0 if collapse is > 0 
		// (safely delete check).

		ivars_all += addIndentation(0) + "int " + ivar + ";\n";

		// If loop is parallel over ivar:    
                if (Pragma_str[funcID][CurStep].indexOf(ivar) != -1) {

		    OCL_steps_dataTran[CurStep-1].typeOfStep = 1;

		    //alert(ivar);
		    ivars_get_glob += addIndentation(0) + ivar + 
			    	      "= get_global_id(" + cur_glob++ + 
				      ")";

		    // If step!=1
		    if (index_data_int[iv*3+1] == 1) {
			
			switch(iv) {
			    case 0:
				ivars_get_glob += "*index_dat[1]";
				break;
			    case 1:
				ivars_get_glob += "*index_dat[3]";
				break;
			    case 2:
				ivars_get_glob += "*index_dat[5]";
				break;
			    default:
				alert("Max: 3 dimensions in OpenCL");

			}

		    }

		    // If start!=1
		    if (index_data_int[iv*3] == 1) {

			switch(iv) {
			    case 0:
				ivars_get_glob += "+index_dat[0]";
				break;
			    case 1:
				ivars_get_glob += "+index_dat[2]";
				break;
			    case 2:
				ivars_get_glob += "+index_dat[4]";
				break;
			    default:
				alert("Max: 3 dimensions in OpenCL");

			}

		    }
				      
		    ivars_get_glob += ";\n";


                    if (collapse_int != 0) {

                        forstr += addIndentation(0) +
                            	  "#pragma omp parallel for collapse(" + 
				  collapse + ")\n";

                        collapse_int = 0; 
			// So that only first parallelizable dimension is 
			// written (with collapse thereby incorporating all 
			// parallelizable dimensions).
			
			// Add this step as an OpenCL kernel function.
		    	kernelHead = "__kernel void " + 
				     var2OCL(fO.funcCallExpr.str) +
			     	     "_" + CurStep + "_dev";

                    }

		    // TODO: This will be needed when we allow multiple 
		    // combinations in auto-tuner.
                    // Here maintain a stack of OMP DO loops, so we can 
		    // close accordingly.
                    //stepOmpDoStack.push("!$OMP END PARALLEL DO\n");

                    //forstr += addIndentation(iv) + "for (" + ivar + " = " +
                    //          start + "; " + ivar + " <= " + end + "; " +
                    //          ivar + " += " + step + ") {\n";

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
                //          start + "; " + ivar + " <= " + end + "; " + ivar + 
		//	  " += " + step + ") {\n";
		
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

                loop_close += addIndentation(num_index_vars - iv - 1) + "}\n";

                if (collapse && iv == num_index_vars - 1) {

                    //Only for last one and only if collapse
                    loop_close += addIndentation(0) + "\n";

                }

		// TODO: May be needed in autotuning multiple combinations
		// of collapsing/non-collapsing loops. 
                //Here need to close the appropriate OMP DO loop.
                //loop_close += stepOmpDoStack.pop();

            } else {

                loop_close += addIndentation(num_index_vars - iv - 1) + "}\n";

            }

        }

    } else if (rangeExpr && rangeExpr.isForever()) {

        forstr += addIndentation(0) + "while(1) {\n";
        loop_close = addIndentation(0) + "}\n";

    }


    // STEP: Go through all the boxes and create mask/formula.

    var stmt = "";
    var prev_indent = 1;    // First statement always has 1 tab indentation.
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

	    indent = 1; // This only occurs for empty IF statement.

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

            } else if (boxexpr && boxexpr.exprArr && boxexpr.exprArr.length) {

                // condition with child expression  -- if/elseif/breakif.

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
                            "if" + "(" + expr2OCLstring(boxexpr) + ") {\n";

                    last_if_indent = indent; // So that we can close it later.
                    
                } else if (boxexpr.isElseIf()) {

                    var tmp2 = last_if_indent;
                    for (var i = indent; i < tmp2; i++) {

                        stmt += addIndentation(last_if_indent) + "}\n";
                        mask_unmatched--;
                        last_if_indent--;

                    }

                    stmt += addIndentation(indent + index_extra_indent) +
                            "} else if" + "(" + expr2OCLstring(boxexpr) +
                            ") {\n";
                    
                } else {

                    // TODO: What is this case doing? (breakif)
                    stmt += addIndentation(indent + index_extra_indent) +
                            boxexpr.str + "(" + expr2OCLstring(boxexpr) + 
			    ") {\n";
                    //alert(boxexpr.str);
                }

            } else {

                stmt += "";
                //TODO: This is not correct? Delete else 
                //altogether?

            }

        } else {

            // Step: Process a formula statement

            // TODO: Do some more checking on the following. May be trickier 
            // than that
            if (sO.boxAttribs[box].indent <= last_if_indent) {

                for (var i = 0; i <= (last_if_indent - sO.boxAttribs[box].indent); i++) {

                    stmt += addIndentation(last_if_indent + 
			    index_extra_indent) + "}\n"
                    mask_unmatched--;
                    last_if_indent--;

                }

            }

            if (sO.boxAttribs[box].isFormula() && boxexpr.exprArr.length > 2
                && (boxexpr.exprArr[1].isAssignOp() ||
                boxexpr.exprArr[1].isXAssignOp())) {

                // Should cover all cases: simple equality OR
                // the case of +=, -=, *=, /= (assertion of valid 
                // exprArr[1] is guarranteed by previous steps in GUI).

                // TODO: Here what if name changes (or is set by LET). 
                // Need to take special care.

		var t_lhsCap = boxexpr.exprArr[0].gO.caption;

		// 'If' refers to +=, etc. cases, 'else' to normal assignments
                if (boxexpr.exprArr[1].isXAssignOp()) {
                    
		    // HERE IF isXAssignOp() then find the two parts BEFORE
                    // and AFTER the XAssignOp and output RHS = RHS + LHS (use
		    // indexOf() and substring() JS methods).
                    // Because ifort does not support the +=, etc. notation.

                    //TODO: May it have any side effects?
                    var wholestr = expr2OCLstring(boxexpr);
                    var pre_str = wholestr.substring(0, wholestr.indexOf(
                                  boxexpr.exprArr[1].str) - 1);
                    var post_str = wholestr.substring(wholestr.indexOf(
                                   "+=") + 3);

                    stmt += addIndentation(indent + index_extra_indent) +
                            pre_str + " = " + pre_str + " " +
                            boxexpr.exprArr[1].str.substring(0, 1) + " " +
                            post_str + ";\n";

		    OCL_steps_dataTran[CurStep-1].gridsInStep[t_lhsCap] = 1;

                } else {

                    stmt += addIndentation(indent + index_extra_indent) +
                            expr2OCLstring(boxexpr) + ";\n";

		    OCL_steps_dataTran[CurStep-1].gridsInStep[t_lhsCap] = 1;

                }

                // In current formula box: Loop through the exprArr[] 
                // array and detect if there are functions (isFuncCall()).
                for (var lp = 2; lp < boxexpr.exprArr.length; lp++) {
                    
		    // TODO: If +=, -=, etc. then checking 2 is redundant?
		    
		    var t_str = boxexpr.exprArr[lp].str;

                    // If it is a function, save its type to pass when 
                    // building the function code
                    // Note: For library function calls we don't need 
		    // explicit declaration.
                    if (boxexpr.exprArr[lp].isUserFuncCall()) {

                        // Get global id of function within module
                        var f = getFuncIdByName(mO, t_str);

                        // Only add to declarations if NOT already declared 
			// (i.e., 1st time called).
                        if (Func_decl.indexOf(var2OCL(t_str) + ";\n") == -1) {

                            // Used for data type declaration of function in 
			    // caller.
                            Func_decl += addIndentation(0) +
                                getDataTypeString_OCL(
				mO.allFuncs[f].allGrids[0],null) + " " +
                                var2OCL(boxexpr.exprArr[0].str) + ";\n"; 

                            // TODO: This means that
                            // the function will have a limited scope (within 
                            // current function-caller, declared there). Is 
			    // that OK? 

                            TypesAllFuncs[GID_function] =
                                    getDataTypeString_OCL(
                                    mO.allFuncs[f].allGrids[0], null);

                            NamesAllFuncs[GID_function] = var2OCL(
                                boxexpr.exprArr[lp].str);

                            GID_function++; 
			    //Increase ID to represent next called function.

                        }

                    }

                }


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

                    return_expression += expr2OCLstring(boxexpr.exprArr[i]);

                }

                if (boxexpr.exprArr.length != 1) {

                    ret_val_assignment = "return " + return_expression;

                } else {

                    ret_val_assignment = "return " + 
			                 var2OCL(fO.allGrids[0].caption);

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
		    // need to "pipe" through dataTypeIntToStr_OCL after we 
		    // get the (language-independent) data type integer 
		    // representation of the framework.
		    let_data_type = dataTypeStrToInt(let_data_type);
		    let_data_type = dataTypeIntToStr_OCL(let_data_type);

                    let_grids += addIndentation(0) + let_data_type + " " +
                             var2OCL(boxexpr.exprArr[0].exprArr[0].str) +
                             ";\n";
		    
                    if (ShowParallel) {
			    
			private_vars += 
			     var2OCL(boxexpr.exprArr[0].exprArr[0].str) + ", ";

		    }

                    var let_RHS = "";

                    for (var let_ass = 1; let_ass < boxexpr.exprArr.length; let_ass++) {

                        let_RHS += expr2OCLstring(boxexpr.exprArr[let_ass]);

		    }

                    stmt += addIndentation(indent + index_extra_indent) +
                            var2OCL(boxexpr.exprArr[0].exprArr[0].str) +
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
                    if (Func_decl.indexOf(var2OCL(boxexpr.exprArr[0].str) +
                        ";\n") == -1) { //TODO: C: CAUTION: Check 

                        // Used for data type declaration of function in 
			// caller.
                        Func_decl += addIndentation(0) +
                            getDataTypeString_OCL(mO.allFuncs[f].allGrids[0],
                            null) + " " +
                            var2OCL(boxexpr.exprArr[0].str) + ";\n";


                        // Used for declaration of type in header of function.
                        TypesAllFuncs[GID_function] = getDataTypeString_OCL(
                            mO.allFuncs[f].allGrids[0], null);

                        NamesAllFuncs[GID_function] = var2OCL(
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

			var t_res = mO.allFuncs[f].allGrids[0];
                        grids += addIndentation(0) + 
				 getDataTypeString_OCL(t_res, null) + " " +
                                 "res_" + boxexpr.exprArr[0].str + ";\n";

                    }

                    stmt += addIndentation(indent + index_extra_indent) +
                            "res_" + boxexpr.exprArr[0].str + " = " +
                            expr2OCLstring(boxexpr) + ";\n";

                } else { 
		
		    // Anything else (including library functions - taken care
		    // within expr2OCLstring).

                    stmt += addIndentation(indent + index_extra_indent) +
                            expr2OCLstring(boxexpr) + ";\n";

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
    // TODO: DO NOT START FROM 1 IF EMPTY.

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
    Grids_new_decl += grids + let_grids;


    // If any of the start/step for any of the loop's indices (if there
    // is a loop) is non-default (i.e., if step!=1 && start!=0), the
    // information will be passed to the kernel, so we need to add this
    // as the last kernel argument.
    if (index_data_int[2] || index_data_int[5] || index_data_int[8]) {

        kernelHeadArgs += "__global int* restrict index_dat, ";

    }

    kernelHead += "(" + kernelHeadArgs.replace(/, $/,"") + ")";
    kernelHead = kernelHead.replace(/, \)/, ')');
    kernelHead = kernelHead.replace(/, ,/g, ', ');

    // Add OMP PRIVATE clause, for parallel version, if we have a
    // OMP PARALLEL DO directive AND private variables.
    if (ShowParallel) {

        // If we don't have a parallel loop, then do not use private.
        if (forstr.length != 0 && Pragma_reduction[funcID][CurStep] != "") {
            
            // Save pragma_reduction global to local, make small leters, 
            // remove preceding "!$ omp " and trailing \n.
            var tmp_reduction_pr = Pragma_reduction[funcID][CurStep];
            tmp_reduction_pr = tmp_reduction_pr.replace("!$OMP REDUCTION", 
			       "reduction");
            tmp_reduction_pr = tmp_reduction_pr.replace("\n", "");

	    if (private_vars != "") {

		//TODO: Add indentation for private_vars.
                private_vars = private_vars.replace(/, +$/, "");
                private_vars = "private(" + private_vars + ")";    
	        forstr = forstr.replace(/\n/, " " + private_vars + "\n");

	    }

            forstr = forstr.replace(/\n/, " " + tmp_reduction_pr + "\n");
            //TODO: Add indentation for private_vars.
            
        }

    }


    // In order to obtain the dynamic variables we parse the allocatablesOfStep
    // variable and Struct_el_decl and process accordingly.
    var allocatablesFree = allocatablesOfStep.replace(/^\tdev\_.+$/mg, "");

    if (DataTran == 0) {

        allocatablesFree = allocatablesFree.replace(/^\tft_(.+) = .+$/mg, 
		           "\tclSVMFree(context, ft_$1);");
        allocatablesFree = allocatablesFree.replace(
			   /^(?!\tclSVMFree).*\r?\n?/mg, "");
        allocatablesFree += Struct_el_decl.replace(/^\tft_(.+) = .+$/mg, 
		            "\tclSVMFree(context, ft_$1);");

    } else {

	 allocatablesFree = allocatablesFree.replace(/^\tft_(.+) = .+$/mg, 
		           "\tfree(ft_$1);\n\tclReleaseMemObject(ft_$1_dev);");
         allocatablesFree += Struct_el_decl.replace(/^\tft_(.+) = .+$/mg, 
		            "\tfree(ft_$1);");
	 allocatablesFree = allocatablesFree.replace(/^\tfree\((.+\_dev).*$/mg, 
		            "\tclReleaseMemObject($1);");
         var frees = allocatablesFree.replace(/^(?!\tfree).*\r?\n?/mg, 
         			    "");
	 var releases = allocatablesFree.replace(/^(?!\tclRelease).*\r?\n?/mg, 
         			    "");
	 allocatablesFree = frees + releases;

    }

    AllocFreePerFunc += allocatablesFree;

    // STEP: Combine all the above components together:
    // a) Loop variables' initialization from collapsed loops.
    // b) Allocatable variables of step.
    // c) DO/OMP DO loops (their combinations depending on the case).
    // d) Actual step computation code.
    // e) Loop closing clauses (serial and/or parallel).
    // Create two versions of each step. One in[0] would be the step for ALL 
    // serial (so it can be the _device code).
    // The one in [1] will be -1 if non-parallelizable, or the parallel
    // version of the step.
    var code = new Array();
    code[0] = allocatablesOfStep + Struct_el_decl + collapsed_loop_vars + 
	      forstr + forstr2 + replaceCallsFromKernel(stmt) + loop_close;


    // In case we don't have an FPGA (assume fine-grained system support)
    // we need to Map the index_data variable before a host step.
    var addSVM_index_host = "";
    if (Device_type != DevT.FPGA && DataTran == 0) {

        if (index_data_int[2] || index_data_int[5] || index_data_int[8]) {
            addSVM_index_host = addIndentation(0) + "if (SVM_Support==2) {\n" +
		                addIndentation(1) + 
				"SVM_Map(index_data, sizeof(int)*9);\n" +
				addIndentation(0) + "}\n"; 
	}

    } else if (DataTran == 1) {

	 if (index_data_int[2] || index_data_int[5] || index_data_int[8]) {
	    addSVM_index_host = addIndentation(0) +
		    "clEnqueueWriteBuffer(commands, index_data_dev, " +
		    "CL_TRUE, 0, sizeof(int)*9, index_data, 0, NULL, NULL);\n";
	 }

    }

    // For serial in HOST (i.e., NON-parallel/ble).
    // TODO: Optimize so as NOT to use allocatablesOCL if not needed at
    // a LATER step (bec. even if current step is serial later may be 
    // parallel.)
    var tmp_host_code = allocatablesOfStep + allocatablesOCL + 
	    		Struct_el_decl + 
	    		addSVM_index_host + "__SVM__" + 
	                collapsed_loop_vars + forstr + forstr2 + stmt + 
			loop_close;

    loop_close = ""; 
    for (var iv = 0; iv <  rangeExpr.exprArr.length - cur_glob; iv++) {

        loop_close += addIndentation( rangeExpr.exprArr.length-iv - 1) + 
		      "}\n";

    }

    if(OCL_steps_dataTran[CurStep-1].typeOfStep == 1 && sO.potOCLstep == 1){

        var stmt2=stmt.replace(/^\t+/mg, "\t");
	stmt2=stmt2.replace(/fun\_/mg, "ft_"); // TODO: Make sure it does not
					       // affect anything else. 
        // This won't be true if e.g., one of the ivars is NOT parallelizable.
        // In the general case we need to find the number of tabs and decrease 
	// by the number of parallelizable ones.

	// Find and replace all function calls from this __kernel called
	// and rename them with the __device suffix (since they need to be 
	// device functions).
	stmt2 = replaceCallsFromKernel(stmt2);

	// For the functions above, create SERIAL version and create the 
	// actual __device function that is called and place it in the 
	// CL_Funcs[] string.

	func_ret_decl = extractFunctionReturnDecl();

        CL_Funcs.push(kernelHead + " {\n" + ivars_all + func_ret_decl + 
		 let_grids + ivars_get_glob + titleDefs_per_step + forstr2 + 
		 stmt2 + loop_close + "}");
        //alert(CL_Funcs[CL_Funcs.length-1]);
	//
    }

    glob_work_size = glob_work_size.replace(/, +$/, "};\n");
    glob_work_size = glob_work_size.replace("[]", "[" + 
		     glob_work_size.split(",").length + "]");


    // Process kernel head to get kernel name and arguments to set.
    // alert(kernelHead);
    
    var tmpstep = CurStep - 1;

    var createKernel_call = "\n" + addIndentation(0) + "//Creating kernel " +
	    		    "(kernel function code generated in .cl " +
			    "file from parallel loop's body).\n";
    createKernel_call += addIndentation(0) + 
	                    "kernel_cl = clCreateKernel(" + "program, " + 
			    "\"" + var2OCL(fO.funcCallExpr.str) + "_" + 
			    CurStep + "_dev\"" + " , &error);\n" +
		      	    addIndentation(0) + 
		      	    "assert(error == CL_SUCCESS);\n\n";

    var kernel_call = "\n" + addIndentation(0) + 
	    	      "//Enqueuing OpenCL kernel for " + 
	    	      "execution.\n";
    kernel_call += addIndentation(0) + 
	              "clEnqueueNDRangeKernel(commands, " + "kernel_cl, " + 
		      glob_work_size.split(",").length +
		      ", NULL, globalWorkSize_s" + tmpstep + 
		      ", NULL, 0, NULL, NULL);\n";	    

    var barrier_call = addIndentation(0) + "clFinish(commands);\n";

    var arguments_set;

    arguments_set = "\n" + addIndentation(0) + "//Setting kernel arguments " +
     		    "(based on parallel step's input, output grids).\n" + 
	    	    getOCLargCalls(ocl_step_grid_info, index_data_int);


    if (OCL_steps_dataTran[CurStep-1].typeOfStep == 1 && sO.potOCLstep == 1) {
	
        // In case we don't have an FPGA (assume fine-grained system support)
        // we need to unmap the index_data variable before a device step.
        var addSVM_index_dev = "";
        if (DataTran == 0 && Device_type != DevT.FPGA) {

            if (index_data_int[2] || index_data_int[5] || index_data_int[8]) {

                addSVM_index_dev = addIndentation(0) + 
			           "if (SVM_Support==2) {\n" +
		                   addIndentation(1) + 
				   "SVM_Unmap(index_data);\n" +
				   addIndentation(0) + "}\n"; 

	    }

        } else if (DataTran == 1) {


            if (index_data_int[2] || index_data_int[5] || index_data_int[8]) {

		addSVM_index_dev = addIndentation(0) + 
			           "clEnqueueWriteBuffer(commands, " +
				   "index_data_dev, CL_TRUE, 0, " + 
				   "sizeof(int)*9, (void *)index_data, 0," +
				   "NULL, NULL);\n"; 

	    }

	}

        code[1] = allocatablesOfStep + allocatablesOCL + Struct_el_decl + 
		  collapsed_loop_vars +
		  index_data_set + glob_work_size +
		  createKernel_call + addSVM_index_dev + 
		  "__SVM__" + arguments_set + 
	    	  kernel_call + barrier_call;

	//alert(allocatablesOCL);

    } else {

	//TODO: Move tmp_host_code here, as well as addSVM_index_host
	//	(the latter, unify with addSVM_index_dev).
	code[1] = tmp_host_code;

    }

    // Save sizes in case data transfers are needed.
    for (var i = 0; i < ocl_step_grid_info.length; i++) {

	var name = ocl_step_grid_info[i].name;
	var size = ocl_step_grid_info[i].size;
	OCL_steps_dataTran[CurStep-1].gridsSizes[name] = size;
	// If read-only in step then mark as such:
	if (OCL_steps_dataTran[CurStep-1].gridsInStep[name] == null)
		OCL_steps_dataTran[CurStep-1].gridsInStep[name] = 0;
    }

    return (code);

}




//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// PART C: Utility Functions
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------


//----------------------------------------------------------------------------
// Expands a struct as separate elements (data flattening) for use in the
// header of a kernel function.
//----------------------------------------------------------------------------
function expandKernelHeadStruct(ocl_step_grid_info, gO) {

    var kernelHeadArgs = "";
    var typeVariable = "typvar_" + gO.caption;
    var tmp_length = gO.dimDynSize[gO.typesInDim];
    tmp_length = (tmp_length) ? tmp_length : gO.dimActSize[gO.typesInDim];

    // TODO:C: This should NOT be allowed by GUI.
    if(isNaN(tmp_length)) 
        alert("Number of elements in a struct cannot be dynamic.");

    // In OpenCL we need this form.
    if (Soa) {
        
	for (var i = 0; i < tmp_length; i++) {

	    // Get dimensions of struct elements, so as to allocate in 
	    // function.
	    var tmp_alloc_str = getDimensionString_OCL(gO);
	    tmp_alloc_str = tmp_alloc_str.replace("][", "*"); 
	    tmp_alloc_str = tmp_alloc_str.replace(/^\[|\]$/g, "");
	    if (tmp_alloc_str == "") tmp_alloc_str = 1; 

	    ocl_step_grid_info.push(new OCL_grid_info(gO, typeVariable + 
			"_dim" + i, 
			"sizeof(" + dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
			")*" + tmp_alloc_str));

	    kernelHeadArgs += "__global " + 
		              dataTypeIntToStr_OCL(gO.dataTypes[i]) +
		              "* restrict " + var2OCL(typeVariable + "_dim" +
			      i) + ", ";

	    // CAUTION: The code for isNonScalar starts from tmp_length+1 and 
	    // goes down to 2. This is done so that in setting the kernel
	    // arguments (getOCLargCalls())we now when to add dynamic size 
	    // (IF dynamic size is used for dimX's. General simple non-scalar 
	    // grids have isNonScalar == 1 and scalar ones have 
	    // isNonScalar == 0
	    ocl_step_grid_info[ocl_step_grid_info.length-1].isNonScalar = 
		    tmp_length + 1 - i;
        
	}

    }

    return kernelHeadArgs;

}


//----------------------------------------------------------------------------
// Expands a struct (non-scalar grid with types and titles in one
// dimension) to its constituent elements.
// type: If used to expand for header for called argument.
//       0: Caller (calling host side function).
//       1: Callee (function header) for host function.
//       2: Callee (function heder) for kernel (device side).
//       * Caller for kernel is via setting kernel arguments.
//----------------------------------------------------------------------------
function expandStruct(gO, type) {

    var retStr = "";
    var retStrDev = "";

    for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

	if (type == 0) {

	    retStr += var2OCL("typvar_" + gO.caption + "_dim" + i) + ", ";

	    if (DataTran == 1) {

		retStrDev += "&" + var2OCL("typvar_" + gO.caption + "_dim" + 
			     i + "_dev, ");

	    }

	} else if (type == 1) {    

	    retStr += dataTypeIntToStr_OCL(gO.dataTypes[i]) + "* " +
		      var2OCL("typvar_" + gO.caption + "_dim" + i) + ", ";

	    if (DataTran == 1) {

		retStrDev += "cl_mem* " + var2OCL("typvar_" + gO.caption +
			     "_dim" + i + "_dev, ");

	    }

	} else if (type == 2) {

	    retStr += "__global " + dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
		      "* " + var2OCL("typvar_" + gO.caption + "_dim" + i) + 
		      ", "; 

	}

    }

    retStr = retStr.replace(/, $/, "");

    return retStrDev + retStr;

}


//----------------------------------------------------------------------------
// Replaces instances of function calls that have been recorded
// for a step in CalledFuncsFromStep[] with the corresponding __device
// suffix function call (since called from a __kernel context, functions
// called need to be of __device type in OpenCL).
// Also, removes the &<cl_mem ptr> arguments from function calls (which
// is needed when calling host from host function, but NOT when we are
// calling a function from within a kernel.
//----------------------------------------------------------------------------
function replaceCallsFromKernel(old_stmt) {

    for (var i=0; i < CalledFuncsFromStep.length; i++) {

	// Find this function call and (globally) replace it in old_stmt
	var repl_str_regex = new RegExp(CalledFuncsFromStep[i], "g");
	old_stmt = old_stmt.replace(repl_str_regex, CalledFuncsFromStep[i] + 
		   "_device");

	// Remove &<cl_mem_ptr> from function call (covers simple + structs).
	old_stmt = old_stmt.replace(/\&.*?_dev, /mg, "");
	//old_stmt = old_stmt.replace(/\&.*?, /, "");
						    
    }

    return old_stmt;

}


//----------------------------------------------------------------------------
// Remove '&' from &<cl_mem_ptr> from function call (covers simple + structs).
// For these cases where the argument has been passed in the callee 
// function as an parameter and has not been declared in the function
// itself (so cl_mem var is ALREADY a pointer and doesn't need a '&').
// It scans the function header of the step and removes '&' for every cl_mem*
// variable found in it.
//----------------------------------------------------------------------------
function replaceClArgsInFuncCall(func_head, old_stmt) {

    var matches = [];
    var match;
    var str = func_head; 
    var myRegexp = /cl\_mem\* (.*?)[,\)]/g;

    while (match = myRegexp.exec(str)) {

        //alert(match[1]);
        matches.push(match[1]);

    }
    // In matches we have all the cl_mem* parameters of the function.

    for (i=0; i<matches.length; i++) {

	var regex = new RegExp("&" + matches[i], "mg");
        old_stmt = old_stmt.replace(regex, matches[i]);

    }

    return old_stmt;

}


//----------------------------------------------------------------------------
// Finds function(s) called from within step/kernel and records appropriate
// variables in which we "save" the return value from the function (in the 
// form res_<func>_dev.
//----------------------------------------------------------------------------
function extractFunctionReturnDecl() {

    var func_ret_decl = "";
    var mO = CurModObj;

    for (var i=0; i < CalledFuncsFromStep.length; i++) {

	var fO = getFuncIdByName(mO, CalledFuncsFromStep[i]);
	var func_ret_type = getDataTypeString_OCL(mO.allFuncs[fO].allGrids[0],
		            null);
	func_ret_decl += addIndentation(0) + func_ret_type + " res_" + 
			 CalledFuncsFromStep[i] + "_device;\n";
	
    }

    return func_ret_decl;

}


//----------------------------------------------------------------------------
// Print all information in current function's OCL_steps_dataTran array.
//----------------------------------------------------------------------------
function printOCL_steps_dataTran() {

    var str = "";

    for (var i = 0; i < OCL_steps_dataTran.length; i++) {

	str += "TYPE=" + OCL_steps_dataTran[i].typeOfStep + "\n";

	for(var index in OCL_steps_dataTran[i].gridsInStep) {

	    str += "VARNAME=" + index + " value=" + 
		    OCL_steps_dataTran[i].gridsInStep[index] + "\n";

	}

    }

    alert(str);

}


//----------------------------------------------------------------------------
// Adding SVM Map/Unmap calls, replacing __SVM__ placeholder text in step's
// code.
//----------------------------------------------------------------------------
function addSVMcalls(fO, curstep, stepCode) {

    var type = OCL_steps_dataTran[curstep-1].typeOfStep;  
    var flag = 0;

    // If index_data is passed, then "if (SVM_Support==2)... has been set
    // so doesn't need to be set again.
    var index_present = stepCode.indexOf(/SVM_.+ap(index_data)/g);

    for (var g = 0; g < fO.allSteps[curstep].allGridIds.length; g++) {

        var gId = fO.allSteps[curstep].allGridIds[g];
        var gO = fO.allGrids[gId];

	var sz_info = OCL_steps_dataTran[curstep-1].gridsSizes[gO.caption]


	// Only relevant for non-scalar grids.
	if (gO.numDims > 0) {

	    if (!flag && index_present == -1) {

		stepCode = stepCode.replace("__SVM__", addIndentation(0) +
			   "if (SVM_Support==2) {\n" + "__SVM__");
		flag = 1;

	    }


	    // If gO.typesInDim != -1, we need to flatten all struct elements
	    // for mapping/unmapping.
	    // If step is serial or parallel but running on host.
	    if (type == 0 || 
	       (type == 1 && fO.allSteps[curstep].potOCLstep == 0)) {

		if (gO.typesInDim == -1) {
		    
		    stepCode = stepCode.replace("__SVM__", addIndentation(0) +
			   "SVM_Map(" + var2OCL(gO.caption) + ", " + 
			   sz_info + ");\n" + "__SVM__");

		} else {

		    for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {
			
			// Get dimensions of struct elements, so as to 
			// allocate in function.
	   		var tmp_alloc_str = getDimensionString_OCL(gO);
	   		tmp_alloc_str = tmp_alloc_str.replace("][", "*"); 
	   		tmp_alloc_str = tmp_alloc_str.replace(/^\[|\]$/g, "");
	   		if (tmp_alloc_str == "") tmp_alloc_str = 1;

			stepCode = stepCode.replace("__SVM__", 
				   addIndentation(0) + "SVM_Map(" +  
				   var2OCL("typvar_" + gO.caption + "_dim" + 
				   i) + ", " + "sizeof(" + 
				   dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
				   ")*" + tmp_alloc_str + ");\n" + "__SVM__");

		    }

		}

	    } else {

		if (gO.typesInDim == -1) {

		    stepCode = stepCode.replace("__SVM__", addIndentation(1) +
			       "SVM_Unmap(" + var2OCL(gO.caption) + ");\n" + 
			       "__SVM__");

		} else {

		    for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

		        stepCode = stepCode.replace("__SVM__", 
				   addIndentation(0) + "SVM_Unmap(" + 
				   var2OCL("typvar_" + gO.caption + "_dim" + 
			 	   i) + ");\n" + "__SVM__");	    

		    }

		}

	    }

	}

    }


    if (flag || index_present != -1) {

        stepCode = stepCode.replace("__SVM__", addIndentation(0) + "}\n" + 
		   "__SVM__");

    }    

    return stepCode.replace("__SVM__", "");

}


//----------------------------------------------------------------------------
// Adding data transfer calls, replacing __SVM__ placeholder text in step's
// code.
//----------------------------------------------------------------------------
function addDataTranCalls(fO, curstep, stepCode) {

    var type = OCL_steps_dataTran[curstep-1].typeOfStep;  

    // If index_data is passed, then "if (SVM_Support==2)... has been set
    // so doesn't need to be set again.
    var index_present = stepCode.indexOf(/SVM_.+ap(index_data)/g);

    for (var g = 0; g < fO.allSteps[curstep].allGridIds.length; g++) {

        var gId = fO.allSteps[curstep].allGridIds[g];
        var gO = fO.allGrids[gId];

	var sz_info = OCL_steps_dataTran[curstep-1].gridsSizes[gO.caption]


	// Only relevant for non-scalar grids.
	if (gO.numDims > 0) {


	    // If gO.typesInDim != -1, we need to flatten all struct elements
	    // for mapping/unmapping.
	    // If step is serial or parallel but running on host.
	    if (type == 0 || 
	       (type == 1 && fO.allSteps[curstep].potOCLstep == 0)) {

		if (gO.typesInDim == -1) {
		    
		    stepCode = stepCode.replace("__SVM__",
			   addIndentation(0) +
			   "d2h_tran(" + var2OCL(gO.caption) + ", " + 
			   sz_info + ");\n" + "__SVM__");

		} else {

		    for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {
			
			// Get dimensions of struct elements, so as to 
			// allocate in function.
	   		var tmp_alloc_str = getDimensionString_OCL(gO);
	   		tmp_alloc_str = tmp_alloc_str.replace("][", "*"); 
	   		tmp_alloc_str = tmp_alloc_str.replace(/^\[|\]$/g, "");
	   		if (tmp_alloc_str == "") tmp_alloc_str = 1;

			stepCode = stepCode.replace("__SVM__", 
				   addIndentation(0) +
				   "d2h_tran(" +  
				   var2OCL("typvar_" + gO.caption + "_dim" + 
				   i) + ", " + "sizeof(" + 
				   dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
				   ")*" + tmp_alloc_str + ");\n" + "__SVM__");

		    }

		}

	    } else {

		if (gO.typesInDim == -1) {

		    stepCode = stepCode.replace("__SVM__",
			       addIndentation(0) +
			       "h2d_tran(" + var2OCL(gO.caption) + ", " + 
			       sz_info + ");\n" + 
			       "__SVM__");

		} else {

		    for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

			// Get dimensions of struct elements, so as to 
			// allocate in function.
	   		var tmp_alloc_str = getDimensionString_OCL(gO);
	   		tmp_alloc_str = tmp_alloc_str.replace("][", "*"); 
	   		tmp_alloc_str = tmp_alloc_str.replace(/^\[|\]$/g, "");
	   		if (tmp_alloc_str == "") tmp_alloc_str = 1;

		        stepCode = stepCode.replace("__SVM__", 
				   addIndentation(0) +
				   "h2d_tran(" + 
				   var2OCL("typvar_" + gO.caption + "_dim" + 
			 	   i) +  ", " + "sizeof(" + 
				   dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
				   ")*" + tmp_alloc_str + ");\n" + "__SVM__");	    

		    }

		}

	    }

	}

    }


    if (index_present != -1) {

        stepCode = stepCode.replace("__SVM__", addIndentation(0) + "}\n" + 
		   "__SVM__");

    }    

    return stepCode.replace("__SVM__", "");

}


//----------------------------------------------------------------------------
// Check all steps going backwards and checking only steps of *different* type 
// (i.e., if current is of device type, check of host type). On the first one 
// found, say X, check for all non-scalar grids of current step if they exist 
// in X's grid array. If they do and *are* indicated as written, they need to 
// be transfered. If so, mark them at X as 0, since we now have the current 
// valid version (if written in current step they'll be noted accordingly 
// there). If they are not written, then we have the current valid version 
// and *no* transfer is needed, nor searching backwards in previous step. 
// If they do *not* exist, continue searching in steps X-1, etc.
// TODO: If in current step *only (or *first*) written, then no need to
// transfer (but such code wouldn't make sense in the first place).
//----------------------------------------------------------------------------
function findIfWritten(curstep, codesofar) {

    var type = OCL_steps_dataTran[curstep-1].typeOfStep; 
    // Counting starts from 0.
    var basicStr = "clEnqueue__TYPE__Buffer(commands, __DEVNAME__, CL_TRUE," +
	           " 0, __SIZE__, (void*)__HOSTNAME__, 0, NULL, NULL);\n";

    var changed = 0;

    for (var index in OCL_steps_dataTran[curstep-1].gridsInStep) {

	//alert("Checking: " + index);

	for (var i = curstep - 2; i >= 0; i--) {

	    if (OCL_steps_dataTran[i].typeOfStep != type) {


		if (OCL_steps_dataTran[i].gridsInStep[index] != null) {

		    if (OCL_steps_dataTran[i].gridsInStep[index] == 1) {

		        //alert("D2H or H2D");
			codesofar = codesofar.replace("__SVM__", basicStr);

			if (type == 0) {

		 	    codesofar = codesofar.replace("__TYPE__", "Read");

			} else {

			    codesofar = codesofar.replace("__TYPE__", 
					"Write");

			}

			codesofar = codesofar.replace("__DEVNAME__",
				    var2OCL(index+"_dev"));

			codesofar = codesofar.replace("__HOSTNAME__",
				    var2OCL(index));

			codesofar = codesofar.replace("__SIZE__",
				    OCL_steps_dataTran[i].gridsSizes[index]);
		
			changed = 1;

			OCL_steps_dataTran[i].gridsInStep[index] == 0;

		    }

		    break;

		}

	    }

	}

    }

    if (changed == 1) {

        return codesofar; // New copy includes data copies.

    } else {

	return codesofar.replace("__SVM__", ""); 
	// If none, remove __SVM__

    }

}


//----------------------------------------------------------------------------
// Returns the string containing all setting argument calls for a step's 
// kernel.
//----------------------------------------------------------------------------
function getOCLargCalls(step_grid_list, index_data_int) {

    var returnedString = "";

    // Basic call, in which we will repetitevely replace the appropriate parts
    // with information from the step_grid_list list.
    var basicString = addIndentation(0) + "__TYPESETARG__(kernel_cl, " +
	    	      "__I__, __SIZE__, __NAME__);\n";

    var argNum = 0;

    var arr_dynValues = new Array();

    for (var i = 0; i < step_grid_list.length; i++) {

        var name = step_grid_list[i].name;
	var size = step_grid_list[i].size;
	var isNonScalar = step_grid_list[i].isNonScalar;

	returnedString += basicString.replace("__I__", argNum++);
		
	if (isNonScalar > 0) {

	    if (DataTran == 0) {

		returnedString = returnedString.replace("__NAME__", 
			         var2OCL(name));

	        // Size not needed in SVM pointer args.
	        returnedString = returnedString.replace("__SIZE__,", "");

		returnedString = returnedString.replace("__TYPESETARG__", 
			     "clSetKernelArgSVMPointer");

	    } else {

		// If passed as parameter in function (cl_mem pointer) do 
	        // not use "&" (since it is already pointer).
	        if (CLMEMSinFunc.indexOf(name) == -1) {

		    returnedString = returnedString.replace("__NAME__", 
				     "&" + var2OCL(name + "_dev"));

	        } else {

		    returnedString = returnedString.replace("__NAME__", 
				     var2OCL(name + "_dev"));

	        }

		returnedString = returnedString.replace("__SIZE__", 
				 "sizeof(cl_mem)");

		returnedString = returnedString.replace("__TYPESETARG__", 
			     "clSetKernelArg");

	    }

	    

	    // The below condition restricts checking for dynamic values, ONLY
	    // for "simple" non-scalar grids, or for structs ONLY AFTER the
	    // last (flattened) struct element (to align with the way the
	    // kernel header is built).
	    if (isNonScalar <= 2) {
	        
		// TODO: HERE we need the gO, get all dimension variables,
		// dynArr will be outside the loop so each is used ONLY once.
	        // For dynamically allocated pointers we need to pass the 
	        // dimensions in the kernel in case used in called kernel.
	        // **IF** it is dynamic if not do nothing.
	        var dynArgsList = getDynValArgs("", step_grid_list[i].gO, 
		  	          arr_dynValues);

	        // Split on commas, remove "int", count and loop on it later.
	        dynArgsList = dynArgsList.split(", int ");
	        if (dynArgsList.length != 0) {

		    for (var k = 1; k < dynArgsList.length; k++) {

	    	        returnedString += basicString.replace("__NAME__", 
				      "(void* )&" + dynArgsList[k]);
	    	        returnedString = returnedString.replace("__I__", 
				     argNum++);
	    	        returnedString = returnedString.replace("__SIZE__", 
				     "sizeof(int)");
	    	        returnedString =returnedString.replace("__TYPESETARG__",
			             "clSetKernelArg");

		    }

	        }

	    }

	} else {
	
		var t_sz = var2OCL(name);
		if (arr_dynValues.indexOf(t_sz) == -1) {
                    	    
		    arr_dynValues.push(t_sz);

		    returnedString = returnedString.replace("__NAME__", 
		  	             "(void* )&" + 
				     var2OCL(name));

	    	    returnedString = returnedString.replace("__SIZE__", size);
		    returnedString = returnedString.replace("__TYPESETARG__", 
			             "clSetKernelArg");

		
		}
		
	}

    }


    // Check if for any of the index vars start, or step is different than
    // the default (i.e., step!=1, and, start!=0). This has been stored in
    // positions index_data_int[2], [5], [8]. A value of 0 means that step,
    // start are the default for the index var, 1 means at least one is 
    // different. If no loop, all would be initialized to 0.
    if (index_data_int[2] || index_data_int[5] || index_data_int[8]) {

	 if (DataTran == 0) {

             returnedString += addIndentation(0) + 
		               "clSetKernelArgSVMPointer(" +
   	   	               "kernel_cl, " + argNum + ", index_data);\n";

	 } else {

	     returnedString += addIndentation(0) + "clSetKernelArg(" +
   	   	               "kernel_cl, " + argNum + ", sizeof(cl_mem)" +
			       ", &index_data_dev);\n";

	 }

    }

    return returnedString;

}


//----------------------------------------------------------------------------
// Returns the appropriate code for library functions, in C/OpenCL.
// TODO: No type or similar checking performed.
// TODO: CAUTION: ROUND is not supported.
//----------------------------------------------------------------------------
function processLibFunctions_OCL(e) {

    var pre = "";
    var ret = "";
    var post = "";
    var sep = ", "

    for (var i = 0; i < e.exprArr.length; i++) {

        if (i > 0) ret += sep;
        ret += expr2OCLstring(e.exprArr[i]);

    }

    post = ")";

    if (e.str == "Math.mod") { //TODO:C:

        pre = "(";
	ret = expr2OCLstring(e.exprArr[0]) + "%" + 
	      expr2OCLstring(e.exprArr[1]);
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
        ret = expr2OCLstring(e.exprArr[0]) + ", " +
              expr2OCLstring(e.exprArr[1]);
        post = ")";

    } else if (e.str == "System.runCommand") {

        pre = "system(";
        ret = "\'" + e.exprArr[0].str + "\'";

    } else if (e.str == "Math.random") { //TODO:C: seed?

        // TODO: Must be a REAL grid cell or a REAL grid name.
        pre = "rand("

    }

    if(e.str.indexOf("Math.") != -1 && e.str != "Math.mod" && 
	e.str != "Math.min" && e.str != "Math.max") {

        InclMath = 1;

    }

    return pre + ret + post;

}


//----------------------------------------------------------------------------
// Grid Language identifiers may not always be acceptable to OpenCL -- e.g.,
// reserved keywords. One strategy is to prefix every identifier with "ft_" 
// OpenCL/C variable names must start with a letter.
//----------------------------------------------------------------------------
function var2OCL(str) {

    return "ft_" + str;

}


//----------------------------------------------------------------------------
// Method called to get OpenCL/C code for an expression.
//----------------------------------------------------------------------------
function expr2OCLstring(e) {

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
        
	// TODO: Implement NOT/AND/OR.

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
            ret = expandStruct(e.gO, 0); //"typvar_" + e.gO.caption;

        } else {

	    // TODO: This is to pass a cl_mem pointer together with any
	    // non-scalar grid references in a function call, so if used
	    // within a parallel step on the callee to have the info.
	    // Right now we are NOT checking whether the grid references
	    // passed are INDEED used in a parallel (i.e., kernel) step.
	    // Do this as an optimization, so as NOT to pass cl_mem 
	    // pointers
	    // if they are NOT going to be needed.
	    if (DataTran == 1) {

		if (e.isGridRef() && e.gO != null & e.gO.numDims >= 1) {

		    // For simple grid non-scalar variables (no struct).
		    ret += "&" + var2OCL(e.str) + "_dev, ";

	        }

	    }

            ret += var2OCL(e.str);

        }

    } else if (e.isGridCell()) { // Any non-scalar grid

        // If the grid has multiple data types we will need to generate
	// special code than for "typical" grids with a global (single) type.
        if (e.gO.typesInDim != -1 /*&& e.gO.numDims > 1*/) {

            pre = var2OCL("typvar_" + e.gO.caption);
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

                pre += "_dim" + dimChosen + "[";

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

		    ret += expr2OCLstring(e.exprArr[i]);

		    for (var j=i-1; j>=0; j--) {

			if (j!=e.gO.typesInDim) {

			    var dim = (e.gO.dimDynSize[j]) ? 
	               	               var2OCL(e.gO.dimDynSize[j]) : 
				       e.gO.dimActSize[j];

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

            pre = var2OCL(e.gO.caption);

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
                tmp_expr_indices[i] = expr2OCLstring(e.exprArr[i]);
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
	               	        var2OCL(tmp_dyn_size[j]) : tmp_act_size[j];

			    ret += "*" + dim;

			}

		    }

		    if (e.gO.numDims != 1 && i != 0) ret += " + ";

		}

            } 

            if (lastd != -1) ret += "]";
             
        }

	// The code below takes care of the corner case of 1D structs, where 
	// the normal rules above do not apply.
        if (e.gO.numDims == 1 && e.gO.typesInDim != -1) {
            pre = pre.replace("[", "");
	    if (Soa) {

	        ret = ""; // In case of 1D struct.

	    } else {

		ret = ret.substring(ret.indexOf("."));

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

                ret = processLibFunctions_OCL(e);

            }
            // TODO: Here, put code for handling other LIBRARY function calls.

        } else if (e.isUserFuncCall()) {

            pre = var2OCL(e.str);
            pre += "(";
            post = ")";
            sep = ", ";

	    // Record in per step called functions (does not contain 
	    // ft_ prefix).
   	    // Add only once per step (i.e., do not add duplicates).
	    if (CalledFuncsFromStep.indexOf(e.str) == -1)
	        CalledFuncsFromStep.push(e.str); 
	    if (CalledFuncsFromKernels.indexOf(e.str) == -1)
	        CalledFuncsFromKernels.push(e.str);								

	    var arr_dynValues = new Array();

            for (var i = 0; i < e.exprArr.length; i++) {

	        var cur_gO = e.exprArr[i].gO;
		var dynValues = ",";
				
		// If it is a non-scalar grid argument.
		if(cur_gO != null && cur_gO.numDims > 0) {

		    // Loop through all dimensions and record variable ones.
		    for (var j = 0; j < cur_gO.dimActSize.length; j++) {

		        if(cur_gO.dimDynSize[j] != null) {

			    var t_sz = var2OCL(cur_gO.dimDynSize[j]);

			    if(arr_dynValues.indexOf(t_sz) == -1) {

			        arr_dynValues.push(t_sz);
				dynValues += t_sz + ",";

			    }

			}

		    }

		}

                if (i > 0) ret += sep;

		if (dynValues != ",") {

		    dynValues = dynValues.replace(/,+$/, "");
		    ret += expr2OCLstring(e.exprArr[i]) + dynValues;

                } else {
               	 
                    // Do not print if scalar gO that has been 
		    // implicitly used in dynamic non-scalar grid.
		    var t_sz;
		    if (cur_gO != null) t_sz = var2OCL(cur_gO.caption);
		    if (cur_gO != null && cur_gO.numDims == 0 && 
			    arr_dynValues.indexOf(t_sz) == -1) {
                    	    
			    ret += expr2OCLstring(e.exprArr[i]);
			    arr_dynValues.push(t_sz);	
		
		    } else if (cur_gO!= null && cur_gO.numDims == 0 &&
			       arr_dynValues.indexOf(t_sz) != -1) {

			// Has been declared, do nothing.
			// Trim last comma (unneeded).
			ret = ret.replace(/, $/, "");

		    } else {

			ret += expr2OCLstring(e.exprArr[i]);

		    }
                
                }

	    }

        } else {

            sep = " ";

            for (var i = 0; i < e.exprArr.length; i++) {

                if (i > 0) ret += sep;
                ret += expr2OCLstring(e.exprArr[i]);

            }
        }

    }

    return pre + ret + post;

}


//----------------------------------------------------------------------------
// Returns integer code for string of data type.
// TODO: When supporting all OpenCL/C types, revise this with new types
//----------------------------------------------------------------------------
function dataTypeStrToInt_OCL(dt_string) {

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
    else
        alert("TYPE NOT YET SUPPORTED");

}


//----------------------------------------------------------------------------
// Returns data type string for given integer code.
// TODO: When supporting all OpenCL/C types, revise this with new types
//----------------------------------------------------------------------------
function dataTypeIntToStr_OCL(typecode) {

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
	    return "float";
            //TODO:C: Support it.
            break;
        case 6:
            //alert("Data type: const not supported yet"); //TODO:C:
	    return "INTEGER"; //TT TODO:CAUTION (how it interacts w/ rest).
            break;
        case 7:
            alert("Data type: func not supported yet"); //TODO:C:	
            break;
        default:
            alert("Invalid input as data type");

    }

}


//----------------------------------------------------------------------------
// Given we dynamically allocate all non-scalar grids, we need to pass 
// dimensions that are variables (the constant dimensions are auto-generated 
// as numbers in resulting code). They are also needed to be used in loops (in 
// Fortran we could get this info using SIZE(), but in OpenCL/C there is 
// nothing similar).
//----------------------------------------------------------------------------
function getDynValArgs(func_head, gO, arr_dynValues) {

    var dynamValArgs;
    var dynVals = ", ";
    
    for (var i = 0; i < gO.dimActSize.length; i++) {

        if (gO.dimDynSize[i] != null) {
	
	    if(arr_dynValues.indexOf(var2OCL(gO.dimDynSize[i]))==-1 &&
		func_head.indexOf(var2OCL(gO.dimDynSize[i]))==-1) {
		
	        arr_dynValues.push(var2OCL(gO.dimDynSize[i]));
		dynVals += "int "+var2OCL(gO.dimDynSize[i]) + ", ";
		            
	    }
			    
        }

    }

    if (dynVals != ", ") {
	
        dynamValArgs = dynVals.replace(/,\s$/, "");
		    
    } else {

	dynamValArgs = "";

    }

    return dynamValArgs;

}


//----------------------------------------------------------------------------
// Returns data type from TypesArray[] as string for a given grid.
// TODO: Special care is needed for multiple data types when titles
//       are present (when multiple data types for rows/columns).
// TODO: CAUTION: wrong for multidimensional, but correct. Only called from
// 	 where gO.typesInDIm == -1? If so, fix.
// TODO: Added parameter 'e' for when we have an expression and want to find
//  	 the data type from the appropriate dimension for multidimensional
//	 grids with multiple data types for the dimension which has types.
//	 If e = null AND gO.typesInDim == -1 then it is a scalar grid (e.g., 
//	 return value) OR non-scalar grid reference.
//	 Otherwise, it is EITHER a gridcell OR a typevar reference.
//----------------------------------------------------------------------------
function getDataTypeString_OCL(gO, e) {

    // Where to search for dataType (if global, search in position 0)
    var typeDim;
    if (gO.typesInDim == -1) {

        typeDim = 0;

    } else {

        if (e != null) {

            // Which index's value should we get the value from (e.g., d3Tab2)
            var indx = expr2OCLstring(e.exprArr[gO.typesInDim]);
            
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

            return findTypeVarType_OCL(gO, 1);

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
//       gO should be called with getDimensionString_OCL().
//----------------------------------------------------------------------------
function getDimensionString_OCL(gO) {

    //TODO: Be careful of dimDynSize.
    var modarr = gO.dimActSize.slice(0); 

    modarr[0] = gO.dimDynSize[ColDimId] ? 
	    var2OCL(gO.dimDynSize[ColDimId]) : gO.dimActSize[ColDimId];
    modarr[1] = gO.dimDynSize[RowDimId] ? 
	    var2OCL(gO.dimDynSize[RowDimId]) : gO.dimActSize[RowDimId];

    for (var i = modarr.length - 1; i >= 2; i--) {

        modarr[i] = gO.dimDynSize[i] ? var2OCL(gO.dimDynSize[i]) : gO.dimActSize[i];

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
function createTypeString_OCL(gO) {

    var gridDefinition = "";

    if (gO.numDims == 0) {

        gridDefinition = getDataTypeString_OCL(gO, null) + " " +
                         var2OCL(gO.caption) + ";\n";
        // For scalars no need to display DIMENSIONS().

    } else if (gO.numDims == 1 && gO.typesInDim == -1) {

        // 1D cannot have different data types, unless a struct.
        // This also applies to 1D (vectors),in which case will be: 
	// DIMENSIONS(1,X).
        // This is a grid AND is NOT an incoming arg. 
        gridDefinition = getDataTypeString_OCL(gO, null) + " *" + 
                         var2OCL(gO.caption) + getDimensionString_OCL(gO) + 
			 ";\n";

    } else { // gO.numDims > 1.

	// 'If' refers to the case where we have a TYPE.    
        if (gO.typesInDim != -1) {

            gridDefinition = findTypeVarType_OCL(gO, 0);

        } else {
	// 'Else' case refers to a "typical" grid.

            gridDefinition = getDataTypeString_OCL(gO, null) + " *" +
                             var2OCL(gO.caption) +  
			     getDimensionString_OCL(gO) + ";\n";

        }

    }

    return gridDefinition;

}


// ---------------------------------------------------------------------------
// Function used to return declaration of structs.
// If declType is 1, this means this is the first time we are declaring this
// type, so we need to append to TypeStr the declaration of the struct and  
// its body.
// ---------------------------------------------------------------------------
function findTypeVarType_OCL(gO, onlyDataType) {

    var gridDefinition = "";
    var typename = "";

    // Convention: variable named as <typvar_><grid name> 
    // (e.g., Out -> typvar_Out).
    var typeVariable = "typvar_" + gO.caption;

    var flag = 0;
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

	    var gridNam = var2OCL(typeVariable + "_dim" + i);
	    // For SoA, elements are dynamically allocated (since
	    // stack may not be big enough, and to allow passing
	    // of pointers when passign a struct as a parameter).

	    //var ind = "";
	    //if (i != 0) ind = addIndentation(0);

            tmp_type_body += addIndentation(0) + 
		    	     dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
		    	     "* " + gridNam + ";\n";
            
	    // Get dimensions of struct elements, so as to allocate in 
	    // function.
	    var tmp_alloc_str = getDimensionString_OCL(gO);
	    tmp_alloc_str = tmp_alloc_str.replace("][", "*"); 
	    tmp_alloc_str = tmp_alloc_str.replace(/^\[|\]$/g, "");

	    if (tmp_alloc_str == "") tmp_alloc_str = 1;
	
	    if ((gO.inArgNum < 0) && (!gO.isRetVal)) { 

		if (GridsInFuncOCL.indexOf(gO) == -1) {

		    flag = 1;

		    if (DataTran == 0) {

	                Struct_el_decl += addIndentation(0) + gridNam + 
		            " = " + "(" + 
			    dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
			    " *)" + 
		            "clSVMAlloc(context, CL_MEM_READ_WRITE, " + 
		            "sizeof(" + dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
		            ")*" + tmp_alloc_str + ", 0);\n";

		    } else {

			Struct_el_decl += addIndentation(0) + gridNam + 
		            " = " + "(" + 
			    dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
			    " *)" + 
		            "malloc(sizeof(" + 
			    dataTypeIntToStr_OCL(gO.dataTypes[i]) + 
		            ")*" + tmp_alloc_str + ");\n";

			Struct_el_decl += addIndentation(0) + gridNam + 
			    		 "_dev = clCreateBuffer(" +
	    				 "context, CL_MEM_READ_WRITE, " +
					 "sizeof(" + 
					 dataTypeIntToStr_OCL(gO.dataTypes[i]) +
		            		  ")*" + tmp_alloc_str +
					  ", NULL, &error);\n";


		    }

		}

	    }
        
	}

	// If declared and allocated memory for struct elements, merge all
	// addNewAddress() for struct elements altogether.
	if (flag) {

	    if (Device_type != DevT.FPGA && DataTran == 0) {

	        Struct_el_decl += addIndentation(0) + 
			          "if (SVM_Support==2) {\n";

	        for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

	            Struct_el_decl += addIndentation(0) + "addNewAddress(" + 
		                      var2OCL(typeVariable + "_dim" + i) + 
				      ");\n"; 

	        }

	        Struct_el_decl += addIndentation(0) + "}\n";

	    } else if (DataTran == 1) {

		for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

	            Struct_el_decl += addIndentation(0) + "addNewAddress(" + 
		                      var2OCL(typeVariable + "_dim" + i) +
				      ", &" + 
				      var2OCL(typeVariable + "_dim" + i + 
				      "_dev") + ");\n"; 

	        }

	    }
	    // Else fine-grained system SVM for FPGA.

	}


    } else {

        for (var i = 0; i < gO.dimActSize[gO.typesInDim]; i++) {

            tmp_type_body += dataTypeIntToStr_OCL(gO.dataTypes[i]) + " " +
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
	// naming scheme based on IDs)
        typename = gO.caption;

    }

    gridDefinition = tmp_type_body;
    
    return gridDefinition;

}

