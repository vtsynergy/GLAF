<!---------------------------------------------------------------------------
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
----------------------------------------------------------------------------->

<?php
 //echo $_GET['code'];
 //echo $_GET['parallel'];
if($_GET['soa'] != 99){
 if($_GET['parallel']==0){
	if($_GET['soa']==0){
 		$filename = "./outputs/ser_aos/grid_src_ser_aos.f90";
	}else{
		$filename = "./outputs/ser_soa/grid_src_ser_soa.f90";
	}
 }else{
	if($_GET['soa']==0){
		$filename = "./outputs/par_aos/grid_src_par_aos.f90";
	}else{
		$filename = "./outputs/par_soa/grid_src_par_soa.f90";
	}
 }
}else{

	$filename = "./outputs/grid_autotune_script.sh";

}
 $myfile = fopen($filename, "w") or die("Unable to open file!");	
 //Make sure your php file is granted access to write-read on drive
 fwrite($myfile, $_GET['code']);
 fclose($myfile);
 //shell_exec('sh /usr/local/apache2/cgi-bin/grid_exec_local.sh');
 //sleep(2);
 shell_exec('perl /usr/local/apache2/cgi-bin/grid_exec_local.pl');
?>
