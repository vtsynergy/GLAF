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

    $filenames = json_decode($_POST['fileNames']);
    $sourcecodes = json_decode($_POST['sourceCodes']);
    $folderpath = "./outputs/";
    for ($i=0; $i<count($filenames); $i++) {

	$folderpath_alt = "";
	$folderpath_alt = $folderpath.preg_replace("/\..+$/", "/", $filenames[$i]);
        $filename = $folderpath_alt.$filenames[$i];

	// TODO: Delete ALL PREVIOUS FILES/FOLDERS in here.
	// TODO: Be careful of permissions!
	if(!file_exists(dirname($filename)))
	    mkdir(dirname($filename),0777,true);
	$myfile = fopen($filename, "w") or die("Unable to open file");
	fwrite($myfile, $sourcecodes[$i]); 
	fclose($myfile);
	
    } 

?>
