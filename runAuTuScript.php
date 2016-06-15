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

    $execNames = json_decode($_POST['runExScript']);
    $folderpath = "./outputs/";
    $resultsFileName = "results.html";
    $runExFileName = "runExhaustive.sh";

    $usrtime = "/usr/bin/time -f \"";
    $usrend = "\" -o ".$resultsFileName;
    $usrendap = "\" -a -o ".$resultsFileName;
    
    $fileName = $folderpath.$resultsFileName;
    $runExFilePtr = $folderpath.$runExFileName;
    
    $output = "";
    
    $myfile2 = fopen($runExFilePtr, "w") or die("Unable to open results file");

    // Write the exhaustive script that runs all compiled versions.
    for ($i=0; $i<count($execNames); $i++) {

	$output .= $usrtime;
	if ($i == 0) {

	    $output .= "<table><tr><th>Name</th><th>Time</th></tr><tr><td>".$execNames[$i]."</td><td>%e</td></tr>".$usrend;

	} elseif ($i == count($execNames)-1) {

	    $output .= "<tr><td>".$execNames[$i]."</td><td>%e</td></tr></table>".$usrendap;

	} else {

	    $output .= "<tr><td>".$execNames[$i]."</td><td>%e</td></tr>".$usrendap;

	}

	$output .= " ".$execNames[$i]."\n";

    }
    
    fwrite($myfile2, $output);

    // Calls the script to run the shell script written above.
    shell_exec('perl /usr/local/apache2/cgi-bin/runAuTuScript.pl');

    $myfile = fopen($fileName, "r") or die("Unable to open results file");
    // Prints the contents of the file (they are in HTML table format).
    while (!feof($myfile)) {
	echo fgets($myfile)."<br />";
    }

    fclose($myfile);
    fclose($myfile2);
?>
