<?php
// PHP file to save haskell code passed and execute it

// runghc location
$ghc = "/home/dmundra/ghc/live/bin/ghc";
$runghc = "/home/dmundra/ghc/live/bin/runghc";

// Filename and code passed when run.php is called
$fname = $_POST["fname"];
$module = $_POST["module"];
$code = $_POST["code"];

$code = str_replace("\\","",$code); // For unix only replace slashes that were generated by the code

if($code != "")	{

	// Create hs file with code
	$fp = fopen('' . $fname, 'w+');
	fwrite($fp, $code);
	fclose($fp);

	// Compile haskell file
	//$dummy = exec($ghc . ' --make ' . $fname . ' -o ' . $fname . '.exe 2>&1',$compile);

	// If the file is a module it will have no main so it will not be executed, it will be only saved as a file
	if($module == "false") {
		echo "<pre>Running...</pre>";

		$dummy = exec($runghc . ' ' . $fname . ' 2>&1',$output);

		echo "<pre>Output: <br>";

		for($i = 0; $i < count($output); $i++)	{
			echo $output[$i] . "<br>";
		}

		echo "</pre>";
	} else {
		echo "<pre>Module saved!</pre>";
	}

} else {
	echo "<pre>No workflow selected! </pre>";
}

?>
