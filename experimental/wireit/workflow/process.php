<?php

$op = $_POST["op"];

echo "Closing HaskWool Server Instance...\n";

$dummy = exec('kill '. $op, $output1);

if(count($output1) != 0) {
	echo "<pre>Error: <br>";

	for($i = 0; $i < count($output1); $i++)	{
		echo $output1[$i] . "<br>";
	}

	echo "</pre>";
}

$dummy = exec("ps -A | grep ghc", $output2);
$ghc = substr($output2[0],0,5);
$dummy = exec('kill '. $ghc, $output3);

if(count($output3) != 0) {
	echo "<pre>Error: <br>";

	for($i = 0; $i < count($output3); $i++)	{
		echo $output3[$i] . "<br>";
	}

	echo "</pre>";
}

?>

<br><br>

<a href="hwserver/haskwool.log">Log File</a>
