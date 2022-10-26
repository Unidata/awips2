$path="/awips2/data_store/radar";

$syscmd = "find $path -name \"*\" -mmin +240 -delete";
`$syscmd`;
