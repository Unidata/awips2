#!/bin/bash

script_directory=$(dirname "$(readlink -f ${BASH_SOURCE[0]})")
library_directory=$(cd $script_directory/../lib;pwd)
gluegen_jar=$library_directory/gluegen.jar

java -jar ${gluegen_jar} $@
