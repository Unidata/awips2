#!/bin/bash

# calls into the main routines in the adappt.climate.ui library
# create links to this script with the appropriate name for each
# main routine

program_name=$(basename $0)

script_dir=$(cd $(dirname $0);pwd)

# set the appropriate environment variables
. $script_dir/set_climate_env.sh

# the climate apps assume the cwd is the data dir
cd $CLIMATE_DATA_DIR

# call into the library to launch the program
runso rary.adappt.climate.ui ${program_name}_main $@
