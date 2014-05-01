#!/bin/sh

#===============================================================================
#  COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
#  ALL RIGHTS RESERVED, An Unpublished Work
# 
#  RAYTHEON PROPRIETARY
#  If the end user is not the U.S. Government or any agency thereof, use
#  or disclosure of data contained in this source code file is subject to
#  the proprietary restrictions set forth in the Master Rights File.
# 
#  U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
#  If the end user is the U.S. Government or any agency thereof, this source
#  code is provided to the U.S. Government with Government Purpose Rights.
#  Use or disclosure of data contained in this source code file is subject to
#  the "Government Purpose Rights" restriction in the Master Rights File.
# 
#  U.S. EXPORT CONTROLLED TECHNICAL DATA
#  Use or disclosure of data contained in this source code file is subject to
#  the export restrictions set forth in the Master Rights File.
#===============================================================================

# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Oct 7, 2008             jelkins     - Initial creation

script_dir="$(dirname "$(readlink -f ${BASH_SOURCE[0]})")"
cdbgen_dir="${script_dir}/cdbgen"

db_driver_class_name="org.postgresql.Driver"

# when changing the db_driver the connection_url must also be changed
# the connection_url is set below in the "run cdbgen" section

if [ ! $# == 1 ]; then
    echo "Usage: $0 project_dir"
    exit 1
fi

project_dir="$1"

find ${project_dir} -name "*_CDB.cfg" | while read config_file
do
    echo "loading: ${config_file}"
    . ${config_file};
    echo "- done"
    
    # Check output directory --------------------------------------------------
    
    # go into the config_file's directory so relative paths work
    pushd `dirname ${config_file}` > /dev/null
    
    echo "checking: ${output_dir}"
    if [ -d ${output_dir} ]; then
    
        echo "- directory exists, nothing else to do"
        popd > /dev/null
        continue
    
    else
    
        echo "- directory not found, creating directory"
        mkdir ${output_dir}
    
    fi
    
    # Find related table name file --------------------------------------------
    
    table_name_file="`basename ${config_file%.cfg}.txt`"
    
    echo "checking: ${table_name_file}"
    if [ -f ${table_name_file} ]; then
    
        echo "- found"
    
    else

        echo "- file not found, continuing anyway"
    
    fi
    
    # Run cdbgen --------------------------------------------------------------
    
    connection_url="jdbc:postgresql://${db_host}/${db_name}?user=${db_user}"
    
    echo "running: cdbgen"
    echo "- using connection_url: ${connection_url}"
    java -jar ${cdbgen_dir}/cdbgen.jar ${connection_url} ${table_name_file} \
        ${db_name} ${output_dir} ${db_driver_class_name}
    echo "- done"
    
    # Move headers ------------------------------------------------------------
    
    echo "moving: *.h -> ${header_dir}"
    mv ${output_dir}/*.h ${header_dir}
    echo "- done"
         
    popd > /dev/null
    
done
