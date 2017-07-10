#!/bin/sh

##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Feb 20, 2009            jelkins     - Initial creation

# build.sh: prepares build tools, performs initial build setup, and builds
#           the native code from the command line


set -o errexit

# returns a fully resolved path
get_full_path() {
    cd "$1"
    echo $(pwd)
}

script_dir=$(get_full_path $(dirname $0))

log_file=$(get_full_path $(dirname $0))/build.log

workspace_dir=$(get_full_path $script_dir/..)

eclipse_dir=""

build_options=""

configuration_list=""

# initialize local variables from command arguments
if [ "$#" = "0" ]
then
    cat << TO_HERE
    
    Builds the workspace using the given Eclipse.
    
    Usage: $(basename $0) [OPTION] eclipse_dir
    
    -w workspace_dir    The directory in which all projects are located.
                        defaults to: $workspace_dir
                        
    -b build options    Options to pass to the command line builder.
                        Currently the only build option is "clean"
                            
    -c configuration    Enable the given configurations for the target.  If a
                        a project has multiple matching configurations, the last
                        configuration is used.
                        
                        Example:
                        
                            -c Default -c Debug
    
TO_HERE
    exit 1
else
        
    while getopts "w:b:c:" flag
    do
        case $flag in
            w)
                workspace_dir=$OPTARG
                ;;
            b)
                build_options="-b $OPTARG"
                ;;
            c)
                configuration_list="$configuration_list -c $OPTARG"
                ;;
        esac
    done

    shift `expr $OPTIND - 1`

    eclipse_dir=$(get_full_path $1)
fi


echo "Using $eclipse_dir"
#exit

echo "Cleaning Eclipse plugins... "

rm -vf $eclipse_dir/plugins/com.raytheon*

if [[ "$build_options" =~ "clean" ]]
then
    echo "Cleaning workspace metadata..."
    rm -fr $workspace_dir/.metadata
fi

echo "Building Eclipse plugins ..."

launcher_jar=$(ls $eclipse_dir/plugins/org.eclipse.equinox.launcher_*.jar)

pde_buildfile=$(ls -d $eclipse_dir/plugins/org.eclipse.pde.build_*)/scripts/productBuild/productBuild.xml

build_properties_dir=$script_dir

build_dir=${script_dir}/generated_tools

java -jar $launcher_jar -application org.eclipse.ant.core.antRunner \
    -buildfile ${pde_buildfile} \
    -Dbuilder=$build_properties_dir \
    -DbaseLocation=$eclipse_dir \
    -DbuildDirectory=$build_dir \
    -Dbase=$eclipse_dir \

echo "Copying build plugins into Eclipse ..."

cp -vf $build_dir/tmp/eclipse/plugins/com.raytheon* $eclipse_dir/plugins
rm -fr $build_dir

echo "Building all projects in the workspace ..."

$eclipse_dir/eclipse -data $workspace_dir -nosplash \
     $build_options $configuration_list \
    -application com.raytheon.uf.nativelib.cimport.application \
    -vmargs -Dorg.eclipse.cdt.core.console=org.eclipse.cdt.core.systemConsole \
    -Xmx512m -XX:PermSize=128m \ | tee $log_file
    
error_code=$?

echo ""
echo "==== Build Complete"
echo "---- Status:"

egrep "^\*\*\*\* |] Error" $log_file

echo ""
echo "Build log saved to: $log_file"

exit $error_code
