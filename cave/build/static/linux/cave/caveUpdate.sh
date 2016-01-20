#!/bin/bash

# This script can be used to upgrade the currently installed CAVE features to the latest
# versions available in the repositories.

# This script currently expects one argument: the location of the Eclipse repository.

# Only the root user and the awips user are able to successfully run this script.
if [ "${USER}" != "root" -a "${USER}" != "awips" ]; then
    echo "ERROR: ${USER} does not have the required privileges to run ${0}."
    exit 1
fi

_awips_tmp_dir=/awips2/tmp
_cave_install_prefix=cave-upgrade-

# Common function to print usage information.
function usage() {
    echo "Usage:   ${0} <repository-location>"
    echo "Example: ${0} /path/to/repo-location/eclipse/v2.7"
    exit 1
}

# Verify that the expected arguments have been provided.
if [ $# -ne 1 ]; then
    echo "ERROR: Invalid arguments provided!"
    usage
fi

repository_loc=${1}

# Verify that the specified repository exists.
if [ ! -d ${repository_loc} ]; then
    echo "ERROR: The specified repository does not exist!"
    usage
fi

_unique_ident=`echo $$`
_staging_dir=${_awips_tmp_dir}/${_cave_install_prefix}${_unique_ident}
if [ -d ${_staging_dir} ]; then
    rm -rf ${_staging_dir}
    if [ $? -ne 0 ]; then
        echo "ERROR: Failed to remove the existing staging directory: ${_staging_dir}!"
        exit 1
    fi
fi 
mkdir -p ${_staging_dir}
if [ $? -ne 0 ]; then
    echo "ERROR: Failed to create the staging directory: ${_staging_dir}!"
    exit 1
fi
_output=${_staging_dir}/upgrades.txt

/awips2/java/bin/java -jar -DcaveInstall=/awips2/cave -DeclipseRepo=${repository_loc} \
   -DoutputFile=${_output} /awips2/cave/VizUpdater.jar
if [ $? -ne 0 ]; then
   echo "ERROR: The upgrade has failed!"
   exit 1
fi 

# Determine if any upgrades were found.
if [ ! -f ${_output} ]; then
   echo "INFO: There are no upgrades to apply."
   exit 0
fi

# Iterate through the upgrades and apply them.
for feature in `cat ${_output}`; do
   echo "Upgrading feature: ${feature} ..."
   /bin/bash /awips2/cave/caveInstall.sh "${repository_loc}" ${feature}
   if [ $? -ne 0 ]; then
      echo "Failed to upgrade feature: ${feature}!"
   fi
done

# Relocate localization files.
/bin/bash /awips2/cave/relocateLocalization.sh
if [ $? -ne 0 ]; then
   exit 1
fi

echo "INFO: Upgrades Finished."
exit 0