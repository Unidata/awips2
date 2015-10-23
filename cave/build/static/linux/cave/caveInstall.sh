#!/bin/bash

# This script can be used to install additional features into CAVE. This
# script will also handle the removal and installation of a feature if
# a feature is upgraded.

# This script currently expects two arguments: the location of the Eclipse repository
# and the id of the feature that should be installed.

# Only the root user and the awips user are able to successfully run this script.
if [ "${USER}" != "root" -a "${USER}" != "awips" ]; then
    echo "ERROR: ${USER} does not have the required privileges to run ${0}."
    exit 1
fi

_awips_tmp_dir=/awips2/tmp
_cave_install_prefix=cave-install-

# Common function to print usage information.
function usage() {
    echo "Usage:   ${0} <repository-location> <feature-id>"
    echo "Example: ${0} /path/to/repo-location/eclipse/v2.7 com.raytheon.uf.viz.base.feature"
    exit 1
}

# Verify that the expected arguments have been provided.
if [ $# -ne 2 ]; then
    echo "ERROR: Invalid arguments provided!"
    usage
fi

repository_loc=${1}
feature_id=${2}

# Verify that the specified repository exists.
if [ ! -d ${repository_loc} ]; then
    echo "ERROR: The specified repository does not exist!"
    usage
fi

_feature_zip=${repository_loc}/${feature_id}.zip

# Verify that a feature repository exists for the specified feature.
if [ ! -f ${_feature_zip} ]; then
    echo "ERROR: Unable to find a repository for the specified feature!"
    usage
fi

# Prepare to stage the repository.
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

unzip ${_feature_zip} -d ${_staging_dir} > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "ERROR: Failed to stage the repository: ${_feature_zip}!"
    exit 1  
fi

# Installation / Upgrade Constants.
CAVE_EXE="/awips2/cave/cave"
NOSPLASH_ARG="-nosplash"
DIRECTOR_APP="-application org.eclipse.equinox.p2.director"
DESTINATION_ARG="-destination /awips2/cave"
INSTALL_ARG="-i ${feature_id}.feature.group"
UNINSTALL_ARG="-u ${feature_id}.feature.group"
# Used to ensure that the awips2-java is used.
VM_ARG=/awips2/java/bin/java
REPO="-repository file:${_staging_dir}"

COMMON_CMD="${CAVE_EXE} -vm ${VM_ARG} ${NOSPLASH_ARG} ${DIRECTOR_APP} ${DESTINATION_ARG}"
INSTALL_CMD="${COMMON_CMD} ${INSTALL_ARG} ${REPO}"
UNINSTALL_CMD="${COMMON_CMD} ${UNINSTALL_ARG}"

# Determine if this is an upgrade. A version of the feature is currently
# already installed.
${UNINSTALL_CMD} -verifyOnly > /dev/null 2>&1
if [ $? -eq 0 ]; then
    # An existing feature is installed. Remove the existing for an upgrade.
    ${UNINSTALL_CMD}
    if [ $? -ne 0 ]; then
        echo "ERROR: Failed to remove feature: ${feature_id} during upgrade preparation!"
        exit 1
    fi
fi

# Install the new feature.
${INSTALL_CMD}
if [ $? -ne 0 ]; then
    echo "ERROR: Failed to install feature: ${feature_id}!"
    exit 1      
fi

rm -rf ${_staging_dir}
if [ $? -ne 0 ]; then
    echo "WARNING: Failed to purge the staging directory: ${_staging_dir}!"
    exit 1
fi

echo "Successfully installed feature: ${feature_id}."
exit 0
