%define _component_zip_file_name  %{_component_feature}-repo-linux.%{_build_arch}.zip
#
# awips2-cave Spec File
#
%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

Name: %{_component_name}
Summary: %{_component_name} Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: %{_component_name} = %{_component_version}
requires: %{_downstream_requires}
requires: awips2

%description
%{_component_desc}

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

if [ -d ${RPM_BUILD_ROOT} ]; then
   rm -rf ${RPM_BUILD_ROOT}
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/cave/.repository
CAVE_DIST_DIR="%{_baseline_workspace}/rpms/awips2.cave/setup/dist"

if [ ! -f ${CAVE_DIST_DIR}/%{_component_zip_file_name} ]; then
   echo "ERROR: Unable to find - ${CAVE_DIST_DIR}/%{_component_zip_file_name}."
   exit 1
fi

cp ${CAVE_DIST_DIR}/%{_component_zip_file_name} \
   ${RPM_BUILD_ROOT}/awips2/cave/.repository

%build

%install

%pre
# Ensure that CAVE is available to backup and to use to
# apply p2 updates.
if [ ! -f /awips2/cave/cave ]; then
   echo "ERROR: The cave executable was not found or is corrupt - /awips2/cave/cave;"
   echo "       awips2-cave must be re-installed. This installation will be terminated."
   exit 1
fi

CAVE_BACKUP="/awips2/cave.bak"

# Remove any existing backups.
if [ -d ${CAVE_BACKUP} ]; then
   rm -rf ${CAVE_BACKUP}
fi

# Create a backup of CAVE as it is.
LOG_TIMESTAMP=`date`
echo "backup STARTED: ${LOG_TIMESTAMP}"
cp -r /awips2/cave ${CAVE_BACKUP}
LOG_TIMESTAMP=`date`
echo "backup COMPLETE: ${LOG_TIMESTAMP}"
RC=$?
if [ ${RC} -ne 0 ]; then
   exit 1
fi

%post
CAVE_BACKUP="/awips2/cave.bak"
function cleanupUnzip()
{
   if [ -f /awips2/cave/.repository/artifacts.xml ]; then
      rm -f /awips2/cave/.repository/artifacts.xml
   fi
   
   if [ -f /awips2/cave/.repository/content.xml ]; then
      rm -f /awips2/cave/.repository/content.xml
   fi
   
   if [ -d /awips2/cave/.repository/features ]; then
      rm -rf /awips2/cave/.repository/features
   fi
   
   if [ -d /awips2/cave/.repository/plugins ]; then
      rm -rf /awips2/cave/.repository/plugins
   fi
}

function restoreCAVEAndFail()
{
   rm -rf /awips2/cave
   LOG_TIMESTAMP=`date`
   echo "restoring backup STARTED: ${LOG_TIMESTAMP}"
   mv -f ${CAVE_BACKUP} /awips2/cave
   LOG_TIMESTAMP=`date`
   echo "restoring backup COMPLETE: ${LOG_TIMESTAMP}"
   cleanupUnzip
   exit 1
}

# Set all paths required by CAVE before installing.
export LD_LIBRARY_PATH=/awips2/java/lib:/awips2/python/lib:$LD_LIBRARY_PATH
export LD_PRELOAD=libpython.so
if [ -d /awips2/cave/lib ]; then
   export LD_LIBRARY_PATH=/awips2/cave/lib/lib_illusion:$LD_LIBRARY_PATH
fi
if [ -d /awips2/cave/lib64 ]; then
   export LD_LIBRARY_PATH=/awips2/cave/lib64/lib_illusion:$LD_LIBRARY_PATH
fi
# Need to use awips2-java to do this.
export PATH=/awips2/java/bin:/awips2/python/bin:${PATH}
export JAVA_HOME="/awips2/java/jre"

# Use the eclipse p2 manager.
CAVE_EXE="/awips2/cave/cave"
NOSPLASH_ARG="-nosplash"
DIRECTOR_APP="-application org.eclipse.equinox.p2.director"
DESTINATION_ARG="-destination /awips2/cave"
INSTALL_ARG="-i %{_component_feature}.feature.group"
UNINSTALL_ARG="-u %{_component_feature}.feature.group"
REPO="-repository file:/awips2/cave/.repository/"

COMMON_CMD="${CAVE_EXE} ${NOSPLASH_ARG} ${DIRECTOR_APP} ${DESTINATION_ARG}"
INSTALL_CMD="${COMMON_CMD} ${INSTALL_ARG} ${REPO}"
UNINSTALL_CMD="${COMMON_CMD} ${UNINSTALL_ARG}"

# Uninstall any existing components since the p2 director does not
# support updating.
# If the feature is not installed, this does not fail quietly.
# Determine if the feature needs to be uninstalled.
${UNINSTALL_CMD} -verifyOnly > /dev/null 2>&1
RC=$?
if [ ${RC} -eq 0 ]; then
   LOG_TIMESTAMP=`date`
   echo "uninstall previous STARTED: ${LOG_TIMESTAMP}"
   ${UNINSTALL_CMD}
   LOG_TIMESTAMP=`date`
   echo "uninstall previous COMPLETE: ${LOG_TIMESTAMP}"
fi

# unzip the repository
cd /awips2/cave/.repository
cleanupUnzip
unzip %{_component_zip_file_name} > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Unzip of repository FAILED."
   restoreCAVEAndFail
fi

# Install the component.
LOG_TIMESTAMP=`date`
echo "installation STARTED: ${LOG_TIMESTAMP}"
${INSTALL_CMD}
RC=$?
if [ ${RC} -ne 0 ]; then
   restoreCAVEAndFail
fi
LOG_TIMESTAMP=`date`
echo "installation COMPLETE: ${LOG_TIMESTAMP}" 

# Cleanup the unzip
cleanupUnzip

# Remove the backup.
rm -rf /awips2/cave.bak

%preun
# Do not use p2 to remove the feature if this is an upgrade.
if [ "${1}" = "1" ]; then
   exit 0
fi

# Ensure that CAVE is available.
if [ ! -f /awips2/cave/cave ]; then
   # Even though we are not correctly uninstalled, there is nothing
   # we can do about that because of an rpm bug. If the user is
   # uninstalling then they will need to remove what is left of
   # /awips2 anyway.
   exit 0
fi

# Ensure that awips2-java is available.
if [ ! -f /awips2/java/bin/java ]; then
   exit 0
fi

# Ensure that awips2-python is available.
if [ ! -f /awips2/python/bin/python ]; then
   exit 0
fi

# Set all paths required by CAVE before installing.
export LD_LIBRARY_PATH=/awips2/java/lib:/awips2/python/lib:$LD_LIBRARY_PATH
export LD_PRELOAD=libpython.so
if [ -d /awips2/cave/lib ]; then
   export LD_LIBRARY_PATH=/awips2/cave/lib/lib_illusion:$LD_LIBRARY_PATH
fi
if [ -d /awips2/cave/lib64 ]; then
   export LD_LIBRARY_PATH=/awips2/cave/lib64/lib_illusion:$LD_LIBRARY_PATH
fi
# Need to use awips2-java to do this.
export PATH=/awips2/java/bin:/awips2/python/bin:${PATH}
export JAVA_HOME="/awips2/java/jre"

# Use the eclipse p2 manager.
CAVE_EXE="/awips2/cave/cave"
NOSPLASH_ARG="-nosplash"
DIRECTOR_APP="-application org.eclipse.equinox.p2.director"
DESTINATION_ARG="-destination /awips2/cave"
UNINSTALL_ARG="-u %{_component_feature}.feature.group"

COMMON_CMD="${CAVE_EXE} ${NOSPLASH_ARG} ${DIRECTOR_APP} ${DESTINATION_ARG}"
UNINSTALL_CMD="${COMMON_CMD} ${UNINSTALL_ARG}"

LOG_TIMESTAMP=`date`
echo "uninstall STARTED: ${LOG_TIMESTAMP}"
${UNINSTALL_CMD}
LOG_TIMESTAMP=`date`
echo "uninstall COMPLETE: ${LOG_TIMESTAMP}"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/cave/.repository
/awips2/cave/.repository/*
