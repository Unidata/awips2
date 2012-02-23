DB_FILE="${WORKSPACE}/Installer.rpm/awips2-rpm.db"

# Now, it is time to build the AWIPS II Python Site-Package rpms.
echo "INFO: Begin - Building AWIPS II Python Site-Package RPMs."

export RPM_TOP_DIR="${WORKSPACE}/rpmbuild"
export WORKSPACE_DIR="${WORKSPACE}"
export BUILDROOT_DIR=/tmp/awips2-component

COMPONENT=""
COMPONENT_DIR=""

export AWIPSCM_SHARE=${SHARE_DIR}
export CPPFLAGS="-m32"
export PYTHON_EXE="/awips2/python/bin/python"
export LD_LIBRARY_PATH="/awips2/python/lib"
# Just In Case python-devel 2.7 is not installed. scipy ignored
# C++ environment flags.
if [ ! -f /usr/local/lib/libpython2.7.so ]; then
   # Without this, we will not be able to build every python site-package.
   echo "ERROR: There Is No lpython2.7 In /usr/local/lib."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

function updateSitePackageRepository()
{
   mv ${RPM_TOP_DIR}/RPMS/i386/* ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/python.site-packages
}

function updateSpecsFileIfNecessary()
{
   SQL="SELECT dynamicVersion FROM awips2_python_site_package_rpms WHERE component = '${COMPONENT}';"

   UPDATE_FLAG=`echo ${SQL} | sqlite3 ${DB_FILE}`
   if [ "${UPDATE_FLAG}" = "Y" ]; then
      perl -p -i -e "s/Version: 1.0.0/Version: ${AWIPSII_VERSION}/g" ${COMPONENT_SPECS}
      perl -p -i -e "s/Release: 1/Release: ${AWIPSII_RELEASE}/g" ${COMPONENT_SPECS}
   fi
}

function buildRPM()
{
   if [ -f ${RPM_TOP_DIR}/BUILD/component-files.txt ]; then
      rm -f ${RPM_TOP_DIR}/BUILD/component-files.txt
   fi

   rm -rf ${BUILDROOT_DIR}

   updateSpecsFileIfNecessary
   time rpmbuild -ba --target=i386 \
      --define '_topdir %(echo ${RPM_TOP_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_build_root %(echo ${BUILDROOT_DIR})' \
      --buildroot ${BUILDROOT_DIR} ${COMPONENT_SPECS}
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "ERROR: The build of '${COMPONENT}' has FAILED."
   fi
}

# Need To Add Logic For Installing Dependencies Based On The Build Order.
function loopThroughAllSitePackageRPMs()
{
   SELECT_ALL_SQL="SELECT component FROM awips2_python_site_package_rpms ORDER BY buildOrder, component;"

   # Select All RPMs From The Python Site-Package DB
   for component in `echo ${SELECT_ALL_SQL} | sqlite3 ${DB_FILE}`; do
      COMPONENT="${component}"
       
      # Scan DB for Python Site-Package RPMs
      SQL="SELECT buildDirectory FROM awips2_python_site_package_rpms WHERE component = '${COMPONENT}';"

      COMPONENT_DIR=`echo ${SQL} | sqlite3 ${DB_FILE}`
      # We will be building the rpms directly, instead of using the "batch" build scripts that are included in the rpm
      # build projects.
      COMPONENT_SPECS="${WORKSPACE_DIR}/Installer.rpm/${COMPONENT_DIR}/component.spec"
      buildRPM
   done
}

function loopThroughSpecifiedRPMs()
{
   # Scan DB for Python Site-Package RPMs
   for component in ${RPMS_TO_BUILD[*]}; do
      COMPONENT="${component}"
      SQL="SELECT buildDirectory FROM awips2_python_site_package_rpms WHERE component = '${COMPONENT}';"

      COMPONENT_DIR=`echo ${SQL} | sqlite3 ${DB_FILE}`
      # We will be building the rpms directly, instead of using the "batch" build scripts that are included in the rpm
      # build projects.
      if [ ! "${COMPONENT_DIR}" = "" ]; then
         COMPONENT_SPECS="${WORKSPACE_DIR}/Installer.rpm/${COMPONENT_DIR}/component.spec"
         buildRPM
      fi
   done
}

if [ "${BUILD_ALL_RPMS}" = "false" ]; then
   loopThroughSpecifiedRPMs
else
   loopThroughAllSitePackageRPMs
fi

updateSitePackageRepository

echo "INFO: Finish - Building AWIPS II Python Site-Package RPMs."
