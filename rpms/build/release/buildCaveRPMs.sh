# Now, it is time to build the AWIPS II Cave RPMs.
echo "INFO: Begin - Building AWIPS II Cave RPMs."

export RPM_TOP_DIR="${WORKSPACE}/rpmbuild"
export WORKSPACE_DIR="${WORKSPACE}"
BUILDROOT_DIR=/tmp/awips2-component

COMPONENT=""
COMPONENT_DIR=""

export AWIPSCM_SHARE=${SHARE_DIR}

function updateCaveRepository()
{
   mv ${RPM_TOP_DIR}/RPMS/i386/* ${WORKSPACE}/${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR}/cave
}

cd ${WORKSPACE}/Installer.rpm/awips2.cave/deploy.builder
time ./build.sh ${AWIPSII_VERSION} ${AWIPSII_RELEASE}
RC=$?
if [ ${RC} -ne 0 ]; then
   exit ${RC}
fi
updateCaveRepository

echo "INFO: Finish - Building AWIPS II Cave RPMs."
