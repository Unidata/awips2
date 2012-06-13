#!/bin/bash

# User variables for local builds:
export U_RPM_DESTINATION="/home/bkowal/rpmbuild"
export U_COMPONENT_VERSION="11.9.0"
export U_COMPONENT_RELEASE="1"
export U_BUILD_ROOT="/tmp/${USER}/awips2-build"
export U_UFRAME_ECLIPSE="/opt/uframe-eclipse"
export U_BASELINE_WORKSPACE="/common/bkowal/workspace"

# build variables
if [ "${RPM_DESTINATION}" = "" ]; then
   export RPM_DESTINATION="${U_RPM_DESTINATION}"
fi
if [ "${COMPONENT_VERSION}" = "" ]; then
   export COMPONENT_VERSION="${U_COMPONENT_VERSION}"
fi
if [ "${COMPONENT_RELEASE}" = "" ]; then
   export COMPONENT_RELEASE="${U_COMPONENT_RELEASE}"
fi
if [ "${BUILD_ROOT}" = "" ]; then
   export BUILD_ROOT="${U_BUILD_ROOT}"
fi
if [ "${UFRAME_ECLIPSE}" = "" ]; then
   export UFRAME_ECLIPSE="/opt/uframe-eclipse"
fi
if [ "${BASELINE_WORKSPACE}" = "" ]; then
   export BASELINE_WORKSPACE="${U_BASELINE_WORKSPACE}"
fi

EDEX_ENV="${BASELINE_WORKSPACE}/Installer.rpm/awips2.edex-environment"

/usr/bin/rpmbuild -ba --target=i386 \
   --define '_topdir %(echo ${RPM_DESTINATION})' \
   --define '_component_version %(echo ${COMPONENT_VERSION})' \
   --define '_component_release %(echo ${COMPONENT_RELEASE})' \
   --define '_build_root %(echo ${BUILD_ROOT})' \
   --define '_uframe_eclipse %(echo ${UFRAME_ECLIPSE})' \
   --define '_baseline_workspace %(echo ${BASELINE_WORKSPACE})' \
   --buildroot ${BUILD_ROOT} \
   ${EDEX_ENV}/edex/component.spec
if [ $? -ne 0 ]; then
   exit 1
fi

exit 0
