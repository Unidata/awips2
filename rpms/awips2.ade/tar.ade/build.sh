#!/bin/bash

# This script gathers all of the ade components and places them
# into a single directory so that a tar file can be created.

CURRENT_VERSION="${AWIPSII_VERSION}"
CURRENT_RELEASE="${AWIPSII_RELEASE}"

CURRENT_DATE=`date +"%m-%d-%Y"`

CORE_RPM_DIR="${AWIPSCM_SHARE}/awips2-rpms/rpms"
ECLIPSE_RPM_DIR="/usr/src/redhat/RPMS/i386"
ECLIPSE_RPM="awips2-eclipse-3.6.1-1.i386.rpm"
JAVA_RPM="awips2-java-1.6.0_17-2.i386.rpm"
PYTHON_RPM="awips2-python-2.7.1-1.i386.rpm"
ANT_RPM="awips2-ant-1.7.1-2.i386.rpm"
AWIPS2_SITE_PKG_RPMS=\
(
   'awips2-python-cherrypy-3.1.2-2.i386.rpm' \
   'awips2-python-h5py-1.3.0-2.i386.rpm' \
   'awips2-python-matplotlib-0.99.1.1-2.i386.rpm' \
   'awips2-python-nose-0.11.1-2.i386.rpm' \
   'awips2-python-numpy-1.5.0b1-2.i386.rpm' \
   'awips2-python-pil-1.1.6-2.i386.rpm' \
   'awips2-python-pmw-1.3.2-2.i386.rpm' \
   'awips2-python-pupynere-1.0.13-2.i386.rpm' \
   'awips2-python-qpid-0.6-2.i386.rpm' \
   'awips2-python-scientific-2.8-2.i386.rpm' \
   'awips2-python-scipy-0.8.0-2.i386.rpm' \
   'awips2-python-tables-2.1.2-2.i386.rpm' \
   'awips2-python-thrift-20080411p1-2.i386.rpm' \
   'awips2-python-tpg-3.1.2-2.i386.rpm' \
   'awips2-python-werkzeug-0.6.2-2.i386.rpm' \
)
BASELINE_JAR_DIR="${RPM_BUILD_ROOT}/tmp"
BASELINE_JAR="awips2-ade-baseline-SOURCES.jar"

mkdir -p /tmp/awips2-ade-${CURRENT_DATE}

cp -v ${CORE_RPM_DIR}/${JAVA_RPM} /tmp/awips2-ade-${CURRENT_DATE}
cp -v ${CORE_RPM_DIR}/${PYTHON_RPM} /tmp/awips2-ade-${CURRENT_DATE}
cp -v ${CORE_RPM_DIR}/${ANT_RPM} /tmp/awips2-ade-${CURRENT_DATE}
cp -v ${ECLIPSE_RPM_DIR}/${ECLIPSE_RPM} /tmp/awips2-ade-${CURRENT_DATE}
cp -v ${BASELINE_JAR_DIR}/${BASELINE_JAR} /tmp/awips2-ade-${CURRENT_DATE}
for rpm in ${AWIPS2_SITE_PKG_RPMS[*]};
do
   cp -v ${CORE_RPM_DIR}/${rpm} /tmp/awips2-ade-${CURRENT_DATE}
done

# Copy ufpy and dynamicserialize
# Determine where ufpy and dynamicserialize are located first. They should either
# be in the integration repository or the workspace of RPMBuildNoDeploy-Delivery.
INT_REPO="/var/www/html/repo/awips2-int/i386/python.site-packages"
DEL_WORKSPACE="/usr/share/tomcat5/.hudson/jobs/RPMBuildNoDeploy-Delivery/workspace/var/www/html/repo/awips2-int/i386/python.site-packages"
UFPY_FOUND="N"
DYNAMICSERIALIZE_FOUND="N"

UFPY_RPM="awips2-python-ufpy-${CURRENT_VERSION}-${CURRENT_RELEASE}.i386.rpm"
DYNAMICSERIALIZE_RPM="awips2-python-dynamicserialize-${CURRENT_VERSION}-${CURRENT_RELEASE}.i386.rpm"

# Check the integration repository first.
if [ -f ${INT_REPO}/${UFPY_RPM} ]; then
   cp -v ${INT_REPO}/${UFPY_RPM} /tmp/awips2-ade-${CURRENT_DATE}
   UFPY_FOUND="Y"
fi

if [ -f ${INT_REPO}/${DYNAMICSERIALIZE_RPM} ]; then
   cp -v ${INT_REPO}/${DYNAMICSERIALIZE_RPM} /tmp/awips2-ade-${CURRENT_DATE}
   DYNAMICSERIALIZE_FOUND="Y"
fi

# Check the workspace next - if the rpm was not found in the repo.
if [ "${UFPY_FOUND}" = "N" ]; then
   if [ ! -f ${DEL_WORKSPACE}/${UFPY_RPM} ]; then
      echo "ERROR: Unable to find - ${UFPY_RPM}."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi
   cp -v ${DEL_WORKSPACE}/${UFPY_RPM} /tmp/awips2-ade-${CURRENT_DATE}
fi

if [ "${DYNAMICSERIALIZE_FOUND}" = "N" ]; then
   if [ ! -f ${DEL_WORKSPACE}/${DYNAMICSERIALIZE_RPM} ]; then
      echo "ERROR: Unable to find - ${DYNAMICSERIALIZE_RPM}."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi
   cp -v ${DEL_WORKSPACE}/${DYNAMICSERIALIZE_RPM} /tmp/awips2-ade-${CURRENT_DATE}
fi

# Copy the installation / removal scripts.
cp -v ${WORKSPACE_DIR}/Installer.rpm/awips2.ade/tar.ade/scripts/* \
   /tmp/awips2-ade-${CURRENT_DATE}
chmod a+x /tmp/awips2-ade-${CURRENT_DATE}/*.sh

# Update the installation script.
perl -p -i -e "s/PY_DYNAMICSERIALIZE=/PY_DYNAMICSERIALIZE=${DYNAMICSERIALIZE_RPM}/g" \
   /tmp/awips2-ade-${CURRENT_DATE}/ade_quick_install.sh
perl -p -i -e "s/PY_UFPY=/PY_UFPY=${UFPY_RPM}/g" \
   /tmp/awips2-ade-${CURRENT_DATE}/ade_quick_install.sh

cd /tmp
# Remove any existing tar files
if [ -f awips2-ade-${CURRENT_DATE}.tar ]; then
   rm -f awips2-ade-${CURRENT_DATE}.tar
fi
tar -cjf awips2-ade-${CURRENT_VERSION}-${CURRENT_DATE}.tar awips2-ade-${CURRENT_DATE}
# Ensure that the tar was successful
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: Failed to create tar file - awips2-ade-${CURRENT_DATE}.tar."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

exit 0