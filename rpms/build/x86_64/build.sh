#!/bin/bash

function buildRPM()
{
   # Arguments:
   #   ${1} == the name of the rpm.
   lookupRPM "${1}"
   if [ $? -ne 0 ]; then
      echo "ERROR: '${1}' is not a recognized AWIPS II RPM."
      exit 1
   fi

   /usr/bin/rpmbuild -ba \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_uframe_eclipse %(echo ${UFRAME_ECLIPSE})' \
      --define '_awipscm_share %(echo ${AWIPSCM_SHARE})' \
      --define '_build_root %(echo ${AWIPSII_BUILD_ROOT})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_component_build_date %(echo ${COMPONENT_BUILD_DATE})' \
      --define '_component_build_time %(echo ${COMPONENT_BUILD_TIME})' \
      --define '_component_build_system %(echo ${COMPONENT_BUILD_SYSTEM})' \
      --buildroot ${AWIPSII_BUILD_ROOT} \
      ${RPM_SPECIFICATION}/component.spec
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to build RPM ${1}."
      exit 1
   fi

   return 0
}

# This script will build all of the 64-bit rpms.
# Ensure that we are on a machine with the correct architecture.

architecture=`uname -i`
if [ ! "${architecture}" = "x86_64" ]; then
   echo "ERROR: This build can only be performed on a 64-bit Operating System."
   exit 1
fi

# Determine which directory we are running from.
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

common_dir=`cd ${dir}/../common; pwd;`
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to find the common functions directory."
   exit 1
fi
# source the common functions.
source ${common_dir}/lookupRPM.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to source the common functions."
   exit 1
fi
source ${common_dir}/usage.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to source the common functions."
   exit 1
fi
source ${common_dir}/rpms.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to source the common functions."
   exit 1
fi
source ${common_dir}/systemInfo.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to retrieve the system information."
   exit 1
fi

# prepare the build environment.
source ${dir}/buildEnvironment.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to prepare the build environment."
   exit 1
fi

export LIGHTNING=true
# Determine if the optional '-nobinlightning' argument has been specified.
if [ "${2}" = "-nobinlightning" ]; then
   LIGHTNING=false
fi

if [ "${1}" = "-64bit" ]; then
   buildCAVE
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-alertviz"
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-python"
   buildRPM "awips2-python-cherrypy"
   buildRPM "awips2-python-dynamicserialize"
   buildRPM "awips2-python-h5py"
   buildRPM "awips2-python-jimporter"
   buildRPM "awips2-python-matplotlib"
   buildRPM "awips2-python-nose"
   buildRPM "awips2-python-numpy"
   buildRPM "awips2-python-pil"
   buildRPM "awips2-python-pmw"
   buildRPM "awips2-python-pupynere"
   buildRPM "awips2-python-qpid"
   buildRPM "awips2-python-scientific"
   buildRPM "awips2-python-scipy"
   buildRPM "awips2-python-tables"
   buildRPM "awips2-python-thrift"
   buildRPM "awips2-python-tpg"
   buildRPM "awips2-python-ufpy"
   buildRPM "awips2-python-werkzeug"
   buildRPM "awips2-python-pygtk"
   buildRPM "awips2-python-pycairo"
   buildJava
   buildRPM "awips2"
   buildRPM "awips2-python-shapely"
   buildRPM "awips2-notification"

   exit 0
fi

if [ "${1}" = "-rh6" ]; then
   buildRPM "awips2-common-base"
   buildRPM "awips2-notification"
   buildEDEX
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-hydroapps-shared"
   buildJava
   buildRPM "awips2-python"
   buildRPM "awips2-python-cherrypy"
   buildRPM "awips2-python-nose"
   buildRPM "awips2-python-pil"
   buildRPM "awips2-python-jimporter"
   buildRPM "awips2-python-qpid"
   buildRPM "awips2-python-thrift"
   buildRPM "awips2-python-werkzeug"
   buildRPM "awips2-python-numpy"
   buildRPM "awips2-python-pupynere"
   buildRPM "awips2-python-h5py"
   buildRPM "awips2-python-matplotlib"
   buildRPM "awips2-python-scientific"
   buildRPM "awips2-python-scipy"
   buildRPM "awips2-python-tables"
   buildRPM "awips2-python-pmw"
   buildRPM "awips2-python-tpg"
   buildRPM "awips2-python-ufpy"
   buildRPM "awips2-python-dynamicserialize"
   buildRPM "awips2-python-pycairo"
   buildRPM "awips2-python-pygtk"
   buildRPM "awips2-python-shapely"
   buildRPM "awips2-ant"
   buildRPM "awips2-tools"
   buildRPM "awips2-postgres"
   buildRPM "awips2-pgadmin3"
   unpackHttpdPypies
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-httpd-pypies"
   buildRPM "awips2-httpd-collaboration"
   buildQPID
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-ldm"
   buildCAVE
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-alertviz"
   buildRPM "awips2-database-server-configuration"
   buildRPM "awips2-database-standalone-configuration"
   buildRPM "awips2-database"
   buildRPM "awips2-maps-database"
   buildRPM "awips2-ncep-database"
   buildRPM "awips2-adapt-native"
   buildRPM "awips2-aviation-shared"
   buildRPM "awips2-cli"
   buildRPM "awips2-edex-environment"
   buildRPM "awips2-data.gfe"
   buildRPM "awips2-data.hdf5-gfe.climo"
   buildRPM "awips2-gfesuite-client"
   buildRPM "awips2-gfesuite-server"
   buildRPM "awips2-groovy"
   buildRPM "awips2-localapps-environment"
   buildLocalizationRPMs
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-pypies"
   buildRPM "awips2-rcm"
   buildRPM "awips2-data.hdf5-topo"
   buildRPM "awips2"
   buildOpenfire

   exit 0
fi

if [ "${1}" = "-postgres" ]; then
   buildRPM "awips2-postgres"
   buildRPM "awips2-database-server-configuration"
   buildRPM "awips2-database-standalone-configuration"
   buildRPM "awips2-database"
   buildRPM "awips2-maps-database"
   buildRPM "awips2-ncep-database"
   buildRPM "awips2-pgadmin3"

   exit 0
fi

if [ "${1}" = "-delta" ]; then
   buildRPM "awips2"
   buildRPM "awips2-common-base"
   buildCAVE
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-alertviz"
   buildEDEX
   if [ $? -ne 0 ]; then
      exit 1
   fi

   buildRPM "awips2-ncep-database"
   buildRPM "awips2-gfesuite-client"
   buildRPM "awips2-gfesuite-server"
   buildRPM "awips2-python-dynamicserialize"
   buildRPM "awips2-python-ufpy"

   buildRPM "awips2-aviation-shared"
   buildRPM "awips2-cli"
   buildRPM "awips2-database"
   buildRPM "awips2-database-server-configuration"
   buildRPM "awips2-database-standalone-configuration"
   buildRPM "awips2-data.hdf5-gfe.climo"
   buildRPM "awips2-gfesuite-client"
   buildRPM "awips2-gfesuite-server"
   buildRPM "awips2-localapps-environment"
   buildRPM "awips2-maps-database"
   buildRPM "awips2-notification"
   buildRPM "awips2-pypies"
   buildRPM "awips2-data.hdf5-topo"
   buildRPM "awips2-data.gfe"
   buildRPM "awips2-rcm"
   buildLocalizationRPMs
   if [ $? -ne 0 ]; then
      exit 1
   fi

   exit 0
fi

if [ "${1}" = "-full" ]; then
   buildRPM "awips2-common-base"
   buildCAVE
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-alertviz"
   buildEDEX
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-python"
   buildRPM "awips2-python-cherrypy"
   buildRPM "awips2-python-dynamicserialize"
   buildRPM "awips2-python-h5py"
   buildRPM "awips2-python-jimporter"
   buildRPM "awips2-python-matplotlib"
   buildRPM "awips2-python-nose"
   buildRPM "awips2-python-numpy"
   buildRPM "awips2-python-pil"
   buildRPM "awips2-python-pmw"
   buildRPM "awips2-python-pupynere"
   buildRPM "awips2-python-qpid"
   buildRPM "awips2-python-scientific"
   buildRPM "awips2-python-scipy"
   buildRPM "awips2-python-tables"
   buildRPM "awips2-python-thrift"
   buildRPM "awips2-python-tpg"
   buildRPM "awips2-python-ufpy"
   buildRPM "awips2-python-werkzeug"
   buildRPM "awips2-python-pygtk"
   buildRPM "awips2-python-pycairo"
   buildRPM "awips2-cli"
   buildRPM "awips2-data.hdf5-gfe.climo"
   buildRPM "awips2-gfesuite-client"
   buildRPM "awips2-gfesuite-server"
   buildRPM "awips2-localapps-environment"
   buildRPM "awips2-data.hdf5-topo"
   buildRPM "awips2-data.gfe"
   buildRPM "awips2"
   unpackHttpdPypies
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-httpd-pypies"
   buildJava
   buildRPM "awips2-groovy"
   buildLocalizationRPMs
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-edex-environment"
   buildRPM "awips2-notification"
   buildRPM "awips2-python-shapely"
   buildRPM "awips2-postgres"
   buildRPM "awips2-database"
   buildRPM "awips2-maps-database"
   buildRPM "awips2-ncep-database"
   buildRPM "awips2-pgadmin3"
   buildRPM "awips2-ldm"
   exit 0
fi

#if [ "${1}" = "-ade" ]; then
#   echo "INFO: AWIPS II currently does not support a 64-bit version of the ADE."
#   exit 0
#   buildRPM "awips2-eclipse"
#
#   exit 0
#fi

if [ "${1}" = "-ade" ]; then
   buildRPM "awips2-eclipse"
   buildJava
   buildRPM "awips2-ant"
   buildRPM "awips2-python"
   buildRPM "awips2-python-cherrypy"
   buildRPM "awips2-python-dynamicserialize"
   buildRPM "awips2-python-h5py"
   buildRPM "awips2-python-jimporter"
   buildRPM "awips2-python-matplotlib"
   buildRPM "awips2-python-nose"
   buildRPM "awips2-python-numpy"
   buildRPM "awips2-python-pil"
   buildRPM "awips2-python-pmw"
   buildRPM "awips2-python-pupynere"
   buildRPM "awips2-python-qpid"
   buildRPM "awips2-python-scientific"
   buildRPM "awips2-python-scipy"
   buildRPM "awips2-python-tables"
   buildRPM "awips2-python-thrift"
   buildRPM "awips2-python-tpg"
   buildRPM "awips2-python-ufpy"
   buildRPM "awips2-python-werkzeug"
   buildRPM "awips2-python-pygtk"
   buildRPM "awips2-python-pycairo"
   buildRPM "awips2-python-shapely"
   buildQPID -ade
   if [ $? -ne 0 ]; then
      exit 1
   fi
   
   # Package the ade.
   # Create the containing directory.
   ade_directory="awips2-ade-${AWIPSII_VERSION}-${AWIPSII_RELEASE}"
   if [ -d ${WORKSPACE}/${ade_directory} ]; then
      rm -rf ${WORKSPACE}/${ade_directory}
      if [ $? -ne 0 ]; then
         exit 1
      fi
   fi
   mkdir -p ${WORKSPACE}/${ade_directory}
   if [ $? -ne 0 ]; then
      exit 1
   fi

   # Copy the rpms to the directory.
   cp -v ${AWIPSII_TOP_DIR}/RPMS/x86_64/* \
      ${AWIPSII_TOP_DIR}/RPMS/noarch/* \
      ${WORKSPACE}/${ade_directory}
   if [ $? -ne 0 ]; then
      exit 1
   fi

   awips2_ade_directory="${WORKSPACE}/rpms/awips2.ade"
   # Copy the install and uninstall script to the directory.
   cp -v ${awips2_ade_directory}/tar.ade/scripts/*.sh \
      ${WORKSPACE}/${ade_directory}
   if [ $? -ne 0 ]; then
      exit 1
   fi

    # Build the source jar file
    #ade_work_dir="/home/dmsys/Dim12/build/AWIPS2/AWIPS2-ADE-OB14.1.1-CM"
    #cd $ade_work_dir
    #./build_source_jar.sh
    #cp -v /tmp/awips-component/tmp/awips2-ade-baseline-SOURCES.jar ${WORKSPACE}/${ade_directory}

   # Tar the directory.
   pushd . > /dev/null 2>&1
   cd ${WORKSPACE}
   tar -cvf ${ade_directory}.tar ${ade_directory}
   popd > /dev/null 2>&1
   RC=$?
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi

   exit 0
fi


if [ "${1}" = "-viz" ]; then
   buildRPM "awips2"
   #buildRPM "awips2-common-base"
   #buildRPM "awips2-python-dynamicserialize"
   #buildRPM "awips2-python"
   #buildRPM "awips2-adapt-native"
   #unpackHttpdPypies
   #if [ $? -ne 0 ]; then
   #   exit 1
   #fi
   #buildRPM "awips2-httpd-pypies"
   #buildRPM "awips2-hydroapps-shared"
   #buildRPM "awips2-rcm"
   #buildRPM "awips2-tools"
   #buildRPM "awips2-cli"
   buildCAVE
   if [ $? -ne 0 ]; then
      exit 1
   fi
   buildRPM "awips2-alertviz"

   exit 0
fi

if [ "${1}" = "-edex" ]; then
   ##buildRPM "awips2-common-base"
   buildEDEX
   if [ $? -ne 0 ]; then
      exit 1
   fi
   #buildRPM "awips2-python-dynamicserialize"

   exit 0
fi

if [ "${1}" = "-custom" ]; then
   #buildQPID
   #if [ $? -ne 0 ]; then
   #   exit 1
   #fi
   buildRPM "awips2-adapt-native"
   buildRPM "awips2-hydroapps-shared"
   #buildRPM "awips2-alertviz"
   #buildRPM "awips2-python"
   #buildRPM "awips2-alertviz"
   #buildRPM "awips2-eclipse"

   exit 0
fi

if [ "${1}" = "-qpid" ]; then
   buildRPM "awips2-python-qpid"
   buildQPID
   if [ $? -ne 0 ]; then
      exit 1
   fi

   exit 0
fi

if [ "${1}" = "-ldm" ]; then
   buildRPM "awips2-ldm"

   exit 0
fi

if [ "${1}" = "-package" ]; then
   repository_directory="awips2-repository-${AWIPSII_VERSION}-${AWIPSII_RELEASE}"
   if [ -d ${WORKSPACE}/${repository_directory} ]; then
      rm -rf ${WORKSPACE}/${repository_directory}
      if [ $? -ne 0 ]; then
         exit 1
      fi
   fi
   mkdir -p ${WORKSPACE}/${repository_directory}/${AWIPSII_VERSION}-${AWIPSII_RELEASE}
   if [ $? -ne 0 ]; then
      exit 1
   fi

   cp -r ${AWIPSII_TOP_DIR}/RPMS/* \
      ${WORKSPACE}/${repository_directory}/${AWIPSII_VERSION}-${AWIPSII_RELEASE}
   if [ $? -ne 0 ]; then
      exit 1
   fi

   rpms_directory="${WORKSPACE}/rpms"
   comps_xml="${rpms_directory}/common/yum/arch.x86_64/comps.xml"
   cp -v ${comps_xml} ${WORKSPACE}/${repository_directory}
   if [ $? -ne 0 ]; then
      exit 1
   fi

   pushd . > /dev/null
   cd ${WORKSPACE}
   tar -cvf ${repository_directory}.tar ${repository_directory}
   RC=$?
   popd > /dev/null
   if [ ${RC} -ne 0 ]; then
      exit 1
   fi

   exit 0
fi

usage
exit 0
