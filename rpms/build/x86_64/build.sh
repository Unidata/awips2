#!/bin/bash

function usage()
{
   echo "Usage: $0 OPTION"
   echo "   -b preform a build of an rpm."
   echo "   -WA       perform a build of all work assignments."
   echo "   -rh6      perform a full build of all the rpms."
   echo "   -dev      call functions directly"
   echo "   --help    display this message and exit."

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

source ${dir}/rpms.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to source the RPM functions."
   exit 1
fi

source ${dir}/WA_rpm_build.sh
if [ $? -ne 0 ]; then
	echo "WARNING: Unable to find the WA-RPM Build Contributions."
fi

#Check if the build root directory has execute permissions.
TMPFILE=${AWIPSII_BUILD_ROOT}/tmp.sh
if [ ! -d ${AWIPSII_BUILD_ROOT} ]; then
    mkdir -p ${AWIPSII_BUILD_ROOT}
fi
echo "#!/bin/bash" > ${TMPFILE}
chmod a+x ${TMPFILE}
${TMPFILE}
RTN=$?
rm -f ${TMPFILE}
if [ $RTN -ne 0 ]; then
   echo "Directory ${AWIPSII_BUILD_ROOT} does not have execute permissions!"
   exit 1
fi

if [ "${1}" = "-b" -a -n "${2}" ]; then
   echo "Building RPM: ${2}"
   # also allow buildCAVE, buildEDEX, buildRPM args
   buildName=`echo ${2} | cut -c1-5`
   if [ ${#2} -gt 5 -a "$buildName" = "build" ]; then
      ${2}
   else
      buildRPM ${2}
   fi
   if [ $? -ne 0 ]; then
      exit 1
   fi
   exit 0
fi

# BUILD GROUPS

function build_qpid() {
   buildRPM "awips2-qpid-java-client"
   buildRPM "awips2-qpid-java-broker"
}
function build_python() {
   buildRPM "awips2-python-h5py"
   buildRPM "awips2-python-jep"
   buildRPM "awips2-python-tables"
   buildRPM "awips2-python-matplotlib"
   buildRPM "awips2-python-shapely"
   buildRPM "awips2-python-scientific"
   buildRPM "awips2-python-qpid"
   buildRPM "awips2-python-werkzeug"
   buildRPM "awips2-python-cycler"
   buildRPM "awips2-python-gfe"
   buildRPM "awips2-python-awips"
}
function build_database() {
   buildRPM "awips2-database"
   buildRPM "awips2-postgresql"
   buildRPM "awips2-maps-database"
   buildRPM "awips2-ncep-database"
   buildRPM "awips2-edex-shapefiles"
   #buildRPM "awips2-data.hdf5-topo" # too big right now
   buildRPM "awips2-data.gfe"
}
function build_pypies() {
   buildRPM "awips2-pypies"
   buildRPM "awips2-httpd-pypies"
}
function build_server() {
   buildRPM "awips2"
   buildRPM "awips2-ldm"
   buildRPM "awips2-tools"
   build_pypies
   buildRPM "awips2-cli"
   buildRPM "awips2-data.gfe"
   buildLocalization
}
function build_ade() {
   buildRPM "awips2"
   buildRPM "awips2-java"
   buildRPM "awips2-eclipse"
   buildRPM "awips2-ant"
   buildRPM "awips2-groovy"
   buildRPM "awips2-yajsw"
   buildRPM "awips2-python"
   buildRPM "awips2-python-dateutil"
   buildRPM "awips2-python-numpy"
   buildRPM "awips2-python-pyparsing"
   buildRPM "awips2-python-pytz"
   buildRPM "awips2-python-setuptools"
   buildRPM "awips2-python-six"
   buildRPM "awips2-python-pytz"
   buildRPM "awips2-qpid-lib"
}

if [ "${1}" = "-ade" ]; then build_ade && exit 0; fi
if [ "${1}" = "-python" ]; then build_python && exit 0; fi
if [ "${1}" = "-qpid" ]; then build_qpid && exit 0; fi
if [ "${1}" = "-server" ]; then build_server && exit 0; fi
if [ "${1}" = "-pypies" ]; then build_pypies && exit 0; fi
if [ "${1}" = "-localization" ]; then buildLocalization && exit 0; fi
if [ "${1}" = "-database" ]; then build_database && exit 0; fi
if [ "${1}" = "-edex" ]; then buildEDEX && exit 0; fi
if [ "${1}" = "-cave" ]; then buildCAVE && exit 0; fi
if [ "${1}" = "-WA" ]; then WA_rpm_build && exit 0; fi

# BUILD ALL PACKAGES

if [ "${1}" = "-all" ]; then
   build_ade
   build_python
   build_qpid
   build_server
   build_database
   buildEDEX
   buildCAVE
   exit 0
fi
usage
exit 0
