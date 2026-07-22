#!/bin/bash

function usage()
{
   echo "Usage: $0 OPTION [-nobinlightning]"
   echo "   -buildRPM preform a build of an rpm."
   echo "   -WA       perform a build of all work assignments."
   echo "   -rh6      perform a full build of all the rpms."
   echo "   -dev      call functions directly"
   echo "   -ade      preform a ADE build."
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


# set lightning to false
LIGHTNING=false


if [ "${1}" = "-buildRPM" -a -n "${2}" ]; then
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

if [ "${1}" = "-WA" ]; then
   WA_rpm_build
   exit 0
fi
#BUILD GROUPS
function build_ade(){
   buildRPM "awips2"
   buildRPM "awips2-java"
   buildRPM "awips2-eclipse"
   buildRPM "awips2-python"
   buildRPM "awips2-qpid-proton"
   buildRPM "awips2-gradle"
   buildRPM "awips2-ninja-build"
   buildRPM "awips2-python-tomli"
   buildRPM "awips2-python-typing_extensions"
   buildRPM "awips2-python-setuptools_scm"
   buildRPM "awips2-python-jaraco.functools"
   buildRPM "awips2-python-portend"
   buildRPM "awips2-python-tempora"
   buildRPM "awips2-python-zc.lockfile"
   buildRPM "awips2-thrift"
   buildRPM "awips2-python-markupsafe"
   buildRPM "awips2-python-numpy"
   buildRPM "awips2-hdf5"
   buildRPM "awips2-python-cython"
   buildRPM "awips2-python-pkgconfig"
   buildRPM "awips2-python-packaging"
   buildRPM "awips2-python-scipy"
   buildRPM "awips2-python-h5py"
   buildRPM "awips2-python-certifi"
   buildRPM "awips2-python-pillow"
   buildRPM "awips2-python-matplotlib"
   buildRPM "awips2-python-numexpr"
   buildRPM "awips2-python-geos"
   buildRPM "awips2-python-shapely"
   buildRPM "awips2-python-dateutil"
   buildRPM "awips2-python-cycler"
   buildRPM "awips2-python-cppy"
   buildRPM "awips2-python-kiwisolver"
   buildRPM "awips2-python-cftime"
   buildRPM "awips2-python-importlib-resources"
   buildRPM "awips2-python-more_itertools"
   buildRPM "awips2-python-sqlite3"
   buildRPM "awips2-python-typing_extensions"
   buildRPM "awips2-python-tomli"
   buildRPM "awips2-python-pytz"
   buildRPM "awips2-python-pyproject_metadata"
   buildRPM "awips2-meson"
   buildRPM "awips2-python-meson_python"
   buildRPM "awips2-python-contourpy"
   buildRPM "awips2-python-fonttools"
   buildRPM "awips2-python-msgpack"
   buildRPM "awips2-python-py_cpuinfo"
   buildRPM "awips2-python-blosc2"
   buildRPM "awips2-ant"
   buildRPM "awips2-netcdf"
   #buildRPM "awips2-udunits/i686"

#local apps foss
   #buildRPM "awips2-eccodes"
}

function build_python()
{
   buildRPM "awips2-python-werkzeug"
   buildRPM "awips2-python-tables"
   buildRPM "awips2-python-tpg"
   buildRPM "awips2-python-ufpy"
   buildRPM "awips2-python-jep"
   buildRPM "awips2-python-stomp.py"
   buildRPM "awips2-python-pyshp"
   buildRPM "awips2-python-netcdf4"
   buildRPM "awips2-python-pmw"
   buildRPM "awips2-python-jaraco.classes"
   buildRPM "awips2-python-jaraco.text"
   buildRPM "awips2-python-jaraco.collections"
   buildRPM "awips2-python-cheroot"
   buildRPM "awips2-python-cherrypy"
   buildRPM "awips2-python-proj"
   buildRPM "awips2-python-gdal" # dependent on proj
   buildRPM "awips2-python-geojson"
   buildRPM "awips2-python-whoosh"
   buildRPM "awips2-python-qtpy"
   buildRPM "awips2-python-qtawesome"
   buildRPM "awips2-python-shiboken6"
   buildRPM "awips2-python-pyside6"
   buildRPM "awips2-python-zipp"
   buildRPM "awips2-python-importlib-metadata"
   buildRPM "awips2-python-pint"
   buildRPM "awips2-python-grib2"
   buildRPM "awips2-python-gridslice"
   buildRPM "awips2-python-pytest-qt"
   buildRPM "awips2-python-pyenchant"
   buildRPM "awips2-python-pykdtree"
   buildRPM "awips2-python-pyproj" # dependent on proj
   buildRPM "awips2-python-pyresample"
   buildRPM "awips2-python-six"
   buildRPM "awips2-python-natsort"
   buildRPM "awips2-python-click"
   buildRPM "awips2-python-mercantile"
   buildRPM "awips2-python-imageio"
   buildRPM "awips2-python-imageio-ffmpeg"
   buildRPM "awips2-python-marshmallow"
   buildRPM "awips2-python-casadi"
   buildRPM "awips2-python-bottleneck"
   buildRPM "awips2-python-pandas" #takes forever to build
   buildRPM "awips2-python-mpmath"
   buildRPM "awips2-python-sympy"
   buildRPM "awips2-python-antlr4"
   buildRPM "awips2-python-pymoca"
}

function build_qpid()
{
   buildRPM "awips2-qpid-broker-j"
   buildRPM "awips2-qpid-proton"
   buildRPM "awips2-qpid-proton-python"
   #buildRPM "awips2-qpid-proton/i386"
}

function build_server()
{
   #buildRPM "awips2-alertviz"
   buildTargetPlatform
   buildRPM "awips2-common-base" 
   buildRPM "awips2-java-security"
   #buildRPM "awips2-thrift/i686"
   #buildRPM "awips2-netcdf/i686"
   #buildRPM "awips2-netcdf-cxx/i686"
   #buildRPM "awips2-netcdf-fortran/i686"
   buildRPM "awips2-postgis" # dependent on proj, gdal 
   buildRPM "awips2-aviation-shared"
   buildRPM "awips2-cli"
   buildRPM "awips2-edex-environment"
   buildRPM "awips2-edex-enableservices"
   buildRPM "awips2-gfesuite"
   buildRPM "awips2-groovy"
   buildRPM "awips2-localapps-environment"
   buildRPM "awips2-scripts"
   buildRPM "awips2-pgtcl"
   #buildRPM "awips2-tkblt/i686"
   buildLocalization
   buildRPM "awips2-ignite"
   buildRPM "awips2-apps"
   buildRPM "awips2-devel"
   buildRPM "awips2-version"
   buildRPM "awips2-yajsw"
   buildRPM "awips2-watchdog"
   buildRPM "awips2-ffmpeg"
   buildRPM "awips2-g2c"
   #buildRPM "awips2-g2c/i386"
   #buildRPM "awips2-expect-libs/i686"

   #buildRPM "awips2-edex-hazards-scripts"
   #buildRPM "awips2-python-pygresql" # Needed for hazard services but is failing to build
   buildRPM "awips2-ecpg-compat"
   buildRPM "awips2-adapt-native"
   buildRPM "awips2-notification"

#Uniata additions
   buildRPM "awips2-ldm"
   build_pypies
   buildRPM "awips2-python-awips"
}

function build_database(){
   yum install postgresql-server-devel -y
   buildRPM "awips2-postgresql"
   buildRPM "awips2-database-server-configuration"
   buildRPM "awips2-database-standalone-configuration"
   buildRPM "awips2-database"
   buildRPM "awips2-maps-database"
   buildRPM "awips2-ncep-database"
   buildRPM "awips2-edex-shapefiles"
   buildRPM "awips2-data.gfe"
   buildRPM "awips2-data.hdf5-topo"
   buildRPM "awips2-rehost-support-postgresql"
}
function build_pypies(){
   buildRPM "awips2-pypies"
   buildRPM "awips2-httpd-pypies"
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

if [ "${1}" = "-dev" ]; then

        if [ ! $#  -eq 2 ]; then
        usage
        exit 1;
        fi

        echo -e "\n*** Executing $2  ***"
        $2
        if [ $? -ne 0 ]; then
           exit 1
        fi
        echo -e "*** $2 Complete ***\n"
        exit 0
fi

if [ "${1}" = "-ade" ]; then
    # Build the source jar file
    echo "============================="
    echo "Building the source Jar file"
    echo "============================="

    echo "WORKSPACE: ${WORKSPACE}"
    cd ${WORKSPACE}/rpms/awips2.ade/deploy.builder
    ./build.sh

   # Create the containing directory.
   ade_directory="${WORKSPACE}/../awips2-ade-${AWIPSII_VERSION}-${AWIPSII_RELEASE}"
   if [ -d ${ade_directory} ]; then
      rm -rf ${ade_directory}
      if [ $? -ne 0 ]; then
         exit 1
      fi
   fi
   mkdir -p ${ade_directory}
   if [ $? -ne 0 ]; then
      exit 1
   fi

   awips2_ade_directory="${WORKSPACE}/rpms/awips2.ade"
   # Copy the install and uninstall script to the directory.
   cp -v ${awips2_ade_directory}/tar.ade/scripts/*.sh \
      ${ade_directory}
   if [ $? -ne 0 ]; then
      exit 1
   fi


    if [ -f ${WORKSPACE}/../tmp/awips-component/tmp/awips2-ade-baseline-SOURCES.jar ]; then
       mv ${WORKSPACE}/../tmp/awips-component/tmp/awips2-ade-baseline-SOURCES.jar ${ade_directory}
    else
       echo "awips2-ade-baseline-SOURCES.jar not found in ${WORKSPACE}/../tmp/awips-component/tmp/"
       exit 1
    fi
    echo "============================="
    echo "Building source Jar file is complete"
    echo "============================="

   exit 0
fi

usage
exit 0
