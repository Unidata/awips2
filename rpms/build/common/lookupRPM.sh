#!/bin/bash

# This is a simple utility script used to determine which specs file should be
# used to build a specified rpm by name.

function lookupRPM()
{
   # Arguments:
   #   ${1} == the name of the rpm.

   # Determine which directory we are running from.
   path_to_script=`readlink -f $0`
   dir=$(dirname $path_to_script)

   rpms_dir=`cd ${dir}/../../../rpms; pwd;`
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to locate the rpm projects directory."
      exit 1
   fi
   
   export RPM_SPECIFICATION=
   export RPM_PROJECT_DIR=

   awips2_cave_dir="${rpms_dir}/awips2.cave"
   awips2_edex_dir="${rpms_dir}/awips2.edex"
   awips2_core_dir="${rpms_dir}/awips2.core"
   awips2_qpid_dir="${rpms_dir}/awips2.qpid"
   awips2_upc_dir="${rpms_dir}/awips2.upc"
   installer_dir="${rpms_dir}/../installers/RPMs"

   # foss rpms -> python rpms.
   if [ "${1}" = "awips2-maven" ]; then
      export RPM_SPECIFICATION="${installer_dir}/maven"
      return 0
   fi
   if [ "${1}" = "awips2-python" ]; then
      export RPM_SPECIFICATION="${installer_dir}/python"
      return 0
   fi
   if [ "${1}" = "awips2-python-h5py" ]; then
      export RPM_SPECIFICATION="${installer_dir}/h5py"
      return 0
   fi
   if [ "${1}" = "awips2-python-setuptools" ]; then
      export RPM_SPECIFICATION="${installer_dir}/setuptools"
      return 0
   fi
   if [ "${1}" = "awips2-python-matplotlib" ]; then
      export RPM_SPECIFICATION="${installer_dir}/matplotlib"
      return 0
   fi
   if [ "${1}" = "awips2-python-dateutil" ]; then
      export RPM_SPECIFICATION="${installer_dir}/python-dateutil"
      return 0
   fi
   if [ "${1}" = "awips2-python-pytz" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pytz"
      return 0
   fi
   if [ "${1}" = "awips2-python-six" ]; then
      export RPM_SPECIFICATION="${installer_dir}/six"
      return 0
   fi
   if [ "${1}" = "awips2-python-pyparsing" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyparsing"
      return 0
   fi
   if [ "${1}" = "awips2-python-numpy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/numpy"
      return 0
   fi
   if [ "${1}" = "awips2-python-nose" ]; then
      export RPM_SPECIFICATION="${installer_dir}/nose"
      return 0
   fi
   if [ "${1}" = "awips2-python-jep" ]; then
      export RPM_SPECIFICATION="${installer_dir}/jep"
      return 0
   fi
   if [ "${1}" = "awips2-python-qpid" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qpid-python"
      return 0
   fi
   if [ "${1}" = "awips2-python-scientific" ]; then
      export RPM_SPECIFICATION="${installer_dir}/scientific"
      return 0
   fi
   if [ "${1}" = "awips2-python-scipy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/scipy"
      return 0
   fi
   if [ "${1}" = "awips2-python-tables" ]; then
      export RPM_SPECIFICATION="${installer_dir}/tables"
      return 0
   fi
   if [ "${1}" = "awips2-python-werkzeug" ]; then
      export RPM_SPECIFICATION="${installer_dir}/werkzeug"
      return 0
   fi
   if [ "${1}" = "awips2-python-shapely" ]; then
      export RPM_SPECIFICATION="${installer_dir}/shapely"
      return 0
   fi
   if [ "${1}" = "awips2-ncep-database" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.ncep-database"
      return 0
   fi
   if [ "${1}" = "awips2-database" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.database"
      return 0
   fi
   if [ "${1}" = "awips2-gfesuite" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.gfesuite"
      return 0
   fi
   if [ "${1}" = "awips2-maps-database" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.maps-database"
      return 0
   fi
   if [ "${1}" = "awips2-pypies" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.pypies"
      return 0
   fi
   if [ "${1}" = "awips2-data.hdf5-topo" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.topo"
      return 0
   fi
   if [ "${1}" = "awips2-data.gfe" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.gfe"
      return 0
   fi
   if [ "${1}" = "awips2" ]; then
      export RPM_SPECIFICATION="${awips2_upc_dir}/Installer.awips"
      return 0
   fi
   if [ "${1}" = "awips2-common-base" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.common-base"
      return 0
   fi
   if [ "${1}" = "awips2-tools" ]; then
      export RPM_SPECIFICATION="${installer_dir}/tools"
      return 0
   fi
   # FOSS RPMs
   if [ "${1}" = "awips2-qpid-java" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qpid-java"
      return 0
   fi
   if [ "${1}" = "awips2-qpid-java-client" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qpid-java-client"
      return 0
   fi
   if [ "${1}" = "awips2-qpid-java-broker" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qpid-java-broker"
      return 0
   fi
   if [ "${1}" = "awips2-qpid-lib" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qpid-lib"
      return 0
   fi
   if [ "${1}" = "awips2-ant" ]; then
      export RPM_SPECIFICATION="${installer_dir}/ant"
      return 0
   fi
   if [ "${1}" = "awips2-httpd-pypies" ]; then
      export RPM_SPECIFICATION="${installer_dir}/httpd-pypies"
      return 0
   fi
   if [ "${1}" = "awips2-java" ]; then
      export RPM_SPECIFICATION="${installer_dir}/java"
      return 0
   fi
   if [ "${1}" = "awips2-groovy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/groovy"
      return 0
   fi
   if [ "${1}" = "awips2-postgresql" ]; then
      export RPM_SPECIFICATION="${installer_dir}/postgresql"
      return 0
   fi
   if [ "${1}" = "awips2-pgadmin3" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pgadmin3"
      return 0
   fi
   if [ "${1}" = "awips2-eclipse" ]; then
      export RPM_SPECIFICATION="${installer_dir}/eclipse"
      return 0
   fi
   if [ "${1}" = "awips2-yajsw" ]; then
      export RPM_SPECIFICATION="${installer_dir}/yajsw"
      return 0
   fi

   # awips2 rpms -> cave rpms.
   if [ "${1}" = "-cave" ]; then
      return 0
   fi

   # awips2 rpms -> edex rpms.
   if [ "${1}" = "-edex" ]; then
      return 0
   fi
   if [ "${1}" = "awips2-edex-environment" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.edex-environment/edex"
      return 0
   fi
   if [ "${1}" = "awips2-edex-shapefiles" ]; then
      export RPM_SPECIFICATION="${awips2_edex_dir}/Installer.edex-shapefiles"
      return 0
   fi

   # Unidata additions
   if [ "${1}" = "awips2-ldm" ]; then
      export RPM_SPECIFICATION="${awips2_upc_dir}/Installer.ldm"
      return 0
   fi
   if [ "${1}" = "awips2-python-awips" ]; then
      export RPM_SPECIFICATION="/awips2/repo/python-awips"
      return 0
   fi
   if [ "${1}" = "awips2-python-cython" ]; then
      export RPM_SPECIFICATION="${installer_dir}/cython"
      return 0
   fi
   if [ "${1}" = "awips2-python-cycler" ]; then
      export RPM_SPECIFICATION="${installer_dir}/cycler"
      return 0
   fi
   if [ "${1}" = "awips2-python-gfe" ]; then
      export RPM_SPECIFICATION="${awips2_upc_dir}/Installer.gfe"
      return 0
   fi
   return 1
}
