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
   python_site__dir="${rpms_dir}/python.site-packages"

   installer_dir="${rpms_dir}/../installers/RPMs"

   # lookup the rpm.

   # foss rpms -> python rpms.
   if [ "${1}" = "awips2-maven" ]; then
      export RPM_SPECIFICATION="${installer_dir}/maven/"
      return 0
   fi
   if [ "${1}" = "awips2-python" ]; then
      export RPM_SPECIFICATION="${installer_dir}/python/"
      return 0
   fi
   if [ "${1}" = "awips2-python-cherrypy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/cherrypy"
      return 0
   fi
   if [ "${1}" = "awips2-python-dynamicserialize" ]; then
      export RPM_SPECIFICATION="${python_site__dir}/Installer.dynamicserialize"
      return 0
   fi
   if [ "${1}" = "awips2-python-h5py" ]; then
      export RPM_SPECIFICATION="${installer_dir}/h5py/"
      return 0
   fi
   if [ "${1}" = "awips2-python-setuptools" ]; then
      export RPM_SPECIFICATION="${installer_dir}/setuptools/"
      return 0
   fi
   if [ "${1}" = "awips2-python-setuptools_scm" ]; then
      export RPM_SPECIFICATION="${installer_dir}/setuptools_scm/"
      return 0
   fi
   if [ "${1}" = "awips2-python-matplotlib" ]; then
      export RPM_SPECIFICATION="${installer_dir}/matplotlib/"
      return 0
   fi
   if [ "${1}" = "awips2-python-dateutil" ]; then
      export RPM_SPECIFICATION="${installer_dir}/python-dateutil/"
      return 0
   fi
   if [ "${1}" = "awips2-python-pytz" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pytz/"
      return 0
   fi
   if [ "${1}" = "awips2-python-six" ]; then
      export RPM_SPECIFICATION="${installer_dir}/six/"
      return 0
   fi
   if [ "${1}" = "awips2-python-pyparsing" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyparsing/"
      return 0
   fi
   if [ "${1}" = "awips2-python-numpy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/numpy/"
      return 0
   fi
   if [ "${1}" = "awips2-python-jep" ]; then
      export RPM_SPECIFICATION="${installer_dir}/jep/"
      return 0
   fi
   if [ "${1}" = "awips2-python-scipy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/scipy/"
      return 0
   fi
   if [ "${1}" = "awips2-python-tables" ]; then
      export RPM_SPECIFICATION="${installer_dir}/tables/"
      return 0
   fi
   if [ "${1}" = "awips2-python-thrift" ]; then
      export RPM_SPECIFICATION="${installer_dir}/thrift"
      return 0
   fi
   if [ "${1}" = "awips2-python-tpg" ]; then
      export RPM_SPECIFICATION="${installer_dir}/tpg"
      return 0
   fi
   if [ "${1}" = "awips2-python-ufpy" ]; then
      export RPM_SPECIFICATION="${python_site__dir}/Installer.ufpy"
      return 0
   fi
   if [ "${1}" = "awips2-python-werkzeug" ]; then
      export RPM_SPECIFICATION="${installer_dir}/werkzeug"
      return 0
   fi
   if [ "${1}" = "awips2-python-shapely" ]; then
      export RPM_SPECIFICATION="${installer_dir}/shapely/"
      return 0
   fi
   if [ "${1}" = "awips2-python-stomp.py" ]; then
      export RPM_SPECIFICATION="${installer_dir}/stomp.py/"
      return 0
   fi
   if [ "${1}" = "awips2-python-pkgconfig" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pkgconfig/"
      return 0
   fi
   if [ "${1}" = "awips2-python-cython" ]; then
      export RPM_SPECIFICATION="${installer_dir}/cython/"
      return 0
   fi
   if [ "${1}" = "awips2-python-cycler" ]; then
      export RPM_SPECIFICATION="${installer_dir}/cycler/"
      return 0
   fi
   if [ "${1}" = "awips2-python-kiwisolver" ]; then
      export RPM_SPECIFICATION="${installer_dir}/kiwisolver/"
      return 0
   fi
   if [ "${1}" = "awips2-python-backports-lru_cache" ]; then
      export RPM_SPECIFICATION="${installer_dir}/backports-lru_cache/"
      return 0
   fi
   if [ "${1}" = "awips2-python-netcdf4" ]; then
      export RPM_SPECIFICATION="${installer_dir}/python-netcdf4/"
      return 0
   fi
   if [ "${1}" = "awips2-python-cftime" ]; then
      export RPM_SPECIFICATION="${installer_dir}/cftime/"
      return 0
   fi
   if [ "${1}" = "awips2-python-cheroot" ]; then
      export RPM_SPECIFICATION="${installer_dir}/cheroot/"
      return 0
   fi
   if [ "${1}" = "awips2-python-contextlib2" ]; then
      export RPM_SPECIFICATION="${installer_dir}/contextlib2/"
      return 0
   fi
   if [ "${1}" = "awips2-python-jaraco.functools" ]; then
      export RPM_SPECIFICATION="${installer_dir}/jaraco.functools/"
      return 0
   fi
   if [ "${1}" = "awips2-python-more-itertools" ]; then
      export RPM_SPECIFICATION="${installer_dir}/more-itertools/"
      return 0
   fi
   if [ "${1}" = "awips2-python-portend" ]; then
      export RPM_SPECIFICATION="${installer_dir}/portend/"
      return 0
   fi
   if [ "${1}" = "awips2-python-setuptools_scm_git_archive" ]; then
      export RPM_SPECIFICATION="${installer_dir}/setuptools_scm_git_archive/"
      return 0
   fi
   if [ "${1}" = "awips2-python-tempora" ]; then
      export RPM_SPECIFICATION="${installer_dir}/tempora/"
      return 0
   fi
   if [ "${1}" = "awips2-python-zc.lockfile" ]; then
      export RPM_SPECIFICATION="${installer_dir}/zc.lockfile/"
      return 0
   fi
   if [ "${1}" = "awips2-python-funcsigs" ]; then
      export RPM_SPECIFICATION="${installer_dir}/funcsigs/"
      return 0
   fi
   if [ "${1}" = "awips2-python-mock" ]; then
      export RPM_SPECIFICATION="${installer_dir}/mock/"
      return 0
   fi
   if [ "${1}" = "awips2-python-numexpr" ]; then
      export RPM_SPECIFICATION="${installer_dir}/numexpr/"
      return 0
   fi
   if [ "${1}" = "awips2-python-pbr" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pbr/"
      return 0
   fi

   if [ "${1}" = "awips2-python-pyshp" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyshp"
      return 0
   fi

   # awips2 rpms.
   if [ "${1}" = "awips2-ncep-database" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.ncep-database"
      return 0
   fi
   if [ "${1}" = "awips2-aviation-shared" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.aviation"
      return 0
   fi
   if [ "${1}" = "awips2-cli" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.cli"
      return 0
   fi
   if [ "${1}" = "awips2-database" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.database"
      return 0
   fi
   if [ "${1}" = "awips2-database-server-configuration" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.database-server-configuration"
      return 0
   fi
   if [ "${1}" = "awips2-database-standalone-configuration" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.database-standalone-configuration"
      return 0
   fi
   if [ "${1}" = "awips2-gfesuite" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.gfesuite"
      return 0
   fi
   if [ "${1}" = "awips2-localapps-environment" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.localapps-environment"
      return 0
   fi
   if [ "${1}" = "-localization" ]; then
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
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.awips"
      return 0
   fi
   if [ "${1}" = "awips2-devel" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.awips2-devel"
      return 0
   fi
   if [ "${1}" = "awips2-version" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.version"
      return 0
   fi
   if [ "${1}" = "awips2-common-base" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.common-base"
      return 0
   fi
   if [ "${1}" = "awips2-rehost-support-postgresql" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.rehost-support"
      return 0
   fi
   if [ "${1}" = "awips2-scripts" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.scripts"
      return 0
   fi
   if [ "${1}" = "awips2-watchdog" ]; then
      export RPM_SPECIFICATION="${installer_dir}/watchdog"
      return 0
   fi

   # foss rpms.
   if [ "${1}" = "awips2-qpid-proton" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qpid-proton"
      return 0
   fi
   if [ "${1}" = "awips2-qpid-proton-python" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qpid-proton-python"
      return 0
   fi
   if [ "${1}" = "awips2-qpid-broker-j" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qpid-broker-j"
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
   if [ "${1}" = "awips2-hdf5" ]; then
      export RPM_SPECIFICATION="${installer_dir}/hdf5"
      return 0
   fi
   if [ "${1}" = "awips2-netcdf" ]; then
      export RPM_SPECIFICATION="${installer_dir}/netcdf"
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
   if [ "${1}" = "awips2-ignite" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.ignite"
      return 0
   fi
   if [ "${1}" = "awips2-ffmpeg" ]; then
      export RPM_SPECIFICATION="${installer_dir}/ffmpeg"
      return 0
   fi

   # awips2 rpms -> viz rpms.
   if [ "${1}" = "awips2-alertviz" ]; then
      export RPM_SPECIFICATION="${awips2_cave_dir}/Installer.alertviz"
      return 0
   fi
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

   return 1
}
