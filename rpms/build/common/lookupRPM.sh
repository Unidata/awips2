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
   awips2_upc_dir="${rpms_dir}/awips2.upc"
   python_site__dir="${rpms_dir}/python.site-packages"

   installer_dir="${rpms_dir}/../installers/RPMs"

   # lookup the rpm.

   # foss rpms -> python rpms.
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
   if [ "${1}" = "awips2-python-numpy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/numpy/"
      return 0
   fi
   if [ "${1}" = "awips2-python-scipy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/scipy/"
      return 0
   fi
   if [ "${1}" = "awips2-python-h5py" ]; then
      export RPM_SPECIFICATION="${installer_dir}/h5py/"
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
   if [ "${1}" = "awips2-python-jep" ]; then
      export RPM_SPECIFICATION="${installer_dir}/jep/"
      return 0
   fi
   if [ "${1}" = "awips2-python-tables" ]; then
      export RPM_SPECIFICATION="${installer_dir}/tables/"
      return 0
   fi
   if [ "${1}" = "awips2-thrift" ]; then
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
   if [ "${1}" = "awips2-python-geos" ]; then
      export RPM_SPECIFICATION="${installer_dir}/geos"
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
   if [ "${1}" = "awips2-python-jaraco.classes" ]; then
      export RPM_SPECIFICATION="${installer_dir}/jaraco.classes/"
      return 0
   fi
   if [ "${1}" = "awips2-python-jaraco.collections" ]; then
      export RPM_SPECIFICATION="${installer_dir}/jaraco.collections/"
      return 0
   fi
   if [ "${1}" = "awips2-python-jaraco.functools" ]; then
      export RPM_SPECIFICATION="${installer_dir}/jaraco.functools/"
      return 0
   fi
   if [ "${1}" = "awips2-python-jaraco.text" ]; then
      export RPM_SPECIFICATION="${installer_dir}/jaraco.text/"
      return 0
   fi
   if [ "${1}" = "awips2-python-portend" ]; then
      export RPM_SPECIFICATION="${installer_dir}/portend/"
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
   if [ "${1}" = "awips2-python-numexpr" ]; then
      export RPM_SPECIFICATION="${installer_dir}/numexpr/"
      return 0
   fi
   if [ "${1}" = "awips2-python-pyshp" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyshp"
      return 0
   fi
   if [ "${1}" = "awips2-python-importlib-resources" ]; then
      export RPM_SPECIFICATION="${installer_dir}/importlib-resources"
      return 0
   fi
   if [ "${1}" = "awips2-python-sqlite3" ]; then
      export RPM_SPECIFICATION="${installer_dir}/sqlite3"
      return 0
   fi
   if [ "${1}" = "awips2-python-proj" ]; then
      export RPM_SPECIFICATION="${installer_dir}/proj"
      return 0
   fi
   if [ "${1}" = "awips2-python-gdal" ]; then
      export RPM_SPECIFICATION="${installer_dir}/gdal"
      return 0
   fi
   if [ "${1}" = "awips2-python-certifi" ]; then
      export RPM_SPECIFICATION="${installer_dir}/certifi"
      return 0
   fi
   if [ "${1}" = "awips2-python-pillow" ]; then
      export RPM_SPECIFICATION="${installer_dir}/Pillow"
      return 0
   fi
   if [ "${1}" = "awips2-python-geojson" ]; then
      export RPM_SPECIFICATION="${installer_dir}/geojson"
      return 0
   fi
   if [ "${1}" = "awips2-python-whoosh" ]; then
      export RPM_SPECIFICATION="${installer_dir}/whoosh"
      return 0
   fi
   if [ "${1}" = "awips2-python-qtpy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qtpy"
      return 0
   fi
   if [ "${1}" = "awips2-python-qtawesome" ]; then
      export RPM_SPECIFICATION="${installer_dir}/qtawesome"
      return 0
   fi
   if [ "${1}" = "awips2-python-shiboken6" ]; then
      export RPM_SPECIFICATION="${installer_dir}/shiboken6"
      return 0
   fi
   if [ "${1}" = "awips2-python-pyside6" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyside6"
      return 0
   fi
   if [ "${1}" = "awips2-python-packaging" ]; then
      export RPM_SPECIFICATION="${installer_dir}/packaging"
      return 0
   fi
   if [ "${1}" = "awips2-python-zipp" ]; then
      export RPM_SPECIFICATION="${installer_dir}/zipp"
      return 0
   fi
   if [ "${1}" = "awips2-python-importlib-metadata" ]; then
      export RPM_SPECIFICATION="${installer_dir}/importlib-metadata"
      return 0
   fi
   if [ "${1}" = "awips2-python-typing_extensions" ]; then
      export RPM_SPECIFICATION="${installer_dir}/typing_extensions"
      return 0
   fi
   if [ "${1}" = "awips2-python-importlib-resources" ]; then
      export RPM_SPECIFICATION="${installer_dir}/importlib-resources"
      return 0
   fi
   if [ "${1}" = "awips2-python-pint" ]; then
      export RPM_SPECIFICATION="${installer_dir}/Pint"
      return 0
   fi
   if [ "${1}" = "awips2-python-toml" ]; then
      export RPM_SPECIFICATION="${installer_dir}/toml"
      return 0
   fi
   if [ "${1}" = "awips2-python-py" ]; then
      export RPM_SPECIFICATION="${installer_dir}/py"
      return 0
   fi
   if [ "${1}" = "awips2-python-pluggy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pluggy"
      return 0
   fi
   if [ "${1}" = "awips2-python-iniconfig" ]; then
      export RPM_SPECIFICATION="${installer_dir}/iniconfig"
      return 0
   fi
   if [ "${1}" = "awips2-python-attrs" ]; then
      export RPM_SPECIFICATION="${installer_dir}/attrs"
      return 0
   fi
   if [ "${1}" = "awips2-python-pytest" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pytest"
      return 0
   fi
   if [ "${1}" = "awips2-python-pytest-qt" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pytest-qt"
      return 0
   fi
   if [ "${1}" = "awips2-python-pyenchant" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyenchant"
      return 0
   fi
   if [ "${1}" = "awips2-python-pykdtree" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pykdtree"
      return 0
   fi
   if [ "${1}" = "awips2-python-pyproj" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyproj"
      return 0
   fi
   if [ "${1}" = "awips2-python-pyresample" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyresample"
      return 0
   fi
   if [ "${1}" = "awips2-python-natsort" ]; then
      export RPM_SPECIFICATION="${installer_dir}/natsort"
      return 0
   fi
   if [ "${1}" = "awips2-python-click" ]; then
      export RPM_SPECIFICATION="${installer_dir}/click"
      return 0
   fi
   if [ "${1}" = "awips2-python-mercantile" ]; then
      export RPM_SPECIFICATION="${installer_dir}/mercantile"
      return 0
   fi
   if [ "${1}" = "awips2-python-pillow" ]; then
      export RPM_SPECIFICATION="${installer_dir}/Pillow"
      return 0
   fi
   if [ "${1}" = "awips2-python-imageio" ]; then
      export RPM_SPECIFICATION="${installer_dir}/imageio"
      return 0
   fi
   if [ "${1}" = "awips2-python-imageio-ffmpeg" ]; then
      export RPM_SPECIFICATION="${installer_dir}/imageio-ffmpeg"
      return 0
   fi
   if [ "${1}" = "awips2-python-marshmallow" ]; then
      export RPM_SPECIFICATION="${installer_dir}/marshmallow"
      return 0
   fi
   if [ "${1}" = "awips2-python-casadi" ]; then
      export RPM_SPECIFICATION="${installer_dir}/casadi"
      return 0
   fi

   if [ "${1}" = "awips2-python-bottleneck" ]; then
      export RPM_SPECIFICATION="${installer_dir}/bottleneck"
      return 0
   fi

   if [ "${1}" = "awips2-python-pandas" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pandas"
      return 0
   fi

   if [ "${1}" = "awips2-python-mpmath" ]; then
      export RPM_SPECIFICATION="${installer_dir}/mpmath"
      return 0
   fi

   if [ "${1}" = "awips2-python-sympy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/sympy"
      return 0
   fi

   if [ "${1}" = "awips2-python-antlr4" ]; then
      export RPM_SPECIFICATION="${installer_dir}/python-antlr4"
      return 0
   fi

   if [ "${1}" = "awips2-python-pymoca" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pymoca"
      return 0
   fi
   if [ "${1}" = "awips2-python-more_itertools" ]; then
      export RPM_SPECIFICATION="${installer_dir}/more_itertools"
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
      export RPM_SPECIFICATION="${awips2_upc_dir}/Installer.awips"
      return 0
   fi
   if [ "${1}" = "awips2-apps" ]; then
      export RPM_SPECIFICATION="${awips2_core_dir}/Installer.apps"
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
   if [ "${1}" = "awips2-gradle" ]; then
      export RPM_SPECIFICATION="${installer_dir}/gradle"
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
   if [ "${1}" = "awips2-java-security" ]; then
      export RPM_SPECIFICATION="${installer_dir}/java-security"
      return 0
   fi
   if [ "${1}" = "awips2-groovy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/groovy"
      return 0
   fi
   if [ "${1}" = "awips2-postgis" ]; then
      export RPM_SPECIFICATION="${installer_dir}/postgis"
      return 0
   fi
   if [ "${1}" = "awips2-postgresql" ]; then
      export RPM_SPECIFICATION="${installer_dir}/postgresql"
      return 0
   fi
   if [ "${1}" = "awips2-ninja-build" ]; then
      export RPM_SPECIFICATION="${installer_dir}/ninja-build"
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
   if [ "${1}" = "awips2-netcdf-cxx" ]; then
      export RPM_SPECIFICATION="${installer_dir}/netcdf-cxx"
      return 0
   fi
   if [ "${1}" = "awips2-netcdf-fortran" ]; then
      export RPM_SPECIFICATION="${installer_dir}/netcdf-fortran"
      return 0
   fi
   if [ "${1}" = "awips2-udunits" ]; then
      export RPM_SPECIFICATION="${installer_dir}/udunits"
      return 0
   fi
   if [ "${1}" = "awips2-pgtcl" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pgtcl"
      return 0
   fi
   if [ "${1}" = "awips2-tkblt" ]; then
      export RPM_SPECIFICATION="${installer_dir}/tkblt"
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
   if [ "${1}" = "awips2-python-pmw" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pmw"
      return 0
   fi
   if [ "${1}" = "awips2-python-markupsafe" ]; then
      export RPM_SPECIFICATION="${installer_dir}/markupsafe"
      return 0
   fi
   if [ "${1}" = "awips2-python-tomli" ]; then
      export RPM_SPECIFICATION="${installer_dir}/tomli"
      return 0
   fi
   if [ "${1}" = "awips2-python-pytz" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pytz"
      return 0
   fi
   if [ "${1}" = "awips2-python-cppy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/cppy"
      return 0
   fi

   if [ "${1}" = "awips2-g2c" ]; then
      export RPM_SPECIFICATION="${installer_dir}/g2c"
      return 0
   fi

   if [ "${1}" = "awips2-expect-libs" ]; then
      export RPM_SPECIFICATION="${installer_dir}/expect-libs"
      return 0
   fi
   if [ "${1}" = "awips2-meson" ]; then
      export RPM_SPECIFICATION="${installer_dir}/meson"
      return 0
   fi
   if [ "${1}" = "awips2-python-pyproject_metadata" ]; then
      export RPM_SPECIFICATION="${installer_dir}/pyproject_metadata"
      return 0
   fi
   if [ "${1}" = "awips2-python-meson_python" ]; then
      export RPM_SPECIFICATION="${installer_dir}/meson_python"
      return 0
   fi
   if [ "${1}" = "awips2-python-contourpy" ]; then
      export RPM_SPECIFICATION="${installer_dir}/contourpy"
      return 0
   fi
   if [ "${1}" = "awips2-python-fonttools" ]; then
      export RPM_SPECIFICATION="${installer_dir}/fonttools"
      return 0
   fi

   if [ "${1}" = "awips2-python-msgpack" ]; then
      export RPM_SPECIFICATION="${installer_dir}/msgpack"
      return 0
   fi

   if [ "${1}" = "awips2-python-py_cpuinfo" ]; then
      export RPM_SPECIFICATION="${installer_dir}/py_cpuinfo"
      return 0
   fi

   if [ "${1}" = "awips2-python-blosc2" ]; then
      export RPM_SPECIFICATION="${installer_dir}/blosc2"
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
   if [ "${1}" = "awips2-edex-enableservices" ]; then
      export RPM_SPECIFICATION="${awips2_edex_dir}/Installer.edex-enableservices"
      return 0
   fi

   # Unidata additions
   if [ "${1}" = "awips2-ldm" ]; then
      export RPM_SPECIFICATION="${awips2_upc_dir}/Installer.ldm"
      return 0
   fi
   if [ "${1}" = "awips2-python-awips" ]; then
      export RPM_SPECIFICATION="/awips2/repo/python-awips/rpm"
      return 0
   fi
   if [ "${1}" = "awips2-python-six" ]; then
      export RPM_SPECIFICATION="${installer_dir}/six/"
      return 0
   fi
   return 1
}
