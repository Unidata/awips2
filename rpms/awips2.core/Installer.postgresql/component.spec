#
# AWIPS II PostgreSQL Spec File
#

Name: awips2-postgresql
Summary: AWIPS II PostgreSQL Distribution
Version: 8.3.4
Release: 4
Group: AWIPSII
BuildRoot: /tmp
Prefix: /awips2
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-postgresql
provides: awips2-base-component

%description
AWIPS II PostgreSQL Distribution - Contains the AWIPS II PostgreSQL Distribution.
This is just the postgresql application. There is a separate rpm that will initialize
and populate the AWIPS II databases.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

# Ensure That awips2-psql Has Been Installed.
COMMAND=`rpm -q awips2-psql`
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: awips2-psql Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

POSTGRESQL_BUILD_DIR="awips2/postgresql-build"
POSTGRESQL_SOURCE_DIR="${AWIPSCM_SHARE}/packages/postgresqlSource"
POSTGRESQL_TAR_FILE="postgresql-8.3.4.tar.gz"

mkdir -p ${RPM_BUILD_ROOT}/awips2/postgresql
mkdir -p ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
mkdir -p ${RPM_BUILD_ROOT}/etc/ld.so.conf.d
touch ${RPM_BUILD_ROOT}/etc/ld.so.conf.d/awips2-postgresql-i386.conf

PROFILE_D_DIR="Installer.rpm/awips2.core/Installer.postgresql/scripts/profile.d"
cp ${WORKSPACE_DIR}/${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

# Copy our source tar file to the build directory.
cp ${POSTGRESQL_SOURCE_DIR}/${POSTGRESQL_TAR_FILE} \
   ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}

# Untar the postgresql source
cd ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}

tar -xvf ${POSTGRESQL_TAR_FILE}

%build
export LD_LIBRARY_PATH=/awips2/psql/lib
export LDFLAGS="-L${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.postgresql/pre-reqs/lib -L/awips2/psql/lib"

POSTGRESQL_BUILD_DIR="awips2/postgresql-build"

cd ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}/postgresql-8.3.4

./configure --prefix=${RPM_BUILD_ROOT}/awips2/postgresql
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi
make clean
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

# !!! HACK ! HACK ! HACK !!!
# Note: This May Not Be Necessary With Newer Versions Of PostgreSQL.
POSTGRESQL_MANUAL_BUILD_DIR="contrib/spi"
POSTGRESQL_MANUAL_BUILD_FILES=( 'autoinc' \
   'insert_username' 'moddatetime' 'refint' \
   'timetravel' )
cd ${POSTGRESQL_MANUAL_BUILD_DIR}

# Note: This Will Only Be Needed For 32-Bit Compiles On A 64-Bit Machine.
for file in ${POSTGRESQL_MANUAL_BUILD_FILES[*]};
do
   gcc -m32 -O2 -Wall -Wmissing-prototypes -Wpointer-arith -Winline -Wdeclaration-after-statement -Wendif-labels \
      -fno-strict-aliasing -fwrapv -fpic -DREFINT_VERBOSE -I. -I../../src/include -D_GNU_SOURCE \
      -c -o ${file}.o ${file}.c
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      exit 1
   fi
   gcc -m32 -shared -o ${file}.so ${file}.o
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      exit 1
   fi
done

cd ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}/postgresql-8.3.4
make
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

%install
# Copies the standard Raytheon licenses into a license directory for the
# current component.
function copyLegal()
{
   # $1 == Component Build Root
   
   COMPONENT_BUILD_DIR=${1}
   
   mkdir -p ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   
   # Create a Tar file with our FOSS licenses.
   tar -cjf ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar \
      ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses/
   
   cp ${WORKSPACE_DIR}/Installer.rpm/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "${WORKSPACE_DIR}/Installer.rpm/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f ${WORKSPACE_DIR}/Installer.rpm/legal/FOSS_licenses.tar    
}
POSTGRESQL_BUILD_DIR="awips2/postgresql-build"

cd ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}/postgresql-8.3.4

make install
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

POSTGRESQL_EXT_SRC_DIR="${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.postgresql/extensions"
PROJ_SRC="proj-4.6.1.tar.gz"
POSTGIS_SRC="postgis-1.3.5.tar.gz"
GEOS_SRC="geos-3.0.2.tar.bz2"

# The directory that the src will be in after the tars are unzipped.
PROJ_SRC_DIR="proj-4.6.1"
POSTGIS_SRC_DIR="postgis-1.3.5"
GEOS_SRC_DIR="geos-3.0.2"

cp ${POSTGRESQL_EXT_SRC_DIR}/${POSTGIS_SRC} \
   ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}
cp ${POSTGRESQL_EXT_SRC_DIR}/${PROJ_SRC} \
   ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}
cp ${POSTGRESQL_EXT_SRC_DIR}/${GEOS_SRC} \
   ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}

cd ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}
tar -xvf ${PROJ_SRC}
tar -xvf ${POSTGIS_SRC}
tar -xvf ${GEOS_SRC}

# a few of the postgresql extensions ignore CFLAGS
# if statement; check for 32 / 64 bit compile flag
export CPPFLAGS="-m32"

cd ${GEOS_SRC_DIR}
./configure --prefix=${RPM_BUILD_ROOT}/awips2/postgresql
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi
make
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi
make install
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

cd ../${PROJ_SRC_DIR}
./configure --prefix=${RPM_BUILD_ROOT}/awips2/postgresql
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi
make
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi
make install
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

cd ../${POSTGIS_SRC_DIR}

export PATH=${RPM_BUILD_ROOT}/awips2/postgresql/bin:$PATH
./configure \
   --with-pgconfig=${RPM_BUILD_ROOT}/awips2/postgresql/bin/pg_config \
   --with-geosconfig=${RPM_BUILD_ROOT}/awips2/postgresql/bin/geos-config \
   --with-projdir=${RPM_BUILD_ROOT}/awips2/postgresql \
   --prefix=${RPM_BUILD_ROOT}/awips2/postgresql
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi
make
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi
make install
RC="$?"
if [ ! "${RC}" = "0" ]; then
   exit 1
fi

# Remove the postgresql build directory.
rm -rf ${RPM_BUILD_ROOT}/${POSTGRESQL_BUILD_DIR}

CONFIG_FILE_TO_INCLUDE="postgresql.conf"
PATH_TO_DDL="build.edex/opt/db/ddl"
EXPECTED_PATH_TO_CONFIG="${PATH_TO_DDL}/setup"
KNOWN_CONFIG_DESTINATION="awips2/postgresql/share/sql"
# Ensure That We Have Access To The Configuration Files Before Continuing.
if [ ! -f ${WORKSPACE_DIR}/${EXPECTED_PATH_TO_CONFIG}/${CONFIG_FILE_TO_INCLUDE} ]; then
   echo "The ${CONFIG_FILE_TO_INCLUDE} PostgreSQL Configuration File Can Not Be Found."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

# Create The "Share SQL" Directory
mkdir -p ${RPM_BUILD_ROOT}/${KNOWN_CONFIG_DESTINATION}
# Create The PostgreSQL Data Directory
mkdir -p ${RPM_BUILD_ROOT}/awips2/data

# Copy The Configuration File
cp -r ${WORKSPACE_DIR}/${EXPECTED_PATH_TO_CONFIG}/${CONFIG_FILE_TO_INCLUDE} \
   ${RPM_BUILD_ROOT}/${KNOWN_CONFIG_DESTINATION} 

STARTUP_SCRIPTS_TO_INCLUDE=('start_developer_postgres.sh' 'start_postgres.sh')
PATH_TO_STARTUP_SCRIPTS="Installer.rpm/awips2.core/Installer.postgresql/scripts"
STARTUP_SCRIPT_DESTINATION="awips2/postgresql/bin"
# Copy The Startup Scripts
for script in ${STARTUP_SCRIPTS_TO_INCLUDE[*]};
do
   cp -r ${WORKSPACE_DIR}/${PATH_TO_STARTUP_SCRIPTS}/${script} \
      ${RPM_BUILD_ROOT}/${STARTUP_SCRIPT_DESTINATION}
done

copyLegal "awips2/postgresql"

# Include the postgresql service script
mkdir -p ${RPM_BUILD_ROOT}/etc/init.d
cp ${WORKSPACE_DIR}/Installer.rpm/awips2.core/Installer.postgresql/scripts/init.d/edex_postgres \
   ${RPM_BUILD_ROOT}/etc/init.d

%pre
if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing the AWIPS II PostgreSQL Distribution...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}/postgresql\e[m"

%post
function printFailureMessage()
{
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
   echo -e "\e[1;31m\| AWIPS II PostgreSQL Distribution Installation - FAILED\e[m"
   echo -e "\e[1;31m--------------------------------------------------------------------------------\e[m"
}

echo "--------------------------------------------------------------------------------"
echo "\| Setting up AWIPS II PostgreSQL Runtime and Environment..."
echo "--------------------------------------------------------------------------------"

echo "${RPM_INSTALL_PREFIX}/postgresql/lib" >> /etc/ld.so.conf.d/awips2-postgresql-i386.conf

# Run ldconfig
/sbin/ldconfig

echo "--------------------------------------------------------------------------------"
echo "\| Updating PostgreSQL Startup Scripts"
echo "--------------------------------------------------------------------------------"

echo "--------------------------------------------------------------------------------"
echo "\| Adding Environment Variables for AWIPS II PostgreSQL"
echo "--------------------------------------------------------------------------------"

if [ "${1}" = "2" ]; then
   exit 0
fi
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| AWIPS II PostgreSQL Distribution Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi
if [ -f /etc/init.d/edex_postgres ]; then
   /sbin/chkconfig --del edex_postgres
fi

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| The AWIPS II PostgreSQL Distribution Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%attr(755,root,root) /etc/profile.d/awips2Postgres.csh
%attr(755,root,root) /etc/profile.d/awips2Postgres.sh
%attr(755,root,root) /etc/ld.so.conf.d/awips2-postgresql-i386.conf
%attr(744,root,root) /etc/init.d/edex_postgres
%attr(700,awips,fxalpha) /awips2/data
%dir /awips2/postgresql
%docdir /awips2/postgresql/doc
%dir /awips2/postgresql/doc
/awips2/postgresql/doc/*
%dir /awips2/postgresql/include
/awips2/postgresql/include/*
%dir /awips2/postgresql/lib
/awips2/postgresql/lib/*
%docdir /awips2/postgresql/licenses
%dir /awips2/postgresql/licenses
/awips2/postgresql/licenses/*
%docdir /awips2/postgresql/man
%dir /awips2/postgresql/man
/awips2/postgresql/man/*
%dir /awips2/postgresql/share
/awips2/postgresql/share/*

%defattr(755,awips,fxalpha,755)
%dir /awips2/postgresql/bin
/awips2/postgresql/bin/*