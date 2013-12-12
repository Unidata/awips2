%define _build_arch %(uname -i)
%define _postgresql_version 9.2.4
%define _postgres_build_loc %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
#
# AWIPS II PostgreSQL Spec File
#

Name: awips2-postgresql
Summary: AWIPS II PostgreSQL Distribution
Version: %{_postgresql_version}
Release: 2.el6
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: %{_build_arch}
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
# Ensure that a "buildroot" has been specified.
if [ "%{_build_root}" = "" ]; then
   echo "ERROR: A BuildRoot has not been specified."
   echo "FATAL: Unable to Continue ... Terminating."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi
/bin/mkdir -p %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi
if [ -d %{_postgres_build_loc} ]; then
   rm -rf %{_postgres_build_loc}
fi
mkdir -p %{_postgres_build_loc}
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_postgres_build_loc}/awips2/postgresql
if [ $? -ne 0 ]; then
   exit 1
fi

SRC_DIR="%{_baseline_workspace}/rpms/awips2.core/Installer.postgres/SOURCES"
POSTGRESQL_TAR_FILE="postgresql-%{_postgresql_version}.tar.gz"

# Copy our source tar file to the build directory.
cp ${SRC_DIR}/${POSTGRESQL_TAR_FILE} %{_postgres_build_loc}

# Untar the postgresql source
cd %{_postgres_build_loc}

tar -xvf ${POSTGRESQL_TAR_FILE}

%build
cd %{_postgres_build_loc}/postgresql-%{_postgresql_version}

./configure --prefix=%{_postgres_build_loc}/awips2/postgresql \
   --with-libxml
if [ $? -ne 0 ]; then
   exit 1
fi
make clean
if [ $? -ne 0 ]; then
   exit 1
fi

make
if [ $? -ne 0 ]; then
   exit 1
fi

cd %{_postgres_build_loc}/postgresql-%{_postgresql_version}/contrib/xml2
make
if [ $? -ne 0 ]; then
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
   tar -cjf %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      %{_baseline_workspace}/rpms/legal/FOSS_licenses/
   
   cp %{_baseline_workspace}/rpms/legal/license.txt \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp "%{_baseline_workspace}/rpms/legal/Master Rights File.pdf" \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
   cp %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar \
      ${RPM_BUILD_ROOT}/${COMPONENT_BUILD_DIR}/licenses
      
   rm -f %{_baseline_workspace}/rpms/legal/FOSS_licenses.tar    
}

mkdir -p %{_build_root}/awips2/postgresql
mkdir -p %{_build_root}/awips2/psql

cd %{_postgres_build_loc}/postgresql-%{_postgresql_version}

make install
if [ $? -ne 0 ]; then
   exit 1
fi

cd %{_postgres_build_loc}/postgresql-%{_postgresql_version}/contrib/xml2

make install
if [ $? -ne 0 ]; then
   exit 1
fi

# relocate the psql executable
mkdir -p %{_build_root}/awips2/psql/bin
mv -v %{_postgres_build_loc}/awips2/postgresql/bin/psql \
   %{_build_root}/awips2/psql/bin/psql
if [ $? -ne 0 ]; then
   exit 1
fi
# duplicate libpq; eventually, we should just have PostgreSQL
# reference the libpq in /awips2/psq/lib
mkdir -p %{_build_root}/awips2/psql/lib
cp -Pv %{_postgres_build_loc}/awips2/postgresql/lib/libpq.so* \
   %{_build_root}/awips2/psql/lib
if [ $? -ne 0 ]; then
   exit 1
fi

SRC_DIR="%{_baseline_workspace}/rpms/awips2.core/Installer.postgres/SOURCES"
PROJ_SRC="proj-4.8.0.zip"
POSTGIS_SRC="postgis-2.0.2.tar.gz"
GEOS_SRC="geos-3.3.7.tar.bz2"
GDAL_SRC="gdal192.zip"

# The directory that the src will be in after the tars are unzipped.
PROJ_SRC_DIR="proj-4.8.0"
POSTGIS_SRC_DIR="postgis-2.0.2"
GEOS_SRC_DIR="geos-3.3.7"
GDAL_SRC_DIR="gdal-1.9.2"

cp ${SRC_DIR}/${POSTGIS_SRC} %{_postgres_build_loc}
cp ${SRC_DIR}/${PROJ_SRC} %{_postgres_build_loc}
cp ${SRC_DIR}/${GEOS_SRC} %{_postgres_build_loc}
cp ${SRC_DIR}/${GDAL_SRC} %{_postgres_build_loc}

cd %{_postgres_build_loc}
unzip ${PROJ_SRC}
if [ $? -ne 0 ]; then
   exit 1
fi
tar -xvf ${POSTGIS_SRC}
if [ $? -ne 0 ]; then
   exit 1
fi
tar -xvf ${GEOS_SRC}
if [ $? -ne 0 ]; then
   exit 1
fi
unzip ${GDAL_SRC}
if [ $? -ne 0 ]; then
   exit 1
fi

cd ${GEOS_SRC_DIR}
./configure --prefix=%{_postgres_build_loc}/awips2/postgresql
if [ $? -ne 0 ]; then
   exit 1
fi
make
if [ $? -ne 0 ]; then
   exit 1
fi
make install
if [ $? -ne 0 ]; then
   exit 1
fi

cd ../${PROJ_SRC_DIR}
./configure --prefix=%{_postgres_build_loc}/awips2/postgresql --without-jni
if [ $? -ne 0 ]; then
   exit 1
fi
make
if [ $? -ne 0 ]; then
   exit 1
fi
make install
if [ $? -ne 0 ]; then
   exit 1
fi

cd ../${GDAL_SRC_DIR}
./configure --prefix=%{_postgres_build_loc}/awips2/postgresql \
   --with-expat-lib=%{_usr}/%{_lib}
if [ $? -ne 0 ]; then
   exit 1
fi
make
if [ $? -ne 0 ]; then
   exit 1
fi
make install
if [ $? -ne 0 ]; then
   exit 1
fi

cd ../${POSTGIS_SRC_DIR}
_POSTGRESQL_ROOT=%{_postgres_build_loc}/awips2/postgresql
_POSTGRESQL_BIN=${_POSTGRESQL_ROOT}/bin
./configure \
   --with-pgconfig=${_POSTGRESQL_BIN}/pg_config \
   --with-geosconfig=${_POSTGRESQL_BIN}/geos-config \
   --with-projdir=${_POSTGRESQL_ROOT} \
   --with-gdalconfig=${_POSTGRESQL_BIN}/gdal-config \
   --prefix=%{_postgres_build_loc}/awips2/postgresql
if [ $? -ne 0 ]; then
   exit 1
fi
# disable doc since it attempts to download files from
# the internet
echo "#Do Nothing" > doc/Makefile.in
echo "docs:" > doc/Makefile
echo "" >> doc/Makefile
echo "docs-install:" >> doc/Makefile
echo "" >> doc/Makefile
echo "docs-uninstall:" >> doc/Makefile
echo "" >> doc/Makefile
echo "comments-install:" >> doc/Makefile
echo "" >> doc/Makefile
echo "comments-uninstall:" >> doc/Makefile
echo "" >> doc/Makefile
echo "clean:" >> doc/Makefile
echo "" >> doc/Makefile
make
# run make twice - the first time may fail due to doc
make
if [ $? -ne 0 ]; then
   exit 1
fi
make install
if [ $? -ne 0 ]; then
   exit 1
fi

# Create The PostgreSQL Data Directory
mkdir -p ${RPM_BUILD_ROOT}/awips2/data

/bin/cp -Rf %{_postgres_build_loc}/awips2/postgresql/* %{_build_root}/awips2/postgresql
if [ $? -ne 0 ]; then
   exit 1
fi

STARTUP_SCRIPTS_TO_INCLUDE=('start_developer_postgres.sh' 'start_postgres.sh')
PATH_TO_STARTUP_SCRIPTS="rpms/awips2.core/Installer.postgres/scripts"
STARTUP_SCRIPT_DESTINATION="awips2/postgresql/bin"
# Copy The Startup Scripts
for script in ${STARTUP_SCRIPTS_TO_INCLUDE[*]};
do
   cp -r %{_baseline_workspace}/${PATH_TO_STARTUP_SCRIPTS}/${script} \
      ${RPM_BUILD_ROOT}/${STARTUP_SCRIPT_DESTINATION}
done

copyLegal "awips2/postgresql"

mkdir -p %{_build_root}/etc/profile.d
mkdir -p %{_build_root}/etc/ld.so.conf.d
mkdir -p %{_build_root}/etc/init.d
touch %{_build_root}/etc/ld.so.conf.d/awips2-postgresql-%{_build_arch}.conf
echo "/awips2/postgresql/lib" >> %{_build_root}/etc/ld.so.conf.d/awips2-postgresql-%{_build_arch}.conf

PROFILE_D_DIR="rpms/awips2.core/Installer.postgres/scripts/profile.d"
cp %{_baseline_workspace}/${PROFILE_D_DIR}/* %{_build_root}/etc/profile.d 

# Include the postgresql service script
cp %{_baseline_workspace}/rpms/awips2.core/Installer.postgres/scripts/init.d/edex_postgres \
   %{_build_root}/etc/init.d

%pre

%post

# Run ldconfig
/sbin/ldconfig

%preun
if [ "${1}" = "1" ]; then
   exit 0
fi
if [ -f /etc/init.d/edex_postgres ]; then
   /sbin/chkconfig --del edex_postgres
fi

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}
rm -rf %{_postgres_build_loc}

%package -n awips2-psql

Summary: AWIPS II PSQL Distribution
Group: AWIPSII

provides: awips2-psql

%description -n awips2-psql
AWIPS II PSQL Distribution - Contains the AWIPS II PSQL Distribution.
This is just the postgresql application. There is a separate rpm that will initialize
and populate the AWIPS II databases.

%files
%defattr(644,awips,fxalpha,755)
%attr(755,root,root) /etc/profile.d/awips2Postgres.csh
%attr(755,root,root) /etc/profile.d/awips2Postgres.sh
%attr(755,root,root) /etc/ld.so.conf.d/awips2-postgresql-%{_build_arch}.conf
%attr(744,root,root) /etc/init.d/edex_postgres
%attr(700,awips,fxalpha) /awips2/data
%dir /awips2/postgresql
%dir /awips2/postgresql/include
/awips2/postgresql/include/*
%dir /awips2/postgresql/lib
/awips2/postgresql/lib/*
%docdir /awips2/postgresql/licenses
%dir /awips2/postgresql/licenses
/awips2/postgresql/licenses/*
%dir /awips2/postgresql/share
/awips2/postgresql/share/*

%defattr(755,awips,fxalpha,755)
%dir /awips2/postgresql/bin
/awips2/postgresql/bin/*

%files -n awips2-psql
%defattr(755,awips,fxalpha,755)
%attr(755,root,root) /etc/profile.d/awips2PSQL.csh
%attr(755,root,root) /etc/profile.d/awips2PSQL.sh
%dir /awips2
%dir /awips2/psql
%dir /awips2/psql/bin
/awips2/psql/bin/*

%defattr(644,awips,fxalpha,755)
%dir /awips2/psql/lib
/awips2/psql/lib/*
