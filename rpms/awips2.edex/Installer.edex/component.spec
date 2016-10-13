#
# AWIPS II EDEX Spec File
#
Name: awips2-edex
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

provides: awips2-edex
provides: awips2-base-component
provides: awips2-base
requires: awips2-python
requires: awips2-java
requires: awips2-psql
requires: awips2-yajsw
Obsoletes: awips2-edex-grib < 16.1.6

%{?filter_setup:
%filter_from_requires /guava/d; /raytheon/d
%filter_setup
}

%description
AWIPS II Edex Installation - Installs and configures AWIPS II Edex.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "" ]
then
   echo "ERROR: The RPM Build Root has not been specified."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi

%build

%install
mkdir -p %{_build_root}/awips2/edex
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/awips2/edex/bin
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi

# remove any .gitignore files
# currently, the ebxml webapp includes a .gitignore file
/usr/bin/find ${RPM_BUILD_ROOT}/awips2/edex -name .gitignore -exec rm -f {} \;
if [ $? -ne 0 ]; then
   exit 1
fi

INSTALLER_RPM="%{_baseline_workspace}/rpms"
# copy the service script.
EDEX_BASE="${INSTALLER_RPM}/awips2.edex/Installer.edex"
cp -v ${EDEX_BASE}/scripts/init.d/* \
   %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi

# copy versions.sh.
UTILITY="${INSTALLER_RPM}/utility"
cp -v ${UTILITY}/scripts/versions.sh \
   %{_build_root}/awips2/edex/bin
if [ $? -ne 0 ]; then
   exit 1
fi


#create a list of all files packaged for /awips2/edex/data/utility
UTILITY=/awips2/edex/data/utility
if [ -d %{_build_root}/$UTILITY ]; then
   cd %{_build_root}/$UTILITY
   find . -type f > %{_build_root}/awips2/edex/etc/util_filelist.%{name}.txt
fi


%pre

%post

# We need to create a link to the python shared library if it does not exist.
pushd . > /dev/null 2>&1
if [ -d /awips2/python/lib ]; then
   cd /awips2/python/lib
   if [ -L libpython.so ]; then
      # Ensure that we are pointing to the correct shared library.
      rm -f libpython.so
   fi
      
   if [ -f libpython2.7.so.1.0 ]; then
      ln -s libpython2.7.so.1.0 libpython.so
   fi
fi
popd > /dev/null 2>&1

if [ -f /etc/init.d/edex_camel ]; then
   /sbin/chkconfig --add edex_camel
fi

# determine if an installation of awips2-common-base is already present
# (CAVE has been installed before edex on an ADAM machine)
if [ -d /awips2/.edex ]; then
   # copy the common-base contributions to the EDEX installation
   cp -r /awips2/.edex/* /awips2/edex
   
   # cleanup
   rm -rf /awips2/.edex
fi

#change date stamp of utility files
UTILITY=/awips2/edex/data/utility
UTIL_FILENAME=/awips2/edex/etc/util_filelist.%{name}.txt
if [ -d $UTILITY ] && [ -f $UTIL_FILENAME ]; then
   while read fileName
   do
      touch "$UTILITY/$fileName"
   done < $UTIL_FILENAME
   rm -f $UTIL_FILENAME
fi


# From 15.1.1 deltaScripts
#
# New column volumeScanNumber for plugin Radar
POSTGRESQL_INSTALL="/awips2/postgresql"
DATABASE_INSTALL="/awips2/database"
AWIPS2_DATA_DIRECTORY="/awips2/data"
PSQL_INSTALL="/awips2/psql"
POSTMASTER="${POSTGRESQL_INSTALL}/bin/postmaster"
PG_CTL="${POSTGRESQL_INSTALL}/bin/pg_ctl"
DROPDB="${POSTGRESQL_INSTALL}/bin/dropdb"
PSQL="${PSQL_INSTALL}/bin/psql"
#DB_OWNER=`ls -ld ${AWIPS2_DATA_DIRECTORY} | grep -w 'data' | awk '{print $3}'`
DB_OWNER="awips"

# Determine if PostgreSQL is running.
I_STARTED_POSTGRESQL="NO"
su ${DB_OWNER} -c \
   "${PG_CTL} status -D ${AWIPS2_DATA_DIRECTORY} > /dev/null 2>&1"
RC="$?"

# Start PostgreSQL if it is not running.
if [ ! "${RC}" = "0" ]; then
   echo "Starting PostgreSQL As User - ${DB_OWNER}..."
   su ${DB_OWNER} -c \
      "${POSTMASTER} -D ${AWIPS2_DATA_DIRECTORY} > /dev/null 2>&1 &"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "Error - failed to start the PostgreSQL Server."
      printFailureMessage
   fi
   sleep 5
   I_STARTED_POSTGRESQL="YES"
else
   echo "Found Running PostgreSQL Server..."
   su ${DB_OWNER} -c \
      "${PG_CTL} status -D ${AWIPS2_DATA_DIRECTORY}"
fi


echo "15.1.1 Updating radar table to include volume scan number."

SQL="
DO \$\$
BEGIN
    ALTER TABLE radar ADD COLUMN volumescannumber integer;
EXCEPTION
    WHEN duplicate_column THEN RAISE NOTICE 'column volumescannumber already exists in radar.';
END;
\$\$
"
/awips2/psql/bin/psql -U ${DB_OWNER} -d metadata -c "${SQL}"
if [[ $? != 0 ]]
then
    echo "Radar update not needed. Continuing..."
else
  /awips2/psql/bin/psql -U ${DB_OWNER} -d metadata -c "UPDATE radar SET volumescannumber=0 WHERE volumescannumber IS NULL;"
  echo "Done"
fi

# stop PostgreSQL if we started it.
if [ "${I_STARTED_POSTGRESQL}" = "YES" ]; then
   echo "Stopping PostgreSQL As User - ${DB_OWNER}..."
   su ${DB_OWNER} -c \
      "${PG_CTL} stop -D /awips2/data"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "Warning: Failed to shutdown PostgreSQL."
   fi
   sleep 10
fi


%preun
if [ "${1}" = "1" ]; then
   exit 0
fi
if [ -f /etc/init.d/edex_camel ]; then
   /sbin/chkconfig --del edex_camel
fi

%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,awips,755)
%dir /awips2
%dir /awips2/edex

%defattr(755,awips,awips,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*.sh

%attr(744,root,root) /etc/init.d/*
