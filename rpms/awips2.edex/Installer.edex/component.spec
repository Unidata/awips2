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


echo "Updates for 15.1.1: Checking radar table for volume scan number."
SQL="
DO \$\$
BEGIN
    ALTER TABLE radar ADD COLUMN volumescannumber integer;
EXCEPTION
    WHEN duplicate_column THEN RAISE NOTICE 'column volumescannumber already exists in radar.';
END;
\$\$
"
${PSQL} -U ${DB_OWNER} -d metadata -c "${SQL}"
if [[ $? != 0 ]]
then
    echo "Radar update not needed. Continuing..."
else
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE radar SET volumescannumber=0 WHERE volumescannumber IS NULL;"
  echo "Done"
fi

# 16.2.2. deltaScripts
table_exists() {
    ${PSQL} -U ${DB_OWNER} -Aqt -d metadata -c \
    "select 1 from information_schema.tables where table_name = '$1'"
}
# Given table name as argument, return the name of the FK constraint referencing bufrmos_location.
get_constraint_name() {
    ${PSQL} -U ${DB_OWNER} -Aqt -d metadata << EOF
    SELECT tc.constraint_name
    FROM information_schema.table_constraints AS tc 
    JOIN information_schema.key_column_usage AS kcu
    ON tc.constraint_name = kcu.constraint_name
    JOIN information_schema.constraint_column_usage AS ccu
    ON ccu.constraint_name = tc.constraint_name
    WHERE constraint_type = 'FOREIGN KEY'
    AND tc.table_name='$1'
    and ccu.table_name = 'bufrmos_location';
EOF
}

# Check for existence of bufrmos_locationseq
sequence_exists() {
    ${PSQL} -U ${DB_OWNER} -Aqt -d metadata << EOF
    select 0
    from information_schema.sequences
    where sequence_name = 'bufrmos_locationseq'
EOF
}

get_min_pk() {
    ${PSQL} -U ${DB_OWNER} -Aqt -d metadata << EOF
    select min(id) id
    from bufrmos_location;
EOF
}

get_max_pk() {
    ${PSQL} -U ${DB_OWNER} -Aqt -d metadata << EOF
    select max(id) id
    from bufrmos_location;
EOF
}

last_bufrmos_locationseq_value() { 
    ${PSQL} -U ${DB_OWNER} -Aqt -d metadata -c \
        "select last_value from bufrmos_locationseq;"
}

if [[ $(table_exists bufrmod_location) == "1" ]]; then

  if [[ "$(sequence_exists)" != "0" ]]; then
    echo "INFO: bufrmos_locationseq does not exist in the database"
    echo "INFO: Attempting to create bufrmos_locationseq"
    ${PSQL} -U ${DB_OWNER} -d metadata -c \
        "create sequence bufrmos_locationseq increment 1 start 1;" > /dev/null
    if [[ "$?" != "0" || "$(sequence_exists)" != "0" ]]; then
        echo "ERROR: Failed to create bufrmos_locationseq"
        exit 1
    else
        echo "INFO Successfully created bufrmos_locationseq"
    fi
  fi

  min_pk="$(get_min_pk)"
  max_pk="$(get_max_pk)"

  if [[ ("$min_pk" -gt 0) && ("$max_pk" -le "$(last_bufrmos_locationseq_value)") ]]; then
    echo "INFO: bufrmos_locationseq is already updated."
  fi

  all_tables=(bufrmosavn bufrmoseta bufrmosgfs bufrmoshpc bufrmoslamp bufrmosmrf)
  tables=
  fkeys=

  for table in "${all_tables[@]}"; do
    if [[ $(table_exists $table) == "1" ]]; then
        tables+=("$table")
        fkeys+=("$(get_constraint_name $table)")
    fi
  done

  scriptfile=$(mktemp)

  if [[ "$scriptfile" == "" ]]; then
    echo "ERROR: Failed to create temp file for script in /tmp"
    exit 1
  fi

  echo "begin transaction;" > $scriptfile

  for i in $(seq 1 $(expr ${#tables[@]} - 1)); do
    cat << EOF >> $scriptfile
alter table ${tables[$i]}
drop constraint ${fkeys[$i]},
add constraint ${fkeys[$i]}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE NO ACTION;
EOF
  done

  cat << EOF >> $scriptfile
UPDATE bufrmos_location
   SET id=nextval('bufrmos_locationseq');
EOF

  for i in $(seq 1 $(expr ${#tables[@]} - 1)); do
    cat << EOF >> $scriptfile
alter table ${tables[$i]}
drop constraint ${fkeys[$i]},
add constraint ${fkeys[$i]}
      FOREIGN KEY (location_id)
      REFERENCES bufrmos_location (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION;
EOF
  done

  echo "commit;" >> $scriptfile

  echo "INFO: Updating bufrmos_location keys. This may take a few minutes..."

  ${PSQL} -U ${DB_OWNER} -d metadata < $scriptfile

  rm -f $scriptfile

  echo "INFO: Removing bufrquikscat table if it exists."

  ${PSQL} -U ${DB_OWNER} -d metadata -c "DROP TABLE IF EXISTS bufrquikscat;"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "DROP SEQUENCE IF EXISTS bufrquikscatseq;"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d fxatext -q -c "DROP TABLE IF EXISTS watchwarn CASCADE;"  > /dev/null 
  if [ $? -eq 0 ]; then
      echo "INFO: watchwarn table successfully dropped."
  else
      echo "WARN: Unable to drop watchwarn table."
      exit 1
  fi

  echo "INFO: Updating TOTSN and FRZR parameter names."
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '12hr ' || name WHERE abbreviation like 'TOTSN%pct12hr' AND name NOT LIKE '12hr%';"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '12hr ' || name WHERE abbreviation like 'TOTSN%in12hr' AND name NOT LIKE '12hr%';" > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '12hr ' || name WHERE abbreviation like 'FRZR%pct12hr' AND name NOT LIKE '12hr%';" > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '12hr ' || name WHERE abbreviation like 'FRZR%in12hr' AND name NOT LIKE '12hr%';" > /dev/null 

  # echo "Updating 24hr parameter names."
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '24hr ' || name WHERE abbreviation like 'TOTSN%pct24hr' AND name NOT LIKE '24hr%';" > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '24hr ' || name WHERE abbreviation like 'TOTSN%in24hr' AND name NOT LIKE '24hr%';"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '24hr ' || name WHERE abbreviation like 'FRZR%pct24hr' AND name NOT LIKE '24hr%';" > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '24hr ' || name WHERE abbreviation like 'FRZR%in24hr' AND name NOT LIKE '24hr%';" > /dev/null 

  #echo "Updating 48hr parameter names."
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '48hr ' || name WHERE abbreviation like 'TOTSN%pct48hr' AND name NOT LIKE '48hr%';" > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '48hr ' || name WHERE abbreviation like 'TOTSN%in48hr' AND name NOT LIKE '48hr%';" > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '48hr ' || name WHERE abbreviation like 'FRZR%pct48hr' AND name NOT LIKE '48hr%';"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '48hr ' || name WHERE abbreviation like 'FRZR%in48hr' AND name NOT LIKE '48hr%';"  > /dev/null 

  #echo "Updating 72hr parameter names."
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '72hr ' || name WHERE abbreviation like 'TOTSN%pct72hr' AND name NOT LIKE '72hr%';"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '72hr ' || name WHERE abbreviation like 'TOTSN%in72hr' AND name NOT LIKE '72hr%';"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '72hr ' || name WHERE abbreviation like 'FRZR%pct72hr' AND name NOT LIKE '72hr%';"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "UPDATE parameter SET name = '72hr ' || name WHERE abbreviation like 'FRZR%in72hr' AND name NOT LIKE '72hr%';"  > /dev/null 

  echo "INFO: Attempting to drop bufrmosngm table"

  ${PSQL} -U ${DB_OWNER} -d metadata -c "DROP TABLE IF EXISTS bufrmosngm" 

  echo "INFO: Attempting to remove bufrmosNGM plugin references from other tables"

  ${PSQL} -U ${DB_OWNER} -d metadata -c "DELETE FROM plugin_info where name = 'bufrmosNGM'"  > /dev/null 
  ${PSQL} -U ${DB_OWNER} -d metadata -c "DELETE FROM purgejobs where plugin = 'bufrmosNGM'"  > /dev/null 

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
