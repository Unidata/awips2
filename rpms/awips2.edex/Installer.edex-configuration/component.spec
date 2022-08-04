#
# AWIPS II edex-configuration Spec File
#
Name: awips2-edex-configuration
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
#BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
provides: awips2-edex-configuration
requires: awips2
requires: awips2-edex-base
requires: sed

%description
AWIPS II Edex Configuration - the edex setup.env file.

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
mkdir -p %{_build_root}/awips2/edex/logs
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/awips2/edex/webapps
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/awips2/edex/data/share
if [ $? -ne 0 ]; then
   exit 1
fi

DEPLOY_SCRIPT="deploy.edex.awips2/deploy/deploy-esb-configuration.xml"

# use deploy-install to deploy edex-configuration.
pushd . > /dev/null
cd %{_baseline_workspace}
/awips2/ant/bin/ant -f ${DEPLOY_SCRIPT} \
   -Desb.overwrite=true \
   -Desb.directory=%{_baseline_workspace}/deploy.edex.awips2/esb \
   -Dedex.root.directory=${RPM_BUILD_ROOT}/awips2/edex
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null

DEPLOY_SCRIPT="deploy.edex.awips2/deploy/deploy-esb.xml"

# use deploy-install to deploy edex.
pushd . > /dev/null
cd %{_baseline_workspace}
/awips2/ant/bin/ant -f ${DEPLOY_SCRIPT} \
   -Ddeploy.data=true -Ddeploy.web=true \
   -Desb.overwrite=true \
   -Desb.directory=%{_baseline_workspace}/deploy.edex.awips2/esb \
   -Dedex.root.directory=${RPM_BUILD_ROOT}/awips2/edex \
   -Dbasedir=%{_baseline_workspace}/deploy.edex.awips2
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null

# remove any .gitignore files
# currently, the ebxml webapp includes a .gitignore file
/usr/bin/find ${RPM_BUILD_ROOT}/awips2/edex -name .gitignore -exec rm -f {} \;
if [ $? -ne 0 ]; then
   exit 1
fi

# remove the test logback configuration used for development
rm -f ${RPM_BUILD_ROOT}/awips2/edex/conf/logback-test.xml
if [ $? -ne 0 ]; then
   exit 1
fi

%pre

%post

SETUP_ENV="/awips2/edex/bin/setup.env"
SETUP_ENV_NEW="/awips2/edex/bin/setup.env.rpmnew"

function updateNewSetupEnv()
{
   # Use this method when we are setting auto-configured variables in a brand
   # new setup.env. This simply updates a variable to a given value, without
   # having to worry about if the variable was previously configured to
   # something else (like updateSetupEnv below does).
   #
   # Arguments:
   # 1) name of the variable to change.
   # 2) default value of the variable.
   # 3) value to set variable to.

   local VARIABLE="${1}"
   local DEFAULT="${2}"
   local VALUE="${3}"

   VALUE=$(echo "${VALUE}" | sed 's|/|\\/|g')
   DEFAULT=$(echo "${DEFAULT}" | sed 's|/|\\/|g')

   sed -i -e "s/export ${VARIABLE}=${DEFAULT}/export ${VARIABLE}=${VALUE}/g" "${SETUP_ENV}"
}

function updateSetupEnv()
{
   # Use this method when we are updating an already existing setup.env. This
   # updates a variable in setup.env.rpmnew with the previously configured value
   # from setup.env, and optionally is given an auto-configured value to use if
   # the variable was not previously configured.
   #
   # Arguments:
   # 1) name of the variable to change.
   # 2) default value of the variable.
   # 3) optional - value to set variable to if previously unset or default.

   local VARIABLE="${1}"
   local DEFAULT="${2}"
   local VALUE="${!VARIABLE}"

   if [[ ! -v "${VARIABLE}" || "${VALUE}" = "${DEFAULT}" ]]; then
      # previously unset or default
      if [[ "$#" -gt 2 ]]; then
         VALUE="${3}"
      else
         return
      fi
   fi

   VALUE=$(echo "${VALUE}" | sed 's|/|\\/|g')
   DEFAULT=$(echo "${DEFAULT}" | sed 's|/|\\/|g')

   sed -i -e "s/export ${VARIABLE}=${DEFAULT}/export ${VARIABLE}=${VALUE}/g" "${SETUP_ENV_NEW}"
}

function isAnyReachable()
{
   # Return 0/true if any of the given hostnames are reachable, otherwise
   # return 1/false.
   #
   # Arguments:
   # any number of hostnames to attempt to reach
   for server in "$@"; do
      if ping -c 1 -W 3 "${server}" >/dev/null 2>&1; then
         return 0
      fi
   done
   return 1
}

# Determine ignite cluster values to use in setup.env
if isAnyReachable cache4 cache5 cache6; then
   clusterServers1=cache1,cache2,cache3
   clusterServers2=cache4,cache5,cache6
   localAddress="$(hostname -I | cut -d' ' -f1)"
   pypiesCompatibilityHost=ev
elif isAnyReachable cache1 cache2 cache3; then
   clusterServers1=cache1,cache2,cache3
   clusterServers2=
   localAddress="$(hostname -I | cut -d' ' -f1)"
   pypiesCompatibilityHost=ev
else
   clusterServers1=localhost
   clusterServers2=
   localAddress=127.0.0.1
   pypiesCompatibilityHost=localhost
fi

if [[ -f "${SETUP_ENV_NEW}" ]]; then
   # Rewrite the new setup.env with the existing
   # configuration - provided as a convenience.
   source "${SETUP_ENV}"

   # setup.env variables to only attempt to reuse previous values for.
   # Update when a variable is added to or removed from setup.env.
   updateSetupEnv "AW_SITE_IDENTIFIER" "OAX"
   updateSetupEnv "CLUSTER_ID" ""

   updateSetupEnv "DC_DB_NAME" "dc_ob7oax"
   updateSetupEnv "FXA_DB_NAME" "fxatext"
   updateSetupEnv "HM_DB_NAME" "hmdb"
   updateSetupEnv "IH_DB_NAME" "hd_ob92oax"
   updateSetupEnv "CLIMATE_DB_NAME" "climate"

   updateSetupEnv "PREFERRED_AFOS_FIRST_LETTER" "\"KCPTXM\""

   updateSetupEnv "DATA_ARCHIVE_ROOT" "/tmp/sbn"

   updateSetupEnv "DB_HOST" "localhost"
   updateSetupEnv "DB_PORT" "5432"
   updateSetupEnv "DB_SSLMODE" "verify-ca"

   updateSetupEnv "BROKER_HOST" "localhost"
   updateSetupEnv "BROKER_PORT" "5672"
   updateSetupEnv "BROKER_HTTP" "8180"

   # skip DATASTORE_PROVIDER since it should only be modified via local profile entries
   updateSetupEnv "PYPIES_COMPATIBILITY_HOST" "localhost"
   updateSetupEnv "PYPIES_COMPATIBILITY_PORT" "9586"
   updateSetupEnv "IGNITE_SSL_CERT_DB" "/awips2/edex/conf/ignite/auth"

   updateSetupEnv "PYPIES_HOST" "localhost"
   updateSetupEnv "PYPIES_PORT" "9582"

   updateSetupEnv "HTTP_HOST" "localhost"
   updateSetupEnv "HTTP_PORT" "9581"
   updateSetupEnv "HTTP_SERVER_PATH" "services"
   updateSetupEnv "JMS_VIRTUALHOST" "edex"
   updateSetupEnv "JMS_SSL_ENABLED" "true"
   updateSetupEnv "QPID_SSL_CERT_DB" "/awips2/edex/conf/jms/auth"
   updateSetupEnv "QPID_SSL_CERT_NAME" "guest"
   updateSetupEnv "RADAR_HOST" "localhost"
   updateSetupEnv "RADAR_PORT" "8813"

   updateSetupEnv "SHARE_DIR" "/awips2/edex/data/share"

   updateSetupEnv "TEMP_DIR" "/awips2/edex/data/tmp"

   updateSetupEnv "LDAD_EXTERNAL_HOME" "/ldad"
   updateSetupEnv "LDAD_EXTERNAL_PUBLIC" "/data/ldad/public"
   updateSetupEnv "AWIPS2_TEMP" "/awips2/tmp"

   # setup.env variables to auto-configure if previously unset/default.
   if [[ ! -v IGNITE_CLUSTER_1_SERVERS || "${IGNITE_CLUSTER_1_SERVERS}" = localhost ]] && [[ "${IGNITE_CLUSTER_2_SERVERS}" == "" ]]; then
      # Auto-configure ignite cluster values if both were unset or default values before
      updateSetupEnv "IGNITE_CLUSTER_1_SERVERS" "localhost" "${clusterServers1}"
      updateSetupEnv "IGNITE_CLUSTER_2_SERVERS" "" "${clusterServers2}"
   else
      # Otherwise use values from previous config
      updateSetupEnv "IGNITE_CLUSTER_1_SERVERS" "localhost"
      updateSetupEnv "IGNITE_CLUSTER_2_SERVERS" ""
   fi
   updateSetupEnv "LOCAL_ADDRESS" "127.0.0.1" "${localAddress}"
   updateSetupEnv "PYPIES_COMPATIBILITY_HOST" "localhost" "${pypiesCompatibilityHost}"

   # Remove the existing setup.env.
   rm -f "${SETUP_ENV}"
   
   # Rename setup.env.rpmnew to setup.env.
   mv "${SETUP_ENV_NEW}" "${SETUP_ENV}"
else
   # New setup.env, auto-configure ignite cluster values
   updateNewSetupEnv "IGNITE_CLUSTER_1_SERVERS" "localhost" "${clusterServers1}"
   updateNewSetupEnv "IGNITE_CLUSTER_2_SERVERS" "" "${clusterServers2}"
   updateNewSetupEnv "LOCAL_ADDRESS" "127.0.0.1" "${localAddress}"
   updateNewSetupEnv "PYPIES_COMPATIBILITY_HOST" "localhost" "${pypiesCompatibilityHost}"
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/edex/conf
/awips2/edex/conf/*
%dir /awips2/edex/data
/awips2/edex/data/*
%dir /awips2/edex/data/share
%dir /awips2/edex/etc
/awips2/edex/etc/*
%dir /awips2/edex/lib
/awips2/edex/lib/*
%dir /awips2/edex/logs
%dir /awips2/edex/webapps
%config(noreplace) /awips2/edex/bin/setup.env
%defattr(755,awips,fxalpha,755)
/awips2/edex/bin/*.sh
/awips2/edex/bin/scriptLauncher
%config(noreplace) /awips2/edex/conf/db/auth/*.crt
%defattr(600,awips,fxalpha,755)
%config(noreplace) /awips2/edex/conf/db/auth/*.key
%config(noreplace) /awips2/edex/conf/db/auth/*.pk8
%config(noreplace) /awips2/edex/conf/jms/auth/*.crt
%config(noreplace) /awips2/edex/conf/jms/auth/*.key
%config(noreplace) /awips2/edex/conf/ignite/auth/*.crt
%config(noreplace) /awips2/edex/conf/ignite/auth/*.key
%config(noreplace) /awips2/edex/conf/ignite/auth/passwords.properties
