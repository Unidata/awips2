#
# AWIPS II Ignite Spec File
#
Name: awips2-ignite
Summary: AWIPS II Ignite
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: %{_build_arch}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no
BuildRequires: systemd
Requires: awips2-java
Requires: sed
Requires: awips2-watchdog

%description
AWIPS II Ignite Installation - Sets Up AWIPS II Ignite.

# disable jar repacking
%define __jar_repack 0

%prep
# Ensure that a "buildroot" has been specified.
if [ "${RPM_BUILD_ROOT}" = "" ]; then
   echo "ERROR: A BuildRoot has not been specified."
   echo "FATAL: Unable to Continue ... Terminating."
   exit 1
fi

if [ -d ${RPM_BUILD_ROOT} ]; then
   rm -rf ${RPM_BUILD_ROOT}
fi
/bin/mkdir -p ${RPM_BUILD_ROOT}
if [ $? -ne 0 ]; then
   exit 1
fi

%build
_build_xml=build.xml
BUILD_IGNITE=%{_baseline_workspace}/build.edex

cd ${BUILD_IGNITE}
/awips2/ant/bin/ant -f ${_build_xml} \
   -Dbuild.product=ignite \
   -Dfeature=com.raytheon.uf.ignite.core.feature \
   -Duframe.eclipse=%{_uframe_eclipse} \
   build \
   clean
if [ $? -ne 0 ]; then
   exit 1
fi

%install
mkdir -p ${RPM_BUILD_ROOT}
if [ $? -ne 0 ]; then
   exit 1
fi

unzip %{_baseline_workspace}/build.edex/ignite/dist/ignite.zip \
   -d ${RPM_BUILD_ROOT}

if [ $? -ne 0 ]; then
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/ignite/config
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/ignite/tls
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/ignite/bin
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/ignite/logs
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/ignite/conf/jms/auth
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/%{_unitdir}/
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/etc/watchdog.d
if [ $? -ne 0 ]; then
   exit 1
fi

IGNITE_FILE_SRC_DIR="com.raytheon.uf.ignite.core/"
cp %{_baseline_workspace}/${IGNITE_FILE_SRC_DIR}/config/* \
   ${RPM_BUILD_ROOT}/awips2/ignite/config/
cp %{_baseline_workspace}/${IGNITE_FILE_SRC_DIR}/tls/* \
   ${RPM_BUILD_ROOT}/awips2/ignite/tls/
cp %{_baseline_workspace}/${IGNITE_FILE_SRC_DIR}/scripts/* \
   ${RPM_BUILD_ROOT}/awips2/ignite/bin/
cp %{_baseline_workspace}/deploy.edex.awips2/esb/conf/jms/auth/* \
   ${RPM_BUILD_ROOT}/awips2/ignite/conf/jms/auth/
cp %{_baseline_workspace}/${IGNITE_FILE_SRC_DIR}/service/* \
   ${RPM_BUILD_ROOT}/%{_unitdir}/
# deploy the Apache Ignite watchdog scripts
cp %{_baseline_workspace}/rpms/awips2.core/Installer.ignite/scripts/watchdog/*watchdog.sh \
   ${RPM_BUILD_ROOT}/etc/watchdog.d
if [ $? -ne 0 ]; then
   echo "FAILED TO DEPLOY THE APACHE IGNITE WATCHDOG SCRIPTS!"
   exit 1
fi

%post

SETUP_ENV="/awips2/ignite/bin/setup.env"
SETUP_ENV_NEW="/awips2/ignite/bin/setup.env.rpmnew"

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

# Determine which ignite cluster we are part of
clusterServers=localhost
clusterIndex=1
localAddress="127.0.0.1"
localhostName="$(hostname)"
# regex doesn't work right with quotes around the second operand for some reason
if [[ ${localhostName} =~ cache[1-6].* ]]; then
   cacheNum="${localhostName:5:1}"
   if [[ "${cacheNum}" -lt 4 ]]; then
      clusterServers=cache1,cache2,cache3
      clusterIndex=1
      localAddress="$(hostname -I | cut -d' ' -f1)"
   else
      clusterServers=cache4,cache5,cache6
      clusterIndex=2
      localAddress="$(hostname -I | cut -d' ' -f1)"
   fi
fi

if [[ -f "${SETUP_ENV_NEW}" ]]; then
   # rewrite the new setup.env with the existing
   # configuration - provided as a convenience.
   source "${SETUP_ENV}"

   # setup.env variables to only attempt to reuse previous values for.
   # Update when a variable is added to or removed from setup.env.
   updateSetupEnv "IGNITE_SSL_CERT_DB" '"${IGNITE_HOME}/tls"'
   updateSetupEnv "BROKER_HOST" "localhost"
   updateSetupEnv "BROKER_PORT" "5672"
   updateSetupEnv "BROKER_HTTP" "8180"
   updateSetupEnv "JMS_VIRTUALHOST" "edex"
   updateSetupEnv "JMS_SSL_ENABLED" "true"
   updateSetupEnv "QPID_SSL_CERT_DB" "/awips2/ignite/conf/jms/auth"
   updateSetupEnv "QPID_SSL_CERT_NAME" "guest"

   # setup.env variables to auto-configure if previously unset/default.
   updateSetupEnv "IGNITE_CLUSTER_SERVERS" "localhost" "${clusterServers}"
   updateSetupEnv "IGNITE_CLUSTER_INDEX" "1" "${clusterIndex}"
   updateSetupEnv "LOCAL_ADDRESS" "127.0.0.1" "${localAddress}"

   # Remove the existing setup.env.
   rm -f "${SETUP_ENV}"

   # Rename setup.env.rpmnew to setup.env.
   mv "${SETUP_ENV_NEW}" "${SETUP_ENV}"
else
   # New setup.env, auto-configure ignite cluster values
   updateNewSetupEnv "IGNITE_CLUSTER_SERVERS" "localhost" "${clusterServers}"
   updateNewSetupEnv "IGNITE_CLUSTER_INDEX" "1" "${clusterIndex}"
   updateNewSetupEnv "LOCAL_ADDRESS" "127.0.0.1" "${localAddress}"
fi

# enable the service script
/bin/systemctl enable --quiet ignite@production

%preun
# if uninstalling
if [ "${1}" == "0" ]
then
    # stop the service and disable the service script
    /bin/systemctl disable --now --quiet ignite@production
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2/ignite
%dir /awips2/ignite/logs

/awips2/ignite/config
/awips2/ignite/lib

%defattr(600,awips,fxalpha,755)
%dir /awips2/ignite/conf
%dir /awips2/ignite/conf/jms
%dir /awips2/ignite/conf/jms/auth
%config(noreplace) /awips2/ignite/conf/jms/auth/*.crt
%config(noreplace) /awips2/ignite/conf/jms/auth/*.key

%defattr(755,awips,fxalpha,755)
%dir /awips2/ignite/bin
%attr(755,awips,fxalpha) /awips2/ignite/bin/*.sh
%config(noreplace) /awips2/ignite/bin/setup.env

%defattr(600,awips,fxalpha,700)
%dir /awips2/ignite/tls
%config(noreplace) /awips2/ignite/tls/*.crt
%config(noreplace) /awips2/ignite/tls/*.key
%config(noreplace) /awips2/ignite/tls/passwords.properties
%attr(644,awips,fxalpha) /awips2/ignite/tls/README

%attr(644,root,root) %{_unitdir}/*

%attr(744,root,root) /etc/watchdog.d/*
