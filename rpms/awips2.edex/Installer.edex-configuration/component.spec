%define CORE_DELTA_SETUP ${WORKSPACE_DIR}/Installer.rpm/delta/setup/updateSetup.sh
%define _component_name           awips2-edex-configuration
%define _component_project_dir    awips2.core/Installer.edex-configuration
%define _component_default_prefix /awips2
#
# AWIPS II Edex Configuration Spec File
#
Name: %{_component_name}
Summary: AWIPS II Edex Configuration Distribution
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
Prefix: %{_component_default_prefix}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-edex-configuration
requires: awips2
requires: awips2-edex-base

%description
AWIPS II Edex Configuration Distribution - Includes the machine-specific/site-specific
configuration files for edex.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}/awips2/edex

%build
#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
source %{CORE_DELTA_SETUP}
copySetupCore ${RPM_BUILD_ROOT} %{_component_default_prefix}
copyApplicableDeltas ${RPM_BUILD_ROOT} %{_component_name} \
   %{_component_project_dir} %{_component_default_prefix}
#---------------------------------------------------------------------------#

%install
DEPLOY_SCRIPT="build.edex/deploy-common/deploy-esb-configuration.xml"

# Deploy Edex To Our Temporary Build Directory.

# Determine which ant executable to use.
COMMAND=`rpm -q awips2-ant`
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: awips2-ant Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

ANT_EXE="/awips2/ant/bin/ant"

${ANT_EXE} -file ${WORKSPACE_DIR}/${DEPLOY_SCRIPT} \
   -Desb.overwrite=true \
   -Desb.directory=${WORKSPACE_DIR}/build.edex/esb \
   -Dedex.root.directory=${RPM_BUILD_ROOT}/awips2/edex
if [ $? -ne 0 ]; then
   exit 1
fi
   
%pre
if [ "${1}" = "2" ]; then
   exit 0
fi

echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing AWIPS II Edex Configuration...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m   Installation Root = ${RPM_INSTALL_PREFIX}/edex\e[m"

%post
# Determine if we are installing an updated setup.env.

SETUP_ENV="${RPM_INSTALL_PREFIX}/edex/bin/setup.env"
SETUP_ENV_NEW="${RPM_INSTALL_PREFIX}/edex/bin/setup.env.rpmnew"

if [ -f "${SETUP_ENV_NEW}" ]; then
   # Source the existing setup.env.
   source ${SETUP_ENV}
   
   # Replace the contents of the rpmnew setup.env environment
   # variable by environment variable.
   
   # Add Escape Characters To Certain Variables.
   echo ${DATA_ARCHIVE_ROOT} | sed 's/\//\\\//g' > .awips2_escape.tmp
   DATA_ARCHIVE_ROOT=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   echo ${DB_ADDR} | sed 's/\//\\\//g' > .awips2_escape.tmp
   DB_ADDR=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   echo ${BROKER_ADDR} | sed 's/\//\\\//g' > .awips2_escape.tmp
   BROKER_ADDR=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   echo ${PYPIES_SERVER} | sed 's/\//\\\//g' > .awips2_escape.tmp
   PYPIES_SERVER=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   echo ${HTTP_SERVER} | sed 's/\//\\\//g' > .awips2_escape.tmp
   HTTP_SERVER=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   echo ${JMS_SERVER} | sed 's/\//\\\//g' > .awips2_escape.tmp
   JMS_SERVER=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   echo ${apps_dir} | sed 's/\//\\\//g' > .awips2_escape.tmp
   apps_dir=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   #================================================================================
   
   # EDEX localization related variables
   perl -p -i -e "s/export AW_SITE_IDENTIFIER=OAX/export AW_SITE_IDENTIFIER=${AW_SITE_IDENTIFIER}/g" \
      ${SETUP_ENV_NEW}
      
   # database names
   perl -p -i -e "s/export DC_DB_NAME=dc_ob7oax/export DC_DB_NAME=${DC_DB_NAME}/g" \
      ${SETUP_ENV_NEW}
   perl -p -i -e "s/export FXA_DB_NAME=fxatext/export FXA_DB_NAME=${FXA_DB_NAME}/g" \
      ${SETUP_ENV_NEW}
   perl -p -i -e "s/export HM_DB_NAME=hmdb/export HM_DB_NAME=${HM_DB_NAME}/g" \
      ${SETUP_ENV_NEW}
   perl -p -i -e "s/export IH_DB_NAME=hd_ob83oax/export IH_DB_NAME=${IH_DB_NAME}/g" \
      ${SETUP_ENV_NEW}
   
   # end of EDEX localization related variables
      
   # setup environment for HPE
   perl -p -i -e "s/export DATA_ARCHIVE_ROOT=\/tmp\/sbn/export DATA_ARCHIVE_ROOT=${DATA_ARCHIVE_ROOT}/g" \
      ${SETUP_ENV_NEW}
      
   # setup db connections
   perl -p -i -e "s/export DB_ADDR=localhost/export DB_ADDR=${DB_ADDR}/g" \
      ${SETUP_ENV_NEW}
   perl -p -i -e "s/export DB_PORT=5432/export DB_PORT=${DB_PORT}/g" \
      ${SETUP_ENV_NEW}
   
   # setup connection to qpid
   perl -p -i -e "s/export BROKER_ADDR=localhost/export BROKER_ADDR=${BROKER_ADDR}/g" \
      ${SETUP_ENV_NEW}
   
   # setup hdf5 connection if pypies is enabled
   perl -p -i -e "s/export PYPIES_SERVER=http:\/\/localhost:9582/export PYPIES_SERVER=${PYPIES_SERVER}/g" \
      ${SETUP_ENV_NEW}
   
   # moved here from environment.xml
   # these values are returned to clients that contact the localization service
   perl -p -i -e "s/export HTTP_SERVER=http:\/\/localhost:9581\/services/export HTTP_SERVER=${HTTP_SERVER}/g" \
      ${SETUP_ENV_NEW}
   perl -p -i -e "s/export JMS_SERVER=tcp:\/\/localhost:5672/export JMS_SERVER=${JMS_SERVER}/g" \
      ${SETUP_ENV_NEW}
   
   # set hydroapps directory path
   perl -p -i -e "s/export apps_dir=\\\$EDEX_HOME\/data\/hdf5\/hydroapps/export apps_dir=\\\$EDEX_HOME${apps_dir}/g" \
      ${SETUP_ENV_NEW}
   
   #================================================================================
   
   # Remove the existing setup.env.
   rm -f ${SETUP_ENV}
   
   # Rename setup.env.rpmnew to setup.env.
   mv ${SETUP_ENV_NEW} ${SETUP_ENV}
fi

#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
if [ "${1}" = "2" ]; then
   echo "INFO: Performing %{_component_name} Upgrade."
   echo "Preparing ..."
   
   # Check the delta directory to see if there are updates that
   # may need to be applied.
   cd ${RPM_INSTALL_PREFIX}/delta/%{_component_name}
   COUNT=`ls -1 | wc -l`
   
   if [ "${COUNT}" = "0" ]; then
      echo "INFO: No Updates To Perform."
      exit 0
   fi
   
   echo "INFO: Potentially Applying ${COUNT} Updates."
   
   # The Update Manager Is In: ${RPM_INSTALL_PREFIX}/delta
   UPDATE_MANAGER="${RPM_INSTALL_PREFIX}/delta/updateManager.sh"
   cd ${RPM_INSTALL_PREFIX}/delta
   export COMPONENT_INSTALL="${RPM_INSTALL_PREFIX}"
   ${UPDATE_MANAGER} %{_component_name}
   
   exit 0
fi
#---------------------------------------------------------------------------#

%postun
if [ "${1}" = "1" ]; then
   exit 0
fi
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| AWIPS II Edex Configuration Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
#---------------------------------------------------------------------------#
# Delta-Enabled RPM
#---------------------------------------------------------------------------#
%dir %{_component_default_prefix}/delta
%attr(700,root,root) %{_component_default_prefix}/delta/updateManager.sh
%attr(700,root,root) %{_component_default_prefix}/delta/createUpdateRegistry.sh
%{_component_default_prefix}/delta/%{_component_name}
#---------------------------------------------------------------------------#
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/bin
%config(noreplace) /awips2/edex/bin/setup.env
