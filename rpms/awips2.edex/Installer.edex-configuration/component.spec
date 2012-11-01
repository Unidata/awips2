#
# AWIPS II edex-configuration Spec File
#
Name: awips2-edex-configuration
Summary: AWIPS II Edex
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: noarch
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
mkdir -p %{_build_root}/awips2/edex
if [ $? -ne 0 ]; then
   exit 1
fi

%build

%install
DEPLOY_SCRIPT="build.edex/deploy-common/deploy-esb-configuration.xml"

# use deploy-install to deploy edex-configuration.
pushd . > /dev/null
cd %{_baseline_workspace}
/awips2/ant/bin/ant -f ${DEPLOY_SCRIPT} \
   -Desb.overwrite=true \
   -Desb.directory=%{_baseline_workspace}/build.edex/esb \
   -Dedex.root.directory=${RPM_BUILD_ROOT}/awips2/edex
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null

%pre

%post

SETUP_ENV="/awips2/edex/bin/setup.env"
SETUP_ENV_NEW="/awips2/edex/bin/setup.env.rpmnew"

function updateSetupEnv()
{
   # Arguments:
   # 1) value of the variable to change.
   # 2) name of the variable to change.
   # 3) default value of the variable.
   
   local VALUE="${1}"
   local VARIABLE="${2}"
   local DEFAULT="${3}"   
   
   echo "${VALUE}" | sed 's/\//\\\//g' > .awips2_escape.tmp
   VALUE=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   echo "${DEFAULT}" | sed 's/\//\\\//g' > .awips2_escape.tmp
   DEFAULT=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   
   perl -p -i -e "s/export ${VARIABLE}=${DEFAULT}/export ${VARIABLE}=${VALUE}/g" \
      ${SETUP_ENV_NEW}
}

if [ -f "${SETUP_ENV_NEW}" ]; then
   # rewrite the new setup.env with the existing
   # configuration - provided as a convenience.
   source ${SETUP_ENV}
   
   # update when a variable is added to or removed from setup.env.
   updateSetupEnv "${AW_SITE_IDENTIFIER}" "AW_SITE_IDENTIFIER" "OAX"
   updateSetupEnv "${DC_DB_NAME}" "DC_DB_NAME" "dc_ob7oax"
   updateSetupEnv "${FXA_DB_NAME}" "FXA_DB_NAME" "fxatext"
   updateSetupEnv "${HM_DB_NAME}" "HM_DB_NAME" "hmdb"
   updateSetupEnv "${IH_DB_NAME}" "IH_DB_NAME" "hd_ob83oax"
   updateSetupEnv "${DATA_ARCHIVE_ROOT}" "DATA_ARCHIVE_ROOT" "/tmp/sbn"
   updateSetupEnv "${DB_ADDR}" "DB_ADDR" "localhost"
   updateSetupEnv "${DB_PORT}" "DB_PORT" "5432"
   updateSetupEnv "${BROKER_ADDR}" "BROKER_ADDR" "localhost"
   updateSetupEnv "${PYPIES_SERVER}" "PYPIES_SERVER" "http://localhost:9582"
   updateSetupEnv "${HTTP_SERVER}" "HTTP_SERVER" \
      "http://localhost:9581/services"
   updateSetupEnv "${JMS_SERVER}" "JMS_SERVER" "tcp://localhost:5672"
   updateSetupEnv "${RADAR_SERVER}" "RADAR_SERVER" "tcp://localhost:8813"
   updateSetupEnv "${SHARE_DIR}" "SHARE_DIR" "/awips2/edex/data/share"
   updateSetupEnv "${LDAD_EXTERNAL_HOME}" "LDAD_EXTERNAL_HOME" \
      "/ldad"
   updateSetupEnv "${LDAD_EXTERNAL_PUBLIC}" "LDAD_EXTERNAL_PUBLIC" \
      "/data/ldad/public"
      
   # Remove the existing setup.env.
   rm -f ${SETUP_ENV}
   
   # Rename setup.env.rpmnew to setup.env.
   mv ${SETUP_ENV_NEW} ${SETUP_ENV}
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/edex
%dir /awips2/edex/bin
%config(noreplace) /awips2/edex/bin/setup.env