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
requires:  awips2-qpid-java-broker
Obsoletes: awips2-edex-grib < 16.1.6
Obsoletes: awips2-edex-configuration

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
# create build root directory

mkdir -p %{_build_root}/awips2/edex/{bin,logs,webapps}
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p %{_build_root}/etc/init.d
if [ $? -ne 0 ]; then
   exit 1
fi

# Unidata programs 'edex' and 'qpidNotify.py'
/bin/cp -r %{_baseline_workspace}/rpms/awips2.edex/Installer.edex/programs/* ${RPM_BUILD_ROOT}/awips2/edex/bin/

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

%pre

%post
# Set ipaddress in setup.env and run chkconfig for init.d services
/awips2/edex/bin/edex setup > /dev/null 2>&1

# We need to create a link to the python shared library if it does not exist.
#pushd . > /dev/null 2>&1
#if [ -d /awips2/python/lib ]; then
#   cd /awips2/python/lib
#   if [ -L libpython.so ]; then
#      # Ensure that we are pointing to the correct shared library.
#      rm -f libpython.so
#   fi
#   if [ -f libpython2.7.so.1.0 ]; then
#      ln -s libpython2.7.so.1.0 libpython.so
#   fi
#fi
#popd > /dev/null 2>&1

if [ -f /etc/init.d/edex_camel ]; then
   /sbin/chkconfig --add edex_camel
fi

# determine if an installation of awips2-common-base is already present
# (CAVE has been installed before edex on a standalone machine)
if [ -d /awips2/.edex ]; then
   # copy the common-base contributions to the EDEX installation
   cp -r /awips2/.edex/* /awips2/edex
   
   # cleanup
   rm -rf /awips2/.edex
fi

SETUP_ENV="/awips2/edex/bin/setup.env"
SETUP_ENV_NEW="/awips2/edex/bin/setup.env.rpmnew"

function updateSetupEnv() {
   # args
   # 1 value of the variable to change
   # 2 name of the variable to change
   # 3 default value of the variable
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
   # rewrite the new setup.env with the existing configuration
   source ${SETUP_ENV}
   # update when a variable is added to or removed from setup.env.
   updateSetupEnv "${AW_SITE_IDENTIFIER}" "AW_SITE_IDENTIFIER" "OAX"
   updateSetupEnv "${DC_DB_NAME}" "DC_DB_NAME" "dc_ob7oax"
   updateSetupEnv "${FXA_DB_NAME}" "FXA_DB_NAME" "fxatext"
   updateSetupEnv "${HM_DB_NAME}" "HM_DB_NAME" "hmdb"
   updateSetupEnv "${IH_DB_NAME}" "IH_DB_NAME" "hd_ob92oax"
   updateSetupEnv "${DATA_ARCHIVE_ROOT}" "DATA_ARCHIVE_ROOT" "/tmp/sbn"
   updateSetupEnv "${DB_ADDR}" "DB_ADDR" "localhost"
   updateSetupEnv "${DB_PORT}" "DB_PORT" "5432"
   updateSetupEnv "${BROKER_ADDR}" "BROKER_ADDR" "localhost"
   updateSetupEnv "${PYPIES_SERVER}" "PYPIES_SERVER" "http://localhost:9582"
   updateSetupEnv "${HTTP_SERVER}" "HTTP_SERVER" "http://localhost:9581/services"
   updateSetupEnv "${JMS_SERVER}" "JMS_SERVER" "tcp://localhost:5672"
   updateSetupEnv "${SHARE_DIR}" "SHARE_DIR" "/awips2/edex/data/share"
   updateSetupEnv "${LDAD_EXTERNAL_HOME}" "LDAD_EXTERNAL_HOME" "/ldad"
   updateSetupEnv "${LDAD_EXTERNAL_PUBLIC}" "LDAD_EXTERNAL_PUBLIC" "/data/ldad/public"
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
%defattr(644,awips,fxalpha,755)
%dir /awips2/edex
%dir /awips2/edex/conf
/awips2/edex/conf/*
/awips2/edex/data/*
%dir /awips2/edex/etc
/awips2/edex/etc/*
%dir /awips2/edex/lib
/awips2/edex/lib/*
%dir /awips2/edex/logs
%dir /awips2/edex/webapps
%config(noreplace) /awips2/edex/bin/setup.env
%defattr(755,awips,fxalpha,755)
%dir /awips2/edex/bin
/awips2/edex/bin/*.sh
%attr(755,awips,fxalpha) /awips2/edex/bin/edex
%attr(755,awips,fxalpha) /awips2/edex/bin/qpidNotify.py
%attr(744,root,root) /etc/init.d/*
