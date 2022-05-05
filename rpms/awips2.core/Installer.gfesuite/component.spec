#
# AWIPS II gfesuite Spec File
#
Name: awips2-gfesuite
Summary: AWIPS II gfesuite Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

Obsoletes: awips2-gfesuite-server
Obsoletes: awips2-gfesuite-client

AutoReq: no
Provides: awips2-gfesuite
Requires: awips2-python
Requires: awips2-java
Requires: awips2-cave
Requires: xorg-x11-server-Xvfb

BuildRequires: awips2-ant
BuildRequires: awips2-java

%description
AWIPS II gfesuite Installation - Contains The AWIPS II gfesuite Component.

# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

%build

%install
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
if [ $? -ne 0 ]; then
   exit 1
fi

GFESUITE_PROJECT="com.raytheon.uf.tools.gfesuite"
GFESUITE_DEPLOY_SCRIPT="%{_baseline_workspace}/${GFESUITE_PROJECT}/deploy.xml"

/awips2/ant/bin/ant -f ${GFESUITE_DEPLOY_SCRIPT} \
   -Dinstall.dir=${RPM_BUILD_ROOT}/awips2/GFESuite \
   -Dinstaller=true
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: ant failed."
   exit 1
fi

# Create additional directories that are required.
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/exportgrids/primary
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/exportgrids/backup
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/products/ISC
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/products/ATBL
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/rsyncGridsToCWF/data
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/rsyncGridsToCWF/etc
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/rsyncGridsToCWF/log
if [ $? -ne 0 ]; then
   exit 1
fi


# Copy the profile.d scripts.
PROFILE_D_DIR="rpms/common/environment/awips2-gfesuite/profile.d"
cp %{_baseline_workspace}/${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

%post
SETUP_ENV="/awips2/GFESuite/bin/setup.env"
SETUP_ENV_NEW="${SETUP_ENV}.rpmnew"

function updateSetupEnv()
{
   # Arguments:
   # 1) name of the variable to change.
   # 2) default value of the variable.

   local VARIABLE="${1}"
   local DEFAULT="${2}"
   local VALUE=${!VARIABLE}

   if [ -z "${VALUE}" ]; then
      return
   fi

   VALUE=$(echo "${VALUE}" | sed 's|/|\\/|g')
   DEFAULT=$(echo "${DEFAULT}" | sed 's|/|\\/|g')

   perl -p -i -e "s/export ${VARIABLE}=\\$\{${VARIABLE}:\-${DEFAULT}\}/export ${VARIABLE}=\\$\{${VARIABLE}:\-${VALUE}\}/g" \
      ${SETUP_ENV_NEW}
}

if [ -f "${SETUP_ENV_NEW}" ]; then
   # rewrite the new setup.env with the existing
   # configuration - provided as a convenience.
   source ${SETUP_ENV}

   # update when a variable is added to or removed from setup.env.
   updateSetupEnv "DEFAULT_HOST" "localhost"
   updateSetupEnv "JMS_HOST" "localhost"

   # Remove the existing setup.env.
   rm -f ${SETUP_ENV}

   # Rename setup.env.rpmnew to setup.env.
   mv ${SETUP_ENV_NEW} ${SETUP_ENV}
fi

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,root,root,-)
/etc/profile.d/*

%defattr(644,awips,fxalpha,755)
%dir /awips2/GFESuite
%dir /awips2/GFESuite/ServiceBackup
%dir /awips2/GFESuite/ServiceBackup/configuration
/awips2/GFESuite/ServiceBackup/configuration/*
%defattr(755,awips,fxalpha,777)
%dir /awips2/GFESuite/ServiceBackup/scripts
/awips2/GFESuite/ServiceBackup/scripts/*

%defattr(755,awips,fxalpha,755)
%dir /awips2/GFESuite/rsyncGridsToCWF
%dir /awips2/GFESuite/rsyncGridsToCWF/bin
/awips2/GFESuite/rsyncGridsToCWF/bin/*
%defattr(755,awips,fxalpha,775)
%dir /awips2/GFESuite/rsyncGridsToCWF/data
%dir /awips2/GFESuite/rsyncGridsToCWF/etc
%dir /awips2/GFESuite/rsyncGridsToCWF/log
%dir /awips2/GFESuite/rsyncGridsToCWF/config
/awips2/GFESuite/rsyncGridsToCWF/config/*
%defattr(644,awips,fxalpha,775)
%dir /awips2/GFESuite/nwps
/awips2/GFESuite/nwps/*
%defattr(775,awips,fxalpha,775)
%dir /awips2/GFESuite/hti
/awips2/GFESuite/hti/*
%dir /awips2/GFESuite/nwps/bin
/awips2/GFESuite/nwps/bin/*
%dir /awips2/GFESuite/nwps/etc
/awips2/GFESuite/nwps/etc/*
%dir /awips2/GFESuite/nwps/domains
/awips2/GFESuite/nwps/domains/*
%defattr(755,awips,fxalpha,755)
%dir /awips2/GFESuite/bin
%config(noreplace) /awips2/GFESuite/bin/setup.env
/awips2/GFESuite/bin/*
%dir /awips2/GFESuite/hti/bin
/awips2/GFESuite/hti/bin/*
%defattr(755,awips,fxalpha,777)
%dir /awips2/GFESuite/hti/etc
/awips2/GFESuite/hti/etc/*
%defattr(644,awips,fxalpha,755)
%dir /awips2/GFESuite/bin/src
/awips2/GFESuite/bin/src/*
%dir /awips2/GFESuite/exportgrids
/awips2/GFESuite/exportgrids/*
%defattr(644,awips,fxalpha,775)
%dir /awips2/GFESuite/products
/awips2/GFESuite/products/*
