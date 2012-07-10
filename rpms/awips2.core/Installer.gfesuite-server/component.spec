#
# AWIPS II gfesuite server Spec File
#
Name: awips2-gfesuite-server
Summary: AWIPS II gfesuite server Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-gfesuite-server
requires: awips2-python
requires: awips2-edex-gfe

%description
AWIPS II gfesuite-client Installation - Contains The AWIPS II gfesuite-server Component.

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

mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d

%build
# Verify that awips2-ant is installed.
if [ ! -f /awips2/ant/bin/ant ]; then
   echo "ERROR: Unable to find the awips2-ant executable."
   exit 1
fi

%install
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
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/exportgrids/tmp
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/exportgrids2
if [ $? -ne 0 ]; then
   exit 1
fi
mkdir -p ${RPM_BUILD_ROOT}/awips2/GFESuite/products/ISC
if [ $? -ne 0 ]; then
   exit 1
fi


# Copy the profile.d scripts.
PROFILE_D_DIR="rpms/common/environment/awips2-gfesuite/profile.d"
cp %{_baseline_workspace}/${PROFILE_D_DIR}/* ${RPM_BUILD_ROOT}/etc/profile.d

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,root,root,-)
/etc/profile.d/*
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/GFESuite
/awips2/GFESuite/*
%defattr(755,awips,fxalpha,755)
%dir /awips2/GFESuite/bin
/awips2/GFESuite/bin/*
%defattr(644,awips,fxalpha,755)
%dir /awips2/GFESuite/bin/src
/awips2/GFESuite/bin/src/*
%dir /awips2/GFESuite/exportgrids
/awips2/GFESuite/exportgrids/*
%dir /awips2/GFESuite/exportgrids2
%defattr(755,awips,fxalpha,777)
%dir /awips2/GFESuite/ServiceBackup/scripts
/awips2/GFESuite/ServiceBackup/scripts/*
%config(noreplace) /awips2/GFESuite/ServiceBackup/configuration/svcbu.properties
%defattr(644,awips,fxalpha,775)
%dir /awips2/GFESuite/products
/awips2/GFESuite/products/*