#
# AWIPS II gfesuite client Spec File
#
Name: awips2-gfesuite-client
Summary: AWIPS II gfesuite client Installation
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Provides: awips2-gfesuite-client
Requires: awips2-python
Requires: awips2-cave
Requires: awips2-java

BuildRequires: awips2-ant
BuildRequires: awips2-java

%description
AWIPS II gfesuite-client Installation - Contains The AWIPS II gfesuite-client Component.

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

GFESUITE_PROJECT="com.raytheon.uf.tools.gfesuite"
GFESUITE_DEPLOY_SCRIPT="%{_baseline_workspace}/${GFESUITE_PROJECT}/deploy.xml"

/awips2/ant/bin/ant -f ${GFESUITE_DEPLOY_SCRIPT} \
   -Dinstall.dir=${RPM_BUILD_ROOT}/awips2/GFESuite \
   -Dinstaller=true -Dclient.build=true
if [ $? -ne 0 ]; then
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

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,root,root,-)
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
%defattr(644,awips,fxalpha,775)
%dir /awips2/GFESuite/products
/awips2/GFESuite/products/*
