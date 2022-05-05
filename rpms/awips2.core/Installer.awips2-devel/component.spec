# RPM Metadata
# Other vars:
#   %{_component_build_date}
#   %{_component_build_time}
#   %{_component_build_system}
#
# awips2 Spec File
#

Name: awips2-devel
Summary: awips2 Installation
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

AutoReq: no
Requires: awips2-ant
Requires: awips2-eclipse
Requires: awips2-localization-OAX
Requires: awips2-rcm
Requires: awips2-edex
Requires: awips2-postgresql
Requires: awips2-qpid-broker-j
Requires: awips2-httpd-pypies
Requires: awips2-bmh
Requires: awips2-ignite

%description
AWIPS2 development support

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

mkdir -p ${RPM_BUILD_ROOT}

%post
# disable starting edex services at start up
/sbin/chkconfig edex_rcm off
/sbin/chkconfig edex_camel off
/sbin/chkconfig edex_postgres off
/sbin/chkconfig qpidd off
/sbin/chkconfig httpd-pypies off
/sbin/chkconfig htcacheclean-pypies off
/sbin/chkconfig comms_manager off
/bin/systemctl disable --now --quiet ignite@production

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)

