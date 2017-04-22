#
# Spec file for AWIPS II rehost support.
#
# Curently, this is just a JDBC driver
#
%define _prefix /usr/local/awips2-postgresql
%define _libdir %{_prefix}/lib
%define source_jar_file postgresql-9.3-1104.jdbc4.jar

Name: awips2-rehost-support-postgresql
Summary: AWIPS II Rehost Support (PostgreSQL)
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildArch: noarch
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}

AutoReq: no

%description
Provides support for rehosted software under AWIPS II.  Specifically,
this package contains a PostgreSQL JDBC driver.

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
dest="${RPM_BUILD_ROOT}"/%{_libdir}
mkdir -p "$dest" \
    && cp %{_baseline_workspace}/org.postgres/%{source_jar_file} "$dest" \
    || exit 1

%pre
%post
%preun
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,root,root,755)
# Own the prefix dir because it is non-standard and nothing else should
# be installed there.
%dir %{_prefix}
%dir %{_libdir}
%{_libdir}/%{source_jar_file}
