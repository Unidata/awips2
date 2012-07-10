%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')
%define _build_arch %(uname -i)

#
# AWIPS II AlertViz Spec File
#
Name: awips2-alertviz
Summary: AWIPS II AlertViz
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
BuildArch: %{_build_arch}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-alertviz
requires: awips2-python
requires: awips2-java

%description
AWIPS II AlertViz Distribution - the AWIPS II AlertViz application.

%prep
# Ensure that a "buildroot" has been specified.
if [ "%{_build_root}" = "" ]; then
   echo "ERROR: A BuildRoot has not been specified."
   echo "FATAL: Unable to Continue ... Terminating."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm -rf %{_build_root}
fi
/bin/mkdir %{_build_root}
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir %{_build_root}/awips2
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/awips2/alertviz/alertvizEnvironment
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/etc/xdg/autostart
if [ $? -ne 0 ]; then
   exit 1
fi

%build
build_arch=%{_build_arch}
if [ "${build_arch}" = "i386" ]; then
   build_arch="x86"
fi

pushd . > /dev/null
# Build alertviz.
cd %{_baseline_workspace}/build
if [ $? -ne 0 ]; then
   exit 1
fi

/awips2/ant/bin/ant -f build.xml \
   -Declipse.dir=%{_uframe_eclipse} \
   -Dbuild.arch=${build_arch} \
   alertviz
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null

%install
build_arch=%{_build_arch}
if [ "${build_arch}" = "i386" ]; then
   build_arch="x86"
fi
alertviz_zip="AlertViz-linux.gtk.${build_arch}.zip"

pushd . > /dev/null
cd %{_baseline_workspace}/build/alertviz/tmp/I.AlertViz
/usr/bin/unzip ${alertviz_zip} -d %{_build_root}/awips2
if [ $? -ne 0 ]; then
   exit 1
fi

# install the alertviz autostart script.
viz_rpm_dir="%{_baseline_workspace}/rpms/awips2.cave"
alertviz_project="${viz_rpm_dir}/Installer.alertviz"
script_="${alertviz_project}/scripts/autostart/awips2-alertviz.desktop"
/bin/cp ${script_} %{_build_root}/etc/xdg/autostart

# add the license information.
license_dir="%{_baseline_workspace}/rpms/legal"
cp "${license_dir}/license.txt" \
   %{_build_root}/awips2/alertviz
if [ $? -ne 0 ]; then
   exit 1
fi

cp "${license_dir}/Master Rights File.pdf" \
   %{_build_root}/awips2/alertviz
if [ $? -ne 0 ]; then
   exit 1
fi

%pre

%post
# Remove the alertviz autostart script if we have been
# installed on an xt machine.
if [ "`hostname | cut -b 1-2`" = "xt" ]; then
   # Remove the awips2-alertviz autostart script.
   rm -f /etc/xdg/autostart/awips2-alertviz.desktop
fi

%preun
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(644,awips,fxalpha,755)
%dir /awips2
%dir /awips2/alertviz
/awips2/alertviz/.eclipseproduct
%docdir /awips2/alertviz/about_files
%dir /awips2/alertviz/about_files
/awips2/alertviz/about_files/*
%doc /awips2/alertviz/about.html
%dir /awips2/alertviz/alertvizEnvironment
/awips2/alertviz/alertviz.ini
%dir /awips2/alertviz/configuration
/awips2/alertviz/configuration/*
%dir /awips2/alertviz/etc
/awips2/alertviz/etc/*
%dir /awips2/alertviz/features
/awips2/alertviz/features/*
%doc /awips2/alertviz/*.pdf
%doc /awips2/alertviz/license.txt
%dir /awips2/alertviz/plugins
/awips2/alertviz/plugins/*

%defattr(755,awips,fxalpha,755)
/awips2/alertviz/alertviz
/awips2/alertviz/*.so
/awips2/alertviz/*.sh

%attr(644,root,root) /etc/xdg/autostart/awips2-alertviz.desktop