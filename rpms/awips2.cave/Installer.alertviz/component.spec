%define __prelink_undo_cmd %{nil}
# Turn off the brp-python-bytecompile script
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')

%define _swt_version 3.104.1.v20150825-0743

#
# AWIPS II AlertViz Spec File
#
Name: awips2-alertviz
Summary: AWIPS II AlertViz
Version: %{_component_version}
Release: %{_component_release}%{?dist}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Provides: awips2-alertviz
Requires: awips2-python
Requires: awips2-java

BuildRequires: awips2-ant
BuildRequires: awips2-java

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

%build
pushd . > /dev/null
# Build alertviz.
cd %{_baseline_workspace}/build
if [ $? -ne 0 ]; then
   exit 1
fi

/awips2/ant/bin/ant -f build.xml \
   -Declipse.dir=%{_uframe_eclipse} \
   alertviz
if [ $? -ne 0 ]; then
   exit 1
fi
popd > /dev/null

%install
/bin/mkdir -p %{_build_root}/awips2/alertviz/alertvizEnvironment
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/etc/xdg/autostart
if [ $? -ne 0 ]; then
   exit 1
fi
/bin/mkdir -p %{_build_root}/etc/gdm/PostSession
if [ $? -ne 0 ]; then
   exit 1
fi

alertviz_zip="AlertViz-linux.gtk.x86_64.zip"

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

# install the gnome session kill script for cave and alertviz
script_="%{_baseline_workspace}/build/static/linux/cave/awips2VisualizeUtility.sh"
/bin/cp ${script_} %{_build_root}/etc/gdm/PostSession

# add the license information.
license_dir="%{_baseline_workspace}/rpms/legal"

cp "${license_dir}/Master_Rights_File.pdf" \
   %{_build_root}/awips2/alertviz
if [ $? -ne 0 ]; then
   exit 1
fi

%pre

%post
echo -e "\nInstalling A2 gdm PostSession Default script"
scp /etc/gdm/PostSession/awips2VisualizeUtility.sh /etc/gdm/PostSession/Default

pushd . > /dev/null 2>&1
cd /awips2/alertviz/plugins
# Forcefully unzip: org.eclipse.swt.gtk.linux.x86_64_*.jar
# : if x86_64
if [ -f org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar ]; then
   mkdir org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}
   unzip -qq org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar \
      -d org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}
   rm -f org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar
   mv org.eclipse.swt.gtk.linux.x86_64_%{_swt_version} \
      org.eclipse.swt.gtk.linux.x86_64_%{_swt_version}.jar
fi

popd > /dev/null 2>&1

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
%dir /awips2/alertviz/features
/awips2/alertviz/features/*
%doc /awips2/alertviz/*.pdf
%dir /awips2/alertviz/plugins
/awips2/alertviz/plugins/*

%defattr(755,awips,fxalpha,755)
/awips2/alertviz/alertviz
/awips2/alertviz/*.so
/awips2/alertviz/*.sh

%attr(644,root,root) /etc/xdg/autostart/awips2-alertviz.desktop
%attr(644,root,root) /etc/gdm/PostSession/awips2VisualizeUtility.sh
