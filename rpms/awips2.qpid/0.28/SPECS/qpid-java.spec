%define _awips2_directory "/awips2/qpid"

Name:           awips2-qpid-java
Version:        0.28
Release:        5%{?dist}
Summary:        Java implementation of Apache Qpid
License:        Apache Software License
Group:          Development/Java
URL:            http://qpid.apache.org/

%global qpid_src_dir qpid-%{version}
%global qpid_deps_src_dir qpid-deps-%{version}

Source0:        %{qpid_src_dir}.tar.gz

Patch0:         build.patch
Patch1:         examples.patch
Patch2:         awips.patch

BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
BuildArch:      noarch
BuildRequires:  ant >= 1.6.5
BuildRequires:  ant-nodeps >= 1.6.5
BuildRequires:  java-devel >= 1.6.0
BuildRequires:  ant-trax >= 1.6.5

%description
Java implementation of Apache Qpid.

%package common
Summary:	Java implementation of Apache Qpid - common files
Group: 		Development/Java
BuildArch:	noarch

%description common
Java implementation of Apache Qpid - common files

%package client
Summary:	Java implementation of Apache Qpid - client
Group: 		Development/Java
BuildArch:	noarch
Requires:	awips2-qpid-java-common = %{version}-%{release}
Requires:	log4j >= 1.2.12

%description client
Java implementation of Apache Qpid - client

%package example
Summary:        Java implementation of Apache Qpid - example
Group:          Development/Java
BuildArch:      noarch
Requires:       awips2-qpid-java-client = %{version}-%{release}

%description example
Java implementation of Apache Qpid - example

%prep
%setup -q -n %{qpid_src_dir}
mkdir -p java/lib/required
# copy baseline libraries and other libraries to required
for dependency in `cat %{_topdir}/SOURCES/awips2/dependencies.txt`;
do
   cp -f %{_baseline_workspace}/${dependency} java/lib/required
   if [ $? -ne 0 ]; then
      exit 1
   fi
done
# copy dependencies that are not native to the baseline to required
cp -f %{_topdir}/SOURCES/awips2/*.jar %{_topdir}/SOURCES/awips2/*.zip  java/lib/required
if [ $? -ne 0 ]; then
   exit 1
fi

%patch0 -p2
%patch1 -p2
# apply the awips patch
%patch2 -p2

%build
cd ..

(
    cd %{qpid_src_dir}/java
    for module in ${QPID_MODULES[*]}; do
       ant -Dretrieve.dependencies=false \
           -Dmodules=${module}
    done
    ant -Dretrieve.dependencies=false \
       -Dmodules=qpid-test-utils\ common\ management/common\ client\ amqp-1-0-common\ amqp-1-0-client\ amqp-1-0-client-jms\ jca

    if [ $? -ne 0 ]; then
       exit 1
    fi

    # blacklisted jars are either provided by the Requires: or not needed.
    BLACKLIST="slf4j qpid-client-tests qpid-all qpid-common-tests"
    for jar in $BLACKLIST; do rm build/lib/${jar}*.jar; done
)

%install
rm -rf %{buildroot}

cd ..

install -dm 755 %{buildroot}%{_awips2_directory}/lib/opt
install -pm 644 %{qpid_src_dir}/java/build/lib/qpid-client-%{version}.jar \
	%{buildroot}%{_awips2_directory}/lib/opt
install -pm 644 %{qpid_src_dir}/java/build/lib/qpid-common-%{version}.jar \
	%{buildroot}%{_awips2_directory}/lib/opt
install -pm 644 %{qpid_src_dir}/java/build/lib/qpid-management-common-%{version}.jar \
        %{buildroot}%{_awips2_directory}/lib/opt

# foss dependencies
install -dm 755 %{buildroot}%{_awips2_directory}/lib/opt/qpid-deps
install -pm 644 %{qpid_src_dir}/java/lib/required/*.jar \
	%{buildroot}%{_awips2_directory}/lib/opt/qpid-deps

# license & notice
install -pm 644 %{qpid_src_dir}/java/resources/LICENSE \
	%{buildroot}%{_awips2_directory}
install -pm 644 %{qpid_src_dir}/java/resources/NOTICE \
	%{buildroot}%{_awips2_directory}

# examples

install -dm 755 %{buildroot}%{_awips2_directory}/doc/%{name}-%{version}/examples/org/apache/qpid/example/
install -pm 644 %{qpid_src_dir}/java/client/example/src/main/java/org/apache/qpid/example/*.java \
        %{buildroot}%{_awips2_directory}/doc/%{name}-%{version}/examples/org/apache/qpid/example/
install -pm 644 %{qpid_src_dir}/java/client/example/src/main/java/org/apache/qpid/example/hello.properties \
        %{buildroot}%{_awips2_directory}/doc/%{name}-%{version}/examples/org/apache/qpid/example/
install -pm 644 %{qpid_src_dir}/java/client/example/example.log4j \
        %{buildroot}%{_awips2_directory}/doc/%{name}-%{version}/examples/
install -pm 755 %{qpid_src_dir}/java/client/example/bin/run_example.sh \
        %{buildroot}%{_awips2_directory}/doc/%{name}-%{version}/examples/

%clean
rm -rf %{buildroot}

%files common
%defattr(-,awips,fxalpha,-)
%dir /awips2/qpid
%dir /awips2/qpid/lib
%dir /awips2/qpid/lib/opt
/awips2/qpid/lib/opt/qpid-common-%{version}.jar
/awips2/qpid/lib/opt/qpid-management-common-%{version}.jar
%dir /awips2/qpid/lib/opt/qpid-deps
/awips2/qpid/lib/opt/qpid-deps/*
%doc /awips2/qpid/LICENSE
%doc /awips2/qpid/NOTICE

%files client
%defattr(-,awips,fxalpha,-)
%dir /awips2/qpid
%dir /awips2/qpid/lib
%dir /awips2/qpid/lib/opt
/awips2/qpid/lib/opt/qpid-client-%{version}.jar

%files example
%defattr(-,awips,fxalpha,-)
/awips2/qpid/doc/%{name}-%{version}/examples/

%changelog
* Thu Sep  6 2012 Irina Boverman <iboverma@redhat.com> - 0.18-2
- Resolved bz 851574

* Tue Jul 10 2012 Justin Ross <jross@redhat.com> - 0.18-1
- Refactored spec file to use standard Qpid source export
- Rebased to Qpid 0.18 beta source

* Thu Feb 23 2012 Rajith Attapattu <rattapat@redhat.com> - 0:0.14-3
- Added a patch to account for changes between 0.14-rc1 and HEAD of 0.14-mrg-preview.

* Thu Feb 16 2012 Rajith Attapattu <rattapat@redhat.com> - 0:0.14-2
- Added a patch to account for changes between 0.14-rc1 and HEAD of 0.14-mrg-preview.

* Mon Dec 12 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.14-1
- Rebased the client to use the Qpid 0.14 RC1 source.
- Added a patch to account for changes between 0.14-rc1 and HEAD of 0.14-mrg-preview.

* Fri Sep 23 2011 Andrew Stitcher <astitcher@redhat.com> - 0:0.10-11
- Added new qpid-java-jca-zip package which wraps a zip of everything
  in qpid jca.
- Make qpid-deps jar have correct 0.10 version
- Updated patch to include latest Qpid JCA work:
  BZ.738316
  BZ 733383
  BZ 736252
  BZ 700500
  BZ 707535
  BZ 722614
  BZ 735030
  BZ 738615
  BZ 700494
  BZ 705913
  BZ 723714
  BZ 735384
  BZ 736794
  BZ 737880
  BZ 735322
  BZ 736785

* Tue Sep 14 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.10-9
- Added a patch to account for changes between 0.10 and mrg_2.0.3 head rev on Sep 14th.

* Mon May 16 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.10.-6
- Added a patch to account for changes between 0.10 and qpid-0.10-package-set-7 tags.

* Thu Apr 28 2011 Andrew Stitcher <astitcher@redhat.com> - 0:0.10.-5
- Removed jar file that was not meant to be in qpid-java-common

* Wed Apr 20 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.10.-4
- Added a patch to account for changes between 0.10 and qpid-0.10-package-set-5 tags.

* Thu Apr 14 2011 Ted Ross <tross@redhat.com> - 0:0.10-3
- Added a patch to fix BZ694617

* Wed Apr 6 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.10.-2
- Added a patch to include QPID-3177 which is included in the upstream 0.10 release.
- Fixed an error in the qpid-deps source tarball.

* Thu Mar 24 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.10.-1
- Change the version from 0.10.1083082 to 0.10.

* Wed Mar 23 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.10.1083082-1
- Updated the source files to rev 1083082 in Qpid 0.10 branch.  

* Thu Mar 10 2011 Andrew Stitcher <astitcher@redhat.com> - 0:0.9.1080013-3
- Fixed issue that stopped deployment on JBoss 5
- Added sample deployment resource
- Added some JCA example code
- Added separate Readme for JBoss deployment

* Thu Mar 10 2011 Andrew Stitcher <astitcher@redhat.com> - 0:0.9.1080013-2
- Updated JCA package added some documentation to it.
- Add qpid-deps jar into the qpid-ra rar file to supply dependencies for
  qpid-common/qpid-client jars

* Wed Mar 9 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.9.1080013-1
- Updated the source files to rev 1080013 in Qpid 0.10 branch.

* Tue Mar 8 2011 Andrew Stitcher <astitcher@redhat.com> - 0:0.9.1073306-3
- Changed JCA build to use source for jar dependents

* Wed Mar 2 2011 Andrew Stitcher <astitcher@redhat.com> - 0:0.9.1073306-2
- Added new sub-package for JCA resource adapter

* Thu Feb 24 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.9.1073306-1
- Updated the source files to rev 1073306 in Qpid trunk.

* Thu Jan 27 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-15
- Applied another patch for BZ 656488, 672212.

* Wed Jan 12 2011 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-14
- Applied patches for BZ 667428, 656488.

* Wed Nov 10 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-12
- Applied patches for BZ 645855

* Tue Oct 12 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-11
- Added "Obsoletes: qpid-java-qman <= 0.5.751061-9.el5" to the spec file. 

* Wed Sep 22 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-10
- Applied patches for BZ 634794, 636097, 633969.

* Mon Sep 13 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-9
- Applied patches for BZ 626859.

* Mon Aug 30 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-8
- Applied patches for BZ 620808, 621395, 622619.

* Thu Jul 29 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-7
- Applied patches for BZ 616457, 614580, 618865, 618822, 614589, 513426, 619242.

* Mon Jul 12 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-6
- Applied patches for bz612531, bz612526, bz612535.

* Wed Jun 30 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-5
- Patches are now generated from git.
- Added a fix for bz608052
- Configured the javac target to 1.5 for dependencies

* Tue Jun 15 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-4
- For dependencies that are not available as rpms, they are now built from source.
- For commons lang, the required classes are now added as a patch, removing the required dependency.

* Mon May 24 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-3
- Fixed an error in the run_example script
- Added LICENSE and NOTICE to common rpm

* Mon May 24 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-2
- Added Drain and Spout examples
- Removed dependency from sl4j rpm, instead using the jar in qpid-deps

* Thu May 20 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.946106-1
- Updated to rev 946106 of Qpid trunk

* Thu Apr 15 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.934605-1
- Updated to rev 934605 of Qpid trunk

* Tue Mar 2 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.918215-1
- Updated to rev 918215 of Qpid trunk

* Wed Feb 3 2010 Rajith Attapattu <rattapat@redhat.com> - 0:0.7.906145-1
- Updated to rev 906145 of Qpid trunk
- Stripped the qman package. The java broker is also not built anymore.
  
* Fri Oct 9 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.751061-9
- Removing patch attached to bz494630
- Applying patch attached to bz493559
- Applying patch attached to bz509395

* Tue Jun 25 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.751061-8
- Applying patch attached to bz506739
- Applying patch attached to bz507369

* Mon Jun  8 2009 Rafael Schloming <rafaels@redhat.com> - 0:0.5.751061-7
- Applying the heartbeat echo patch from bz504590

* Thu Jun 04 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.751061-6
- Applying the modified patch attached to bz503539

* Wed Jun 03 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.751061-5
- Applying patch attached to bz503526
- Applying patch attached to bz503539

* Thu May 21 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.751061-4
- Fixed an error in the spec file to apply all patches correctly

* Wed May 20 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.751061-3
- Applying patch attached to bz500146
- Applying patch attached to bz501552

* Wed Apr 8 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.751061-2
- Going back to revision 751061 of Qpid trunk
- Applying patch attached to bz494630

* Fri Mar 27 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.758820-1
- Updated to revision 758820 of the Qpid trunk

* Thu Mar 12 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.5.751061-1
- Updated to revision 751061 of the Qpid trunk

* Wed Mar 04 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.4.750205-1
- Updated to revision 750205 of the Qpid trunk

* Fri Feb 13 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.4.743367-1
- Updated to revision 743367 of the Qpid trunk

* Mon Feb 09 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.4.742278-1
- Updated to revision 742278 of the Qpid trunk

* Wed Jan 28 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.4.738568-1
- Updated to revision 738568 of the Qpid trunk
- Removed thread patch as it's comitted to trunk

* Fri Jan 9 2009 Rajith Attapattu <rattapat@redhat.com> - 0:0.3.733043-2
- Updated to revision 733043 of the Qpid M4-RCs branch

* Mon Nov 19 2008 Rajith Attapattu <rattapat@redhat.com> - 0:0.3.724504-1
- Updated to revision 724504 of the qpid trunk
- Added the Qman package
- Applied the thread abstraction patch

* Mon Nov 10 2008 Rajith Attapattu <rattapat@redhat.com> - 0:0.3.712662-1
- Updated to revision 712662 of the qpid trunk

* Fri Oct 31 2008 Nuno Santos <nsantos@redhat.com> - 0:0.3.709187-1
- Rebased to svn rev 709187

* Mon Oct 28 2008 Rajith Attapattu <rattapat@redhat.com> - 0:0.3.708221-1
- Updated to revision 708221 of the qpid trunk

* Fri Aug 25 2008 Justin Ross <jross@redhat.com> - 0:0.2.687156-1
- Updated to revision 687156 of the qpid.0-10 branch

* Fri Aug 15 2008 Justin Ross <jross@redhat.com> - 0:0.2.686136-1
- Updated to revision 686136 of the qpid.0-10 branch

* Mon Jun 16 2008 Justin Ross <jross@redhat.com> - 0:0.2.668333-1
- Updated to source revision 668333

* Fri Jun 13 2008 Arnaud Simon <asimon@redhat.com> - 0:0.2.667615-1
- Updated to source revision 667615

* Mon Jun 11 2008 Arnaud Simon <asimon@redhat.com> - 0:0.2.666296-2
- Updated common dep and removed package broker

* Mon Jun 10 2008 Justin Ross <jross@redhat.com> - 0:0.2.666296-1
- Updated to source revision 666296

* Mon Jun 9 2008 Arnaud Simon <asimon@redhat.com> - 0:0.2.665769-1
- Updated to source revision 665769

* Thu May 15 2008 Justin Ross <jross@redhat.com> - 0:0.2.656760-1
- Updated source tarballs instructions
- Added svn revision number to version
- Updated to source revision 656760

* Tue May 13 2008 Arnaud Simon  <asimon@redhat.com> - 0:0.2-11
- changed version for solving version conflic

* Tue May 13 2008 Arnaud Simon  <asimon@redhat.com> - 0:0.2-10
- changed some default network configuration 

* Tue May 13 2008 Arnaud Simon  <asimon@redhat.com> - 0:0.2-9
- Bumped release for Beta 4

* Fri Feb 15 2008  Rafael Schloming <rafaels@redhat.com> - 0:0.2-9
- fix for deadlock exposed by TCK

* Wed Feb 13 2008  Rafael Schloming <rafaels@redhat.com> - 0:0.2-8
- More bug fixes for Beta 3

* Tue Feb 12 2008 Rafael Schloming <rafaels@redhat.com> - 0:0.2-7
- Bumped release for Beta 3 bug fixes

* Mon Feb 11 2008 Rafael Schloming <rafaels@redhat.com> - 0:0.2-6
- Bumped release for Beta 3

* Mon Feb 11 2008 Nuno Santos <nsantos@redhat.com> - 0.2-5
- Initial build.

