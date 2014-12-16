%define _awips2_directory "/awips2/qpid"

Name:           awips2-qpid-java
Version:        0.30
Release:        1%{?dist}
Summary:        Java implementation of Apache Qpid
License:        Apache Software License
Group:          Development/Java
URL:            http://qpid.apache.org/

%global qpid_src_dir qpid-client-%{version}-bin
%global qpid_client_dir qpid-client

Source:        %{qpid_src_dir}.tar.gz

BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
BuildArch:      noarch

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

%prep
%setup -q -n %{qpid_client_dir}

%build

%install
rm -rf %{buildroot}

cd ..

install -dm 755 %{buildroot}%{_awips2_directory}/lib/opt
install -pm 644 %{qpid_client_dir}/%{version}/lib/qpid-client-%{version}.jar \
	%{buildroot}%{_awips2_directory}/lib/opt
install -pm 644 %{qpid_client_dir}/%{version}/lib/qpid-common-%{version}.jar \
	%{buildroot}%{_awips2_directory}/lib/opt
install -pm 644 %{qpid_client_dir}/%{version}/lib/geronimo-jms_1.1_spec-1.1.1.jar \
	%{buildroot}%{_awips2_directory}/lib/opt
install -pm 644 %{qpid_client_dir}/%{version}/lib/slf4j-api-1.6.4.jar \
	%{buildroot}%{_awips2_directory}/lib/opt

# license & notice
install -pm 644 %{qpid_client_dir}/%{version}/LICENSE \
	%{buildroot}%{_awips2_directory}
install -pm 644 %{qpid_client_dir}/%{version}/NOTICE \
	%{buildroot}%{_awips2_directory}

%clean
rm -rf %{buildroot}

%files common
%defattr(-,awips,fxalpha,-)
%dir /awips2/qpid
%dir /awips2/qpid/lib
%dir /awips2/qpid/lib/opt
/awips2/qpid/lib/opt/qpid-common-%{version}.jar
/awips2/qpid/lib/opt/geronimo-jms_1.1_spec-1.1.1.jar
/awips2/qpid/lib/opt/slf4j-api-1.6.4.jar 
%doc /awips2/qpid/LICENSE
%doc /awips2/qpid/NOTICE

%files client
%defattr(-,awips,fxalpha,-)
%dir /awips2/qpid
%dir /awips2/qpid/lib
%dir /awips2/qpid/lib/opt
/awips2/qpid/lib/opt/qpid-client-%{version}.jar

