******************
Prerequisites
******************
Check out the source
see http://cwiki.apache.org/qpid/building.html

******************
Install CruiseControl
******************

Download CruiseControl from: http://cruisecontrol.sourceforge.net/

    * Unzip the release to a directory, for example ~/cruisecontrol-bin-2.7.2
    * Check that the scripts cruisecontrol-bin-2.7.2/cruisecontrol.sh and cruisecontrol-bin-2.7.2/apache-ant-1.7.0/bin/ant have execution permission.
    * Make sure your directory ~/.ant/lib contains the following jars:
          o The ant jar files that can be found in cruisecontrol-bin-2.7.2/apache-ant-1.7.0/lib/
          o xalan-2.7.0.jar

******************
Set system variables
******************

Prior to use CruiseControl you'll need to set three system variables:
Variable 	    Value
CC_HOME         	path to your qpid project, for example /home/foo/projects/qpid
CPPSTORE_HOME 	path to your C++ store, for example /home/foo/projects/bdbstore-cpp
NANT_HOME       path to the nant directory -- only required for .net client --
                (nant can be downloaded from http://nant.sourceforge.net/)

Edit the file CC_HOME/config.properties and set the properties so to match your system requirements. 
Notes
    * the cpp store can be checked out from: https://svn.jboss.org/repos/rhmessaging/store/trunk/cpp
    * Only unix scrips are currently provided
    * 


******************
Installing Mono
******************
For building the .net client on a Linux platform you need to install Mono.
Mono website is: http://www.mono-project.com/Main_Page
Here are the instruction for a RHEL5 platform:

Create the file "/etc/yum.repos.d/mono.repo" and add the following lines:

[Mono]
name=Mono Stack (RHEL_5)
type=rpm-md
baseurl=http://download.opensuse.org/repositories/Mono/RHEL_5/
gpgcheck=1
gpgkey=http://download.opensuse.org/repositories/Mono/RHEL_5/repodata/repomd.xml.key
enabled=1

Enter the following command to install Mono:

# yum install mono-complete 


******************
Running CruiseControl
******************

Run cruisecontrol-bin-2.7.2/cruisecontrol.sh from CC_HOME/cc

******************
Running the Sun java TCK
******************

If you wish to run the Sun JMS TCK, follow those two steps:
* Extract the TCK
* (As required by the TCK) Set TS_HOME to the location where the JMS TCK has been installed. 

******************
Projects
******************

Project                     Description
qpid-cpp-trunk 	            Builds and tests the C++ broker
qpid-cpp-trunk-perftests     Runs the C++ performance tests
qpid-java-trunk             	Builds and runs the Java tests with an 0.8 inVM broker, a c++ broker without prefetch and a c++ broker with pre-fetch
bdbstore-cpp-trunk 	        Builds the C++ store (required for the Java tests)
example-automation 	        Runs all the example combinations for python, C++ and java
java-perftests              Runs the java performance tests
java-jmstck                 Runs the java jms tck (see running the tck)