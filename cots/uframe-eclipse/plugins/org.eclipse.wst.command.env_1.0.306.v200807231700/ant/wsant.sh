#*******************************************************************************
# Copyright (c) 2005, 2008 IBM Corporation and others.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
#
# Contributors:
#     IBM Corporation - initial API and implementation
#*******************************************************************************
#!/bin/sh
set +v

#Script to run WebService Ant tasks in headless Eclipse mode

echo "Setting environment variables"

# The JRE java.exe to be used by Ant.  Note: for WTP 2.0 the JDK needs to be 1.5 or higher.
JAVAEXE=/home/tester/jdk1.5.0/jre/bin/java
       
       
# The Eclipse install directory.  Some Eclipse based products may refer to this directory
# as the non shared directory.
INSTALL_DIRECTORY=/productdirectory/eclipse

# The shared Eclipse features directory.  Some Eclipse based products may split their 
# directory structure into shared and non shared folders.  The variable below should be
# set to the shared directory.  Note: a default WTP installation does not split it's
# directory structure so the INSTALL_DIRECTORY and the SHARED_DIRECTORY should be the same.
SHARED_DIRECTORY=$INSTALL_DIRECTORY
                                                                                
# The Eclipse Equinox Launcher jar.  Usually this plugin jar file is located in the
# shared plugin directory(ie. plugins/org.eclipse.equinox.launcher*.jar )
LAUNCHER_JAR=$SHARED_DIRECTORY/plugins/org.eclipse.equinox.launcher_1.0.0.v20070606.jar


# The location of your workspace
WORKSPACE=/home/tester/workspace

run() {
  set -v
  $JAVAEXE -jar $LAUNCHER_JAR -install $INSTALL_DIRECTORY -application org.eclipse.ant.core.antRunner -data $WORKSPACE -file wsgen.xml $ls > wsgen.txt 2>&1
}

if [ ! -e $JAVAEXE ]; then 
  echo "ERROR: incorrect java.exe=$JAVAEXE, edit the script and correct the JAVAEXE environment variable";
  exit 1;
fi
                                                                           
if [ ! -e $LAUNCHER_JAR ]; then
echo "ERROR: incorrect launcher=$LAUNCHER_JAR, edit the script and correct the LAUNCHER_JAR environment variable";
exit 1;
fi  

run
exit 0
