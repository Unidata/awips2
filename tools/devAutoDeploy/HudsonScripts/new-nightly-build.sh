#!/bin/bash -x
#----------------------------------------------------------------------
# Automated build script. Provides export of code base to a single
# directory and some ordering of builds.
#
# Usage:
# new-nightly-build.sh [bundles] [delete] [share] [svn-connect] [group] ... [group] 
#----------------------------------------------------------------------
# Args:
# $1    :: bundles file - path to file containing list of bundles
#          to create.
#          default: don't create an bundle installers
# $2    :: delete flag - true to delete, false to not delete
#          default: true
# $3    :: location of directory containing packages and output location
#          default: /share1
# $4    :: svn repo connection string
#          default: file:///home/keystone/repo/products/awips/trunk2
# $5..  :: list of SVN project groups needed for the build
#          default: cave cots edexOsgi nativeLib RadarServer Installer
#
# Note: all args are optional, but cumulative. i.e., in order to change
#       the svn connection string, you must specify delete flag and
#       location values.
#----------------------------------------------------------------------
# Limitations:
#  1) This script is designed to be executed by Hudson -- it may need
#     modification to run with another auto build system
#  2) The list of bundles to build is defined by a file named
#     'new-nightly-build-bundles', which is must be co-located with
#     this script. See that file for more information.

#----------------------------------------------------------------------
#-- set defaults for arguments
#----------------------------------------------------------------------
#-- bundle definition file -- will be arg $1
bundle_fl=

#-- delete flag -- will be arg $2
delete_projects="true"

#-- share1 location -- will be arg $3 
#-- (allows using different packages archive and output directory)
SHARE1=/share1

#-- connect string for the repo directory -- will be arg $4
connection='file:///home/keystone/repo/products/awips/trunk2'

#-- project groups to export out -- will be arguments $5...
projects='cave cots edexOsgi nativeLib RadarServer Installer'

#----------------------------------------------------------------------
#-- decode the command line
#----------------------------------------------------------------------
if [ -n "${1}" ];then
   bundle_fl=${1}
fi
if [ -n "${2}" ];then
   delete_projects=${2}
fi
if [ -n "${3}" ];then
SHARE1=${3}
fi
if [ -n "${4}" ];then
connection=${4}
fi
if [ $# -gt 4 ];then
   # there are projects listed
   projects=${5}
   shift 5
   for a in $@;do
      projects="${projects} ${a}"
   done
fi

#----------------------------------------------------------------------
# get the bundles list from 'new-nightly-build-bundles'
#----------------------------------------------------------------------
if [ -n "${bundle_fl}" ];then
   if [ ! -e ${bundle_fl} ];then
      echo "bundle file '${bundle_fl}' not found -- no bundles will be processed"
      bundles=
   else
      . ${bundle_fl}
      bundles=`echo "${bundles}" | tr ' ' _`
      bundles=`echo "${bundles}" | tr : ' '`
   fi
else
   echo "no bundle file specified -- no bundles will be processed"
fi

#----------------------------------------------------------------------
# function to determine if an installer project has special handling
# e.g. we want to create the code base installer before we compile 
# the source code
#
# returns:
#      0 :: not a "special" project
#      1 :: is a "special" project
#----------------------------------------------------------------------
special() {
   #just ignore code ant and eclipse since they aren't ready, 
   #javadoc is needed later for the ade, and master is not a 'real' installer
   special_handling='javadoc code ant eclipse master'
   ext=`echo ${1} | /usr/bin/cut -d. -f2`
   count=`echo ${special_handling} | grep -cw ${ext}`
   return `test ${count} -eq 0`
}

#----------------------------------------------------------------------
# function that records a build problem into a text file for later
# use. If the file doesn't already exist, it iw created with a
# header.
#
# args:
#    $1 :: component
#----------------------------------------------------------------------
ERROR_COUNT=0
BUILD_DATE=`date "+%b %d %Y"`
problem() {
   let ERROR_COUNT=ERROR_COUNT+1
   message="encountered problems building component ${1} -- check log for details"
   if [ ! -f /tmp/email.txt ];then
      echo "${BUILD_DATE} Nightly Build Issues:" > /tmp/build-issues.txt
   fi
   echo "`date +%T`: ${message}" >> /tmp/build-issues.txt
}
#----------------------------------------------------------------------
# function to send an email in the event an error has occurred.
#
# args:
#----------------------------------------------------------------------
notify() {
   mail_list="Kace_Chrisman mark_w_fegan Scott_Risch greg.armendariz"
   subject="Nightly Build Issue -- ${BUILD_DATE}"
   for a in ${mail_list};do
      mail -s "${subject}" ${a}@raytheon.com < /tmp/build-issues.txt
   done
}
#----------------------------------------------------------------------
# clear previous build-issues file
#----------------------------------------------------------------------
if [ -e /tmp/build-issues.txt ];then
   rm -f /tmp/build-issues.txt
fi

#----------------------------------------------------------------------
# create directories needed for the build
#----------------------------------------------------------------------
output_dir=${SHARE1}/installers
install_files_dir=${output_dir}/install_files
components_dir=${output_dir}/install_files/components

#----------------------------------------------------------------------
# cleanup workspace -- if not done after last build
#----------------------------------------------------------------------
if [ -e all ]
then
	rm -rf all
fi

mkdir -p all/
cd all
WORKING_DIR=`pwd`
mkdir -p ${components_dir}

#----------------------------------------------------------------------
# export the code baseline
#----------------------------------------------------------------------
echo "Exporting '${projects}'"

for p in $projects;do
   echo "exporting project ${p}"
   for a in `/usr/local/bin/svn list ${connection}/${p}`;do
      if [ "${a}" != ".project" ];then
         /usr/local/bin/svn export -q --force ${connection}/${p}/${a}
      fi
   done
done

#----------------------------------------------------------------------
# create the code base installer before building 
# -- avoids including build products
#----------------------------------------------------------------------
#echo "building ADE Code base installer"
#cd ${WORKING_DIR}/Installer.code
#${ANT_HOME}/bin/ant -Dawips.cm.share=$SHARE1 buildAll
#rc=$?
#if [ $rc -ne 0 ];then
#   problem "Installer.code"
#fi
#/bin/cp release~/*.jar ${components_dir}

#----------------------------------------------------------------------
# build the underlying projects
#----------------------------------------------------------------------
ECLIPSE_HOME=${WORKING_DIR}/uframe-eclipse
echo "building EDEX"
cd ${WORKING_DIR}/build.edex
./build.sh -eclipse=${ECLIPSE_HOME}
rc=$?
if [ $rc -ne 0 ];then
   problem "build.edex"
fi
echo "building RCM"
cd ${WORKING_DIR}/build.rcm
./build.sh -eclipse=${ECLIPSE_HOME}
rc=$?
if [ $rc -ne 0 ];then
   problem "build.rcm"
fi
echo "building CAVE"
cd ${WORKING_DIR}/build
./build.sh -eclipse=${ECLIPSE_HOME}
rc=$?
if [ $rc -ne 0 ];then
   problem "build.cave"
fi

#----------------------------------------------------------------------
# build the installer projects
#----------------------------------------------------------------------
cd ${WORKING_DIR}
for a in `ls | grep Installer`;do
   #-- Installer.master contains common resourses; 
   #-- Installer.javadoc needs to build later 
   #-- special() identifies projects that should be worked elsewhere 
   special ${a}
   if [ $? -eq 0 ]; then
      cd ${WORKING_DIR}/${a}
      ${ANT_HOME}/bin/ant -Dawips.cm.share=$SHARE1 buildAll
      rc=$?
      if [ $rc -ne 0 ];then
         problem "${a}"
      fi
      /bin/cp release~/*.jar ${components_dir}
   fi
done

#----------------------------------------------------------------------
# build the JavaDoc -- also copy to ADE installer directory
#----------------------------------------------------------------------
echo "building ADE JavaDoc installer"
cd ${WORKING_DIR}/Installer.javadoc
${ANT_HOME}/bin/ant -Dawips.cm.share=$SHARE1 buildAll
rc=$?
if [ $rc -ne 0 ];then
   problem "Installer.javadoc"
fi
/bin/cp release~/*.jar ${components_dir}
# -- remove this once we switch to the final version of the installer builds
/bin/cp release~/*.jar ${output_dir}/ade
rm -rf ${output_dir}/ade/javadocs/*
rm -rf ${output_dir}/ade/otherdocs/*
/bin/cp -R temp~/build/jar/javadoc/api/* ${output_dir}/ade/javadocs
/bin/cp -R temp~/build/jar/docs/* ${output_dir}/ade/otherdocs

# ---------------------------------------------------------------------
# Build a new style deployment packages for workstation, ADE, EDEX
# ---------------------------------------------------------------------
echo "building bundle installers"
if [ -n "${bundles}" ];then
   cd ${WORKING_DIR}/deploy.builder

   for b in ${bundles};do
      bundle=`echo ${b} | tr _ ' '`
      ./build ${bundle}
      rc=$?
      if [ $rc -ne 0 ];then
         problem `echo ${bundle} | cut -d ' ' -f1`
      fi
   done

   cp release~/*.jar ${install_files_dir}
   cp release~/*.sh ${output_dir}
   cp release~/*.txt ${output_dir}
   chmod a+x ${output_dir}/*.sh
else
   echo "   no installer bundles specified"
fi

#----------------------------------------------------------------------
# check if emails need to be sent
#----------------------------------------------------------------------
if [ ${ERROR_COUNT} -ne 0 ]; then
   notify
fi

#----------------------------------------------------------------------
# cleanup build -- will be added later
#----------------------------------------------------------------------
if [ "${delete_projects}" == "true" ];then
   cd ${WORKING_DIR}/..
   rm -rf all
fi
