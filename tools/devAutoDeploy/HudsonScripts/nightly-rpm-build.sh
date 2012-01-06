#!/bin/bash -x
#----------------------------------------------------------------------
# Automated build script. Provides export of code base to a single
# directory and some ordering of builds.
#
# Usage:
# nightly-rpm-build.sh [delete] [share] [svn-connect] [group] ... [group] 
#----------------------------------------------------------------------
# Args:
# $1    :: delete flag - true to delete, false to not delete
#          default: true
# $2    :: location of directory containing packages and output location
#          default: /share1
# $3    :: svn repo connection string
#          default: file:///home/keystone/repo/products/awips/trunk2
# $4..  :: list of SVN project groups needed for the build
#          default: cave cots edexOsgi nativeLib RadarServer rpms
#
# Note: all args are optional, but cumulative. i.e., in order to change
#       the svn connection string, you must specify delete flag and
#       location values.
#----------------------------------------------------------------------
# Limitations:
#  1) This script is designed to be executed by Hudson -- it may need
#     modification to run with another auto build system

#----------------------------------------------------------------------
#-- set defaults for arguments
#----------------------------------------------------------------------

#-- delete flag -- will be arg $2
delete_projects="true"

#-- share1 location -- will be arg $3 
#-- (allows using different packages archive and output directory)
SHARE1=/share1

#-- connect string for the repo directory -- will be arg $4
connection='file:///home/keystone/repo/products/awips/trunk2'

#-- project groups to export out -- will be arguments $5...
projects='cave cots edexOsgi nativeLib RadarServer rpms'

#----------------------------------------------------------------------
#-- decode the command line
#----------------------------------------------------------------------
if [ -n "${1}" ];then
   delete_projects=${1}
fi
if [ -n "${2}" ];then
SHARE1=${2}
fi
if [ -n "${3}" ];then
connection=${3}
fi
if [ $# -gt 3 ];then
   # there are projects listed
   projects=${4}
   shift 5
   for a in $@;do
      projects="${projects} ${a}"
   done
fi

#----------------------------------------------------------------------
# function that records a build problem into a text file for later
# use. If the file doesn't already exist, it is created with a
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
# run the archival script to archive the existing rpms
#----------------------------------------------------------------------

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

#----------------------------------------------------------------------
# export the code baseline
#----------------------------------------------------------------------
echo "Exporting '${projects}'"

for p in $projects;do
   if [ "${p}" != "rpms" ]; then
      echo "exporting project ${p}"
      for a in `/usr/local/bin/svn list ${connection}/${p}`;do
         if [ "${a}" != ".project" ];then
            /usr/local/bin/svn export -q --force ${connection}/${p}/${a}
         fi
      done
   else
      /usr/local/bin/svn export -q --force ${connection}/${p} Installer.rpm
   fi
done

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

RPM_DEST_TOP_DIR="share1/awips2-rpms"
RPM_DEST_RPM_DIR="${RPM_DEST_TOP_DIR}/rpms"
RPM_DEST_ARCHIVE_DIR="${RPM_DEST_TOP_DIR}/archive"

#----------------------------------------------------------------------
# build the rpm projects
#----------------------------------------------------------------------
RPM_BUILD_SCRIPT="build.sh"
cd ${WORKING_DIR}/Installer.rpm/deploy.builder

time ./${RPM_BUILD_SCRIPT}

#----------------------------------------------------------------------
# copy all of our built rpms to the share directory
#----------------------------------------------------------------------
cp /usr/src/redhat/RPMS/i386/awips2-*.rpm ${RPM_DEST_RPM_DIR}

#----------------------------------------------------------------------
# create a new temporary date file for todays build
#----------------------------------------------------------------------
TMP_DATE_FILE=".build-date.946825tmp"
if [ -f ${RPM_DEST_RPM_DIR}/${TMP_DATE_FILE} ]; then
   rm -f ${RPM_DEST_RPM_DIR}/${TMP_DATE_FILE}
fi
echo `date +"%m-%d-%Y"` > ${RPM_DEST_RPM_DIR}/${TMP_DATE_FILE}

#----------------------------------------------------------------------
# create our daily md5 checksum html file [worry about this later]
#----------------------------------------------------------------------

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









