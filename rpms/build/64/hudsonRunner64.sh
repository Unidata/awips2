#!/bin/bash

# This is the baselined Hudson script that is used to build the
# 64-bit RPMs for both release and nightly builds.

# Expected Environment Variables:
#   1) ${WORKSPACE} - the location of the hudson workspace.
#   2) ${AWIPSII_VERSION} - the build version; defaults to the contents of version.txt
#   3) ${AWIPSII_RELEASE} - the build release; defaults to the current date in YYYYMMDD

# Provided Variables:
#   1) ${REPOSITORY} - the root directory of the AWIPS II repository.
#   2) ${REPOSITORY_LOCK} - the name of the repository locking file.
REPOSITORY="/install/repository"
REPOSITORY_LOCK="repository.lck"

function logINFO()
{
   # Arguments:
   #   ${1} - the message to log.
   echo "INFO: ${1}"
}

function logERROR()
{
   # Arguments:
   #   ${1} - the message to log.
   echo "ERROR: ${1}"
}

function lockRepository()
{
   if [ -f ${REPOSITORY}/arch.x86_64/${REPOSITORY_LOCK} ]; then
      logINFO "The repository is currently locked."
      sleep 5
      while [ -f ${REPOSITORY}/arch.x86_64/${REPOSITORY_LOCK} ]
      do
         logINFO "The repository is still locked."
         sleep 5
      done
      logINFO "The repository is no longer locked."
   fi
   logINFO "Locking the repository."
   touch ${REPOSITORY}/arch.x86_64/${REPOSITORY_LOCK}
}

function unlockRepository()
{
   sleep 5
   logINFO "Unlocking the repository."
   rm -fv ${REPOSITORY}/arch.x86_64/${REPOSITORY_LOCK}
}

function buildRelease()
{
   REPOSITORY="${REPOSITORY}/release"

   time /bin/bash build.sh
   if [ $? -ne 0 ]; then
      logERROR "Failed to build the x86_64 RPMs."
      return 1
   fi

   return 0
}

function createRepositoryRelease()
{
   #1) Create the needed directories, if they do not already exist.
   if [ ! -d ${REPOSITORY}/arch.x86_64/${AWIPSII_VERSION}-${AWIPSII_RELEASE} ]; then
      mkdir -p ${REPOSITORY}/arch.x86_64/${AWIPSII_VERSION}-${AWIPSII_RELEASE}
      if [ $? -ne 0 ]; then
         return 1
      fi
      chmod 777 ${REPOSITORY}/arch.x86_64/${AWIPSII_VERSION}-${AWIPSII_RELEASE}
      if [ $? -ne 0 ]; then
         return 1
      fi
   fi
   #2) Copy the RPMs that we are responsible for into the repository.
   cd ${WORKSPACE}/rpmbuild/RPMS
   if [ -d ${REPOSITORY}/arch.x86_64/${AWIPSII_VERSION}-${AWIPSII_RELEASE}/x86_64 ]; then
      rm -rf ${REPOSITORY}/arch.x86_64/${AWIPSII_VERSION}-${AWIPSII_RELEASE}/x86_64
   fi
   cp -rv x86_64 ${REPOSITORY}/arch.x86_64/${AWIPSII_VERSION}-${AWIPSII_RELEASE}
   if [ $? -ne 0 ]; then
      return 1
   fi
   #3) Copy the baselined comps.xml file into the repository.
   cd ${WORKSPACE}/Installer.rpm/common/yum/arch.x86_64
   if [ ! -f comps.xml ]; then
      logERROR "The baselined comps.xml file does not exist."
      return 1
   fi
   cp -v comps.xml ${REPOSITORY}/arch.x86_64/${AWIPSII_VERSION}-${AWIPSII_RELEASE}
   if [ $? -ne 0 ]; then
      return 1
   fi   

   return 0
}

function buildNightly()
{
   local RPMS_TO_BUILD="awips2-alertviz awips2-cave awips2-python-dynamicserialize awips2-python-ufpy"
   REPOSITORY="${REPOSITORY}/nightly"

   # Build the RPMs.
   time /bin/bash build.sh "${RPMS_TO_BUILD}"
   if [ $? -ne 0 ]; then
      logERROR "Failed to build the x86_64 RPMs."
      return 1
   fi

   return 0
}

function createRepositoryNightly()
{
   lockRepository
   # 1) Determine what the dated directory should be.
   DATE=`date +"%Y%m%d"`
   # 2) Create the needed directories if they do not already exist.
   if [ ! -d ${REPOSITORY}/arch.x86_64/${DATE} ]; then
      mkdir -p ${REPOSITORY}/arch.x86_64/${DATE}
      if [ $? -ne 0 ]; then
         unlockRepository
         return 1
      fi
      chmod 777 ${REPOSITORY}/arch.x86_64/${DATE}
   fi
   # 3) Copy the RPMs that we are responsible for into the repository.
   cd ${WORKSPACE}/rpmbuild/RPMS
   # 3i) Remove any existing RPMs from a previous build.
   if [ -d ${REPOSITORY}/arch.x86_64/${DATE}/x86_64 ]; then
      rm -rf ${REPOSITORY}/arch.x86_64/${DATE}/x86_64
      if [ $? -ne 0 ]; then
         unlockRepository
         return 1
      fi
   fi
   cp -rv x86_64 ${REPOSITORY}/arch.x86_64/${DATE}
   if [ $? -ne 0 ]; then
      unlockRepository
      return 1
   fi
   # 4) Copy the baselined comps.xml file into the repository.
   cd ${WORKSPACE}/Installer.rpm/common/yum/arch.x86_64
   if [ ! -f comps.xml ]; then
      logERROR "The baselined comps.xml file does not exist."
      unlockRepository
      return 1
   fi
   cp -v comps.xml ${REPOSITORY}/arch.x86_64
   if [ $? -ne 0 ]; then
      unlockRepository
      return 1
   fi
   # 5) Re-build the repository.
   cd ${REPOSITORY}/arch.x86_64
   createrepo ${REPOSITORY}/arch.x86_64
   if [ $? -ne 0 ]; then
      unlockRepository
      return 1
   fi
   createrepo -g comps.xml .
   if [ $? -ne 0 ]; then
      unlockRepository
      return 1
   fi

   unlockRepository

   return 0
}

function buildRPMs()
{
   pushd . > /dev/null
   cd ${WORKSPACE}/Installer.rpm/awips2.64/deploy.builder

   # Determine what type of build we are doing.
   if [ "${AWIPSII_VERSION}" = "" ] &&
      [ "${AWIPSII_RELEASE}" = "" ]; then
      buildNightly
      if [ $? -ne 0 ]; then
         return 1
      fi
      createRepositoryNightly
      RC=$?
   else
      buildRelease
      if [ $? -ne 0 ]; then
         return 1
      fi
      createRepositoryRelease
      RC=$?
   fi

   popd > /dev/null
   return ${RC}
}

function validateEnvironment()
{
   if [ "${WORKSPACE}" = "" ]; then
      logERROR "The location of the Hudson workspace has not been set into the environment."
      return 1
   fi

   return 0
}

function setup()
{
   if [ ! -d ${WORKSPACE}/awips ]; then
      mkdir -p ${WORKSPACE}/awips
      if [ $? -ne 0 ]; then
         return 1
      fi
   fi

   pushd . > /dev/null
   cd ${WORKSPACE}

   # The rsync
   rsync -ruq --delete --exclude-from=/var/lib/hudson/excludes \
      cave/* cots/* edexOsgi/* ncep/* RadarServer/* awips
   if [ $? -ne 0 ]; then
      return 1
   fi
   # sync the remaining project directories.
   if [ -d awips/pythonPackages ]; then
      rm -rf awips/pythonPackages
      if [ $? -ne 0 ]; then
         return 1
      fi
   fi
   if [ -d awips/Installer.rpm ]; then
      rm -rf awips/Installer.rpm
      if [ $? -ne 0 ]; then
         return 1
      fi
   fi
   cp -rv pythonPackages awips
   if [ $? -ne 0 ]; then
      return 1
   fi
   cp -rv rpms awips/Installer.rpm
   if [ $? -ne 0 ]; then
      return 1
   fi

   cd ${WORKSPACE}/awips
   # Cleanup SVN metadata.
   find . -name .svn -exec rm -rf {} \;

   popd > /dev/null

   # Create the standard RPM Build directory structure.
   if [ -d ${WORKSPACE}/awips/rpmbuild ]; then
      rm -rf ${WORKSPACE}/awips/rpmbuild
      if [ $? -ne 0 ]; then
         return 1
      fi
   fi
   mkdir -p ${WORKSPACE}/awips/rpmbuild
   if [ $? -ne 0 ]; then
      return 1
   fi
   mkdir -p ${WORKSPACE}/awips/rpmbuild/BUILD
   if [ $? -ne 0 ]; then
      return 1
   fi
   mkdir -p ${WORKSPACE}/awips/rpmbuild/RPMS
   if [ $? -ne 0 ]; then
      return 1
   fi
   mkdir -p ${WORKSPACE}/awips/rpmbuild/SOURCES
   if [ $? -ne 0 ]; then
      return 1
   fi
   mkdir -p ${WORKSPACE}/awips/rpmbuild/SPECS
   if [ $? -ne 0 ]; then
      return 1
   fi
   mkdir -p ${WORKSPACE}/awips/rpmbuild/SRPMS
   if [ $? -ne 0 ]; then
      return 1
   fi

   export AWIPSII_TOP_DIR="${WORKSPACE}/awips/rpmbuild"
   export WORKSPACE="${WORKSPACE}/awips"

   return 0
}

# Begin
validateEnvironment
if [ $? -ne 0 ]; then
   exit 1
fi
setup
if [ $? -ne 0 ]; then
   exit 1
fi
buildRPMs
if [ $? -ne 0 ]; then
   exit 1
fi
# Finished

exit 0
