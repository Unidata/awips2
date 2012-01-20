#!/bin/bash

# First, we validate the user-supplied arguments.
#   * verify that our awips2 db file exists in the workspace; if not, there is nothing that we can do.
#   * verify that directories exist.
#   * verify that a valid version and release has been specified.
#   * verify that if certain rpms were specified, they are actually rpms that can be built by-request.

DB_FILE="${WORKSPACE}/Installer.rpm/awips2-rpm.db"

if [ ! -f ${DB_FILE} ]; then
   echo "ERROR: Unable To Find The DB File - ${DB_FILE}."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# Are We Building All RPMs?
if [ "${BUILD_ALL_RPMS}" = "false" ]; then

   # Verify that the user has specified valid rpms.
   if [ "${RPMS_TO_BUILD}" = "" ]; then
      echo "ERROR: We Were Told Not To Build All RPMs; However, No RPMs To Build Have Been Specified."
      echo "Unable To Continue ... Terminating."
      exit 1
   fi

   for component in ${RPMS_TO_BUILD[*]};
   do
      SQL="SELECT COUNT(*) FROM awips2_core_rpms WHERE component = '${component}' AND dynamicVersion = 'Y';"
      COUNT=`echo ${SQL} | sqlite3 ${DB_FILE}`

      echo "COUNT for ${component} is ${COUNT}."
      if [ ${COUNT} -eq 0 ]; then
         # Maybe, it is one of the few Python Site-Packages?
         SQL="SELECT COUNT(*) FROM awips2_python_site_package_rpms WHERE component = '${component}' AND dynamicVersion = 'Y';"

         COUNT=`echo ${SQL} | sqlite3 ${DB_FILE}`
      fi

      if [ ${COUNT} -eq 0 ]; then
         # Maybe, it is one of the edex rpms?
         SQL="SELECT COUNT(*) FROM awips2_edex_rpms WHERE component = '${component}';"

         COUNT=`echo ${SQL} | sqlite3 ${DB_FILE}`
      fi

      if [ ${COUNT} -eq 0 ]; then
         # Maybe, it is one of the cave rpms?
         SQL="SELECT COUNT(*) FROM awips2_cave_rpms WHERE component = '${component}';"

         COUNT=`echo ${SQL} | sqlite3 ${DB_FILE}`
      fi

      if [ ${COUNT} -eq 0 ]; then
         # It is not a valid component!
         echo "ERROR: '${component}' Is Not A Valid \"Build-On-Request\" AWIPS II Component."
         echo "Unable To Continue ... Terminating."
         exit 1
      fi 
   done
fi

if [ ! -d ${ECLIPSE_HOME} ]; then
   echo "ERROR: Unable To Find UFrame-Eclipse. Expected Location - ${ECLIPSE_HOME}."
   echo "Unable To Continue ... Terminating."
   exit 1
fi
if [ ! -d ${REPO_ROOT_DIR}/${_32BIT_REPO_RPM_DIR} ]; then
   echo "ERROR: Unable To Find The 32-Bit Repository Directory."
   echo "Unable To Continue ... Terminating."
   exit 1
fi
if [ ! -d ${SHARE_DIR} ]; then
   echo "ERROR: Unable To Find The AWIPSCM Share Directory."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

if [ "${AWIPSII_VERSION}" = "" ]; then
   echo "ERROR: An Invalid Version Has Been Specified."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

if [ "${AWIPSII_RELEASE}" = "" ]; then
   echo "ERROR: An Invalid Release Has Been Specified."
   echo "Unable To Continue ... Terminating."
   exit 1
fi
