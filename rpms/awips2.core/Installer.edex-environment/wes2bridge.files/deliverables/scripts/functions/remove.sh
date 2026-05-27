#!/bin/bash

function removeEnvironment()
{
   # Arguments:
   #   ${1} configuration file.
   #   ${2} name.

   config_file="${1}"
   env_name="${2}"

   if [ ! "${config_file}" = "" ]; then
      # Get the name from the configuration file.
      env_name=`/awips2/java/bin/java -jar ${UTILITIES}/ConfigurationUtility.jar "${config_file}" "-name"`
      if [ $? -ne 0 ]; then
         return 1
      fi
   fi
   removeEnvironmentInternal "${env_name}"
   if [ $? -ne 0 ]; then
      return 1
   fi

   return 0
}

# private
function removeEnvironmentInternal()
{
   # Arguments:
   #   ${1} name.
   env_name="${1}"

   # Ensure that the environment exists.
   if [ ! -d ${EDEX_ENV_DIR}/${env_name} ]; then
      echo "ERROR: The ${env_name} environment does not exist yet."
      return 1
   fi

   # Remove the environment.
   rm -rf ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to remove the ${env_name} environment."
      return 1
   fi

   #6) remove systemd and watchdog files
   usrSystemdPath="/usr/lib/systemd/system/"
   etcSystemdPath="/etc/systemd/system"
   watchdogPath="/etc/watchdog.d"

   env_name_lowercase=$(echo ${env_name} | tr '[:upper:]' '[:lower:]')

   rm $usrSystemdPath/httpd-pypies_${env_name_lowercase}.service
   rm $usrSystemdPath/httpd-pypies-logging_${env_name_lowercase}.service
   rm $usrSystemdPath/qpidd_${env_name_lowercase}.service
   rm $usrSystemdPath/edex_camel_${env_name_lowercase}@.service
   rm $usrSystemdPath/edex_camel_${env_name_lowercase}.target

   rm -rf  $etcSystemdPath/postgresql@awips_${env_name_lowercase}.service.d

   rm $watchdogPath/edex_camel_${env_name_lowercase}_watchdog.sh
   rm $watchdogPath/pypies_${env_name_lowercase}_watchdog.sh
   rm $watchdogPath/qpid_${env_name_lowercase}_watchdog.sh
   rm $watchdogPath/postgres_${env_name_lowercase}_watchdog.sh

   return 0
}
