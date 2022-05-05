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

   return 0
}
