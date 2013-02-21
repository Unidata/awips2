#!/bin/bash

function stopEnvironment()
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
   stopEnvironmentInternal "${env_name}"
   if [ $? -ne 0 ]; then
      return 1
   fi

   return 0
}

# private
function stopEnvironmentInternal()
{
   # Arguments:
   #   ${1} name.
   env_name="${1}"

   # Ensure that the environment exists.
   if [ ! -d ${EDEX_ENV_DIR}/${env_name} ]; then
      echo "ERROR: The ${env_name} environment does not exist yet."
      return 1
   fi

   # Verify that the environment is not missing any startup scripts.
   if [ ! -f ${EDEX_ENV_DIR}/${env_name}/edex-environment/edex_camel ]; then
      echo "ERROR: The ${env_name} environment is corrupt. Recreate it."
      return 1
   fi
   if [ ! -f ${EDEX_ENV_DIR}/${env_name}/edex-environment/edex_postgres ]; then
      echo "ERROR: The ${env_name} environment is corrupt. Recreate it."
      return 1
   fi
   if [ ! -f ${EDEX_ENV_DIR}/${env_name}/edex-environment/httpd-pypies ]; then
      echo "ERROR: The ${env_name} environment is corrupt. Recreate it."
      return 1
   fi
   if [ ! -f ${EDEX_ENV_DIR}/${env_name}/edex-environment/qpidd ]; then
      echo "ERROR: The ${env_name} environment is corrupt. Recreate it."
      return 1
   fi

   # Stop the environment.
   pushd . > /dev/null 2>&1
   cd ${EDEX_ENV_DIR}/${env_name}/edex-environment
   # Stop EDEX.
   /bin/bash edex_camel stop
   echo 
   sleep 10
   # Stop QPID.
   /bin/bash qpidd stop
   echo
   sleep 10
   # Stop httpd-pypies
   /bin/bash httpd-pypies stop
   echo
   sleep 10
   # Stop PostgreSQL.
   /bin/bash edex_postgres stop
   echo
   popd > /dev/null 2>&1

   return 0
}
