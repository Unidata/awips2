#!/bin/bash

function startEnvironment()
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
   startEnvironmentInternal "${env_name}"
   if [ $? -ne 0 ]; then
      return 1
   fi

   return 0
}

# private
function startEnvironmentInternal()
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

   # Start the environment.
   pushd . > /dev/null 2>&1
   cd ${EDEX_ENV_DIR}/${env_name}/edex-environment
   # Start PostgreSQL.
   /bin/bash edex_postgres start
   echo 
   sleep 10
   # Start httpd-pypies
   /bin/bash httpd-pypies start
   echo
   sleep 10
   # Start QPID.
   /bin/bash qpidd start
   echo
   sleep 10
   # Start EDEX (replace with wes mode when available).
   /bin/bash edex_camel start
   echo
   popd > /dev/null 2>&1

   return 0
}
