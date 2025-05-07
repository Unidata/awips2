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

   env_name_lowercase=$(echo ${env_name} | tr '[:upper:]' '[:lower:]')

   # Ensure that the environment exists.
   if [ ! -d ${EDEX_ENV_DIR}/${env_name} ]; then
      echo "ERROR: The ${env_name} environment does not exist yet."
      return 1
   fi

   # Verify that the environment is not missing any startup scripts.
   if [ ! -f ${EDEX_ENV_DIR}/${env_name}/edex-environment/edex_camel_${env_name_lowercase}@.service ]; then
      echo "${EDEX_ENV_DIR}/${env_name}/edex-environment/edex_camel_${env_name_lowercase}@.service missing"
      echo "ERROR: The ${env_name} environment is corrupt. Recreate it."
      return 1
   fi
   if [ ! -d ${EDEX_ENV_DIR}/${env_name}/edex-environment/postgresql@awips_${env_name_lowercase}.service.d ]; then
      echo "${EDEX_ENV_DIR}/${env_name}/edex-environment/postgresql@awips_${env_name_lowercase}.service.d missing"
      echo "ERROR: The ${env_name} environment is corrupt. Recreate it."
      return 1
   fi
   if [ ! -f ${EDEX_ENV_DIR}/${env_name}/edex-environment/httpd-pypies_${env_name_lowercase}.service ]; then
      echo "${EDEX_ENV_DIR}/${env_name}/edex-environment/httpd-pypies_${env_name_lowercase}.service missing"
      echo "ERROR: The ${env_name} environment is corrupt. Recreate it."
      return 1
   fi
   if [ ! -f ${EDEX_ENV_DIR}/${env_name}/edex-environment/qpidd_${env_name_lowercase}.service ]; then
      echo "${EDEX_ENV_DIR}/${env_name}/edex-environment/qpidd_${env_name_lowercase}.service missing"
      echo "ERROR: The ${env_name} environment is corrupt. Recreate it."
      return 1
   fi

   # Start the environment.
   pushd . > /dev/null 2>&1
   cd ${EDEX_ENV_DIR}/${env_name}/edex-environment


   # Start PostgreSQL.
   sudo systemctl start postgresql@awips_${env_name_lowercase}
   echo 
   sleep 10
   # Start httpd-pypies
   sudo systemctl start httpd-pypies_${env_name_lowercase}
   echo
   sleep 10
   # Start QPID.
   sudo systemctl start qpidd_${env_name_lowercase}
   echo
   sleep 10
   # Start EDEX (replace with wes mode when available).
   for service in 'request' 'ingest' 'ingestGrib' 'ingestDat' 'registry';
   do
      sudo systemctl start edex_camel_${env_name_lowercase}@$service
   done
   echo
   popd > /dev/null 2>&1

   return 0
}
