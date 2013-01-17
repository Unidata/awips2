#!/bin/bash

function createEnvironment()
{
   # Arguments:
   #   ${1} configuration file.
   
   config_file="${1}"
   # Determine the name of the environment.
   env_name=`/awips2/java/bin/java -jar ${UTILITIES}/ConfigurationUtility.jar "${config_file}" "-name"`
   if [ $? -ne 0 ]; then
      return 1
   fi
   
   # Ensure that an environment does not already exist.
   if [ -d ${EDEX_ENV_DIR}/${env_name} ]; then
      echo "ERROR: An edex environment with name = ${env_name} already exists."
      return 1
   fi

   # An environment does not exist. Create it.
   # 1) Create the root directory.
   mkdir -p ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to create the ${env_name} environment root directory."
      return 1
   fi
   
   # 2) Create the environment by copying components in /awips2.
   # /awips2/edex
   cp -r /awips2/edex ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to place edex in the ${env_name} environment."
      return 1
   fi
   # Temporary? Remove the edex management plugin.
   instance_plugins_dir=${EDEX_ENV_DIR}/${env_name}/edex/lib/plugins
   jar_file="com.raytheon.uf.edex.management.jar"
   if [ -f ${instance_plugins_dir}/${jar_file} ]; then
      rm -f ${instance_plugins_dir}/${jar_file}
      if [ $? -ne 0 ]; then
         echo "ERROR: Failed to remove ${jar_file} from the ${env_name} environment."
         return 1
      fi
   fi
   # /awips2/GFESuite
   cp -r /awips2/GFESuite ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to place GFESuite in the ${env_name} environment."
      return 1
   fi
   # /awips2/database
   cp -r /awips2/database ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to place the database in the ${env_name} environment."
      return 1
   fi
   # /awips2/data
   cp -r /awips2/data ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to place the database in the ${env_name} environment."
      return 1
   fi
   # /awips2/postgresql
   cp -r /awips2/postgresql ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to place postgresql in the ${env_name} environment."
      return 1
   fi
   # /awips2/qpid
   cp -r /awips2/qpid ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to place qpid in the ${env_name} environment."
      return 1
   fi
   # /awips2/pypies
   cp -r /awips2/pypies ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to place pypies in the ${env_name} environment."
      return 1
   fi 
   # /awips2/httpd-pypies
   cp -r /awips2/httpd_pypies ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to place httpd_pypies in the ${env_name} environment."
      return 1
   fi

   # 3) Update the links in: data/pg_tblspc
   pushd . > /dev/null 2>&1
   cd ${EDEX_ENV_DIR}/${env_name}/data/pg_tblspc
   for link in `ls -1`; do
      linked_dir=`readlink ${link}`
      if [ $? -ne 0 ]; then
         return 1
      fi
      data_dir=`/awips2/java/bin/java -jar ${UTILITIES}/DataLinkUtility.jar "${linked_dir}"`
      if [ $? -ne 0 ]; then
         return 1
      fi

      # Remove the existing link.
      rm -f ${link}
      if [ $? -ne 0 ]; then
         return 1
      fi
      # Create a new link to the correct location.
      ln -sf ${EDEX_ENV_DIR}/${env_name}/data/${data_dir} ${link}
      if [ $? -ne 0 ]; then
         return 1
      fi
   done
   popd > /dev/null 2>&1

   # 4) Create a directory for the environment-specific start (init.d) scripts.
   mkdir -p ${EDEX_ENV_DIR}/${env_name}/edex-environment
   if [ $? -ne 0 ]; then
      exit 1
   fi

   # 5) Re-configure: Edex, QPID, PostgreSQL, and Pypies
   /awips2/java/bin/java -jar ${UTILITIES}/Wes2BridgeManager.jar "${config_file}"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to configure the ${env_name} environment."
      return 1
   fi

   # 6) Adjust permissions.
   chown -R awips:fxalpha ${EDEX_ENV_DIR}/${env_name}
   if [ $? -ne 0 ]; then
      return 1
   fi
   chmod -R 775 ${EDEX_ENV_DIR}/${env_name}/edex/data
   if [ $? -ne 0 ]; then
      return 1
   fi

   # 7) Lockdown the startup scripts.
   chown root:root ${EDEX_ENV_DIR}/${env_name}/edex-environment/*
   if [ $? -ne 0 ]; then
      return 1
   fi
   chmod 644 ${EDEX_ENV_DIR}/${env_name}/edex-environment/*
   if [ $? -ne 0 ]; then
      return 1
   fi

   # FINISHED.
   return 0 
}
