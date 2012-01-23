#!/bin/bash

function listEnvironments()
{
   pushd . > /dev/null 2>&1
   cd ${WES2BRIDGE_DIR}
   echo "Existing EDEX Environment(s):"
   for env_dir in `ls -1`; do
      echo "   -${env_dir}"
   done
   popd > /dev/null 2>&1
}
