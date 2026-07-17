#!/bin/bash

# This script must be run as root or with root privileges.

if [ ! "${USER}" = "root" ]; then
   echo "ERROR: You Must Be 'root' To Run This Script."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

function removeRPMNoMatterWhat()
{
   # Arguments:
   #   $1 == the component name

   AWIPSII_COMPONENT="${1}"
   rpm -e --nodeps ${AWIPSII_COMPONENT}
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      echo "ERROR: Unable To Remove '${AWIPSII_COMPONENT}'."
   fi
}

ADE_COMPONENTS=`rpm -qa --queryformat='%{NAME}\n' |grep awips2 | sort -n`

for component in ${ADE_COMPONENTS[*]};
do
   removeRPMNoMatterWhat "${component}"
done
