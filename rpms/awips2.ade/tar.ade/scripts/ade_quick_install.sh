#!/bin/bash

# This script must be ran as root or with root privileges.

# Arguments:
#   $1 == the Installation Prefix (Optional)

if [ ! "${USER}" = "root" ]; then
   echo "ERROR: You Must Be 'root' To Run This Script."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

#
# Check for existence of user awips 
#
id awips >/dev/null
ID_AWIPS=$?
if [ $ID_AWIPS != 0 ]; then
  echo "An awips user does not exist on this system"
  echo "As root:"
  echo "  useradd awips"
  exit
fi

#
# Check for existence of group fxalpha 
# 
getent group fxalpha >/dev/null
GROUP_FXALPHA=$?
if [ $GROUP_FXALPHA != 0 ]; then
  echo "The fxalpha group does not exist on this system"
  echo "As root:"
  echo "  groupadd fxalpha"
  exit
fi 

#
# Check for user awips in group fxalpha
#
AWIPS_IN_FXALPHA=`groups awips | grep fxalpha`
if [ "${AWIPS_IN_FXALPHA}" = "0" ]; then
  echo "The awips user must be in the fxalpha group on this system"
  echo "As root:"
  echo "  usermod -a -G fxalpha awips"
  exit
fi

dir=${0%/*}

if [ "$dir" = "$0" ]; then
   dir="."
fi

cd ${dir}

yum --disablerepo=* install *.rpm --nogpgcheck -y
if [ $? -ne 0 ]; then
   echo "FATAL: ADE Installation Failed."
fi
