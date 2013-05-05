#!/bin/bash

# determine where the script is
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

# this script will build both versions of qpid unless an argument is passed
# to it with the version of qpid to build.

function build018()
{
   pushd . > /dev/null 2>&1
   cd ${dir}/0.18/deploy.builder
   if [ $? -ne 0 ]; then
      return 1
   fi
   /bin/bash build.sh
   if [ $? -ne 0 ]; then
      return 1
   fi
   popd > /dev/null 2>&1
}

function build07()
{
   pushd . > /dev/null 2>&1
   cd ${dir}/0.7/deploy.builder
   if [ $? -ne 0 ]; then
      return 1
   fi
   /bin/bash build.sh
   if [ $? -ne 0 ]; then
      return 1
   fi
   popd > /dev/null 2>&1
}

VERSION_TO_BUILD="${1}"
if [ "${1}" = "0.7" ]; then
   build07
   exit $?
fi

if [ "${1}" = "0.18" ]; then
   build018
   exit $?
fi

# no version specified; build both
build07
if [ $? -ne 0 ]; then
   exit 1
fi
build018
if [ $? -ne 0 ]; then
   exit 1
fi

exit 0
