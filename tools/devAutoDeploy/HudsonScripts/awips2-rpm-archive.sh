#!/bin/bash -x
#----------------------------------------------------------------------
# Automated archive script. Archives the rpms from the last rpm build.
#
# Usage:
# awips2-rpm-archive.sh [share] 
#----------------------------------------------------------------------
# Args:
# $1    :: location of directory containing packages and output location
#          default: /share1
#
# Note: all args are optional.
#----------------------------------------------------------------------
# Limitations:
#  1) This script is designed to be executed by Hudson -- it may need
#     modification to run with another auto build system

#----------------------------------------------------------------------
#-- set defaults for arguments
#----------------------------------------------------------------------

#-- share1 location -- will be arg $1 
#-- (allows using different packages archive and output directory)
SHARE1=/share1

#----------------------------------------------------------------------
#-- decode the command line
#----------------------------------------------------------------------

if [ -n "${1}" ];then
   SHARE1=${1}
fi
