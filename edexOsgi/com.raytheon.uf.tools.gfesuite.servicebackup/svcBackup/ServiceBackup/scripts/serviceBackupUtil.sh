#!/bin/bash

##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
#
##

##
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer        Description
# ------------- -------- ---------       --------------------------------------------
# Feb 09, 2015  4103     dgilling        Initial Creation.
# Mar 27, 2015  4103     dgilling        Support new location for svcbu.properties.
# Apr 28, 2015  4427     dgilling        Add markTask functions.
# Jul 15, 2016  5747     dgilling        Move edex_static to common_static.
# Jun 29, 2021  8572     randerso        Use command line argument for site Id if provided
#                                        Merged svcbu.env into this file.
#                                        Moved path definitons from svcbu.properties into
#                                        svcbu_paths.properties since sites should not be
#                                        overriding them.
# Jul 27. 2023  2035937  Lisa.Singh      Renamed SITE paramater to prevent conflict
# Aug 29, 2023  2028172  jkelmer         Added function to set PRI_SITES env variable from
#                                        svcbu.properties PRIMARY_SITES, or 
#                                        AW_SITE_IDENTIFIER if unset
# Jan 23, 2025  2038386  tgurney         Ensure log dirs are not 777 permissions
# Nov 03, 2025  2039829  njensen         Update ensurePathExists() to default to umask 002
#

AWIPS_HOME=/awips2

if [[ -z "$1" ]]
then
    AW_SITE=${AW_SITE_IDENTIFIER}
else
    AW_SITE=`echo ${1} | tr [a-z] [A-Z]`
fi

source ${AWIPS_HOME}/edex/data/utility/common_static/base/gfe/config/svcbu.properties
SITE_SVCBU_OVERRIDE=${AWIPS_HOME}/edex/data/utility/common_static/site/${AW_SITE}/gfe/config/svcbu.properties
if [[ -f ${SITE_SVCBU_OVERRIDE} ]]
then
    echo "Using svcbu.properties for site ${AW_SITE}"
    source ${SITE_SVCBU_OVERRIDE}
fi

source ${AWIPS_HOME}/GFESuite/ServiceBackup/configuration/svcbu_paths.properties

export PATH=$PATH:$GFESUITE_HOME/bin:$GFESUITE_HOME/ServiceBackup/scripts:/awips2/fxa/bin/
source /etc/profile.d/awips2Python.sh

# Make the directories
[ ! -d ${SVCBU_HOME} ] && (umask 022;mkdir -p ${SVCBU_HOME})
[ ! -d ${IFPS_LOG} ] && (umask 022;mkdir -p ${IFPS_LOG})
[ ! -d ${LOCK_DIR} ] && (umask 022;mkdir -p ${LOCK_DIR})
[ ! -d ${IFPS_DATA} ] && (umask 022;mkdir -p ${IFPS_DATA})

function configureLogging()
{
    local program_name="$1"
    if [[ -n "$2" ]]
    then
        local siteid=`echo ${2} | tr [a-z] [A-Z]`
    fi

    if [[ -n "${siteid}" ]]
    then
        local log_dir=${IFPS_LOG}/${siteid}/`date +%Y%m%d`
    else
        local log_dir=${IFPS_LOG}/`date +%Y%m%d`
    fi
    local log_basename=${program_name}_`date +%H%M`
    local log_file=${log_dir}/${log_basename}
    ensurePathExists "${log_dir}" "002"
    touch ${log_file}

    exec 1>${log_file} 2>&1
}

function ensurePathExists()
{
    if [[ -n "$1" ]]
    then
        if [[ -n "$2" ]]
        then
            dirUmask="$2"
        else
            dirUmask="002"
        fi
        local path="$1"
        [ ! -d ${path} ] && (umask "$dirUmask";mkdir -p ${path})
    fi
}

function getLockFile()
{
    local lockname="$1"
    local siteid=`echo ${2} | tr [a-z] [A-Z]`

    local site_lock_path="${LOCK_DIR}/${siteid}"
    ensurePathExists "${site_lock_path}"

    echo "${site_lock_path}/${lockname}"
}

function isOperationInProgress()
{
    local lockname="$1"
    local siteid=`echo ${2} | tr [a-z] [A-Z]`

    local site_lock_path=$(getLockFile ${lockname} ${siteid})

    local retval=false
    if [[ -f ${site_lock_path} ]]
    then
        local status=`cat ${site_lock_path} | head -n 1`
        if [[ "${status}" = "IN_PROGRESS" ]]
        then
            retval=true
        fi
    fi

    echo "${retval}"
}

function getTempDirectory()
{
    local program_name=`echo ${1} | tr [A-Z] [a-z]`
    local siteid=`echo ${2} | tr [a-z] [A-Z]`

    local prog_temp_path="${SVCBU_HOME}/${siteid}/${program_name}"
    ensurePathExists "${prog_temp_path}"

    echo "${prog_temp_path}"
}

function getSiteIdFromTarFile()
{
    local tar_file="$1"

    local siteid=""
    if [[ -f ${tar_file} ]]
    then
        siteid=`tar -xOf ${tar_file} siteID.txt | tr '[A-Z]' '[a-z]'` 
    fi

    echo "${siteid}"
}

function markTask()
{
    local lock_file="$1"
    local task_status="$2"
    echo ${task_status} > ${lock_file}
}

function markTaskSuccess()
{
    local lock_file="$1"
    markTask ${lock_file} "SUCCESS"
}

function markTaskFailed()
{
    local lock_file="$1"
    markTask ${lock_file} "FAILED"
}

function markTaskInProgress()
{
    local lock_file="$1"
    markTask ${lock_file} "IN_PROGRESS"
}


#
# Determine local site id(s) using PRIMARY_SITES or AW_SITE_IDENTIFIER
#
# Sets/Modifies the PRI_SITE global variable. Sources primary site's
#  svcbu.properties in a subshell to prevent global variable corruption
# Must temporarily clear the PRIMARY_SITES to prevent backup site's
#  configuration from corrupting this check.
# # TODO: With bash 4.3+, arrays can be returned from functions. This 
# #       method should be changed if/when we upgrade bash
#
function getPrimarySites()
{
    PRIMARY_SVCBU_OVERRIDE=${AWIPS_HOME}/edex/data/utility/common_static/site/${AW_SITE_IDENTIFIER}/gfe/config/svcbu.properties
    AW_PRIMARY_SITES=""
    TEMP_PRIMARY_SITES=${PRIMARY_SITES}
    PRIMARY_SITES=
    echo ${PRIMARY_SVCBU_OVERRIDE}
    if [[ -f ${PRIMARY_SVCBU_OVERRIDE} ]]
    then
        echo "Using svcbu.properties for site ${AW_SITE_IDENTIFIER} to determine PRIMARY_SITES"
        AW_PRIMARY_SITES=$(source ${PRIMARY_SVCBU_OVERRIDE}; echo ${PRIMARY_SITES})
    fi
    IFS=',' read -ra PRI_SITES <<< "${AW_PRIMARY_SITES}"
    if [ ${#PRI_SITES[@]} -eq 0 ]
    then
        PRI_SITES=${AW_SITE_IDENTIFIER}
    fi
    PRIMARY_SITES=${TEMP_PRIMARY_SITES}
}

