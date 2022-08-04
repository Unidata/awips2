#!/bin/bash
#
# link old manual endpoint to the new dropbox location
#
# This delta script should be run on dx3
#

source /awips2/edex/bin/setup.env

dropbox=${DATA_ARCHIVE_ROOT}/dropbox
manual=/awips2/edex/data/manual
tmpManual=${manual}.tmp

echo "INFO: Running delta script for DR 7582: Moving manual endpoint to ${dropbox}."

# if dropbox exists
if [ -d ${dropbox} ]
then
    echo "INFO: ${dropbox} already exists."
else
    # create dropbox
    echo "INFO: creating ${dropbox} directory"
    mkdir -p ${dropbox}
fi
chown awips:fxalpha ${dropbox}
chmod 777 ${dropbox}

# if manual is a symlink 
if [ -h ${manual} ]
then
    # if manual is not symlinked to dropbox
    target=`readlink -f ${manual}`
    if [ ${target} == ${dropbox} ]
        echo "INFO: ${manual} already linked to ${dropbox}"
    then
        # remove manual and link it to dropbox      
        echo "WARN: ${manual} is currently linked to ${target}. Changing to ${dropbox}"
        rm ${manual}
        ln -s ${target} ${tmpManual} 
    fi

# else if manual is a directory
elif [ -d ${manual} ]
then
    # rename manual to tmpManual, then link manual to dropbox
    mv ${manual} ${tmpManual}
    
# else if manual exists, remove it    
elif [ -a ${manual} ]
then
    rm ${manual}
fi

# if manual does not exist, link it to dropbox
if [ ! -a ${manual} ]
then
    echo "INFO: linking ${manual} to ${dropbox}"
    ln -s ${dropbox} ${manual}
fi

# if tmpManual exists and is a directory
if [ -d ${tmpManual} ]
then
    echo "INFO: Moving files to ${dropbox}"
    
    # move any files in the temp endpoint to the new endpoint
    if find ${tmpManual}/ -mindepth 1 -print -quit 2>/dev/null | grep -q .
    then
        mv ${tmpManual}/* ${dropbox}
    fi
    
    if [ -h ${tmpManual} ]
    then
        rm ${tmpManual}
    else
        rmdir ${tmpManual}
    fi
fi

echo "INFO: done"