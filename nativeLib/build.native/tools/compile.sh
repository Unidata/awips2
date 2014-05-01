#!/bin/bash

set -o errexit
set -o nounset
#set -o xtrace

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
##

# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Sep 4,  2008             jelkins     - Initial creation
# Sep 8,  2008             jelkins     - Add source file directory to INCLUDE
#                                      - Add C_COMPILER arguments
# Apr 9,  2009             jelkins     - Support lex files
# Oct 1,  2009			   jelkins     - Support configurable file type "plugins"
# June 4, 2013  #2021      bkowal      - handle the case when there is a space between
#                                        the -o directive and the name of the output
# 
# 
# @author: jelkins

# this wrapper determines the source file type and uses the appropriate compiler
# pass the same arguments to this script as gcc

# file type support is defined through *_filetype.cfg files.  The 
# default_filetype_directory points to a few standard pre-defined 
# filetypes such as c-c++ and fortran.  Addition filetype support can be
# added by adding the directory containing _filetype.cfg files to the
# COMPILE_FILETYPE environment variable.  The COMPILE_FILETYPE
# environment variable is a colon seperated list of directories.

script_directory=$(dirname "$(readlink -f ${BASH_SOURCE[0]})")
script=$(basename "$0")

default_filetype_directory=$script_directory/compile_filetypes
compile_filetype_directories=( $default_filetype_directory )

INCLUDE=
OUT_FILE=
IN_FILE=
OPTIONS=
_out_next=false

# --- Determine the includes, options, input file, and output file

for section in $@
do
    case "$section" in
        -I*)
            INCLUDE="$INCLUDE $section"
            ;;
        -o*)
            OUT_FILE="${section/-o/}"
            if [ "${OUT_FILE}" = "" ]; then
               _out_next=true
            fi
            ;;
        -*)
            OPTIONS="$OPTIONS $section"
            ;;
        *)
            if [ ${_out_next} = true ]; then
               _out_next=false
               OUT_FILE="${section}"
            else
               IN_FILE="$section"
               
               # add the IN_FILE_DIRECTORY to the include path     
               IN_FILE_DIRECTORY="`dirname $IN_FILE`"
               INCLUDE="-I'$IN_FILE_DIRECTORY' $INCLUDE"
            fi               
            ;;
    esac
done

# perform a recursive call to compile.sh
# this is useful to call from a filetype plugin that pre-processes
# a file into another filetype such as c or c++
compile_r() {
    eval "$script_directory/$script $OPTIONS $INCLUDE -o$OUT_FILE $IN_FILE"
}

# compile using the provided compiler
compile() {
    eval "$1 $OPTIONS $INCLUDE -o$OUT_FILE $IN_FILE"
}

# provide an easy variable to use to obtain the extension
extension=${IN_FILE##*.}

# check to see if COMPILE_FILETYPE has been defined
if [ "$(env | egrep ^COMPILE_FILETYPE=)" != "" ]
then
	OLD_IFS="$IFS"
	IFS=":"
	compile_filetype_directories=( $COMPILE_FILETYPE $compile_filetype_directories )
	IFS="$OLD_IFS"
fi

export error="false"

# include filetype configurations	
for directory in ${compile_filetype_directories[@]}; do
	for file in $directory/*_filetype.cfg
	do
		. $file
	done
done

# include the default filetype configuration if we don't have an output yet
if [ ! -e "$OUT_FILE" ]
then
	. ${default_filetype_directory}/c_filetype_default.cfg
fi
