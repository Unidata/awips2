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
##
set -o errexit
set -o nounset
#set -o xtrace

usage="$0 link_program [arguments]"
link_program=""

if [ "$#" = "0" ]
then
    echo $usage
else
    link_program="$1"
    shift
    options=$@
fi

library_directories=""
XLinker_options=""
output_artifact=""

option_array=( $options )

for i in $(seq 0 $((${#option_array[*]} - 1)))
do
    case ${option_array[$i]} in
        -Xlinker)
            XLinker_options="$XLinker_options ${option_array[$(($i +1))]}"
            ;;
        -L*)
            library_directories="$library_directories ${option_array[$i]}"
            ;;
        -o*)
            output_artifact="${option_array[$i]/-o/}"
            ;;
        -m32)
            XLinker_options="$XLinker_options -melf_i386"
            ;;
    esac
done

if [ "$link_program" = "pgf90" ]
then
    # map gcc options to fortran
    for xlinker_opt in $XLinker_options
    do
        options="${options/-Xlinker $xlinker_opt/}"
    done
    options="${options/-m32/-tp px-32}"
    
    invalid_options=$(pgf90 $options --flagcheck 2>&1 | grep "Unknown switch" | sed "s/.*Unknown switch: \(.*\)/\1/")

    if [ ! "$invalid_options" = "" ]
    then
        echo "WARNING: ignoring unsupported options: '$invalid_options'"
    fi

    # remove all invalid options
    for opt in $invalid_options
    do
        options="${options/$opt/}"
    done
    
    eval "$link_program $options && ld ${library_directories/-L/-rpath } $XLinker_options $output_artifact && rm a.out || (rm -f $output_artifact && exit 1)"
    
else
    
    eval "$link_program $options"

fi
