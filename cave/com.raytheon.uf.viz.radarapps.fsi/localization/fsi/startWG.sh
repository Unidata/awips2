#!/bin/sh
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
# startWG.sh - start script for WDSSII wg process (FSI mode)
#
# Based on AWIPS 1 startWG.sh by Greg Stumpf
#
# This version combines the linear buffer operations and launching of wg
# into a single script.

while test "$#" -gt 0; do
    case "$1" in
	-l) lb=$2 ; shift ;;
	-m)
	    msg=$2;
	    msg_set=1;
	    shift
	    ;;
	-r) log=$2 ; shift ;;
	-h)
	    echo "usage: startWG.sh [-l <linear buffer>] [-m <control message>] [-r <log>]"
	    exit 0
	    ;;
	*)
	    echo "error: invalid option $1"
	    exit 1
	    ;;
    esac
    shift
done

# Set FSI environment if needed
if test -z "$FXA_HOME"; then export FXA_HOME=/awips2/fxa ; fi	

if test -z "$WDSS2"; then export WDSS2=${FXA_HOME}/fsi ; fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${WDSS2}/lib
if test -z "$W2_CONFIG_LOCATION"; then export W2_CONFIG_LOCATION=${WDSS2}/w2config ; fi
if test -z "$W2_EXTENSION_LIBS"; then export W2_EXTENSION_LIBS=w2nexrad ; fi
if test -z "$RMTPORT"; then export RMTPORT=50000 ; fi
export PATH=${WDSS2}:${WDSS2}/bin:${PATH}

if test -z "$lb"; then
    lb=$HOME/caveData/.metadata/.plugins/com.raytheon.uf.viz.radarapps.fsi/FSIcontrol.lb
    echo "Linear buffer not specified with --lb=<path>. Using $lb" >&2
fi

if test ! -f "$lb"; then
	$WDSS2/bin/lb_create -n 500 "$lb" || exit 1
fi

if test -n "$msg_set"; then
    echo "$msg" | $WDSS2/bin/lb_cat -w "$lb" || exit 1
fi

umask g+rwx
match=`ps -ef | grep $WDSS2/bin/wg | grep -v grep | grep -o wg`
if [ ! ${match} ]
then
#  wg is not running, starting wg
    if test -n "$log"; then
	$WDSS2/bin/wg -x $W2_CONFIG_LOCATION/misc/fsi-n.xul -l="file:///$lb?protocol=xmllb" --control="file:///$lb?protocol=xmllb" -p=1.0 >"$log" 2>&1 </dev/null &
    else
	$WDSS2/bin/wg -x $W2_CONFIG_LOCATION/misc/fsi-n.xul -l="file:///$lb?protocol=xmllb" --control="file:///$lb?protocol=xmllb" -p=1.0 &
    fi
fi
#wg  wg is running, not starting wg
exit 0
