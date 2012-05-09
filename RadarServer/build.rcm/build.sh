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
USAGE="Usage: `basename $0` [options]\nOptions:\n
\t-eclipse=ECLIPSEPATH\tSets the full path to the eclipse directory REQUIRED\n"

OPTS_FOR_ANT=""

for i in $*
do
        case $i in
        -D*)
                OPTS_FOR_ANT=`echo "$OPTS_FOR_ANT $i "`
                ;;
        -eclipse=*)
                ECLIPSE_HOME=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
                ;;
	-vvv)
		VERBOSE=true
		;;
        *)
                # unknown option
                ;;
        esac
done

if [ -z $ECLIPSE_HOME ]; then
        echo -e $USAGE
        exit 1
fi

if [ -n $VERBOSE ]; then
	echo "AntOpts: '$OPTS_FOR_ANT'"
fi

rm -rf tmp

export BUILDER=`pwd`

#get name of org.eclipse.equinox.launcher_*.jar in ECLIPSE_HOME with version label
export LAUNCHER_JAR=`ls $ECLIPSE_HOME/plugins/org.eclipse.equinox.launcher_*.jar`

#get name of org.eclipse.pde.build in ECLIPSE_HOME with version label
export PDE_BUILD=`ls -d $ECLIPSE_HOME/plugins/org.eclipse.pde.build_*`

#Execute the build
java -jar $LAUNCHER_JAR -application org.eclipse.ant.core.antRunner \
-buildfile ${PDE_BUILD}/scripts/productBuild/productBuild.xml \
-DbaseLocation=$ECLIPSE_HOME \
-Dbuilder=$BUILDER \
-DbuildDirectory=${BUILDER}/tmp \
-Dbase=$BUILDER \
$OPTS_FOR_ANT
