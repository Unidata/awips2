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
# NDM Menu File Ingester

if [ "$1" == "-help" ];
then
	echo ""
	echo "ndmMenuIngester.sh <ndmFileDirectory> <ndmMenuFileName>"
	echo "    ndmFileDirectory is the directory holding the ndm files"
	echo "    ndmMenuFileName is the name of the ndm menu file to convert"
	echo ""
	echo "    Required files:  redbookDataKeys.txt, redbookDepictKeys.txt,"
	echo "       redbookProductButtons.txt, and the menu file to convert"
	exit;
fi
if [ ! -d "$1" ];
then
	echo "Directory [$1] does not exist!"
	exit
fi

if [ ! -f "$1/$2" ];
then
	echo "File [$2] does not exist!"
	exit
fi
SRC_DIR=$1
MENU_FILE=$2
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)
dir=$(dirname $dir)
AWIPS_HOME=$(dirname $dir)

# Search for jars
EDEX_PLUGINS="$AWIPS_HOME/edex/lib/"
JARS=$(find $EDEX_PLUGINS -name "*.jar")

# Add jars to classpath
addSep=false
for i in $JARS
do
	if [[ "$addSep" == true ]];
	then
		LOCAL_CLASSPATH=$LOCAL_CLASSPATH":"$i
	else
		LOCAL_CLASSPATH=$i
		addSep=true
	fi
done

JAVA_INSTALL=/awips2/java/
${JAVA_INSTALL}/bin/java -classpath $LOCAL_CLASSPATH com.raytheon.uf.edex.plugin.redbook.menu.NdmMenuConverter $SRC_DIR $MENU_FILE

