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
# resolve/set local variables
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)
dir=$(dirname $dir)
awips_home=$(dirname $dir)

rpm -q awips2-java > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: awips2-java Must Be Installed."
   echo "Unable To Continue ... Terminating."
fi

JAVA_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}' awips2-java`

${JAVA_INSTALL}/bin/java -classpath $awips_home/edex/lib/plugins/plugin-warning.jar:$awips_home/edex/lib/dependencies/org.geotools/jts-1.9.jar com.raytheon.edex.plugin.warning.tools.DamInfoTranslator $@
