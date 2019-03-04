#!/bin/bash
##
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

JAVA_INSTALL=`rpm -q --queryformat '%{INSTPREFIXES}' awips2-java`

${JAVA_INSTALL}/bin/java -classpath $awips_home/edex/lib/plugins/plugin-warning.jar:$awips_home/edex/lib/dependencies/org.geotools/jts-1.9.jar com.raytheon.edex.plugin.warning.tools.DamInfoTranslator $@
