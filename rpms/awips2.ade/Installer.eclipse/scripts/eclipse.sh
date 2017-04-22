# usage:
# eclipse.sh [args]    -- launches eclipse using specified command line args
# eclipse.sh           -- launches Eclipse with default command line args

dir=${0%/*}
if [ "${dir}" = "$0" ]; then
   dir="."
fi

PYTHON_INSTALL=
JAVA_INSTALL=
ANT_INSTALL=
ECLIPSE_INSTALL=
CAVE_INSTALL=

rpm -q awips2-python > /dev/null 2>&1
if [ $? -ne 0 ]; then
   echo "ERROR: awips2-python Is Not Installed."
   echo "Unable To Continue ... Terminating"
   exit 1
fi
PYTHON_INSTALL="/awips2/python"

rpm -q awips2-java > /dev/null 2>&1
if [ $? -ne 0 ]; then
   echo "ERROR: awips2-java Is Not Installed."
   echo "Unable To Continue ... Terminating"
   exit 1
fi
JAVA_INSTALL="/awips2/java"

rpm -q awips2-ant > /dev/null 2>&1
if [ $? -ne 0 ]; then
   echo "ERROR: awips2-ant Is Not Installed."
   echo "Unable To Continue ... Terminating"
   exit 1
fi
ANT_INSTALL="/awips2/ant"
ECLIPSE_INSTALL="/awips2/eclipse"
cd ${dir}

# grab the CL argument; if none set a reasonable default
if [ $# -ne 0 ]; then
   # there are arguments, convert them into a string
   args=${1}
   shift 1
   for a in $@; do
      args="${args} ${a}"
   done
else
   # set a reasonable default for performance
   args='-clean -vmargs -Xms512m -Xmx1024m -XX:MaxPermSize=256m'
fi

# setup environment variables
export JAVA_HOME=${JAVA_INSTALL}
export ANT_HOME=${ANT_INSTALL}

# set LD_PRELOAD
export LD_PRELOAD=${PYTHON_INSTALL}/lib/libpython2.7.so

# update path type variables
export LD_LIBRARY_PATH=${PYTHON_INSTALL}/lib:$LD_LIBRARY_PATH
export PATH=${ECLIPSE_INSTALL}:${PYTHON_INSTALL}/bin:${JAVA_INSTALL}/bin:$PATH

# determine if cave has been installed for TMCP_HOME
rpm -q awips2-cave > /dev/null 2>&1
if [ $? -ne 0 ]; then
   CAVE_INSTALL="/awips2/cave"
   export TMCP_HOME=${CAVE_INSTALL}/caveEnvironment
else
   echo "WARNING: awips2-cave is not installed; so, TMCP_HOME will not be set."
fi

#%INSTALL_PATH/eclipse/eclipse -clean -vmargs -Xms512m -Xmx1024m -XX:MaxPermSize=256m &
echo "./eclipse ${args} &"
nohup ./eclipse ${args} > /dev/null 2>&1 &
echo ""
echo "Successful Eclipse Startup ..."
exit 0
