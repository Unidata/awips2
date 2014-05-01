#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

# If QPIDD_EXEC ..etc is not set, it will first check to see
# if this is run from a qpid svn check out, if not it will look
# for installed rpms.

abs_path()
{
  D=`dirname "$1"`
  B=`basename "$1"`
  echo "`cd \"$D\" 2>/dev/null && pwd || echo \"$D\"`/$B"
}

# Environment for python tests

if [ -d ../../../python ] ; then
   PYTHON_DIR=../../../python
   PYTHONPATH=$PYTHON_DIR:$PYTHON_DIR/qpid
elif [ -z `echo $PYTHONPATH | awk '$0 ~ /qpid/'` ]; then
   echo "WARNING: skipping test, no qpid python scripts found ."; exit 0;
fi          


if [ "$QPIDD_EXEC" = "" ] ; then
   if [ -x ../../../cpp/src/qpidd ]; then
      QPIDD_EXEC=`abs_path "../../../cpp/src/qpidd"`
   elif [ -n "$(which qpidd)" ] ; then
      QPIDD_EXEC=$(which qpidd)   
   else
      echo "WARNING: skipping test, QPIDD_EXEC not set and qpidd not found."; exit 0;
   fi
fi

if [ "$CLUSTER_LIB" = "" ] ; then
   if [ -x ../../../cpp/src/.libs/cluster.so ]; then
      CLUSTER_LIB=`abs_path "../../../cpp/src/.libs/cluster.so"`
   elif [ -e /usr/lib64/qpid/daemon/cluster.so ] ; then
      CLUSTER_LIB="/usr/lib64/qpid/daemon/cluster.so"
   elif [ -e /usr/lib/qpid/daemon/cluster.so ] ; then 
      CLUSTER_LIB="/usr/lib/qpid/daemon/cluster.so"
   else
     echo "WARNING: skipping test, CLUSTER_LIB not set and cluster.so not found."; exit 0;
   fi
fi

if [ "$QP_CP" = "" ] ; then
   if [ -d ../../build/lib/ ]; then
      QP_JAR_PATH=`abs_path "../../build/lib/"`
   elif [ -d /usr/share/java/qpid-deps ]; then
      QP_JAR_PATH=`abs_path "/usr/share/java"`
   else
      "WARNING: skipping test, QP_CP not set and the Qpid jars are not present."; exit 0; 	
   fi
   QP_CP=`find $QP_JAR_PATH -name '*.jar' | tr '\n' ':'`
fi

if [ "$OUTDIR" = "" ] ; then
   OUTDIR=`abs_path "../output"`
fi

export PYTHONPATH PYTHON_DIR QPIDD_EXEC CLUSTER_LIB QP_CP OUTDIR
