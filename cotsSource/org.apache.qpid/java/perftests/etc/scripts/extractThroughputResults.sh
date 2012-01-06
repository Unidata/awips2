#!/bin/bash +x
#
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
#

#
# Process a given directory (defaults to '.') and provides the throughput results as 
# reported by the tests.
# 
# if a second argument of -n is provided then it will only list the number output for 
# easy copy/paste
#

if [ $# == 0 ] ; then
 dir=.
else
 dir=$1
fi

numeric=0
if [ "$dir" == "-n" ] ; then
 numeric=1
 dir=.
fi

if [ "$2" == "-n" ] ; then
 numeric=1
fi

if [ $numeric == 1 ] ; then
   grep 'Total Tests:' $dir/*Qpid* | sed -e 's/^.*\/\([A-Z]*-[A-Z][A-Z]-Qpid-01\).*Size Throughput:, \([0-9.]*\).*$/\1, \2/' | sed -e 's/\.\([0-9][0-9][0-9]\)/\1\./' | sed -e 's/, 0/, /' | awk '{print $2}'
else
   grep 'Total Tests:' $dir/*Qpid* | sed -e 's/^.*\/\([A-Z]*-[A-Z][A-Z]-Qpid-01\).*Size Throughput:, \([0-9.]*\).*$/\1, \2/' | sed -e 's/\.\([0-9][0-9][0-9]\)/\1\./' | sed -e 's/, 0/, /'
fi
