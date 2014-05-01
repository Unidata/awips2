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
# Process a given directory (defaults to '.') and provide a list of the tests run so 
# identification of any failures can be seen.
#

if [ $# == 0 ] ; then
 dir=.
else
 dir=$1
fi

grep 'Total Tests:' $dir/*Qpid* | sed -e 's/^.*\/\([A-Z\-]*-Qpid-[0-9]*\).*Total Tests:, \([0-9.]*\).*Total Passed:, \([0-9.]*\).*Total Failed:, \([0-9.]*\).*Total Error:, \([0-9.]*\).*$/\1, Total:\t\2,\tPassed:\t\3,\tFailed:\t\4,\tError:\t\5/'
