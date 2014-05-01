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

# Locate the subdirectory where the specified program resides; the program
# must have a directory and a file name, even if the directory is .
param(
    [string] $prog  # program to look for somewhere below cwd
)

$dir = Split-Path $prog
$exe = Split-Path $prog -leaf
$sub = ""
$subs = "Debug","Release","MinSizeRel","RelWithDebInfo"
foreach ($try in $subs) {
   $prog = "$dir\$try\$exe"
   if (Test-Path $prog) {
      $sub = $try
      break
   }
}
