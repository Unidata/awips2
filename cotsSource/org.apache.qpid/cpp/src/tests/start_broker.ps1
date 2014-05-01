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

# Get the directory where this script resides.
function Get-ScriptPath
  { Split-Path $myInvocation.ScriptName }

# Start a test broker and capture it's port (from stdout) to qpidd.port
# This script will exit immediately after spawning the broker process. To avoid
# running more tests before the broker is initialized, wait for the qpidd.port
# file to appear before exiting.
if (Test-Path qpidd.port) {
   Remove-Item qpidd.port
}

# Test runs from the tests directory but the broker executable is one level
# up, and most likely in a subdirectory from there based on what build type.
# Look around for it before trying to start it.
$subs = "Debug","Release","MinSizeRel","RelWithDebInfo"
foreach ($sub in $subs) {
  $prog = "..\$sub\qpidd.exe"
  if (Test-Path $prog) {
     break
  }
}
if (!(Test-Path $prog)) {
    "Cannot locate qpidd.exe"
    exit 1
}
$cmdline = "$prog --auth=no --no-module-dir --port=0 --log-to-file qpidd.log $args | foreach { set-content qpidd.port `$_ }"
$cmdblock = $executioncontext.invokecommand.NewScriptBlock($cmdline)
$srcdir = Get-ScriptPath
. $srcdir\background.ps1 $cmdblock

$wait_time = 0
while (!(Test-Path qpidd.port) -and ($wait_time -lt 10)) {
   Start-Sleep 2
   $wait_time += 2
}
if (Test-Path qpidd.port) {
  exit 0
}
"Time out waiting for broker to start"
exit 1
