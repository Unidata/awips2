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

# Run the C++ topic test
$srcdir = Split-Path $myInvocation.InvocationName

# Parameters with default values: s (subscribers) m (messages) b (batches)
#                                 h (host) t (false; use transactions)
param (
  [int]$subscribers = 10,
  [int]$messages = 2000,
  [int]$batches = 10,
  [string]$broker,
  [switch] $t           # transactional
)

# Clean up old log files
Get-Item subscriber_*.log | Remove-Item

if ($t) {
    $transactional = "--transactional --durable"
}

# Find which subdir the exes are in
. $srcdir\find_prog.ps1 .\topic_listener.exe

function subscribe {
    param ([int]$num)
    "Start subscriber $num"
    $LOG = "subscriber_$num.log"
    $cmdline = ".\$sub\topic_listener $transactional > $LOG 2>&1
                if (`$LastExitCode -ne 0) { Remove-Item $LOG }"
    $cmdblock = $executioncontext.invokecommand.NewScriptBlock($cmdline)
    . $srcdir\background.ps1 $cmdblock
}

function publish {
    Invoke-Expression ".\$sub\topic_publisher --messages $messages --batches $batches --subscribers $subscribers $host $transactional" 2>&1
}

if ($broker.length) {
  $broker = "-h$broker"
}

$i = $subscribers
while ($i -gt 0) {
  subscribe $i
  $i--
}

# FIXME aconway 2007-03-27: Hack around startup race. Fix topic test.
Start-Sleep 2
publish
exit $LastExitCode
