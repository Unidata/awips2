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

# Run the federation tests.

$srcdir = Split-Path $myInvocation.InvocationName
$PYTHON_DIR = "$srcdir\..\..\..\python"
if (!(Test-Path $PYTHON_DIR -pathType Container)) {
    "Skipping federation tests as python libs not found"
    exit 1
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

function start_brokers {
  # Start 2 brokers, saving the port numbers in LOCAL_PORT, REMOTE_PORT.
  . $srcdir\background.ps1 $cmdblock
  while (!(Test-Path qpidd.port)) {
    Start-Sleep 2
  }
  set-item -path env:LOCAL_PORT -value (get-content -path qpidd.port -totalcount 1)
  Remove-Item qpidd.port
  . $srcdir\background.ps1 $cmdblock
  while (!(Test-Path qpidd.port)) {
    Start-Sleep 2
  }
  set-item -path env:REMOTE_PORT -value (get-content -path qpidd.port -totalcount 1)
}

function stop_brokers {
   Invoke-Expression "$prog -q --port $env:LOCAL_PORT" | Out-Default
   Invoke-Expression "$prog -q --port $env:REMOTE_PORT" | Out-Default
}

trap {
  &stop_brokers
  break
}

&start_brokers
"Running federation tests using brokers on ports $env:LOCAL_PORT $env:REMOTE_PORT"
$env:PYTHONPATH=$PYTHON_DIR
python $srcdir/federation.py -v -s $srcdir\..\..\..\specs\amqp.0-10-qpid-errata.xml -b localhost:$env:LOCAL_PORT --remote-port $env:REMOTE_PORT $args
$RETCODE=$LASTEXITCODE
&stop_brokers
if ($RETCODE -ne 0) {
    "FAIL federation tests"
    exit 1
}
