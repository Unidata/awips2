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

# Simple test of encode/decode of a double in application headers
# TODO: this should be expanded to cover a wider set of types and go
# in both directions

$srcdir = Split-Path $myInvocation.InvocationName
$PYTHON_DIR = "$srcdir\..\..\..\python"
if (!(Test-Path $PYTHON_DIR -pathType Container)) {
    "Skipping header test as python libs not found"
    exit 0
}

if (Test-Path qpidd.port) {
   set-item -path env:QPID_PORT -value (get-content -path qpidd.port -totalcount 1)
}

# Test runs from the tests directory but the test executables are in a
# subdirectory based on the build type. Look around for it before trying
# to start it.
. $srcdir\find_prog.ps1 .\header_test.exe
if (!(Test-Path $prog)) {
    "Cannot locate header_test.exe"
    exit 1
}

Invoke-Expression "$prog -p $env:QPID_PORT" | Write-Output
$env:PYTHONPATH="$PYTHON_DIR;$env:PYTHONPATH"
Invoke-Expression "python $srcdir/header_test.py localhost $env:QPID_PORT" | Write-Output
exit $LASTEXITCODE
