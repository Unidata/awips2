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

# Run the python tests; intended to be run by run_test.ps1 which sets up
# QPID_PORT
$srcdir = Split-Path $myInvocation.InvocationName
$PYTHON_DIR = "$srcdir\..\..\..\python"
if (!(Test-Path $PYTHON_DIR -pathType Container)) {
    "Skipping python tests as python libs not found"
    exit 1
}

if (Test-Path env:FAILING) {
    $fails = "-I $env:FAILING"
}
if (Test-Path env:PYTHON_TESTS) {
    $tests = "$env:PYTHON_TESTS"
}
else {
    $tests = "$args"
}

#cd $PYTHON_DIR
$env:PYTHONPATH="$PYTHON_DIR;$env:PYTHONPATH"
python $PYTHON_DIR/qpid-python-test -b localhost:$env:QPID_PORT $fails $tests
exit $LASTEXITCODE
