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

# Run a PowerShell scriptblock in a background process.
param(
    [scriptblock] $script  # scriptblock to run
)

# break out of the script on any errors
trap { break }

# In order to pass a scriptblock to another powershell instance, it must
# be encoded to pass through the command line.
$encodedScript = [convert]::ToBase64String(
    [Text.Encoding]::Unicode.GetBytes([string] $script))

#$p = new-object System.Diagnostics.Process
$si = new-object System.Diagnostics.ProcessStartInfo
$si.WorkingDirectory = $pwd
$si.FileName = (get-command powershell.exe).Definition
$si.Arguments = "-encodedCommand $encodedScript"

###### debugging setup
#$si.CreateNoWindow = $true
# UseShellExecute false required for RedirectStandard(Error, Output)
#$si.UseShellExecute = $false
#$si.RedirectStandardError = $true
#$si.RedirectStandardOutput = $true
######
$si.UseShellExecute = $true

##### Debugging, instead of the plain Start() above.
#$output = [io.File]::AppendText("start.out")
#$error = [io.File]::AppendText("start.err")
$p = [System.Diagnostics.Process]::Start($si)
#$output.WriteLine($p.StandardOutput.ReadToEnd())
#$error.WriteLine($p.StandardError.ReadToEnd())
#$p.WaitForExit()
#$output.Close()
