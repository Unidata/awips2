#!/bin/bash
###########################################################
#Licensed to the Apache Software Foundation (ASF) under one
#or more contributor license agreements. See the NOTICE file
#distributed with this work for additional information
#regarding copyright ownership. The ASF licenses this file
#to you under the Apache License, Version 2.0 (the
#"License"); you may not use this file except in compliance
#with the License. You may obtain a copy of the License at
#
#http://www.apache.org/licenses/LICENSE-2.0
#
#Unless required by applicable law or agreed to in writing,
#software distributed under the License is distributed on an
#"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#KIND, either express or implied. See the License for the
#specific language governing permissions and limitations
#under the License.
###########################################################

runVerifyScript()
{
 echo "-----------<Run verify scripts>-------------"
 $CC_HOME/cc/scripts/verify_all
 echo "-----------</Run verify scripts>------------"
 echo ""
}

cleanup()
{
rm -f $CC_HOME/script.log
rm -f $CC_HOME/status.log
rm -f $CC_HOME/broker.log
}

checkErrors()
{
if test `cat $CC_HOME/script.log | grep -c 'FAIL'`  -gt 0
then
 echo "FAILED"
 printErrors > $CC_HOME/status.log
fi
}

printErrors(){
 echo "<desc>The following scripts had failures"
 cat CC_HOME/script.log | awk '{
 script = ""
 while ((getline) == 1)
 { 
   if ($1 == "script:")
   {
     script = $0
   }
   if ($1 == "FAIL")
   {
     print substr(script,9)
   }
 }}'
 echo "</desc>"
}

echo "*************************************"
echo "Example Automation "
echo ""
cleanup
runVerifyScript
checkErrors
echo "*************************************"
