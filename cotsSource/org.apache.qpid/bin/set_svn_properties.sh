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

# This script will set the proper svn properties on all the files in the tree.
# It pretty much requires a gnu compatible xargs (for the -r flag).  Running
# on Linux is probably the best option, or gnuxargs is available for OS X from
# darwinports.


find . -name "*.java" | grep -v ".svn" | xargs -n 1 -r svn propset svn:eol-style native
find . -name "*.java" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.py" | grep -v ".svn" | xargs -n 1 -r svn propset svn:eol-style native

find . -name "*.rb" | grep -v ".svn" | xargs -n 1 -r svn propset svn:eol-style native

find . -name "*.xml" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml
find . -name "*.xml" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.xml" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.xsl" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml
find . -name "*.xsl" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.xsl" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.xsd" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml
find . -name "*.xsd" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.xsd" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.wsdl" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml
find . -name "*.wsdl" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.wsdl" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.properties" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain
find . -name "*.properties" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.properties" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.txt" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.txt" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

find . -name "*.htm*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.htm*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/html
find . -name "*.htm*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "README*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "README*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

find . -name "LICENSE*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "LICENSE*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

find . -name "NOTICE*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "NOTICE*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

find . -name "TODO*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "TODO*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

find . -name "KEYS*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "KEYS*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

find . -name "*.png" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type image/png
find . -name "*.gif" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type image/gif
find . -name "*.jpg" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type image/jpeg
find . -name "*.jpeg" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type image/jpeg


find . -name "*.scdl" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.scdl" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml

find . -name "*.fragment" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.fragment" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml

find . -name "*.componentType" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.componentType" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml

find . -name "*.wsdd" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml
find . -name "*.wsdd" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native

find . -name "sca.subsystem" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml
find . -name "sca.subsystem" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "Tuscany-model.config" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/xml
find . -name "Tuscany-model.config" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native



find . -name "*.cpp" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.cpp" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.c" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.c" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.h" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.h" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "*.am" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.am" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:keywords "Rev Date"

find . -name "ChangeLog*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "ChangeLog*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

find . -name "*.sh" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.sh" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain
find . -name "*.sh" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:executable ""

find . -name "*.bat" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style CRLF
find . -name "*.bat" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

find . -name "*.cmd" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "*.cmd" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain
find . -name "*.cmd" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:executable ""

find . -name "INSTALL*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "INSTALL*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain
find . -name "COPYING*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "COPYING*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain
find . -name "NEWS*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:eol-style native
find . -name "NEWS*" | grep -v ".svn" | xargs -n 1 -r  svn propset svn:mime-type text/plain

