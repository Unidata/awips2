package org.apache.qpid.transport;
/*
 * 
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * 
 */


class StructFactory {

    public static Struct create(int type)
    {
        switch (type)
        {
${
from genutil import *

fragment = """        case $name.TYPE:
            return new $name();
"""

for c in composites:
  name = cname(c)
  if c.name == "struct":
    out(fragment)
}        default:
            throw new IllegalArgumentException("type: " + type);
        }
    }

    public static Struct createInstruction(int type)
    {
        switch (type)
        {
${
for c in composites:
  name = cname(c)
  if c.name in ("command", "control"):
    out(fragment)
}        default:
            throw new IllegalArgumentException("type: " + type);
        }
    }

}
