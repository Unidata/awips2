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


${from genutil import *}

public enum Type
{

${
types = spec.query["amqp/type"] + spec.query["amqp/class/type"]
codes = {}
first = True
for t in types:
  code = t["@code"]
  fix_width = t["@fixed-width"]
  var_width = t["@variable-width"]

  if code is None:
    continue

  if fix_width is None:
    width = var_width
    fixed = "false"
  else:
    width = fix_width
    fixed = "true"

  name = scream(t["@name"])
  codes[code] = name

  if first:
    first = False
  else:
    out(",\n")

  out("    $name((byte) $code, $width, $fixed)")
};

    public byte code;
    public int width;
    public boolean fixed;

    Type(byte code, int width, boolean fixed)
    {
        this.code = code;
        this.width = width;
        this.fixed = fixed;
    }

    public static Type get(byte code)
    {
        switch (code)
        {
${
keys = list(codes.keys())
keys.sort()

for code in keys:
  out("        case (byte) $code: return $(codes[code]);\n")
}
        default: return null;
        }
    }
}
