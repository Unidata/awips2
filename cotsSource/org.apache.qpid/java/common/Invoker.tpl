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


import java.nio.ByteBuffer;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public abstract class $(invoker) {
${
from genutil import *

results = False

for c in composites:
  name = cname(c)
  fields = get_fields(c)
  params = get_parameters(c, fields)
  args = get_arguments(c, fields)
  result = c["result"]
  if result:
    results = True
    if not result["@type"]:
      rname = cname(result["struct"])
    else:
      rname = cname(result, "@type")
    jresult = "Future<%s>" % rname
    jreturn = "return "
    jclass = ", %s.class" % rname
  else:
    jresult = "void"
    jreturn = ""
    jclass = ""

  if c.name == "command":
    access = "public "
  else:
    access = ""

  out("""
    $(access)final $jresult $(dromedary(name))($(", ".join(params))) {
        $(jreturn)invoke(new $name($(", ".join(args)))$jclass);
    }
""")
}
    protected abstract void invoke(Method method);
${
if results:
  out("""
    protected abstract <T> Future<T> invoke(Method method, Class<T> resultClass);
""")
}
}
