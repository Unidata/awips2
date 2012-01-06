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

using System;
using org.apache.qpid.transport.codec;
using System.Collections.Generic;
using org.apache.qpid.transport.util;
using org.apache.qpid.transport.network;
using System.IO;

namespace org.apache.qpid.transport
{

${
from genutil import *

cls = klass(type)["@name"]

segments = type["segments"]

if type.name in ("control", "command"):
  override = "override"
  base = "Method"
  size = 0
  pack = 2
  if segments:
    payload = "true"
  else:
    payload = "false"
  if type.name == "control" and cls == "connection":
    track = "Frame.L1"
  elif cls == "session" and type["@name"] in ("attach", "attached", "detach", "detached"):
    track = "Frame.L2"
  elif type.name == "command":
    track = "Frame.L4"
  else:
    track = "Frame.L3"
else:  
  override = ""
  base = "Struct"
  size = type["@size"]
  pack = num(type["@pack"])
  payload = "false"
  track = "4"

PACK_TYPES = {
  1: "byte",
  2: "int",
  4: "int"
}

typecode = code(type)
}

public sealed class $name : $base {

    public const int TYPE = $typecode;

    public override int GetStructType() {
        return TYPE;
    }

    public override int GetSizeWidth() {
        return $size;
    }

    public override int GetPackWidth() {
        return $pack;
    }

    public $override bool HasPayload() {
        return $payload;
    }

    public $override byte EncodedTrack 
    {
       get{ return $track; }
       set { throw new NotImplementedException(); }
    }

${
from dotnetgenutil import *
if pack > 0:
  out("    private $(PACK_TYPES[pack]) packing_flags = 0;\n");

fields = get_fields(type)
params = get_dotnetparameters(type, fields)
options = get_options(fields)

for f in fields:
  if not f.empty:
    out("    private $(f.type) _$(f.name);\n")

if segments:
  out("    private Header _header;\n")
  out("    private MemoryStream _body = new MemoryStream();\n")
}

${
if fields:
  out("    public $name() {}\n")
}

    public $name($(", ".join(params))) {
${
for f in fields:
  if f.option: continue 
  out("        $(f.set)($(f.name));\n")

if segments:
  out("        Header = header;\n")
  out("        Body = body;\n")

if options or base == "Method":
  out("""
        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
""")

  for f in options:
    out("            case Option.$(f.option): packing_flags |= $(f.flag_mask(pack)); break;\n")

  if base == "Method":
    out("""            case Option.SYNC: Sync = true; break;
            case Option.BATCH: Batch = true; break;
""")
  out("""            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }
""")
}
    }

    public $override void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.$(name)(context, this);
    }

${
for f in fields:
  if pack > 0:
    out("""
    public bool $(f.has)() {
        return (packing_flags & $(f.flag_mask(pack))) != 0;
    }

    public $name $(f.clear)() {
        packing_flags = (byte) (packing_flags & ~$(f.flag_mask(pack)));       
${
if (not f.empty and not (f.default == "null")):
  out("        _$(f.name) =  $(f.default);")
}
        Dirty = true;
        return this;
    }
""")

  out("""
    public $(f.type) $(f.get)() {
${
if f.empty:
  out("        return $(f.has)();")
else:
  out("        return _$(f.name);")
}
    }

    public $name $(f.set)($(f.type) value) {
${
if not f.empty:
  out("        _$(f.name) = value;")
}
${
if pack > 0:
  out("        packing_flags |=  $(f.flag_mask(pack));")
}
        Dirty = true;
        return this;
    }

""")
}

${
if segments:
    out("""    public override Header Header {
        get { return _header;}
        set { _header = value;}
	      }
	      
    public $name SetHeader(Header header) {
        Header = header;
        return this;
    }

    public override MemoryStream Body
    {
       get{ return _body;}
       set{ _body = value;}
    }

    public $name  SetBody(MemoryStream body)
    {
        Body = body;
        return this;
    }
""")
}

    public override void Write(IEncoder enc)
    {
${
if pack > 0:
  out("        enc.WriteUint%s(packing_flags);\n" % (pack*8));

for f in fields:
  if f.empty:
    continue
  if pack > 0:
    out("        if ((packing_flags & $(f.flag_mask(pack))) != 0)\n    ")
  pre = ""
  post = ""
  if f.type_node.name == "struct":
    pre = "%s.TYPE, " % cname(f.type_node)
  elif f.type_node.name == "domain":
    post = ""
    pre = "(short)"
  out("        enc.Write$(f.coder)($(pre)_$(f.name)$(post));\n")
}
    }

    public override void Read(IDecoder dec)
    {
${
if pack > 0:
   out("        packing_flags = ($(PACK_TYPES[pack])) dec.ReadUint%s();\n" % (pack*8));

for f in fields:
  if f.empty:
    continue
  if pack > 0:
    out("        if ((packing_flags & $(f.flag_mask(pack))) != 0)\n    ")
  pre = ""
  post = ""
  arg = ""
  if f.type_node.name == "struct":
    pre = "(%s)" % cname(f.type_node)
    arg = "%s.TYPE" % cname(f.type_node)
  elif f.type_node.name == "domain":
    pre = "%sGetter.Get(" % cname(f.type_node)
    post = ")"
  out("        _$(f.name) = $(pre)dec.Read$(f.coder)($(arg))$(post);\n")
}
    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

${
for f in fields:
  if pack > 0:
    out("        	if ((packing_flags & $(f.flag_mask(pack))) != 0)\n    ")
  out('        	result.Add("_$(f.name)", $(f.get)());\n')
}
			return result;
        }
    }

}
}
