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

def camel(offset, *args):
  parts = []
  for a in args:
    parts.extend(a.split("-"))
  return "".join(parts[:offset] + [p[0].upper() + p[1:] for p in parts[offset:]])

def dromedary(s):
  return s[0].lower() + s[1:]

def scream(*args):
  return "_".join([a.replace("-", "_").upper() for a in args])

def num(x, default=None):
  if x is not None and x != "":
    return int(x, 0)
  else:
    return default

def klass(nd):
  parent = nd.parent
  while parent is not None:
    if hasattr(parent, "name") and parent.name == "class":
      return parent
    parent = parent.parent

untyped = -1

def code(nd):
  global untyped
  cd = num(nd["@code"])
  if cd is None:
    cd = untyped
    untyped -= 1
    return cd

  cls = klass(nd)
  if cls:
    cd |= (num(cls["@code"]) << 8)
  return cd

def root(nd):
  if nd.parent is None:
    return nd
  else:
    return root(nd.parent)

def qname(nd):
  name = nd["@name"]
  cls = klass(nd)
  if cls != None:
    return "%s.%s" % (cls["@name"], name)
  else:
    return name

RESOLVED = {}

def resolve(node, name):
  key = (node, name)
  if RESOLVED.has_key(key):
    return RESOLVED[key]
  else:
    spec = root(node)
    cls = klass(node)
    if cls:
      for nd in cls.query["#tag"]:
        if nd["@name"] == name:
          RESOLVED[key] = nd
          return nd
    for nd in spec.query["amqp/#tag"] + spec.query["amqp/class/#tag"]:
      if name == qname(nd):
        RESOLVED[key] = nd
        return nd
    raise Exception("unresolved name: %s" % name)

def resolve_type(nd):
  if hasattr(nd, "_resolved_type"):
    return nd._resolved_type
  else:
    name = nd["@type"]
    type = resolve(nd, name)
    if type.name == "domain" and not type["enum"]:
      type = resolve_type(type)
    nd._resolved_type = type
    return type

TYPES = {
  "bit": "boolean",
  "uint8": "short",
  "uint16": "int",
  "uint32": "long",
  "uint64": "long",
  "datetime": "long",
  "uuid": "UUID",
  "sequence-no": "int",
  "sequence-set": "RangeSet", # XXX
  "byte-ranges": "RangeSet", # XXX
  "str8": "String",
  "str16": "String",
  "vbin8": "byte[]",
  "vbin16": "byte[]",
  "vbin32": "byte[]",
  "struct32": "Struct",
  "map": "Map<String,Object>",
  "array": "List<Object>"
  }

def cname(nd, field="@name"):
  cls = klass(nd)
  if cls:
    if (nd.name in ("struct", "result") and
        cls["@name"] != "session" and
        nd[field] != "header"):
      return camel(0, nd[field])
    else:
      return camel(0, cls["@name"], nd[field])
  else:
    return camel(0, nd[field])

def jtype(nd):
  if nd.name == "struct" or nd["enum"]:
    return cname(nd)
  else:
    return TYPES[nd["@name"]]

REFS = {
  "boolean": "Boolean",
  "byte": "Byte",
  "short": "Short",
  "int": "Integer",
  "long": "Long",
  "float": "Float",
  "double": "Double",
  "char": "Character"
}

def jref(jt):
  return REFS.get(jt, jt)

def jclass(jt):
  idx = jt.find('<')
  if idx > 0:
    return jt[:idx]
  else:
    return jt

DEFAULTS = {
  "long": 0,
  "int": 0,
  "short": 0,
  "byte": 0,
  "char": 0,
  "boolean": "false"
  }

class Field:

  def __init__(self, index, nd):
    self.index = index
    self.name = camel(1, nd["@name"])
    self.type_node = resolve_type(nd)
    if self.type_node.name == "domain":
      self.prim_type = resolve_type(self.type_node)
    else:
      self.prim_type = self.type_node
    self.variable_width = num(self.prim_type["@variable-width"], 0)
    self.fixed_width = num(self.prim_type["@fixed-width"], 0)
    self.empty = self.variable_width == 0 and self.fixed_width == 0 and self.prim_type.name != "struct"
    tname = cname(self.type_node)
    if self.type_node.name == "struct":
      self.read = "(%s) dec.readStruct(%s.TYPE)" % (tname, tname)
      self.write = "enc.writeStruct(%s.TYPE, check(struct).%s)" % (tname, self.name)
      self.coder = "Struct"
    elif self.type_node.name == "domain":
      self.coder = camel(0, self.prim_type["@name"])
      self.read = "%s.get(dec.read%s())" % (tname, self.coder)
      self.write = "enc.write%s(check(struct).%s.getValue())" % (self.coder, self.name)
    else:
      self.coder = camel(0, self.type_node["@name"])
      self.read = "dec.read%s()" % self.coder
      self.write = "enc.write%s(check(struct).%s)" % (self.coder, self.name)
    self.type = jtype(self.type_node)
    self.ref_type = jref(self.type)
    self.default = DEFAULTS.get(self.type, "null")
    self.has = camel(1, "has", self.name)
    self.get = camel(1, "get", self.name)
    self.set = camel(1, "set", self.name)
    self.clear = camel(1, "clear", self.name)
    if self.type == "boolean":
      self.option = scream(nd["@name"])
    else:
      self.option = None

  def flag_mask(self, pack):
    flag = pack * 8 - 8 - (self.index/8)*8 + (self.index % 8)
    return 1 << flag


def get_fields(nd):
  fields = []
  index = 0
  for f in nd.query["field"]:
    fields.append(Field(index, f))
    index += 1
  return fields

def get_parameters(type, fields):
  params = []
  options = False
  for f in fields:
    if f.option:
      options = True
    else:
      params.append("%s %s" % (f.type, f.name))
  if type["segments"]:
    params.append("Header header")
    params.append("ByteBuffer body")
  if options or type.name in ("control", "command"):
    params.append("Option ... _options")
  return params

def get_arguments(type, fields):
  args = []
  options = False
  for f in fields:
    if f.option:
      options = True
    else:
      args.append(f.name)
  if type["segments"]:
    args.append("header")
    args.append("body")
  if options or type.name in ("control", "command"):
    args.append("_options")
  return args

def get_options(fields):
  return [f for f in fields if f.option]
