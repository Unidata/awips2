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

"""
This module loads protocol metadata into python objects. It provides
access to spec metadata via a python object model, and can also
dynamically creating python methods, classes, and modules based on the
spec metadata. All the generated methods have proper signatures and
doc strings based on the spec metadata so the python help system can
be used to browse the spec documentation. The generated methods all
dispatch to the self.invoke(meth, args) callback of the containing
class so that the generated code can be reused in a variety of
situations.
"""

import re, new, mllib, qpid
from util import fill

class SpecContainer:

  def __init__(self):
    self.items = []
    self.byname = {}
    self.byid = {}
    self.indexes = {}

  def add(self, item):
    if self.byname.has_key(item.name):
      raise ValueError("duplicate name: %s" % item)
    if item.id == None:
      item.id = len(self)
    elif self.byid.has_key(item.id):
      raise ValueError("duplicate id: %s" % item)
    self.indexes[item] = len(self.items)
    self.items.append(item)
    self.byname[item.name] = item
    self.byid[item.id] = item

  def index(self, item):
    try:
      return self.indexes[item]
    except KeyError:
      raise ValueError(item)

  def __iter__(self):
    return iter(self.items)

  def __len__(self):
    return len(self.items)

class Metadata:

  PRINT = []

  def __init__(self):
    pass

  def __str__(self):
    args = map(lambda f: "%s=%s" % (f, getattr(self, f)), self.PRINT)
    return "%s(%s)" % (self.__class__.__name__, ", ".join(args))

  def __repr__(self):
    return str(self)

class Spec(Metadata):

  PRINT=["major", "minor", "file"]

  def __init__(self, major, minor, file):
    Metadata.__init__(self)
    self.major = major
    self.minor = minor
    self.file = file
    self.constants = SpecContainer()
    self.domains = SpecContainer()
    self.classes = SpecContainer()
    # methods indexed by classname_methname
    self.methods = {}
    # structs by type code
    self.structs = {}

  def post_load(self):
    self.module = self.define_module("amqp%s%s" % (self.major, self.minor))
    self.klass = self.define_class("Amqp%s%s" % (self.major, self.minor))

  def method(self, name):
    if not self.methods.has_key(name):
      for cls in self.classes:
        clen = len(cls.name)
        if name.startswith(cls.name) and name[clen] == "_":
          end = name[clen + 1:]
          if cls.methods.byname.has_key(end):
            self.methods[name] = cls.methods.byname[end]
    return self.methods.get(name)

  def parse_method(self, name):
    parts = re.split(r"\s*\.\s*", name)
    if len(parts) != 2:
      raise ValueError(name)
    klass, meth = parts
    return self.classes.byname[klass].methods.byname[meth]

  def struct(self, name, *args, **kwargs):
    type = self.domains.byname[name].type
    return qpid.Struct(type, *args, **kwargs)

  def define_module(self, name, doc = None):
    module = new.module(name, doc)
    module.__file__ = self.file
    for c in self.classes:
      cls = c.define_class(c.name)
      cls.__module__ = module.__name__
      setattr(module, c.name, cls)
    return module

  def define_class(self, name):
    methods = {}
    for c in self.classes:
      for m in c.methods:
        meth = m.klass.name + "_" + m.name
        methods[meth] = m.define_method(meth)
    return type(name, (), methods)

class Constant(Metadata):

  PRINT=["name", "id"]

  def __init__(self, spec, name, id, klass, docs):
    Metadata.__init__(self)
    self.spec = spec
    self.name = name
    self.id = id
    self.klass = klass
    self.docs = docs

class Domain(Metadata):

  PRINT=["name", "type"]

  def __init__(self, spec, name, type, description, docs):
    Metadata.__init__(self)
    self.spec = spec
    self.id = None
    self.name = name
    self.type = type
    self.description = description
    self.docs = docs

class Struct(Metadata):

  PRINT=["size", "type", "pack"]

  def __init__(self, size, type, pack):
    Metadata.__init__(self)
    self.size = size
    self.type = type
    self.pack = pack
    self.fields = SpecContainer()

class Class(Metadata):

  PRINT=["name", "id"]

  def __init__(self, spec, name, id, handler, docs):
    Metadata.__init__(self)
    self.spec = spec
    self.name = name
    self.id = id
    self.handler = handler
    self.fields = SpecContainer()
    self.methods = SpecContainer()
    self.docs = docs

  def define_class(self, name):
    methods = {}
    for m in self.methods:
      methods[m.name] = m.define_method(m.name)
    return type(name, (), methods)

class Method(Metadata):

  PRINT=["name", "id"]

  def __init__(self, klass, name, id, content, responses, result, synchronous,
               description, docs):
    Metadata.__init__(self)
    self.klass = klass
    self.name = name
    self.id = id
    self.content = content
    self.responses = responses
    self.result = result
    self.synchronous = synchronous
    self.fields = SpecContainer()
    self.description = description
    self.docs = docs
    self.response = False

  def is_l4_command(self):
    return self.klass.name not in ["execution", "channel", "connection", "session"]

  def arguments(self, *args, **kwargs):
    nargs = len(args) + len(kwargs)
    maxargs = len(self.fields)
    if nargs > maxargs:
      self._type_error("takes at most %s arguments (%s) given", maxargs, nargs)
    result = []
    for f in self.fields:
      idx = self.fields.index(f)
      if idx < len(args):
        result.append(args[idx])
      elif kwargs.has_key(f.name):
        result.append(kwargs.pop(f.name))
      else:
        result.append(Method.DEFAULTS[f.type])
    for key, value in kwargs.items():
      if self.fields.byname.has_key(key):
        self._type_error("got multiple values for keyword argument '%s'", key)
      else:
        self._type_error("got an unexpected keyword argument '%s'", key)
    return tuple(result)

  def _type_error(self, msg, *args):
    raise TypeError("%s %s" % (self.name, msg % args))

  def docstring(self):
    s = "\n\n".join([fill(d, 2) for d in [self.description] + self.docs])
    for f in self.fields:
      if f.docs:
        s += "\n\n" + "\n\n".join([fill(f.docs[0], 4, f.name)] +
                                  [fill(d, 4) for d in f.docs[1:]])
    if self.responses:
      s += "\n\nValid responses: "
      for r in self.responses:
        s += r.name + " "
    return s

  METHOD = "__method__"
  DEFAULTS = {"bit": False,
              "shortstr": "",
              "longstr": "",
              "table": {},
              "array": [],
              "octet": 0,
              "short": 0,
              "long": 0,
              "longlong": 0,
              "timestamp": 0,
              "content": None,
              "uuid": "",
              "rfc1982_long": 0,
              "rfc1982_long_set": [],
              "long_struct": None}

  def define_method(self, name):
    g = {Method.METHOD: self}
    l = {}
    args = [(f.name, Method.DEFAULTS[f.type]) for f in self.fields]
    methargs = args[:]
    if self.content:
      args += [("content", None)]
    code = "def %s(self, %s):\n" % \
           (name, ", ".join(["%s = %r" % a for a in args]))
    code += "  %r\n" % self.docstring()
    argnames = ", ".join([a[0] for a in methargs])
    code += "  return self.invoke(%s" % Method.METHOD
    if argnames:
      code += ", (%s,)" % argnames
    else:
      code += ", ()" 
    if self.content:
      code += ", content"
    code += ")"
    exec code in g, l
    return l[name]

class Field(Metadata):

  PRINT=["name", "id", "type"]

  def __init__(self, name, id, type, domain, description, docs):
    Metadata.__init__(self)
    self.name = name
    self.id = id
    self.type = type
    self.domain = domain
    self.description = description
    self.docs = docs

  def default(self):
    if isinstance(self.type, Struct):
      return None
    else:
      return Method.DEFAULTS[self.type]

WIDTHS = {
  "octet": 1,
  "short": 2,
  "long": 4
  }

def width(st, default=None):
  if st in (None, "none", ""):
    return default
  else:
    return WIDTHS[st]

def get_result(nd, spec):
  result = nd["result"]
  if not result: return None
  name = result["@domain"]
  if name != None: return spec.domains.byname[name]
  st_nd = result["struct"]
  st = Struct(width(st_nd["@size"]), int(result.parent.parent["@index"])*256 +
              int(st_nd["@type"]), width(st_nd["@pack"], 2))
  spec.structs[st.type] = st
  load_fields(st_nd, st.fields, spec.domains.byname)
  return st

def get_desc(nd):
  label = nd["@label"]
  if not label:
    label = nd.text()
  if label:
    label = label.strip()
  return label

def get_docs(nd):
  return [n.text() for n in nd.query["doc"]]

def load_fields(nd, l, domains):
  for f_nd in nd.query["field"]:
    type = f_nd["@domain"]
    if type == None:
      type = f_nd["@type"]
    type = pythonize(type)
    domain = None
    while domains.has_key(type) and domains[type].type != type:
      domain = domains[type]
      type = domain.type
    l.add(Field(pythonize(f_nd["@name"]), f_nd.index(), type, domain,
                get_desc(f_nd), get_docs(f_nd)))

def load(specfile, *errata):
  doc = mllib.xml_parse(specfile)
  spec_root = doc["amqp"]
  spec = Spec(int(spec_root["@major"]), int(spec_root["@minor"]), specfile)

  for root in [spec_root] + map(lambda x: mllib.xml_parse(x)["amqp"], errata):
    # constants
    for nd in root.query["constant"]:
      val = nd["@value"]
      if val.startswith("0x"): val = int(val, 16)
      else: val = int(val)
      const = Constant(spec, pythonize(nd["@name"]), val, nd["@class"],
                       get_docs(nd))
      try:
        spec.constants.add(const)
      except ValueError, e:
        pass
        #print "Warning:", e

    # domains are typedefs
    structs = []
    for nd in root.query["domain"]:
      type = nd["@type"]
      if type == None:
        st_nd = nd["struct"]
        code = st_nd["@type"]
        if code not in (None, "", "none"):
          code = int(code)
        type = Struct(width(st_nd["@size"]), code, width(st_nd["@pack"], 2))
        if type.type != None:
          spec.structs[type.type] = type
        structs.append((type, st_nd))
      else:
        type = pythonize(type)
      domain = Domain(spec, pythonize(nd["@name"]), type, get_desc(nd),
                      get_docs(nd))
      spec.domains.add(domain)

    # structs
    for st, st_nd in structs:
      load_fields(st_nd, st.fields, spec.domains.byname)

    # classes
    for c_nd in root.query["class"]:
      cname = pythonize(c_nd["@name"])
      if spec.classes.byname.has_key(cname):
        klass = spec.classes.byname[cname]
      else:
        klass = Class(spec, cname, int(c_nd["@index"]), c_nd["@handler"],
                      get_docs(c_nd))
        spec.classes.add(klass)

      added_methods = []
      load_fields(c_nd, klass.fields, spec.domains.byname)
      for m_nd in c_nd.query["method"]:
        mname = pythonize(m_nd["@name"])
        if klass.methods.byname.has_key(mname):
          meth = klass.methods.byname[mname]
        else:
          meth = Method(klass, mname,
                        int(m_nd["@index"]),
                        m_nd["@content"] == "1",
                        [pythonize(nd["@name"]) for nd in m_nd.query["response"]],
                        get_result(m_nd, spec),
                        m_nd["@synchronous"] == "1",
                        get_desc(m_nd),
                        get_docs(m_nd))
          klass.methods.add(meth)
          added_methods.append(meth)
        load_fields(m_nd, meth.fields, spec.domains.byname)
      # resolve the responses
      for m in added_methods:
        m.responses = [klass.methods.byname[r] for r in m.responses]
        for resp in m.responses:
          resp.response = True

  spec.post_load()
  return spec

REPLACE = {" ": "_", "-": "_"}
KEYWORDS = {"global": "global_",
            "return": "return_"}

def pythonize(name):
  name = str(name)
  for key, val in REPLACE.items():
    name = name.replace(key, val)
  try:
    name = KEYWORDS[name]
  except KeyError:
    pass
  return name

class Rule(Metadata):

  PRINT = ["text", "implement", "tests"]

  def __init__(self, text, implement, tests, path):
    self.text = text
    self.implement = implement
    self.tests = tests
    self.path = path

def find_rules(node, rules):
  if node.name == "rule":
    rules.append(Rule(node.text, node.get("@implement"),
                      [ch.text for ch in node if ch.name == "test"],
                      node.path()))
  if node.name == "doc" and node.get("@name") == "rule":
    tests = []
    if node.has("@test"):
      tests.append(node["@test"])
    rules.append(Rule(node.text, None, tests, node.path()))
  for child in node:
    find_rules(child, rules)

def load_rules(specfile):
  rules = []
  find_rules(xmlutil.parse(specfile), rules)
  return rules

def test_summary():
  template = """
  <html><head><title>AMQP Tests</title></head>
  <body>
  <table width="80%%" align="center">
  %s
  </table>
  </body>
  </html>
  """
  rows = []
  for rule in load_rules("amqp.org/specs/amqp7.xml"):
    if rule.tests:
      tests = ", ".join(rule.tests)
    else:
      tests = "&nbsp;"
    rows.append('<tr bgcolor="#EEEEEE"><td><b>Path:</b> %s</td>'
                '<td><b>Implement:</b> %s</td>'
                '<td><b>Tests:</b> %s</td></tr>' %
                (rule.path[len("/root/amqp"):], rule.implement, tests))
    rows.append('<tr><td colspan="3">%s</td></tr>' % rule.text)
    rows.append('<tr><td colspan="3">&nbsp;</td></tr>')

  print template % "\n".join(rows)
