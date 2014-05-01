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

from xml.dom.minidom import parse, parseString, Node
from cStringIO       import StringIO
import md5

class Hash:
  """ Manage the hash of an XML sub-tree """
  def __init__(self, node):
    self.md5Sum = md5.new()
    self._compute(node)

  def addSubHash(self, hash):
    """ Use this method to add the hash of a dependend-on XML fragment that is not in the sub-tree """
    self.md5Sum.update(hash.getDigest())

  def getDigest(self):
    return self.md5Sum.digest()

  def _compute(self, node):
    attrs = node.attributes
    self.md5Sum.update(node.nodeName)

    for idx in range(attrs.length):
      self.md5Sum.update(attrs.item(idx).nodeName)
      self.md5Sum.update(attrs.item(idx).nodeValue)

    for child in node.childNodes:
      if child.nodeType == Node.ELEMENT_NODE:
        self._compute(child)


#=====================================================================================
#
#=====================================================================================
class SchemaType:
  def __init__ (self, node):
    self.name      = None
    self.base      = None
    self.cpp       = None
    self.encode    = None
    self.decode    = None
    self.style     = "normal"
    self.accessor  = None
    self.init      = "0"
    self.perThread = False
    self.byRef     = False

    attrs = node.attributes
    for idx in range (attrs.length):
      key = attrs.item(idx).nodeName
      val = attrs.item(idx).nodeValue
      if   key == 'name':
        self.name = val

      elif key == 'base':
        self.base = val

      elif key == 'cpp':
        self.cpp = val

      elif key == 'encode':
        self.encode = val

      elif key == 'decode':
        self.decode = val

      elif key == 'style':
        self.style = val

      elif key == 'accessor':
        self.accessor = val

      elif key == 'init':
        self.init = val

      elif key == 'perThread':
        if val != 'y':
          raise ValueError ("Expected 'y' in perThread attribute")
        self.perThread = True

      elif key == 'byRef':
        if val != 'y':
          raise ValueError ("Expected 'y' in byRef attribute")
        self.byRef = True

      else:
        raise ValueError ("Unknown attribute in type '%s'" % key)

    if self.name == None or self.base == None or self.cpp == None or \
       self.encode == None or self.decode == None:
      raise ValueError ("Missing required attribute(s) in type")

    if self.byRef:
      self.asArg = "const " + self.cpp + "&"
    else:
      self.asArg = self.cpp

  def getName (self):
    return self.name

  def genAccessor (self, stream, varName, changeFlag = None, optional = False):
    if self.perThread:
      prefix = "getThreadStats()->"
      if self.style == "wm":
        raise ValueError ("'wm' style types can't be per-thread")
    else:
      prefix = ""
    if self.accessor == "direct":
      stream.write ("    inline void set_" + varName + " (" + self.asArg + " val) {\n");
      if not self.perThread:
        stream.write ("        ::qpid::sys::Mutex::ScopedLock mutex(accessLock);\n")
      if self.style != "mma":
        stream.write ("        " + prefix + varName + " = val;\n")
        if optional:
          stream.write ("        presenceMask[presenceByte_%s] |= presenceMask_%s;\n" % (varName, varName))
      if self.style == "wm":
        stream.write ("        if (" + varName + "Low  > val)\n")
        stream.write ("            " + varName + "Low  = val;\n")
        stream.write ("        if (" + varName + "High < val)\n")
        stream.write ("            " + varName + "High = val;\n")
      if self.style == "mma":
        stream.write ("        " + prefix + varName + "Count++;\n")
        stream.write ("        " + prefix + varName + "Total += val;\n")
        stream.write ("        if (" + prefix + varName + "Min > val)\n")
        stream.write ("            " + prefix + varName + "Min = val;\n")
        stream.write ("        if (" + prefix + varName + "Max < val)\n")
        stream.write ("            " + prefix + varName + "Max = val;\n")
      if changeFlag != None:
        stream.write ("        " + changeFlag + " = true;\n")
      stream.write ("    }\n")
      if self.style != "mma":
        stream.write ("    inline " + self.asArg + " get_" + varName + "() {\n");
        if not self.perThread:
          stream.write ("        ::qpid::sys::Mutex::ScopedLock mutex(accessLock);\n")
        stream.write ("        return " + prefix + varName + ";\n")
        stream.write ("    }\n")
      if optional:
        stream.write ("    inline void clr_" + varName + "() {\n")
        stream.write ("        presenceMask[presenceByte_%s] &= ~presenceMask_%s;\n" % (varName, varName))
        if changeFlag != None:
          stream.write ("        " + changeFlag + " = true;\n")
        stream.write ("    }\n")
        stream.write ("    inline bool isSet_" + varName + "() {\n")
        stream.write ("        return (presenceMask[presenceByte_%s] & presenceMask_%s) != 0;\n" % (varName, varName))
        stream.write ("    }\n")
    elif self.accessor == "counter":
      stream.write ("    inline void inc_" + varName + " (" + self.asArg + " by = 1) {\n");
      if not self.perThread:
        stream.write ("        ::qpid::sys::Mutex::ScopedLock mutex(accessLock);\n")
      stream.write ("        " + prefix + varName + " += by;\n")
      if self.style == "wm":
        stream.write ("        if (" + varName + "High < " + varName + ")\n")
        stream.write ("            " + varName + "High = " + varName + ";\n")
      if changeFlag != None:
        stream.write ("        " + changeFlag + " = true;\n")
      stream.write ("    }\n");
      stream.write ("    inline void dec_" + varName + " (" + self.asArg + " by = 1) {\n");
      if not self.perThread:
        stream.write ("        ::qpid::sys::Mutex::ScopedLock mutex(accessLock);\n")
      stream.write ("        " + prefix + varName + " -= by;\n")
      if self.style == "wm":
        stream.write ("        if (" + varName + "Low > " + varName + ")\n")
        stream.write ("            " + varName + "Low = " + varName + ";\n")
      if changeFlag != None:
        stream.write ("        " + changeFlag + " = true;\n")
      stream.write ("    }\n");

  def genHiLoStatResets (self, stream, varName):
    if self.style == "wm":
      stream.write ("    " + varName + "High = " + varName + ";\n")
      stream.write ("    " + varName + "Low  = " + varName + ";\n")
    if self.style == "mma":
      stream.write ("    " + varName + "Count = 0;\n")
      stream.write ("    " + varName + "Total = 0;\n")
      stream.write ("    " + varName + "Min   = std::numeric_limits<" + self.type.type.cpp + ">::max();\n")
      stream.write ("    " + varName + "Max   = std::numeric_limits<" + self.type.type.cpp + ">::min();\n")

  def genPerThreadHiLoStatResets (self, stream, varName, cpptype):
    if self.style == "mma":
      stream.write ("        threadStats->" + varName + "Count = 0;\n")
      stream.write ("        threadStats->" + varName + "Total = 0;\n")
      stream.write ("        threadStats->" + varName + "Min   = std::numeric_limits<" + cpptype + ">::max();\n")
      stream.write ("        threadStats->" + varName + "Max   = std::numeric_limits<" + cpptype + ">::min();\n")

  def genWrite (self, stream, varName, indent="    "):
    if self.style != "mma":
      stream.write (indent + self.encode.replace ("@", "buf").replace ("#", varName) + ";\n")
    if self.style == "wm":
      stream.write (indent + self.encode.replace ("@", "buf") \
                    .replace ("#", varName + "High") + ";\n")
      stream.write (indent + self.encode.replace ("@", "buf") \
                    .replace ("#", varName + "Low") + ";\n")
    if self.style == "mma":
      stream.write (indent + self.encode.replace ("@", "buf") \
                    .replace ("#", varName + "Count") + ";\n")
      stream.write (indent + self.encode.replace ("@", "buf") \
                    .replace ("#", varName + "Count ? " + varName + "Min : 0") + ";\n")
      stream.write (indent + self.encode.replace ("@", "buf") \
                    .replace ("#", varName + "Max") + ";\n")
      stream.write (indent + self.encode.replace ("@", "buf") \
                    .replace ("#", varName + "Count ? " + varName + "Total / " +
                              varName + "Count : 0") + ";\n")


  def getReadCode (self, varName, bufName):
    result = self.decode.replace ("@", bufName).replace ("#", varName)
    return result

  def getWriteCode (self, varName, bufName):
    result = self.encode.replace ("@", bufName).replace ("#", varName)
    return result

#=====================================================================================
#
#=====================================================================================
class TypeSpec:
  def __init__ (self, file):
    self.types = {}
    dom = parse (file)
    document = dom.documentElement
    if document.tagName != 'schema-types':
      raise ValueError ("Expected 'schema-types' in type file")

    for child in document.childNodes:
      if child.nodeType == Node.ELEMENT_NODE:
        if child.nodeName == 'type':
          stype = SchemaType (child)
          self.types[stype.getName ()] = stype
        else:
          raise ValueError ("Unknown type tag '%s'" % child.nodeName)

  def getType (self, name):
    return self.types[name]


#=====================================================================================
#
#=====================================================================================
class Type:
  def __init__ (self, name, typespec):
    self.type = typespec.getType (name)

#=====================================================================================
#
#=====================================================================================
class SchemaProperty:
  def __init__ (self, node, typespec):
    self.name         = None
    self.type         = None
    self.ref          = None
    self.access       = "RO"
    self.isIndex      = 0
    self.isParentRef  = 0
    self.isGeneralRef = 0
    self.isOptional   = 0
    self.unit         = None
    self.min          = None
    self.max          = None
    self.maxLen       = None
    self.desc         = None

    attrs = node.attributes
    for idx in range (attrs.length):
      key = attrs.item(idx).nodeName
      val = attrs.item(idx).nodeValue
      if   key == 'name':
        self.name = makeValidCppSymbol(val)

      elif key == 'type':
        self.type = Type (val, typespec)
        if self.type.type.accessor != 'direct':
          raise ValueError ("Class properties must have a type with a direct accessor")

      elif key == 'references':
        self.ref = val
        
      elif key == 'access':
        self.access = val
        
      elif key == 'index':
        if val != 'y':
          raise ValueError ("Expected 'y' in index attribute")
        self.isIndex = 1
        
      elif key == 'parentRef':
        if val != 'y':
          raise ValueError ("Expected 'y' in parentRef attribute")
        self.isParentRef = 1
        
      elif key == 'isGeneralReference':
        if val != 'y':
          raise ValueError ("Expected 'y' in isGeneralReference attribute")
        self.isGeneralRef = 1
        
      elif key == 'optional':
        if val != 'y':
          raise ValueError ("Expected 'y' in optional attribute")
        self.isOptional = 1
        
      elif key == 'unit':
        self.unit = val
        
      elif key == 'min':
        self.min = val
        
      elif key == 'max':
        self.max = val
        
      elif key == 'maxlen':
        self.maxLen = val
        
      elif key == 'desc':
        self.desc = val
        
      else:
        raise ValueError ("Unknown attribute in property '%s'" % key)

    if self.access == "RC" and self.isOptional == 1:
      raise ValueError ("Properties with ReadCreate access must not be optional (%s)" % self.name)

    if self.name == None:
      raise ValueError ("Missing 'name' attribute in property")
    if self.type == None:
      raise ValueError ("Missing 'type' attribute in property")

  def getName (self):
    return self.name

  def isConstructorArg (self):
    if self.access == "RC" and self.isParentRef == 0:
      return 1
    return 0

  def genDeclaration (self, stream, prefix="    "):
    stream.write (prefix + self.type.type.cpp + " " + self.name + ";\n")

  def genFormalParam (self, stream, variables):
    stream.write (self.type.type.asArg + " _" + self.name)

  def genAccessor (self, stream):
    self.type.type.genAccessor (stream, self.name, "configChanged", self.isOptional == 1)

  def genInitialize (self, stream, prefix="", indent="    "):
    val = self.type.type.init
    stream.write (indent + prefix + self.name + " = " + val + ";\n")

  def genSchema (self, stream):
    stream.write ("    ft.clear();\n")
    stream.write ("    ft.setString (NAME, \"" + self.name + "\");\n")
    stream.write ("    ft.setInt    (TYPE, TYPE_" + self.type.type.base +");\n")
    stream.write ("    ft.setInt    (ACCESS, ACCESS_" + self.access + ");\n")
    stream.write ("    ft.setInt    (IS_INDEX, " + str (self.isIndex) + ");\n")
    stream.write ("    ft.setInt    (IS_OPTIONAL, " + str (self.isOptional) + ");\n")
    if self.unit != None:
      stream.write ("    ft.setString (UNIT,   \"" + self.unit   + "\");\n")
    if self.min != None:
      stream.write ("    ft.setInt    (MIN,    " + self.min    + ");\n")
    if self.max != None:
      stream.write ("    ft.setInt    (MAX,    " + self.max    + ");\n")
    if self.maxLen != None:
      stream.write ("    ft.setInt    (MAXLEN, " + self.maxLen + ");\n")
    if self.desc != None:
      stream.write ("    ft.setString (DESC,   \"" + self.desc   + "\");\n")
    stream.write ("    buf.put (ft);\n\n")

  def genWrite (self, stream):
    indent = "    "
    if self.isOptional:
      stream.write("    if (presenceMask[presenceByte_%s] & presenceMask_%s) {\n" % (self.name, self.name))
      indent = "        "
    self.type.type.genWrite (stream, self.name, indent)
    if self.isOptional:
      stream.write("    }\n")


#=====================================================================================
#
#=====================================================================================
class SchemaStatistic:
  def __init__ (self, node, typespec):
    self.name   = None
    self.type   = None
    self.unit   = None
    self.desc   = None
    self.assign = None

    attrs = node.attributes
    for idx in range (attrs.length):
      key = attrs.item(idx).nodeName
      val = attrs.item(idx).nodeValue
      if   key == 'name':
        self.name = makeValidCppSymbol(val)

      elif key == 'type':
        self.type = Type (val, typespec)
        
      elif key == 'unit':
        self.unit = val
        
      elif key == 'desc':
        self.desc = val
        
      elif key == 'assign':
        self.assign = val
        
      else:
        raise ValueError ("Unknown attribute in statistic '%s'" % key)

    if self.name == None:
      raise ValueError ("Missing 'name' attribute in statistic")
    if self.type == None:
      raise ValueError ("Missing 'type' attribute in statistic")

  def getName (self):
    return self.name

  def genDeclaration (self, stream, prefix="    "):
    if self.type.type.style != "mma":
      stream.write (prefix + self.type.type.cpp + "  " + self.name + ";\n")
    if self.type.type.style == 'wm':
      stream.write (prefix + self.type.type.cpp + "  " + self.name + "High;\n")
      stream.write (prefix + self.type.type.cpp + "  " + self.name + "Low;\n")
    if self.type.type.style == "mma":
      stream.write (prefix + self.type.type.cpp + "  " + self.name + "Count;\n")
      stream.write (prefix + "uint64_t  " + self.name + "Total;\n")
      stream.write (prefix + self.type.type.cpp + "  " + self.name + "Min;\n")
      stream.write (prefix + self.type.type.cpp + "  " + self.name + "Max;\n")

  def genAccessor (self, stream):
    self.type.type.genAccessor (stream, self.name, "instChanged")

  def genHiLoStatResets (self, stream):
    self.type.type.genHiLoStatResets (stream, self.name)

  def genPerThreadHiLoStatResets (self, stream):
    self.type.type.genPerThreadHiLoStatResets (stream, self.name, self.type.type.cpp)

  def genSchemaText (self, stream, name, desc):
    stream.write ("    ft.clear();\n")
    stream.write ("    ft.setString (NAME,   \"" + name + "\");\n")
    stream.write ("    ft.setInt    (TYPE,   TYPE_" + self.type.type.base +");\n")
    if self.unit != None:
      stream.write ("    ft.setString (UNIT,   \"" + self.unit   + "\");\n")
    if desc != None:
      stream.write ("    ft.setString (DESC,   \"" + desc   + "\");\n")
    stream.write ("    buf.put (ft);\n\n")

  def genSchema (self, stream):
    if self.type.type.style != "mma":
      self.genSchemaText (stream, self.name, self.desc)
    if self.type.type.style == "wm":
      descHigh = self.desc
      descLow  = self.desc
      if self.desc != None:
        descHigh = descHigh + " (High)"
        descLow  = descLow  + " (Low)"
      self.genSchemaText (stream, self.name + "High", descHigh)
      self.genSchemaText (stream, self.name + "Low",  descLow)
    if self.type.type.style == "mma":
      descCount   = self.desc
      descMin     = self.desc
      descMax     = self.desc
      descAverage = self.desc
      if self.desc != None:
        descCount   = descCount   + " (Samples)"
        descMin     = descMin     + " (Min)"
        descMax     = descMax     + " (Max)"
        descAverage = descAverage + " (Average)"
      self.genSchemaText (stream, self.name + "Samples", descCount)
      self.genSchemaText (stream, self.name + "Min",     descMin)
      self.genSchemaText (stream, self.name + "Max",     descMax)
      self.genSchemaText (stream, self.name + "Average", descAverage)

  def genAssign (self, stream):
    if self.assign != None:
      if self.type.type.perThread:
        prefix = "    threadStats->"
      else:
        prefix = ""
      stream.write ("    " + prefix + self.name + " = (" + self.type.type.cpp +
                    ") (" + self.assign + ");\n")

  def genWrite (self, stream):
    if self.type.type.perThread:
      self.type.type.genWrite (stream, "totals." + self.name)
    else:
      self.type.type.genWrite (stream, self.name)

  def genInitialize (self, stream, prefix="", indent="    "):
    val = self.type.type.init
    if self.type.type.style != "mma":
      stream.write (indent + prefix + self.name + " = " + val + ";\n")
    if self.type.type.style == "wm":
      stream.write (indent + prefix + self.name + "High = " + val + ";\n")
      stream.write (indent + prefix + self.name + "Low  = " + val + ";\n")
    if self.type.type.style == "mma":
      stream.write (indent + prefix + self.name + "Count = 0;\n")
      stream.write (indent + prefix + self.name + "Min   = std::numeric_limits<" + self.type.type.cpp + ">::max();\n")
      stream.write (indent + prefix + self.name + "Max   = std::numeric_limits<" + self.type.type.cpp + ">::min();\n")
      stream.write (indent + prefix + self.name + "Total = 0;\n")

  def genInitializeTotalPerThreadStats (self, stream):
    if self.type.type.style == "mma":
      stream.write ("    totals->" + self.name + "Count = 0;\n")
      stream.write ("    totals->" + self.name + "Min   = std::numeric_limits<" + self.type.type.cpp + ">::max();\n")
      stream.write ("    totals->" + self.name + "Max   = std::numeric_limits<" + self.type.type.cpp + ">::min();\n")
      stream.write ("    totals->" + self.name + "Total = 0;\n")
    else:
      stream.write ("    totals->" + self.name + " = 0;\n")

  def genAggregatePerThreadStats (self, stream):
    if self.type.type.style == "mma":
      stream.write ("            totals->%sCount += threadStats->%sCount;\n" % (self.name, self.name))
      stream.write ("            if (totals->%sMin > threadStats->%sMin)\n" % (self.name, self.name))
      stream.write ("                totals->%sMin = threadStats->%sMin;\n" % (self.name, self.name))
      stream.write ("            if (totals->%sMax < threadStats->%sMax)\n" % (self.name, self.name))
      stream.write ("                totals->%sMax = threadStats->%sMax;\n" % (self.name, self.name))
      stream.write ("            totals->%sTotal += threadStats->%sTotal;\n" % (self.name, self.name))
    else:
      stream.write ("            totals->%s += threadStats->%s;\n" % (self.name, self.name))

#=====================================================================================
#
#=====================================================================================
class SchemaArg:
  def __init__ (self, node, typespec):
    self.name    = None
    self.type    = None
    self.unit    = None
    self.dir     = "I"
    self.min     = None
    self.max     = None
    self.maxLen  = None
    self.desc    = None
    self.default = None
    self.hash    = Hash(node)

    attrs = node.attributes
    for idx in range (attrs.length):
      key = attrs.item(idx).nodeName
      val = attrs.item(idx).nodeValue
      if   key == 'name':
        self.name = makeValidCppSymbol(val)

      elif key == 'type':
        self.type = Type (val, typespec)
        
      elif key == 'unit':
        self.unit = val

      elif key == 'dir':
        self.dir = val.upper ()
        
      elif key == 'min':
        self.min = val
        
      elif key == 'max':
        self.max = val
        
      elif key == 'maxlen':
        self.maxLen = val
        
      elif key == 'desc':
        self.desc = val

      elif key == 'default':
        self.default = val
        
      else:
        raise ValueError ("Unknown attribute in arg '%s'" % key)

    if self.name == None:
      raise ValueError ("Missing 'name' attribute in arg")
    if self.type == None:
      raise ValueError ("Missing 'type' attribute in arg")

  def getName (self):
    return self.name

  def getDir (self):
    return self.dir

  def genSchema (self, stream, event=False):
    stream.write ("    ft.clear();\n")
    stream.write ("    ft.setString (NAME,    \"" + self.name + "\");\n")
    stream.write ("    ft.setInt    (TYPE,    TYPE_" + self.type.type.base +");\n")
    if (not event):
      stream.write ("    ft.setString (DIR,     \"" + self.dir + "\");\n")
    if self.unit != None:
      stream.write ("    ft.setString (UNIT,    \"" + self.unit   + "\");\n")
    if not event:
      if self.min != None:
        stream.write ("    ft.setInt    (MIN,     " + self.min    + ");\n")
      if self.max != None:
        stream.write ("    ft.setInt    (MAX,     " + self.max    + ");\n")
      if self.maxLen != None:
        stream.write ("    ft.setInt    (MAXLEN,  " + self.maxLen + ");\n")
      if self.default != None:
        stream.write ("    ft.setString (DEFAULT, \"" + self.default + "\");\n")
    if self.desc != None:
      stream.write ("    ft.setString (DESC,    \"" + self.desc + "\");\n")
    stream.write ("    buf.put (ft);\n\n")

  def genFormalParam (self, stream, variables):
    stream.write ("%s _%s" % (self.type.type.asArg, self.name))

#=====================================================================================
#
#=====================================================================================
class SchemaMethod:
  def __init__ (self, parent, node, typespec):
    self.parent = parent
    self.name   = None
    self.desc   = None
    self.args   = []

    attrs = node.attributes
    for idx in range (attrs.length):
      key = attrs.item(idx).nodeName
      val = attrs.item(idx).nodeValue
      if   key == 'name':
        self.name = makeValidCppSymbol(val)

      elif key == 'desc':
        self.desc = val

      else:
        raise ValueError ("Unknown attribute in method '%s'" % key)

    for child in node.childNodes:
      if child.nodeType == Node.ELEMENT_NODE:
        if child.nodeName == 'arg':
          arg = SchemaArg (child, typespec)
          self.args.append (arg)
        else:
          raise ValueError ("Unknown method tag '%s'" % child.nodeName)

  def getName (self):
    return self.name

  def getFullName (self):
    return capitalize(self.parent.getName()) + self.name[0:1].upper() +\
           self.name[1:]

  def getArgCount (self):
    return len (self.args)

  #===================================================================================
  # Code Generation Functions.  The names of these functions (minus the leading "gen")
  # match the substitution keywords in the template files.
  #===================================================================================
  def genNameUpper (self, stream, variables):
    stream.write (self.getFullName ().upper ())

  def genNameCamel (self, stream, variables):
    stream.write (self.getFullName ())

  def genOpenNamespaces (self, stream, variables):
    self.parent.genOpenNamespaces(stream, variables)

  def genCloseNamespaces (self, stream, variables):
    self.parent.genCloseNamespaces(stream, variables)

  def genArguments (self, stream, variables):
    for arg in self.args:
      ctype  = arg.type.type.cpp
      dirTag = arg.dir.lower() + "_"
      stream.write ("    " + ctype + " " + dirTag + arg.getName () + ";\n")

  def genNamePackageLower (self, stream, variables):
    self.parent.genNamePackageLower(stream, variables)

  def genSchema (self, stream, variables):
    stream.write ("    ft.clear();\n")
    stream.write ("    ft.setString (NAME,     \"" + self.name + "\");\n")
    stream.write ("    ft.setInt    (ARGCOUNT, " + str (len (self.args)) + ");\n")
    if self.desc != None:
      stream.write ("    ft.setString (DESC,     \"" + self.desc + "\");\n")
    stream.write ("    buf.put (ft);\n\n")
    for arg in self.args:
      arg.genSchema (stream)

#=====================================================================================
#
#=====================================================================================
class SchemaEvent:
  def __init__ (self, package, node, typespec, argset):
    self.packageName = package
    self.name = None
    self.desc = None
    self.sevText = "inform"
    self.args = []
    self.hash = Hash(node)

    attrs = node.attributes
    for idx in range (attrs.length):
      key = attrs.item(idx).nodeName
      val = attrs.item(idx).nodeValue
      if   key == 'name':
        self.name = val

      elif key == 'desc':
        self.desc = val

      elif key == 'sev':
        self.sevText = val

      elif key == 'args':
        list = val.replace(" ", "").split(",")
        for item in list:
          if item not in argset.args:
            raise Exception("undefined argument '%s' in event" % item)
          self.args.append(argset.args[item])
          self.hash.addSubHash(argset.args[item].hash)

      else:
        raise ValueError ("Unknown attribute in event '%s'" % key)

    if   self.sevText == "emerg"  : self.sev = 0
    elif self.sevText == "alert"  : self.sev = 1
    elif self.sevText == "crit"   : self.sev = 2
    elif self.sevText == "error"  : self.sev = 3
    elif self.sevText == "warn"   : self.sev = 4
    elif self.sevText == "notice" : self.sev = 5
    elif self.sevText == "inform" : self.sev = 6
    elif self.sevText == "debug"  : self.sev = 7
    else:
      raise ValueError("Unknown severity '%s' in event '%s'" % (self.sevText, self.name))

  def getName (self):
    return self.name

  def getNameCap(self):
    return capitalize(self.name)

  def getFullName (self):
    return capitalize(self.package + capitalize(self.name))

  def genAgentHeaderLocation (self, stream, variables):
    stream.write(variables["agentHeaderDir"])

  def getArgCount (self):
    return len (self.args)

  def genArgCount (self, stream, variables):
    stream.write("%d" % len(self.args))

  def genArgDeclarations(self, stream, variables):
    for arg in self.args:
      if arg.type.type.byRef:
        ref = "&"
      else:
        ref = ""
      stream.write("    const %s%s %s;\n" % (arg.type.type.cpp, ref, arg.name))

  def genCloseNamespaces (self, stream, variables):
    for item in self.packageName.split("."):
      stream.write ("}")

  def genConstructorArgs(self, stream, variables):
    pre = ""
    for arg in self.args:
      if arg.type.type.byRef:
        ref = "&"
      else:
        ref = ""
      stream.write("%sconst %s%s _%s" % (pre, arg.type.type.cpp, ref, arg.name))
      pre = ",\n        "

  def genConstructorInits(self, stream, variables):
    pre = ""
    for arg in self.args:
      stream.write("%s%s(_%s)" % (pre, arg.name, arg.name))
      pre = ",\n    "

  def genName(self, stream, variables):
    stream.write(self.name)

  def genNameCap(self, stream, variables):
    stream.write(capitalize(self.name))

  def genNamespace (self, stream, variables):
    stream.write("::".join(self.packageName.split(".")))

  def genNameLower(self, stream, variables):
    stream.write(self.name.lower())

  def genNameUpper(self, stream, variables):
    stream.write(self.name.upper())

  def genNamePackageLower(self, stream, variables):
    stream.write(self.packageName.lower())

  def genOpenNamespaces (self, stream, variables):
    for item in self.packageName.split("."):
      stream.write ("namespace %s {\n" % item)

  def genSeverity(self, stream, variables):
    stream.write("%d" % self.sev)

  def genArgEncodes(self, stream, variables):
    for arg in self.args:
      stream.write("    " + arg.type.type.encode.replace("@", "buf").replace("#", arg.name) + ";\n")

  def genArgSchema(self, stream, variables):
    for arg in self.args:
      arg.genSchema(stream, True)

  def genSchemaMD5(self, stream, variables):
    sum = self.hash.getDigest()
    for idx in range (len (sum)):
      if idx != 0:
        stream.write (",")
      stream.write (hex (ord (sum[idx])))


class SchemaClass:
  def __init__ (self, package, node, typespec, fragments, options):
    self.packageName = package
    self.properties  = []
    self.statistics  = []
    self.methods     = []
    self.events      = []
    self.options     = options
    self.hash        = Hash(node)

    attrs = node.attributes
    self.name = makeValidCppSymbol(attrs['name'].nodeValue)

    children = node.childNodes
    for child in children:
      if child.nodeType == Node.ELEMENT_NODE:
        if   child.nodeName == 'property':
          sub = SchemaProperty (child, typespec)
          self.properties.append (sub)

        elif child.nodeName == 'statistic':
          sub = SchemaStatistic (child, typespec)
          self.statistics.append (sub)

        elif child.nodeName == 'method':
          sub = SchemaMethod (self, child, typespec)
          self.methods.append (sub)

        elif child.nodeName == 'group':
          self.expandFragment (child, fragments)

        else:
          raise ValueError ("Unknown class tag '%s'" % child.nodeName)

    # Adjust the 'assign' attributes for each statistic
    for stat in self.statistics:
      if stat.assign != None and stat.type.type.perThread:
        stat.assign = self.adjust (stat.assign, self.statistics)

  def adjust (self, text, statistics):
    result = text
    start  = 0
    while True:
      next = None
      for stat in statistics:
        pos = result.find (stat.name, start)
        if pos != -1 and (next == None or pos < next[0]):
          next = (pos, stat.name)
      if next == None:
        return result
      pos = next[0]
      result = result[0:pos] + "threadStats->" + result[pos:]
      start = pos + 9 + len(next[1])

  def expandFragment (self, node, fragments):
    attrs = node.attributes
    name  = attrs['name'].nodeValue
    for fragment in fragments:
      if fragment.name == name:
        self.hash.addSubHash(fragment.hash)
        for config in fragment.properties:
          self.properties.append (config)
        for inst   in fragment.statistics:
          self.statistics.append (inst)
        for method in fragment.methods:
          self.methods.append (method)
        for event  in fragment.events:
          self.events.append (event)
        return
    raise ValueError ("Undefined group '%s'" % name)

  def getName (self):
    return self.name

  def getNameCap (self):
    return capitalize(self.name)

  def getMethods (self):
    return self.methods

  def getEvents (self):
    return self.events

  def getPackageNameCap (self):
    return capitalize(self.packageName)

  #===================================================================================
  # Code Generation Functions.  The names of these functions (minus the leading "gen")
  # match the substitution keywords in the template files.
  #===================================================================================
  def testExistOptionals (self, variables):
    for prop in self.properties:
      if prop.isOptional == 1:
        return True
    return False

  def testExistPerThreadStats (self, variables):
    for inst in self.statistics:
      if inst.type.type.perThread:
        return True
    return False

  def testExistPerThreadAssign (self, variables):
    for inst in self.statistics:
      if inst.type.type.perThread and inst.assign != None:
        return True
    return False

  def testExistPerThreadResets (self, variables):
    for inst in self.statistics:
      if inst.type.type.perThread and inst.type.type.style == "mma":
        return True
    return False

  def testNoStatistics (self, variables):
    return len (self.statistics) == 0

  def genAccessorMethods (self, stream, variables):
    for config in self.properties:
      if config.access != "RC":
        config.genAccessor (stream)
    for inst in self.statistics:
      if inst.assign == None:
        inst.genAccessor (stream)

  def genAgentHeaderLocation (self, stream, variables):
    stream.write(variables["agentHeaderDir"])

  def genCloseNamespaces (self, stream, variables):
    for item in self.packageName.split("."):
      stream.write ("}")

  def genConfigCount (self, stream, variables):
    stream.write ("%d" % len (self.properties))

  def genConfigDeclarations (self, stream, variables):
    for element in self.properties:
      element.genDeclaration (stream)

  def genConstructorArgs (self, stream, variables):
    # Constructor args are config elements with read-create access
    result = ""
    for element in self.properties:
      if element.isConstructorArg ():
        stream.write (", ")
        element.genFormalParam (stream, variables)

  def genConstructorInits (self, stream, variables):
    for element in self.properties:
      if element.isConstructorArg ():
        stream.write ("," + element.getName () + "(_" + element.getName () + ")")

  def genDoMethodArgs (self, stream, variables):
    methodCount = 0
    inArgCount  = 0
    for method in self.methods:
      methodCount = methodCount + 1
      for arg in method.args:
        if arg.getDir () == "I" or arg.getDir () == "IO":
          inArgCount = inArgCount + 1

    if methodCount == 0:
      stream.write ("string&, Buffer&, Buffer& outBuf")
    else:
      if inArgCount == 0:
        stream.write ("string& methodName, Buffer&, Buffer& outBuf")
      else:
        stream.write ("string& methodName, Buffer& inBuf, Buffer& outBuf")

  def genHiLoStatResets (self, stream, variables):
    for inst in self.statistics:
      if not inst.type.type.perThread:
        inst.genHiLoStatResets (stream)

  def genPerThreadHiLoStatResets (self, stream, variables):
    for inst in self.statistics:
      if inst.type.type.perThread:
        inst.genPerThreadHiLoStatResets (stream)

  def genInitializeElements (self, stream, variables):
    for prop in self.properties:
      if not prop.isConstructorArg() and not prop.isParentRef:
        prop.genInitialize(stream)
    for inst in self.statistics:
      if not inst.type.type.perThread:
        inst.genInitialize (stream)

  def genInitializePerThreadElements (self, stream, variables):
    for inst in self.statistics:
      if inst.type.type.perThread:
        inst.genInitialize (stream, "threadStats->", "            ")

  def genInitializeTotalPerThreadStats (self, stream, variables):
    for inst in self.statistics:
      if inst.type.type.perThread:
        inst.genInitializeTotalPerThreadStats (stream)

  def genAggregatePerThreadStats (self, stream, variables):
    for inst in self.statistics:
      if inst.type.type.perThread:
        inst.genAggregatePerThreadStats (stream)

  def genInstCount (self, stream, variables):
    count = 0
    for inst in self.statistics:
      count = count + 1
      if inst.type.type.style == "wm":
        count = count + 2
      if inst.type.type.style == "mma":
        count = count + 3
    stream.write ("%d" % count)

  def genInstDeclarations (self, stream, variables):
    for element in self.statistics:
      if not element.type.type.perThread:
        element.genDeclaration (stream)

  def genPerThreadDeclarations (self, stream, variables):
    for element in self.statistics:
      if element.type.type.perThread:
        element.genDeclaration (stream, "        ")

  def genNamespace (self, stream, variables):
    stream.write("::".join(self.packageName.split(".")))

  def genMethodArgIncludes (self, stream, variables):
    for method in self.methods:
      if method.getArgCount () > 0:
        stream.write ("#include \"Args" + method.getFullName () + ".h\"\n")

  def genMethodCount (self, stream, variables):
    stream.write ("%d" % len (self.methods))

  def genMethodHandlers (self, stream, variables):
    for method in self.methods:
      stream.write ("\n    if (methodName == \"" + method.getName () + "\") {\n")
      if method.getArgCount () == 0:
        stream.write ("        ::qpid::management::ArgsNone ioArgs;\n")
      else:
        stream.write ("        Args" + method.getFullName () + " ioArgs;\n")
      for arg in method.args:
        if arg.getDir () == "I" or arg.getDir () == "IO":
          stream.write ("        " +\
                        arg.type.type.getReadCode ("ioArgs." +\
                                                   arg.dir.lower () + "_" +\
                                                   arg.name, "inBuf") + ";\n")

      stream.write ("        status = coreObject->ManagementMethod (METHOD_" +\
                    method.getName().upper() + ", ioArgs, text);\n")
      stream.write ("        outBuf.putLong        (status);\n")
      stream.write ("        outBuf.putMediumString(::qpid::management::Manageable::StatusText (status, text));\n")
      for arg in method.args:
        if arg.getDir () == "O" or arg.getDir () == "IO":
          stream.write ("        " +\
                        arg.type.type.getWriteCode ("ioArgs." +\
                                                    arg.dir.lower () + "_" +\
                                                    arg.name, "outBuf") + ";\n")
      stream.write ("        return;\n    }\n")

  def genOpenNamespaces (self, stream, variables):
    for item in self.packageName.split("."):
      stream.write ("namespace %s {\n" % item)

  def genPresenceMaskBytes (self, stream, variables):
    count = 0
    for prop in self.properties:
      if prop.isOptional == 1:
        count += 1
    if count == 0:
      stream.write("0")
    else:
      stream.write (str(((count - 1) / 8) + 1))

  def genPresenceMaskConstants (self, stream, variables):
    count = 0
    for prop in self.properties:
      if prop.isOptional == 1:
        stream.write("    static const uint8_t presenceByte_%s = %d;\n" % (prop.name, count / 8))
        stream.write("    static const uint8_t presenceMask_%s = %d;\n" % (prop.name, 1 << (count % 8)))
        count += 1

  def genPropertySchema (self, stream, variables):
    for prop in self.properties:
      prop.genSchema (stream)

  def genSetGeneralReferenceDeclaration (self, stream, variables):
    for prop in self.properties:
      if prop.isGeneralRef:
        stream.write ("void setReference(::qpid::management::ObjectId objectId) { " + prop.name + " = objectId; }\n")

  def genStatisticSchema (self, stream, variables):
    for stat in self.statistics:
      stat.genSchema (stream)

  def genMethodIdDeclarations (self, stream, variables):
    number = 1
    for method in self.methods:
      stream.write ("    static const uint32_t METHOD_" + method.getName().upper() +\
                    " = %d;\n" % number)
      number = number + 1

  def genMethodSchema (self, stream, variables):
    for method in self.methods:
      method.genSchema (stream, variables)

  def genNameCap (self, stream, variables):
    stream.write (capitalize(self.name))

  def genNameLower (self, stream, variables):
    stream.write (self.name.lower ())

  def genNamePackageCap (self, stream, variables):
    stream.write (self.getPackageNameCap ())

  def genNamePackageLower (self, stream, variables):
    stream.write (self.packageName.lower ())

  def genNameUpper (self, stream, variables):
    stream.write (self.name.upper ())

  def genParentArg (self, stream, variables):
    for config in self.properties:
      if config.isParentRef == 1:
        stream.write (", ::qpid::management::Manageable* _parent")
        return

  def genParentRefAssignment (self, stream, variables):
    for config in self.properties:
      if config.isParentRef == 1:
        stream.write (config.getName () + \
                      " = _parent->GetManagementObject ()->getObjectId ();")
        return

  def genSchemaMD5 (self, stream, variables):
    sum = self.hash.getDigest()
    for idx in range (len (sum)):
      if idx != 0:
        stream.write (",")
      stream.write (hex (ord (sum[idx])))

  def genAssign (self, stream, variables):
    for inst in self.statistics:
      if not inst.type.type.perThread:
        inst.genAssign (stream)

  def genPerThreadAssign (self, stream, variables):
    for inst in self.statistics:
      if inst.type.type.perThread:
        inst.genAssign (stream)

  def genWriteProperties (self, stream, variables):
    for prop in self.properties:
      prop.genWrite (stream)

  def genWriteStatistics (self, stream, variables):
    for stat in self.statistics:
      stat.genWrite (stream)


class SchemaEventArgs:
  def __init__(self, package, node, typespec, fragments, options):
    self.packageName = package
    self.options     = options
    self.args        = {}

    children = node.childNodes
    for child in children:
      if child.nodeType == Node.ELEMENT_NODE:
        if child.nodeName == 'arg':
          arg = SchemaArg(child, typespec)
          self.args[arg.name] = arg
        else:
          raise Exception("Unknown tag '%s' in <eventArguments>" % child.nodeName)

class SchemaPackage:
  def __init__ (self, typefile, schemafile, options):

    self.classes   = []
    self.fragments = []
    self.typespec  = TypeSpec (typefile)
    self.eventArgSet = None
    self.events    = []

    dom = parse (schemafile)
    document = dom.documentElement
    if document.tagName != 'schema':
      raise ValueError ("Expected 'schema' node")
    attrs = document.attributes
    pname = attrs['package'].nodeValue
    namelist = pname.split('.')
    self.packageName = ".".join(namelist)

    children = document.childNodes
    for child in children:
      if child.nodeType == Node.ELEMENT_NODE:
        if child.nodeName == 'class':
          cls = SchemaClass (self.packageName, child, self.typespec,
                             self.fragments, options)
          self.classes.append (cls)

        elif child.nodeName == 'group':
          cls = SchemaClass (self.packageName, child, self.typespec,
                             self.fragments, options)
          self.fragments.append (cls)

        elif child.nodeName == 'eventArguments':
          if self.eventArgSet:
            raise Exception("Only one <eventArguments> may appear in a package")
          self.eventArgSet = SchemaEventArgs(self.packageName, child, self.typespec, self.fragments, options)

        elif child.nodeName == 'event':
          event = SchemaEvent(self.packageName, child, self.typespec, self.eventArgSet)
          self.events.append(event)

        else:
          raise ValueError ("Unknown schema tag '%s'" % child.nodeName)

  def getPackageName (self):
    return self.packageName

  def getPackageNameCap (self):
    return capitalize(self.packageName)

  def getPackageNameLower (self):
    return self.packageName.lower()

  def getClasses (self):
    return self.classes

  def getEvents(self):
    return self.events

  def genAgentHeaderLocation (self, stream, variables):
    stream.write(variables["agentHeaderDir"])

  def genCloseNamespaces (self, stream, variables):
    for item in self.packageName.split("."):
      stream.write ("}")

  def genNamespace (self, stream, variables):
    stream.write("::".join(self.packageName.split(".")))

  def genOpenNamespaces (self, stream, variables):
    for item in self.packageName.split("."):
      stream.write ("namespace %s {\n" % item)

  def genPackageNameUpper (self, stream, variables):
    up = "_".join(self.packageName.split("."))
    stream.write (up.upper())

  def genNamePackageLower (self, stream, variables):
    stream.write (self.packageName.lower ())

  def genClassIncludes (self, stream, variables):
    for _class in self.classes:
      stream.write ("#include \"")
      _class.genNameCap (stream, variables)
      stream.write (".h\"\n")
    for _event in self.events:
      stream.write ("#include \"Event")
      _event.genNameCap(stream, variables)
      stream.write (".h\"\n")

  def genClassRegisters(self, stream, variables):
    for _class in self.classes:
      stream.write("    ")
      _class.genNameCap(stream, variables)
      stream.write("::registerSelf(agent);\n")
    for _event in self.events:
      stream.write("    Event")
      _event.genNameCap(stream, variables)
      stream.write("::registerSelf(agent);\n")


#=====================================================================================
# Utility Functions
#=====================================================================================

# Create a valid C++ symbol from the input string so that it can be
# used in generated C++ source. For instance, change "qpid.mgmt" to
# "qpidMgmt".
#
# Input: Raw string (str) to process
# Output: String (str) suitable for use as a C++ symbol
#
# Limitations: Currently, only strips periods ('.') from strings,
#              eventually should strip :'s and ,'s and ''s, oh my!
def makeValidCppSymbol(input):
  output = str()
  capitalize = False

  for char in input:
    skip = False

    if char == ".":
      capitalize = True
      skip = True

    if not skip:
      output += capitalize and char.upper() or char

      capitalize = False

  return output

# Capitalize a string by /only/ forcing the first character to be
# uppercase. The rest of the string is left alone. This is different
# from str.capitalize(), which forces the first character to uppercase
# and the rest to lowercase.
#
# Input: A string (str) to capitalize
# Output: A string (str) with the first character as uppercase
def capitalize(input):
  return input[0].upper() + input[1:]
