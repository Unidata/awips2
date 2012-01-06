#!/usr/bin/ruby
#
# General purpose C++ code generation.
#
require 'amqpgen'
require 'set'

Copyright=<<EOS
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

///
/// This file was automatically generated from the AMQP specification.
/// Do not edit.
///


EOS

CppKeywords = Set.new(["and", "and_eq", "asm", "auto", "bitand",
                       "bitor", "bool", "break", "case", "catch", "char",
                       "class", "compl", "const", "const_cast", "continue",
                       "default", "delete", "do", "DomainInfo", "double",
                       "dynamic_cast", "else", "enum", "explicit", "extern",
                       "false", "float", "for", "friend", "goto", "if",
                       "inline", "int", "long", "mutable", "namespace", "new",
                       "not", "not_eq", "operator", "or", "or_eq", "private",
                       "protected", "public", "register", "reinterpret_cast",
                       "return", "short", "signed", "sizeof", "static",
                       "static_cast", "struct", "switch", "template", "this",
                       "throw", "true", "try", "typedef", "typeid",
                       "typename", "union", "unsigned", "using", "virtual",
                       "void", "volatile", "wchar_t", "while", "xor",
                       "xor_eq"])
# Names that need a trailing "_" to avoid clashes.
CppMangle = CppKeywords+Set.new(["string"])

class String
  def cppsafe() CppMangle.include?(self) ? self+"_" : self; end

  def amqp2cpp()
    path=split(".")
    name=path.pop
    return name.typename if path.empty?
    path.map! { |n| n.nsname }
    return (path << name.caps.cppsafe).join("::")
  end

  def typename() caps.cppsafe; end
  def nsname() bars.cppsafe; end
  def constname() shout.cppsafe; end
  def funcname() lcaps.cppsafe; end
  def varname() lcaps.cppsafe; end
end

# preview: Hold information about a C++ type.
# 
# new mapping does not use CppType,
# Each amqp type corresponds exactly by dotted name
# to a type, domain or struct, which in turns
# corresponds by name to a C++ type or typedef.
# (see String.amqp2cpp)
# 
class CppType
  def initialize(name) @name=@param=@ret=name; end
  attr_reader :name, :param, :ret, :code
  
  def retref() @ret="#{name}&"; self; end
  def retcref() @ret="const #{name}&"; self; end
  def passcref() @param="const #{name}&"; self; end
  def code(str) @code=str; self; end
  def defval(str) @defval=str; self; end
  def encoded() @code end
  def ret_by_val() @name; end

  def encode(value, buffer)
    @code ? "#{buffer}.put#{@code}(#{value});" : "#{value}.encode(#{buffer});"
  end

  def decode(value,buffer)
    if @code
      if /&$/===param then
        "#{buffer}.get#{@code}(#{value});"
      else
        "#{value} = #{buffer}.get#{@code}();"
      end
    else
      "#{value}.decode(#{buffer});"
    end
  end

  def default_value()
    return @defval ||= "#{name}()"
  end

  def to_s() name; end;
end

class AmqpRoot
  # preview; map 0-10 types to preview code generator types
  @@typemap = {
    "bit"=> CppType.new("bool").code("Octet").defval("false"),
    "boolean"=> CppType.new("bool").code("Octet").defval("false"),
    "uint8"=>CppType.new("uint8_t").code("Octet").defval("0"), 
    "uint16"=>CppType.new("uint16_t").code("Short").defval("0"),
    "uint32"=>CppType.new("uint32_t").code("Long").defval("0"),
    "uint64"=>CppType.new("uint64_t").code("LongLong").defval("0"),
    "datetime"=>CppType.new("uint64_t").code("LongLong").defval("0"),
    "str8"=>CppType.new("string").passcref.retcref.code("ShortString"),
    "str16"=>CppType.new("string").passcref.retcref.code("MediumString"),
    "str32"=>CppType.new("string").passcref.retcref.code("LongString"),
    "vbin8"=>CppType.new("string").passcref.retcref.code("ShortString"),
    "vbin16"=>CppType.new("string").passcref.retcref.code("MediumString"),
    "vbin32"=>CppType.new("string").passcref.retcref.code("LongString"),
    "map"=>CppType.new("FieldTable").passcref.retcref,
    "array"=>CppType.new("Array").passcref.retcref,
    "sequence-no"=>CppType.new("SequenceNumber").passcref,
    "sequence-set"=>CppType.new("SequenceSet").passcref.retcref,
    "struct32"=>CppType.new("string").passcref.retcref.code("LongString"),
    "uuid"=>CppType.new("Uuid").passcref.retcref,
    "byte-ranges"=>CppType.new("ByteRanges").passcref.retcref
  }

  # preview: map amqp types to preview cpp types.
  def lookup_cpptype(t) t = @@typemap[t] and return t end
end


class AmqpElement
  # convert my amqp type_ attribute to a C++ type.
  def amqp2cpp()
    return "ArrayDomain<#{array_type(name).amqp2cpp}> " if type_=="array"
    return type_.amqp2cpp
  end

  # Does this object have a type-like child named name?
  def typechild(name)
    child = domain(name) if respond_to? :domain
    child = struct(name) if not child and respond_to? :struct
    child = type_(name) if not child and respond_to? :type_
    child
  end

  # dotted name to a type object
  def dotted_typechild(name)
    names=name.split('.')
    context = self
    while context and names.size > 1
      context = context.child_named(names.shift) 
    end
    return context.typechild(names[0]) if context
  end
  
  # preview mapping - type_ attribute to C++ type
  def lookup_cpptype(name)
    if t = root.lookup_cpptype(name) then return t 
    elsif c = containing_class.typechild(name) then return c.cpptype
    elsif c= root.dotted_typechild(name) then return c.cpptype
    else raise "Cannot resolve type-name #{name} from #{self}" 
    end
  end
  
  def containing_class()
    return self if is_a? AmqpClass
    return parent && parent.containing_class
  end
end


class AmqpField
  def struct?() 
    c=containing_class
    c.struct(type_)
  end
  def cpptype() lookup_cpptype(type_)  or raise "no cpptype #{type_} for field #{self}" end
  def cppname() name.lcaps.cppsafe; end
  def bit?() type_ == "bit"; end
  def signature() cpptype.param+" "+cppname; end

  def fqtypename()
    unless type_.index(".") 
      c=containing_class
      return c.domain(type_).fqtypename if c.domain(type_)
      return c.struct(type_).fqclassname if c.struct(type_)
    end
    return amqp2cpp
  end
  def paramtype()
    /^(int|uint|char|boolean|bit)/ === type_ ? fqtypename : "const #{fqtypename}&"
  end
  def param_default() "=#{fqtypename}()"  end

  # Default value is normally the C++ default but over-ridden in specific cases
  def default_value()
    defval = cpptype.default_value;
    if name == "accept-mode" and parent.name == "transfer" then defval = "1";  end
    return defval
  end
end

class AmqpMethod
  def cppname() name.lcaps.cppsafe; end
  def param_names() fields.map { |f| f.cppname }; end
  def signature() fields.map { |f| f.signature }; end
  def classname() parent.name; end
  def body_name() 
    classname().caps+name.caps+"Body"      
  end
  def cpp_pack_type() root.lookup_cpptype("uint16") end
end

module AmqpHasFields
  def parameters(with_default=nil)
    fields.map { |f|
      "#{f.paramtype} #{f.cppname}_#{f.param_default if with_default}"
    }
  end
  def unused_parameters() fields.map { |f| "#{f.paramtype} /*#{f.cppname}_*/"} end
  def arguments() fields.map { |f| "#{f.cppname}_"} end
  def values() fields.map { |f| "#{f.cppname}"} end
  def initializers() fields.map { |f| "#{f.cppname}(#{f.cppname}_)"}  end
end

class AmqpAction
  def classname() name.typename; end
  def funcname() parent.name.funcname + name.caps; end
  def fqclassname() parent.name+"::"+classname; end
  def full_code() (containing_class.code.hex << 8)+code.hex; end
  include AmqpHasFields
end

class AmqpType
  def cpptype() root.lookup_cpptype(name) end # preview
  def typename() name.typename; end      # new mapping
  def fixed?() fixed_width; end
end

class AmqpCommand < AmqpAction
  def base() "Command";  end
end

class AmqpControl < AmqpAction
  def base() "Control";  end
end

class AmqpClass
  def cppname() name.caps; end  # preview
  def nsname() name.nsname; end
end

class AmqpDomain
  # preview
  def cpptype() lookup_cpptype(type_) end
  def cppname() name.caps; end

  # new mapping
  def fqtypename()
    return containing_class.nsname+"::"+name.typename if containing_class
    name.typename
  end
end

class AmqpResult
  # preview
  def cpptype()
    if type_ then lookup_cpptype(type_)
    else CppType.new(parent.parent.name.caps+parent.name.caps+"Result").passcref
    end
  end
end

class AmqpStruct
  include AmqpHasFields

  @@pack_types={ "1"=>"uint8", "2"=>"uint16", "4"=>"uint32"}
  def cpp_pack_type()           # preview
    root.lookup_cpptype(@@pack_types[pack])
  end 
  def cpptype() CppType.new(cppname).passcref.retcref end
  #def cppname() containing_class.cppname+name.caps;  end
  def cppname()
    if parent.kind_of? AmqpResult
      parent.parent.parent.name.caps+parent.parent.name.caps+"Result"
    else
      name.caps  
    end
  end
  def fqclassname() containing_class.nsname+"::"+name.typename;  end
  def classname() name.typename; end
  def full_code() (containing_class.code.hex << 8)+code.hex; end
end

class CppGen < Generator
  def initialize(outdir, *specs)
    super(outdir,*specs)
    # need to sort classes for dependencies
    @actions=[]                 # Stack of end-scope actions
  end

  # Write a header file. 
  def h_file(path, &block)
    path = (/\.h$/ === path ? path : path+".h")
    guard=path.upcase.tr('./-','_')
    file(path) { 
      gen "#ifndef #{guard}\n"
      gen "#define #{guard}\n"
      gen Copyright
      yield
      gen "#endif  /*!#{guard}*/\n" 
    }
  end

  # Write a .cpp file.
  def cpp_file(path, &block)
    path = (/\.cpp$/ === path ? path : path+".cpp")
    file(path) do
      gen Copyright
      yield
    end
  end

  def include(header)
    header+=".h" unless /(\.h|[">])$/===header
    header="\"#{header}\"" unless /(^<.*>$)|(^".*"$)/===header
    genl "#include #{header}"
  end

  def scope(open="{",close="}", &block)
    genl open
    indent &block
    genl close
  end
  
  def namespace(name, &block) 
    genl
    names = name.split("::")
    names.each { |n| genl "namespace #{n} {" }
    genl "namespace {" if (names.empty?)
    genl
    yield
    genl
    genl('}'*([names.size, 1].max)+" // namespace "+name)
    genl
  end

  def struct_class(type, name, bases, &block)
    gen "#{type} #{name}"
    if (!bases.empty?)
      genl ":"
      indent { gen "#{bases.join(",\n")}" }
    end
    genl
    scope("{","};", &block)
  end

  def struct(name, *bases, &block)
    struct_class("struct", name, bases, &block);
  end
  def cpp_class(name, *bases, &block)
    struct_class("class", name, bases, &block);
  end

  def typedef(type, name) genl "typedef #{type} #{name};\n"; end

  def variant(types) "boost::variant<#{types.join(", ")}>"; end
  def variantl(types) "boost::variant<#{types.join(", \n")}>"; end
  def blank_variant(types) variant(["boost::blank"]+types); end
  def tuple(types) "boost::tuple<#{types.join(', ')}>"; end

  def public() outdent { genl "public:" } end
  def private() outdent { genl "private:" } end
  def protected() outdent { genl "protected:" } end

  # Returns [namespace, classname, filename]
  def parse_classname(full_cname)
    names=full_cname.split("::")
    return names[0..-2].join('::'), names[-1], names.join("/") 
  end

  def doxygen_comment(&block)
    genl "/**"
    prefix(" * ",&block)
    genl " */"
  end

  # Generate code in namespace for each class
  def each_class_ns()
    @amqp.classes.each { |c| namespace(c.nsname) { yield c } }
  end

  def signature(ret_name, params, trailer="")
    if params.size <= 1
      genl ret_name+"(#{params})"+trailer
    else
      scope(ret_name+"(",")"+trailer) { genl params.join(",\n") }
    end
  end
  
  def function_decl(ret_name, params=[], trailer="")
    signature(ret_name, params, trailer+";")
  end

  def function_defn(ret_name, params=[], trailer="")
    genl
    signature(ret_name, params, trailer)
    scope() { yield }
  end

  def ctor_decl(name, params=[]) function_decl(name, params); end
  
  def ctor_defn(name, params=[], inits=[])
    signature(name, params, inits.empty? ? "" : " :")
    indent { gen inits.join(",\n") } if not inits.empty?
    scope() { yield }
  end

  def function_call(name, params=[], trailer="")
    gen name
    list "(",params, ")"
    gen trailer
  end
end

# Fully-qualified class name
class FqClass < Struct.new(:fqname,:namespace,:name,:file)
  def initialize(fqclass)
    names=fqclass.split "::"
    super(fqclass, names[0..-2].join('::'), names[-1], names.join("/"))
  end
end

