#!/usr/bin/env ruby
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
$: << ".."                      # Include .. in load path
require 'cppgen'


# Dummy element representing an unknown struct type.
class UnknownStruct
  def visitable?() true end
  def fqclassname() "UnknownStruct" end
end

# Dummy element representing a session.header field
class SessionHeaderField
  def amqp2cpp() "session::Header" end
  def cppname() "sessionHeader" end
  def name() "session-header" end
end

class Specification < CppGen
  def initialize(outdir, amqp)
    super(outdir, amqp)
    @ns="qpid::amqp_#{@amqp.version.bars}"
    @dir="qpid/amqp_#{@amqp.version.bars}"
  end

  # domains

  def domain_h(d)
    genl
    typename=d.name.typename
    if d.enum
      scope("enum #{typename} {", "};") { 
        genl d.enum.choices.map { |c|
          "#{c.name.constname} = #{c.value}" }.join(",\n")
      }
      scope("inline SerializeAs<#{typename}, uint8_t> serializable(#{typename}& e) {") {
        genl "return SerializeAs<#{typename}, uint8_t>(e);"
      }
    else
      genl "typedef #{d.amqp2cpp} #{typename};"
    end
  end

  def visitable?(x) x.code and x.size=="4"  end

  # Used by structs, commands and controls.
  def action_struct_h(x, base, consts, &block)
    genl
    base = visitable?(x) ? ["public #{base}"] : []
    struct(x.classname, *base) { 
      x.fields.each { |f| genl "#{f.amqp2cpp} #{f.cppname};" }
      genl
      genl "static const char* NAME;"
      consts.each {
        |c| genl "static const uint8_t #{c.upcase}=#{(x.send c) or 0};"
      }
      genl "static const uint8_t CLASS_CODE=#{x.containing_class.nsname}::CODE;"
      genl "static const char* CLASS_NAME;"
      ctor_decl("explicit #{x.classname}", x.parameters(true))

      if visitable? x
        genl "void accept(Visitor&);" 
        genl "void accept(ConstVisitor&) const;"
      end

      if (x.fields.empty?)
        genl "template <class S> void serialize(S&) {}"
      else
        scope("template <class S> void serialize(S& s) {") {
          gen "s"; x.fields.each { |f| gen "(#{f.cppname})"}; genl ";"
        }
      end
      genl
      yield if block
    }
    case x
    when AmqpCommand then packer =  "CommandPacker"
    when AmqpControl then packer =  "Packer"
    when AmqpStruct then packer =  "SizedPacker"
    end
    genl "inline #{packer}<#{x.classname}> serializable(#{x.classname}& x) { return #{packer}<#{x.classname}>(x); }" unless x.respond_to? :pack and x.pack == "0"
    genl "std::ostream& operator << (std::ostream&, const #{x.classname}&);"
    genl "bool operator==(const #{x.classname}&, const #{x.classname}&);"
  end

  def action_struct_cpp(x, &block)
    genl
    genl "const char* #{x.classname}::NAME=\"#{x.fqname}\";"
    genl "const char* #{x.classname}::CLASS_NAME=#{x.containing_class.nsname}::NAME;"
    genl
    ctor=x.classname+"::"+x.classname
    ctor_defn(ctor, x.parameters, x.initializers) {}

    if visitable? x
      genl "void #{x.classname}::accept(Visitor& v) {  v.visit(*this); }"
      genl "void #{x.classname}::accept(ConstVisitor& v) const { v.visit(*this); }"
    end
    genl
    scope("std::ostream& operator << (std::ostream& o, const #{x.classname}&#{"x" unless x.fields.empty?}) {") {
      genl "o << \"#{x.fqname}[\";";
      x.fields.each{ |f| genl "o << \" #{f.name}=\" << x.#{f.cppname};" }
      genl "o << \"]\";"
      genl "return o;"
    }
    yield if block
  end

  # structs

  def struct_h(s) action_struct_h(s, "Struct", ["size","pack","code"]); end
  def struct_cpp(s) action_struct_cpp(s) end

  # command and control
  
  def action_h(a)
    action_struct_h(a, a.base, ["code"]) {
      struct("Handler") {
        scope("void #{a.funcname}(", ");") {
          genl a.parameters.join(",\n")
        }
      }
      function_defn("template <class T> void invoke", ["T& target"], "const") {
        genl "target.#{a.funcname}(#{a.values.join(', ')} );"
      }
    }
  end
  
  def action_cpp(a)
    action_struct_cpp(a) {
      scope("void #{a.classname}::Handler::#{a.funcname}(", ")") {
        genl a.unused_parameters.join(",\n")
      }
      scope {
        genl "assert(0);"
        genl "throw NotImplementedException(QPID_MSG(\"#{a.fqname} not implemented.\"));"
      }
    }
  end

  # Types that must be generated early because they are used by other types.
  def pregenerate?(x) not @amqp.used_by[x.fqname].empty?;  end

  def pregenerate_class?(c)
    c.children.select{ |t| (t.is_a? AmqpStruct or t.is_a? AmqpDomain) and pregenerate? t} 
  end
  
  # Typedefs, enums and forward declarations for classes.
  def gen_specification_fwd()
    h_file("#{@dir}/specification_fwd") { 
      include "#{@dir}/built_in_types"
      namespace(@ns) {
        # Top level 
        @amqp.domains.each { |d|
          # segment-type and track are are built in
          domain_h d unless ["track","segment-type"].include?(d.name)
        }
        each_class_ns { |c|
          genl "const uint8_t CODE=#{c.code};" # class code
          genl "extern const char* NAME;"
          c.each_descendant { |x|
            case x
            when AmqpDomain then domain_h x 
            when AmqpStruct then genl "class #{x.classname};" 
            when AmqpAction then genl "class #{x.classname};" 
            end
          }
        }
      }
    }
  end

  # Generate struct definitions into a separate header file so the
  # can be included by StructHolder.h without circularity.
  def gen_structs()
    h_file("#{@dir}/structs") { 
      include "#{@dir}/specification_fwd"
      include "#{@dir}/Map.h"
      include "#{@dir}/Array.h"
      include "#{@dir}/Struct.h"
      include "#{@dir}/UnknownStruct.h"
      include "#{@dir}/Packer.h"
      namespace(@ns) {
        each_class_ns { |c|
          c.collect_all(AmqpStruct).each { |s| struct_h s } 
        }
      }
    }
    
    cpp_file("#{@dir}/structs") { 
      include "#{@dir}/structs"
      include "#{@dir}/StructHolder"
      namespace(@ns) {
        each_class_ns { |c|
          c.collect_all(AmqpStruct).each {  |s| struct_cpp(s) }
        }
      }
    }
  end
  
  # Generate the specification files
  def gen_specification()
    h_file("#{@dir}/specification") {
      include "#{@dir}/specification_fwd.h"
      include "#{@dir}/Map.h"
      include "#{@dir}/Array.h"
      include "#{@dir}/UnknownType.h"
      include "#{@dir}/Struct32"
      include "#{@dir}/Control.h"
      include "#{@dir}/Command.h"
      include "#{@dir}/Packer.h"
      include "<iosfwd>"
      namespace(@ns) { 
        each_class_ns { |c|
          c.collect_all(AmqpAction).each { |a| action_h a }
        }
      }}

    cpp_file("#{@dir}/specification") { 
      include "#{@dir}/specification"
      include "#{@dir}/exceptions"
      include "<iostream>"
      ["Command","Control", "Struct"].each { |x| include "#{@dir}/Apply#{x}" }
      namespace(@ns) { 
        each_class_ns { |c|
          genl "const char* NAME=\"#{c.fqname}\";"
          c.actions.each { |a| action_cpp a}
        }
      }
    }
  end

  def gen_proxy()
    h_file("#{@dir}/ProxyTemplate.h") { 
      include "#{@dir}/specification"
      namespace(@ns) { 
        genl "template <class F, class R=typename F::result_type>"
        cpp_class("ProxyTemplate") {
          public
          genl "ProxyTemplate(F f=F()) : functor(f) {}"
          @amqp.classes.each { |c|
            c.actions.each { |a|
              genl
              function_defn("R #{a.funcname}", a.parameters) { 
                var=a.name.funcname
                args = a.arguments.empty? ? "" : "("+a.arguments.join(", ")+")"
                genl("#{a.fqclassname} #{var}#{args};")
                genl "return functor(#{var});"
              }
            }
          }
          private
          genl "F functor;"
        }
      }
    }
  end

  def visitor_interface_h(base, subs, is_const)
    name="#{is_const ? 'Const' : ''}#{base}Visitor"
    const=is_const ? "const " : ""
    struct(name) {
      genl "virtual ~#{name}() {}"
      genl "typedef #{const}#{base} BaseType;"
      subs.each{ |s|
        genl "virtual void visit(#{const}#{s.fqclassname}&) = 0;"
      }}
  end

  def visitor_impl(base, subs, is_const)
    name="#{is_const ? 'Const' : ''}#{base}Visitor"
    const=is_const ? "const " : ""
    genl "template <class F>"
    struct("ApplyVisitor<#{name}, F>", "public ApplyVisitorBase<#{name}, F>") {
      subs.each{ |s|
        genl "virtual void visit(#{const}#{s.fqclassname}& x) { this->invoke(x); }" 
      }}
  end

  def gen_visitor(base, subs)
    h_file("#{@dir}/#{base}Visitor.h") { 
      include base=="Struct" ? "#{@dir}/structs" : "#{@dir}/specification"
      namespace("#{@ns}") { 
        visitor_interface_h(base, subs, false)
        visitor_interface_h(base, subs, true)
      }}
    
    h_file("#{@dir}/Apply#{base}.h") {
      include "#{@dir}/#{base}Visitor.h"
      include "#{@dir}/apply.h"
      namespace("#{@ns}") { 
        visitor_impl(base, subs, false)
        visitor_impl(base, subs, true)
      }
    }
  end

  def gen_holder(base, subs)
    name=base+"Holder"
    h_file("#{@dir}/#{name}") {
      include "#{@dir}/Apply#{base}"
      include "#{@dir}/Holder"
      include base=="Struct" ? "#{@dir}/structs" : "#{@dir}/specification"
      namespace(@ns){
        namespace("#{base.downcase}_max") {
          genl "static const size_t MAX000=0;"
          last="MAX000"
          subs.each { |s|
            sizeof="sizeof(#{s.fqclassname})"
            genl "static const size_t #{last.succ} = #{sizeof} > #{last} ? #{sizeof} : #{last};"
            last.succ!
          }
          genl "static const int MAX=#{last};"
        }
        holder_base="amqp_0_10::Holder<#{name}, #{base}, #{base.downcase}_max::MAX>"
        struct("#{name}", "public #{holder_base}") {
          genl "#{name}() {}"
          genl "template <class T> explicit #{name}(const T& t) : #{holder_base}(t) {}"
          genl "using #{holder_base}::operator=;"
          genl "void set(uint8_t classCode, uint8_t code);"
        }
        genl
        genl "std::ostream& operator<<(std::ostream& o, const #{name}& h);"
      }
    }
    
    cpp_file("#{@dir}/#{name}") {
      include "#{@dir}/#{name}"
      include "#{@dir}/exceptions.h"
      namespace(@ns) {
        genl "using framing::in_place;"
        genl
        scope("void #{name}::set(uint8_t classCode, uint8_t code) {") {
          genl "uint16_t key=(classCode<<8)+code;"
          scope ("switch(key) {") {
            subs.each { |s|
              genl "case 0x#{s.full_code.to_s(16)}: *this=in_place<#{s.fqclassname}>(); break;" unless (s.is_a? UnknownStruct)
            }
            genl "default: "
            indent { 
              if (base=="Struct")
                genl "*this=in_place<UnknownStruct>(classCode, code);"
              else
                genl "throw CommandInvalidException(QPID_MSG(\"Invalid class-#{base.downcase} key \" << std::hex << key));"
              end
            }
          }
        }
        genl
        genl "std::ostream& operator<<(std::ostream& o, const #{name}& h) { return h.get() ? (o << *h.get()) : (o << \"<empty #{name}>\"); }"
      }
    }
  end

  def gen_visitable(base, subs)
     subs << UnknownStruct.new if base=="Struct" # Extra case for unknown structs.
    gen_holder(base, subs)
    gen_visitor(base, subs)
  end

  def generate
    gen_specification_fwd
    gen_specification
    gen_proxy
    gen_structs
    gen_visitable("Command", @amqp.collect_all(AmqpCommand))
    gen_visitable("Control", @amqp.collect_all(AmqpControl))
    gen_visitable("Struct",  @amqp.collect_all(AmqpStruct).select { |s| s.code})
  end
end

Specification.new($outdir, $amqp).generate();
