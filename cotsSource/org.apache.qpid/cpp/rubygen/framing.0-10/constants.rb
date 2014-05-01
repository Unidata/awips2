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

class ConstantsGen < CppGen
  
  def initialize(outdir, amqp)
    super(outdir, amqp)
    @namespace="qpid::framing"
    @dir="qpid/framing"
  end

  def constants_h()
    public_api("#{@dir}/constants.h")
    h_file("#{@dir}/constants.h") {
      namespace(@namespace) {
        # Constants for class/method names.
        scope("enum AmqpConstant {","};") {
          l=[]
          l.concat @amqp.constants.map { |c| "#{c.name.shout}=#{c.value}" }
          @amqp.classes.each { |c|
            l << "#{c.name.shout}_CLASS_ID=#{c.code}"
            l.concat c.methods_.map { |m|
              "#{c.name.shout}_#{m.name.shout}_METHOD_ID=#{m.code}" }
          }
          genl l.join(",\n")
        }
      }
    }
  end

  def typecode_enum(t) "TYPE_CODE_#{t.name.shout}" end

  def typecode_h_cpp
    path="#{@dir}/TypeCode"
    public_api(path+".h") 
    h_file(path) {
      include("<iosfwd>")
      include("\"qpid/sys/IntegerTypes.h\"")
      namespace(@namespace) { 
        scope("enum TypeCode {", "};") {
          genl @amqp.types.map { |t| "#{typecode_enum t} = #{t.code}" if t.code }.compact.join(",\n")
        }
        genl <<EOS

/** True if t is a valid TypeCode value */
bool isTypeCode(uint8_t t);

/** Throw exception if not a valid TypeCode */
TypeCode typeCode(uint8_t);

/**@return 0 if t is not a valid enum TypeCode value. */
const char* typeName(TypeCode t);

std::ostream& operator<<(std::ostream&, TypeCode);
EOS
      }
    }

    cpp_file(path) {
      include(path);
      include("qpid/Exception.h")
      include("<ostream>")
      namespace(@namespace) { 
        scope("const char* typeName(TypeCode t) {") {
          scope("switch (t) {") {
            @amqp.types.each { |t| genl "case #{typecode_enum t}: return \"#{t.name}\";" if t.code }
            genl "default: break;"
          }
          genl "return 0;";
        }
        genl <<EOS

bool isTypeCode(uint8_t t) { return typeName(TypeCode(t)); }

TypeCode typeCode(uint8_t t) {
    if (!isTypeCode(t)) throw Exception(QPID_MSG("Invalid TypeCode " << t));
    return TypeCode(t);
}

std::ostream& operator<<(std::ostream& o, TypeCode t) {
    if (isTypeCode(t)) return o << typeName(t);
    else return o << "Invalid TypeCode " << t;
}
EOS
      }
    }
  end
  
  def enum_h()
    public_api("#{@dir}/enum.h")
    h_file("#{@dir}/enum.h") {
      # Constants for enum domains.
      namespace(@namespace) {
        @amqp.domains.each { |d| declare_enum(d.enum) if d.enum }
        @amqp.classes.each { |c|
          enums=c.collect_all(AmqpEnum)
          if !enums.empty? then
            namespace(c.nsname) { enums.each { |e| declare_enum(e) } }
          end
        }
      }
    }
  end

  def declare_enum(enum)
    # Generated like this: enum containing_class::Foo { FOO_X, FOO_Y; }
    name="#{enum.parent.name.caps}"
    prefix=enum.parent.name.shout+"_" 
    scope("enum #{name} {","};") {
      genl enum.choices.collect { |c| "#{prefix}#{c.name.shout}=#{c.value}" }.join(",\n")
    }
  end

  def declare_exception(c, base, package, enum)
    name=c.name.caps+"Exception"
    value="#{package}::#{enum.parent.name.shout}_#{c.name.shout}"
    genl
    doxygen_comment { genl c.doc }
    struct(c.name.caps+"Exception", base) {
      genl "std::string getPrefix() const { return \"#{c.name}\"; }"
      genl "#{c.name.caps}Exception(const std::string& msg=std::string()) : #{base}(#{value}, \"\"+msg) {}"
    }
  end

  def declare_exceptions(class_name, domain_name, base)
    enum = @amqp.class_(class_name).domain(domain_name).enum
    enum.choices.each { |c| declare_exception(c, base, class_name, enum) unless c.name == "normal" }
    genl
    genl "QPID_COMMON_EXTERN sys::ExceptionHolder create#{base}(int code, const std::string& text);"
  end

  def create_exception(class_name, domain_name, base, invalid)
    scope("sys::ExceptionHolder create#{base}(int code, const std::string& text) {") {
      genl "sys::ExceptionHolder holder;"
      scope("switch (code) {") {
        enum = @amqp.class_(class_name).domain(domain_name).enum
        enum.choices.each { |c|
          assign = "holder = new #{c.name.caps}Exception(text); " unless c.name == "normal"
          genl "case #{c.value}: #{assign}break;" 
        }
        genl "default: holder = new #{invalid}(QPID_MSG(\"Bad #{enum.parent.name}: \" << code << \": \" << text));"
      }
      genl "return holder;"
    }
  end

  def reply_exceptions_h()
    public_api("#{@dir}/reply_exceptions.h")
    h_file("#{@dir}/reply_exceptions.h") {
      include "qpid/Exception"
      include "qpid/sys/ExceptionHolder"
      include "qpid/framing/enum"
      include "qpid/CommonImportExport.h"
      namespace(@namespace) {
        declare_exceptions("execution", "error-code", "SessionException")
        declare_exceptions("connection", "close-code", "ConnectionException")
        declare_exceptions("session", "detach-code", "ChannelException")
      }
    }
  end

  def reply_exceptions_cpp()
    cpp_file("#{@dir}/reply_exceptions") {
      include "#{@dir}/reply_exceptions"
      include "<sstream>"
      include "<assert.h>"
      namespace("qpid::framing") {
        create_exception("execution", "error-code", "SessionException", "InvalidArgumentException")
        # FIXME aconway 2008-10-07: there are no good exception codes in 0-10 for an invalid code.
        # The following choices are arbitrary.
        create_exception("connection", "close-code", "ConnectionException", "FramingErrorException")
        create_exception("session", "detach-code", "ChannelException", "NotAttachedException")
      }
    }
  end

  def generate()
    constants_h
    enum_h
    reply_exceptions_h
    reply_exceptions_cpp
    typecode_h_cpp
  end
end

ConstantsGen.new($outdir, $amqp).generate();

