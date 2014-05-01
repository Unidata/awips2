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

class TypeCode < CppGen
  def initialize(outdir, amqp)
    super(outdir, amqp)
    @ns="qpid::amqp_#{@amqp.version.bars}"
    @dir="qpid/amqp_#{@amqp.version.bars}"
    @types = @amqp.collect_all(AmqpType).select { |t| t.code }

  end

  def type_for_code_h()
    h_file("#{@dir}/TypeForCode") {
      include "#{@dir}/built_in_types.h"      
      include "#{@dir}/UnknownType.h"
      namespace(@ns) {
        genl
        genl "template <uint8_t Code> struct TypeForCode;"
        genl
        @types.each { |t|
          genl "template <> struct TypeForCode<#{t.code}> {  typedef #{t.typename} type; };"
        }
        genl
        genl "template <class V> typename V::result_type"
        scope("apply_visitor(V& visitor, uint8_t code) {") {
          scope("switch (code) {", "}") {
            @types.each { |t|
              genl "case #{t.code}: return visitor((#{t.typename}*)0);"
            }
            genl "default: return visitor((UnknownType*)0);"
          }
        }
        genl
        genl "std::string typeName(uint8_t code);"
      }
    }
  end

  def type_for_code_cpp()
    cpp_file("#{@dir}/TypeForCode") {
      include "<string>"
      include "<sstream>"
      namespace(@ns) {
        namespace("") { 
          struct("Names") {
            scope("Names() {") {
              scope("for (int i =0; i < 256; ++i) {") {
                genl "std::ostringstream os;"
                genl "os << \"UnknownType<\" << i << \">\";"
                genl "names[i] = os.str();"
              }
              @types.each { |t| genl "names[#{t.code}] = \"#{t.name}\";" }
            }  
            genl "std::string names[256];"
          }
          genl "Names names;"
        }
        genl "std::string typeName(uint8_t code) { return names.names[code]; }"
      }}
  end

  def code_for_type_h()
    name="#{@dir}/CodeForType"
    h_file(name) {
      include "#{@dir}/built_in_types.h"
      
      namespace(@ns) {
        genl
        genl "template <class T> struct CodeForType;"
        genl
        @types.each { |t|
          genl "template <> struct CodeForType<#{t.typename}> { static const uint8_t value; };"
        }
        genl
        genl "template <class T> uint8_t codeFor(const T&) { return CodeForType<T>::value; }"
      }
    }

    cpp_file(name) {
      include name
      namespace(@ns) { 
        @types.each { |t|
          genl "const uint8_t CodeForType<#{t.typename}>::value=#{t.code};"
        }
      }
    }
  end
  
  def generate
    type_for_code_h
    type_for_code_cpp
    code_for_type_h
  end
end

TypeCode.new($outdir, $amqp).generate();

