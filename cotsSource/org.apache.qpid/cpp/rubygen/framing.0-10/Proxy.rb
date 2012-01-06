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

class ProxyGen < CppGen
    
  def initialize(chassis, outdir, amqp)
    super(outdir, amqp)
    @chassis=chassis
    @classname="AMQP_#{@chassis.caps}Proxy"
    @filename="qpid/framing/#{@classname}"
  end

  def methods_on(parent, chassis)
    chassis == "all"  ? parent.methods_ : parent.methods_on(chassis)
  end
  
  def proxy_member(c) c.name.lcaps+"Proxy"; end
  
  def inner_class_decl(c)
    cname=c.name.caps
    cpp_class(cname, "public Proxy") {
          gen <<EOS
public:
#{cname}(FrameHandler& f) : Proxy(f) {}
static #{cname}& get(#{@classname}& proxy) { return proxy.get#{cname}(); }
EOS
      methods_on(c, @chassis).each { |m|
        genl "QPID_COMMON_EXTERN virtual void #{m.cppname}(#{m.signature.join(",\n            ")});"
        genl
      }}
  end

  def inner_class_defn(c)
    cname=c.cppname
    methods_on(c, @chassis).each { |m| 
      genl "void #{@classname}::#{cname}::#{m.cppname}(#{m.signature.join(", ")})"
      scope { 
        params=(["getVersion()"]+m.param_names).join(", ")
        genl "send(#{m.body_name}(#{params}));"
      }}
  end

  def generate
    # .h file
    h_file(@filename) {
      include "qpid/framing/Proxy.h"
      include "qpid/framing/Array.h"
      include "qpid/framing/amqp_types.h"
      include "qpid/framing/amqp_structs.h"
      include "qpid/CommonImportExport.h"

      namespace("qpid::framing") { 
        cpp_class(@classname, "public Proxy") {
          public
          genl "QPID_COMMON_EXTERN #{@classname}(FrameHandler& out);"
          genl
          @amqp.classes.each { |c|
            inner_class_decl(c)
            genl
            genl "#{c.cppname}& get#{c.cppname}() { return #{proxy_member(c)}; }"
            genl 
          }
          private
          @amqp.classes.each{ |c| gen c.cppname+" "+proxy_member(c)+";\n" }
        }}}

  # .cpp file
  cpp_file(@filename) {
      include "<sstream>"
      include "qpid/framing/#{@classname}.h"
      include "qpid/framing/amqp_types_full.h"
      methods_on(@amqp, @chassis).each {
        |m| include "qpid/framing/"+m.body_name
      }
      genl
      namespace("qpid::framing") { 
        genl "#{@classname}::#{@classname}(FrameHandler& f) :"
        gen "   Proxy(f)"
        @amqp.classes.each { |c| gen ",\n    "+proxy_member(c)+"(f)" }
        genl "{}\n"
        @amqp.classes.each { |c| inner_class_defn(c) }
      }}
   end
end


ProxyGen.new("client", $outdir, $amqp).generate;
ProxyGen.new("server", $outdir, $amqp).generate;
ProxyGen.new("all", $outdir, $amqp).generate;
    
