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

class MethodBodyDefaultVisitorGen < CppGen
  
  def initialize(outdir, amqp)
    super(outdir, amqp)
    @namespace, @classname, @filename = parse_classname("qpid::framing::MethodBodyDefaultVisitor")
  end

  def generate()
    h_file(@filename) {
      include "qpid/framing/MethodBodyConstVisitor"
      include "qpid/CommonImportExport.h"
      namespace(@namespace) { 
        genl "class AMQMethodBody;"
        cpp_class(@classname, "public MethodBodyConstVisitor") {
          genl "public:"
          genl "virtual void defaultVisit(const AMQMethodBody&) = 0;"
          @amqp.methods_.each { |m|
            genl "QPID_COMMON_EXTERN virtual void visit(const #{m.body_name}&);" }
        }}}

    cpp_file(@filename) {
      include(@filename)
      include("qpid/framing/all_method_bodies.h")
      namespace(@namespace) {
        @amqp.methods_.each { |m|
          genl "void #{@classname}::visit(const #{m.body_name}& b) { defaultVisit(b); }"
        }}}
  end
end

MethodBodyDefaultVisitorGen.new($outdir, $amqp).generate();

