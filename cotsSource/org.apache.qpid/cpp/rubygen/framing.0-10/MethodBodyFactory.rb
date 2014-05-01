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

class MethodBodyFactoryGen < CppGen
  
  def initialize(outdir, amqp)
    super(outdir, amqp)
    @namespace="qpid::framing"
    @classname="MethodBodyFactory"
    @filename="qpid/framing/MethodBodyFactory"
  end

  def generate()
    cpp_file(@filename) {
      include @filename
      include "qpid/framing/BodyFactory"
      @amqp.methods_.each { |m| include "qpid/framing/#{m.body_name}" }
      include "qpid/Exception.h"
      genl
      namespace(@namespace) {
        scope("boost::intrusive_ptr<AMQMethodBody> #{@classname}::create(ClassId c, MethodId m) {") {
          scope("switch (c) {") {
            @amqp.classes.each { |c|
              scope("case #{c.code}: switch(m) {") {
                c.methods_.each { |m|
                  genl "case #{m.code}: return BodyFactory::create<#{m.body_name}>();"
                }
                genl "default: throw Exception(QPID_MSG(\"Invalid method id \" << int(m) << \" for class #{c.name} \"));"
              }
              genl "break;"
            }
            genl "default: throw Exception(QPID_MSG(\"Invalid class id \" << int(c)));"
          }
        }
      }}
  end
end

MethodBodyFactoryGen.new($outdir, $amqp).generate();
