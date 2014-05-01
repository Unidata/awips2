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

class GenAllSegmentTypes < CppGen
  def initialize(outdir, amqp)
    super(outdir, amqp)
  end

  def generate
    h_file("tests/allSegmentTypes.h") { 
      include "qpid/amqp_0_10/specification.h"
      include "qpid/amqp_0_10/Header.h"
      include "qpid/amqp_0_10/Body.h"
      genl
      genl "using namespace qpid::amqp_0_10;"
      genl
      scope("template <class Op> size_t allSegmentTypes(Op& op) {"){
        genl "op(Header());"
        genl "op(Body());"
        n = 2;
        @amqp.classes.each { |c|
          c.commands.each { |s| genl "op(CommandHolder(#{c.nsname}::#{s.classname}()));" }
          c.controls.each { |s| genl "op(ControlHolder(#{c.nsname}::#{s.classname}()));" }
          n += 2
        }
        genl "return #{n};"
      }
    }
  end
end

GenAllSegmentTypes.new($outdir, $amqp).generate();

