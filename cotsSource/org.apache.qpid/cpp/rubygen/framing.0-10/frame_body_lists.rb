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

class FrameBodyListsGen < CppGen
    
  def initialize(outdir, amqp) 
    super(outdir, amqp); 
  end

  def generate
    h_file("qpid/framing/frame_body_lists.h") {
      gen <<EOS
/**@file
 * Macro lists of frame body classes, used to generate Visitors
 */
EOS
      gen "#define METHOD_BODIES() "
      @amqp.methods_.each { |m| gen "\\\n    (#{m.body_name}) " }
      gen <<EOS


#define OTHER_BODIES() (AMQContentBody)(AMQHeaderBody)(AMQHeartbeatBody))

EOS
    }
  end
end

FrameBodyListsGen.new($outdir, $amqp).generate;

    
