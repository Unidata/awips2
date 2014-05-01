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

class GenExceptions < CppGen

  def initialize(outdir, amqp)
    super(outdir, amqp)
    @ns="qpid::amqp_#{@amqp.version.bars}"
    @dir="qpid/amqp_#{@amqp.version.bars}"
  end

  def exceptions_for_enum(enum, base, ns, suffix="")
    enum.choices.each { |c|
      name=c.name.typename+suffix+"Exception"
      genl
      doxygen_comment { genl c.doc }
      struct(name, "public #{base}") {
        genl "#{name}(const std::string& msg=std::string())"
        genl "    : #{base}(#{ns}::#{c.name.shout}, msg) {}"
        protected
        genl "std::string getPrefix() const { return \"#{name}\"; }"
      }
    }
  end
  
  def gen_exceptions()
    h_file("#{@dir}/exceptions") { 
      include "qpid/amqp_0_10/Exception"
      namespace("#{@ns}") { 
        error_code = @amqp.class_("execution").domain("error-code").enum
        exceptions_for_enum(error_code, "SessionAbortedException", "execution")
        genl

        detach_code = @amqp.class_("session").domain("detach-code").enum
        exceptions_for_enum(detach_code, "SessionDetachedException", "session", "Detached")

        genl
        exceptions_for_enum(detach_code, "SessionExpiredException", "session", "Expired")
        genl

        close_code =  @amqp.class_("connection").domain("close-code").enum
        exceptions_for_enum(close_code, "ConnectionException", "connection")
      }
    }
  end
  
  def generate()
    gen_exceptions
  end
end

GenExceptions.new($outdir, $amqp).generate();


