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

module Qpid
  def self.logger
    @logger ||= {}
    @logger
  end
end

require "qpid/util"
require "qpid/queue"
require "qpid/packer"
require "qpid/framer"
require "qpid/codec"
require 'qpid/datatypes'
require 'qpid/spec010'
require 'qpid/delegates'
require 'qpid/invoker'
require "qpid/assembler"
require 'qpid/session'
require "qpid/connection"
require "qpid/spec"
require 'qpid/queue'
require 'qpid/qmf'
