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

import sys

from ops import *

def METHOD(module, op):
  method = lambda self, *args, **kwargs: self.invoke(op, args, kwargs)
  if sys.version_info[:2] > (2, 3):
    method.__name__ = op.__name__
    method.__doc__ = op.__doc__
    method.__module__ = module
  return method

def generate(module, operations):
  dict = {}

  for name, enum in ENUMS.items():
    if isinstance(name, basestring):
      dict[name] = enum

  for name, op in COMPOUND.items():
    if isinstance(name, basestring):
      dict[name] = METHOD(module, op)

  for name, op in operations.items():
    if isinstance(name, basestring):
      dict[name] = METHOD(module, op)

  return dict

def invoker(name, operations):
  return type(name, (), generate(invoker.__module__, operations))

def command_invoker():
  return invoker("CommandInvoker", COMMANDS)

def control_invoker():
  return invoker("ControlInvoker", CONTROLS)
