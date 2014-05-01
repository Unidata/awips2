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

class Qpid::Invoker

  # Requires that client defines a invoke method and overrides
  # resolve_method

  # FIXME: Is it really worth defining methods in method_missing ? We
  # could just dispatch there directly

  def invc_method(name, resolved)
    define_singleton_method(name) { |*args| invoke(resolved, args) }
    # FIXME: the Python code also attaches docs from resolved.pydoc
  end

  def invc_value(name, resolved)
    define_singleton_method(name) { | | resolved }
  end

  def invc_error(name, resolved)
    msg = "%s instance has no attribute '%s'" % [self.class.name, name]
    if resolved
      msg += "\n%s" % resolved
    end
    raise NameError, msg
  end

  def resolve_method(name)
    invocation(:error, nil)
  end

  def method_missing(name, *args)
    disp, resolved = resolve_method(name)
    disp.call(name, resolved)
    send(name, *args)
  end

  def invocation(kind, name = nil)
    [ method("invc_#{kind}"), name ]
  end

  private
  def define_singleton_method(name, &body)
    singleton_class = class << self; self; end
    singleton_class.send(:define_method, name, &body)
  end

end
