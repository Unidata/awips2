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

require 'thread'
require 'monitor'

# Monkeypatch
class MonitorMixin::ConditionVariable

  # Wait until BLOCK returns TRUE or TIMEOUT seconds have passed
  # Return TRUE if BLOCK returned TRUE within the TIMEOUT, FALSE
  # otherswise
  def wait_for(timeout=nil, &block)
    start = Time.now
    passed = 0
    until yield
      if timeout.nil?
        wait
      elsif passed < timeout
        wait(timeout)
      else
        return false
      end
      passed = Time.now - start
    end
    return true
  end
end

module Qpid::Util

  # Similar to Python's threading.Event
  class Event
    def initialize
      @monitor = Monitor.new
      @cond = @monitor.new_cond
      @set = false
    end

    def set
      @monitor.synchronize do
        @set = true
        @cond.signal
      end
    end

    def clear
      @monitor.synchronize { @set = false }
    end

    def wait(timeout = nil)
      @monitor.synchronize do
        unless @set
          @cond.wait_for(timeout) { @set }
        end
      end
    end
  end
end
