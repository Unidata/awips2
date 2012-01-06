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

# Augment the standard python multithreaded Queue implementation to add a
# close() method so that threads blocking on the content of a queue can be
# notified if the queue is no longer in use.

require 'thread'

# Python nominally uses a bounded queue, but the code never establishes
# a maximum size; we therefore use Ruby's unbounded queue
class Qpid::Queue < ::Queue

  DONE = Object.new
  STOP = Object.new

  def initialize
    super
    @error = nil
    @listener = nil
    @exc_listener = nil
    @exc_listener_lock = Monitor.new
    @thread = nil
  end

  def close(error = nil)
    @error = error
    put(DONE)
    unless @thread.nil?
      @thread.join()
      @thread = nil
    end
  end

  def get(block = true, timeout = nil)
    unless timeout.nil?
      raise NotImplementedError
    end
    result = pop(! block)
    if result == DONE
      # this guarantees that any other waiting threads or any future
      # calls to get will also result in a Qpid::Closed exception
      put(DONE)
      raise Qpid::Closed.new(@error)
    else
      return result
    end
  end

  alias :put :push

  def exc_listen(&block)
    @exc_listener_lock.synchronize do
      @exc_listener = block
    end
  end

  def listen(&block)
    if ! block_given? && @thread
      put(STOP)
      @thread.join()
      @thread = nil
    end

    # FIXME: There is a potential race since we could be changing one
    # non-nil listener to another
    @listener = block

    if block_given? && @thread.nil?
      @thread = Thread.new do
        loop do
          begin
            o = get()
            break if o == STOP
            @listener.call(o)
          rescue Qpid::Closed => e
            @exc_listener.call(e) if @exc_listener
            break
          end
        end
      end
    end
  end

end
