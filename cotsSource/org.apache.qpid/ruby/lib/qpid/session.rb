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

require 'monitor'

module Qpid

  class Session < Invoker

    def log; Qpid::logger["qpid.io.cmd"]; end
    def msg; Qpid::logger["qpid.io.msg"]; end


    class Exception < RuntimeError; end
    class Closed < Qpid::Session::Exception; end
    class Detached < Qpid::Session::Exception; end


    INCOMPLETE = Object.new

    def self.client(*args)
      return Qpid::Client(*args)
    end

    def self.server(*args)
      return Server(*args)
    end

    attr_reader :name, :spec, :auto_sync, :timeout, :channel
    attr_reader :results, :exceptions
    attr_accessor :channel, :auto_sync, :send_id, :receiver, :sender

    # FIXME: Pass delegate through a block ?
    def initialize(name, spec, kwargs = {})
      auto_sync = true
      auto_sync = kwargs[:auto_sync] if kwargs.key?(:auto_sync)
      timeout = kwargs[:timeout] || 10
      delegate = kwargs[:delegate]

      @name = name
      @spec = spec
      @auto_sync = auto_sync
      @timeout = timeout
      @invoke_lock = Monitor.new
      @closing = false
      @closed = false

      @cond_lock = Monitor.new
      @condition = @cond_lock.new_cond

      @send_id = true
      @receiver = Receiver.new(self)
      @sender = Sender.new(self)

      @lock = Monitor.new
      @incoming = {}
      @results = {}
      @exceptions = []

      @assembly = nil

      @delegate = delegate.call(self) if delegate

      @ctl_seg = spec[:segment_type].enum[:control].value
      @cmd_seg = spec[:segment_type].enum[:command].value
      @hdr_seg = spec[:segment_type].enum[:header].value
      @body_seg = spec[:segment_type].enum[:body].value
    end

    def incoming(destination)
      @lock.synchronize do
        queue = @incoming[destination]
        unless queue
          queue = Incoming.new(self, destination)
          @incoming[destination] = queue
        end
        return queue
      end
    end

    def error?
      @exceptions.size > 0
    end

    def sync(timeout=nil)
      if channel && Thread.current == channel.connection.thread
        raise Qpid::Session::Exception, "deadlock detected"
      end
      unless @auto_sync
        execution_sync(:sync => true)
      end
      last = @sender.next_id - 1
      @cond_lock.synchronize do
        unless @condition.wait_for(timeout) {
            @sender.completed.include?(last) || error?
          }
          raise Qpid::Timeout
        end
      end
      if error?
        raise Qpid::Session::Exception, @exceptions
      end
    end

    def close(timeout=nil)
      @invoke_lock.synchronize do
        @closing = true
        channel.session_detach(name)
      end
      @cond_lock.synchronize do
        unless @condition.wait_for(timeout) { @closed }
          raise Qpid::Timeout
        end
      end
    end

    def closed
      @lock.synchronize do
        return if @closed

        @results.each { |id, f| f.error(exceptions) }
        @results.clear

        @incoming.values.each { |q| q.close(exceptions) }
        @closed = true
        @cond_lock.synchronize { @condition.signal }
      end
    end

    def resolve_method(name)
      o = @spec.children[name]
      case o
      when Qpid::Spec010::Command
        return invocation(:method, o)
      when Qpid::Spec010::Struct
        return invocation(:method, o)
      when Qpid::Spec010::Domain
        return invocation(:value, o.enum) unless o.enum.nil?
      end

      matches = @spec.children.select { |x|
        x.name.to_s.include?(name.to_s)
      }.collect { |x| x.name.to_s }.sort
      if matches.size == 0
        msg = nil
      elsif matches.size == 1
        msg = "Did you mean #{matches[0]} ? "
      else
        msg =  "Did you mean one of #{matches.join(",")} ? "
      end
      return invocation(:error, msg)
    end

    def invoke(type, args)
      # XXX
      unless type.respond_to?(:track)
        return type.create(*args)
      end
      @invoke_lock.synchronize do
        return do_invoke(type, args)
      end
    end

    def do_invoke(type, args)
      raise Qpid::Session::Closed if @closing
      raise Qpid::Session::Detached unless channel

      # Clumsy simulation of Python's keyword args
      kwargs = {}
      if args.size > 0 && args[-1].is_a?(Hash)
        if args.size > type.fields.size
          kwargs = args.pop
        elsif type.fields[args.size - 1].type != @spec[:map]
          kwargs = args.pop
        end
      end

      if type.payload
        if args.size == type.fields.size + 1
          message = args.pop
        else
          message = kwargs.delete(:message) # XXX Really ?
        end
      else
        message = nil
      end

      hdr = Qpid::struct(@spec[:header])
      hdr.sync = @auto_sync || kwargs.delete(:sync)

      cmd = type.create(*args.push(kwargs))
      sc = Qpid::StringCodec.new(@spec)
      sc.write_command(hdr, cmd)

      seg = Segment.new(true, (message.nil? ||
                               (message.headers.nil? && message.body.nil?)),
                        type.segment_type, type.track, @channel.id, sc.encoded)

      unless type.result.nil?
        result = Future.new(exception=Exception)
        @results[@sender.next_id] = result
      end
      emit(seg)

      log.debug("SENT %s %s %s" % [seg.id, hdr, cmd]) if log

      unless message.nil?
        unless message.headers.nil?
          sc = Qpid::StringCodec.new(@spec)
          message.headers.each { |st| sc.write_struct32(st) }

          seg = Segment.new(false, message.body.nil?, @hdr_seg,
                            type.track, @channel.id, sc.encoded)
          emit(seg)
        end
        unless message.body.nil?
          seg = Segment.new(false, true, @body_seg, type.track,
                            @channel.id, message.body)
          emit(seg)
        end
        msg.debug("SENT %s" % message) if msg
      end

      if !type.result.nil?
        return @auto_sync ? result.get(@timeout) : result
      elsif @auto_sync
        sync(@timeout)
      end
    end

    def received(seg)
      @receiver.received(seg)
      if seg.first_segment?
        raise Qpid::Session::Exception unless @assembly.nil?
        @assembly = []
      end
      @assembly << seg
      if seg.last_segment?
        dispatch(@assembly)
        @assembly = nil
      end
    end

    def dispatch(assembly)
      hdr = nil
      cmd = nil
      header = nil
      body = nil
      assembly.each do |seg|
        d = seg.decode(@spec)
        case seg.type
        when @cmd_seg
          hdr, cmd = d
        when @hdr_seg
          header = d
        when @body_seg
          body = d
        else
          raise Qpid::Session::Exception
        end
      end
      log.debug("RECV %s %s %s" % [cmd.id, hdr, cmd]) if log

      if cmd.st_type.payload
        result = @delegate.send(cmd.st_type.name, cmd, header, body)
      else
        result = @delegate.send(cmd.st_type.name, cmd)
      end

      unless cmd.st_type.result.nil?
        execution_result(cmd.id, result)
      end

      if result != INCOMPLETE
        assembly.each do |seg|
          @receiver.has_completed(seg)
          # XXX: don't forget to obey sync for manual completion as well
          if hdr.sync
            @channel.session_completed(@receiver.completed)
          end
        end
      end
    end

    # Python calls this 'send', but that has a special meaning
    # in Ruby, so we call it 'emit'
    def emit(seg)
      @sender.emit(seg)
    end

    def signal
      @cond_lock.synchronize { @condition.signal }
    end

    def wait_for(timeout = nil, &block)
      @cond_lock.synchronize { @condition.wait_for(timeout, &block) }
    end

    def to_s
      "<Session: #{name}, #{channel}>"
    end

    class Receiver

      attr_reader :completed
      attr_accessor :next_id, :next_offset

      def initialize(session)
        @session = session
        @next_id = nil
        @next_offset = nil
        @completed = Qpid::RangedSet.new()
      end

      def received(seg)
        if @next_id.nil? || @next_offset.nil?
          raise Exception, "todo"
        end
        seg.id = @next_id
        seg.offset = @next_offset
        if seg.last_segment?
          @next_id += 1
          @next_offset = 0
        else
          @next_offset += seg.payload.size
        end
      end

      def has_completed(seg)
        if seg.id.nil?
          raise ArgumentError, "cannot complete unidentified segment"
        end
        if seg.last_segment?
          @completed.add(seg.id)
        end
      end

      def known_completed(commands)
        completed = Qpid::RangedSet.new()
        @completed.ranges.each do |c|
          unless commands.ranges.find { |kc|
              kc.contains(c.lower) && kc.contains(c.upper)
            }
            completed.add_range(c)
          end
        end
        @completed = completed
      end
    end

    class Sender

      def initialize(session)
        @session = session
        @next_id = 0.to_serial
        @next_offset = 0
        @segments = []
        @completed = RangedSet.new()
      end

      attr_reader :next_id, :completed

      def emit(seg)
        seg.id = @next_id
        seg.offset = @next_offset
        if seg.last_segment?
          @next_id += 1
          @next_offset = 0
        else
          @next_offset += seg.payload.size
        end
        @segments << seg
        if @session.send_id
          @session.send_id = false
          @session.channel.session_command_point(seg.id, seg.offset)
        end
        @session.channel.connection.write_segment(seg)
      end

      def has_completed(commands)
        @segments = @segments.reject { |seg| commands.include?(seg.id) }
        commands.ranges.each do |range|
          @completed.add(range.lower, range.upper)
        end
      end
    end

    class Incoming < Qpid::Queue

      def initialize(session, destination)
        super()
        @session = session
        @destination = destination
      end

      def start
        @session.message_credit_unit.choices.each do |unit|
          @session.message_flow(@destination, unit.value, 0xFFFFFFFF)
        end
      end

      def stop
        @session.message_cancel(@destination)
        listen             # Kill the listener
      end
    end

    class Delegate

      def initialize(session)
        @session = session
      end

      #XXX: do something with incoming accepts
      def message_accept(ma) nil; end

      def execution_result(er)
        future = @session.results.delete(er.command_id)
        future.set(er.value)
      end

      def execution_exception(ex)
        @session.exceptions << ex
      end
    end

    class Client < Delegate

      def log ; Qpid::logger["qpid.io.msg"]; end

      def message_transfer(cmd, headers, body)
        m = Qpid::Message.new(body)
        m.headers = headers
        m.id = cmd.id
        messages = @session.incoming(cmd.destination)
        messages.put(m)
        log.debug("RECV %s" % m) if log
        return INCOMPLETE
      end
    end
  end
end
