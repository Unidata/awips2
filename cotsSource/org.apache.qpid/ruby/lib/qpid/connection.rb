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

  class ChannelBusy< Exception ; end

  class ChannelsBusy < Exception ; end

  class SessionBusy < Exception ; end

  class ConnectionFailed < Exception ; end

  class Timeout < Exception ; end

  class Connection < Assembler

    include MonitorMixin

    attr_reader :spec, :attached, :sessions, :thread
    attr_accessor :opened, :failed, :close_code, :user_id

    def initialize(sock, args={})
      super(sock)

      delegate = args[:delegate] || Qpid::Delegate::Client.method(:new)
      spec = args[:spec] || nil

      @spec = Qpid::Spec010::load(spec)
      @track = @spec["track"]

      @attached = {}
      @sessions = {}

      @condition = new_cond
      @opened = false
      @failed = false
      @close_code = [nil, "connection aborted"]

      @thread = nil

      @channel_max = 65535
      @user_id = nil

      @delegate = delegate.call(self, args)
    end

    def attach(name, ch, delegate, force=false)
      synchronize do
        ssn = @attached[ch.id]
        if ssn
          raise ChannelBusy.new(ch, ssn) unless ssn.name == name
        else
          ssn = @sessions[name]
          if ssn.nil?
            ssn = Session.new(name, @spec, :delegate => delegate)
            @sessions[name] = ssn
          elsif ssn.channel
            if force
              @attached.delete(ssn.channel.id)
              ssn.channel = nil
            else
              raise SessionBusy.new(ssn)
            end
          end
          @attached[ch.id] = ssn
          ssn.channel = ch
        end
        ch.session = ssn
        return ssn
      end
    end

    def detach(name, ch)
      synchronize do
        @attached.delete(ch.id)
        ssn = @sessions.delete(name)
        if ssn
          ssn.channel = nil
          ssn.closed
          return ssn
        end
      end
    end

    def session(name, kwargs = {})
      timeout = kwargs[:timeout]
      delegate = kwargs[:delegate] || Qpid::Session::Client.method(:new)

      # FIXME: Python has cryptic comment about 'ch 0 ?'
      channel = (0..@channel_max).detect { |i| ! @attached.key?(i) }
      raise ChannelsBusy unless channel

      synchronize do
        ch = Channel.new(self, channel)
        ssn = attach(name, ch, delegate)
        ssn.channel.session_attach(name)
        if ssn.wait_for(timeout) { ssn.channel }
          return ssn
        else
          detach(name, ch)
          raise Timeout
        end
      end
    end

    def detach_all
      synchronize do
        attached.values.each do |ssn|
          ssn.exceptions << @close_code unless @close_code[0] == 200
          detach(ssn.name, ssn.channel)
        end
      end
    end

    def start(timeout=nil)
      @delegate.start
      @thread = Thread.new { run }
      @thread[:name] = 'conn'
      synchronize do
        unless @condition.wait_for(timeout) { @opened || @failed }
          raise Timeout
        end
      end
      if @failed
        raise ConnectionFailed.new(@close_code)
      end
    end

    def run
      # XXX: we don't really have a good way to exit this loop without
      # getting the other end to kill the socket
      loop do
        begin
          seg = read_segment
        rescue Qpid::Closed => e
          detach_all
          break
        end
        @delegate.received(seg)
      end
    end

    def close(timeout=nil)
      return unless @opened
      Channel.new(self, 0).connection_close(200)
      synchronize do
        unless @condition.wait_for(timeout) { ! @opened }
          raise Timeout
        end
      end
      @thread.join(timeout)
      @thread = nil
    end

    def signal
      synchronize { @condition.signal }
    end

    def to_s
      # FIXME: We'd like to report something like HOST:PORT
      return @sock.to_s
    end

    class Channel < Invoker

      attr_reader :id, :connection
      attr_accessor :session

      def initialize(connection, id)
        @connection = connection
        @id = id
        @session = nil
      end

      def resolve_method(name)
        inst = @connection.spec[name]
        if inst.is_a?(Qpid::Spec010::Control)
          return invocation(:method, inst)
        else
          return invocation(:error, nil)
        end
      end

      def invoke(type, args)
        ctl = type.create(*args)
        sc = StringCodec.new(@connection.spec)
        sc.write_control(ctl)
        @connection.write_segment(Segment.new(true, true, type.segment_type,
                                              type.track, self.id, sc.encoded))

        log = Qpid::logger["qpid.io.ctl"]
        log.debug("SENT %s", ctl) if log
      end

      def to_s
        return "#{@connection}[#{@id}]"
      end

    end

  end

end
