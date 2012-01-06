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

require 'rbconfig'
require 'sasl'

module Qpid

  class Delegate

    def initialize(connection, args={})
      @connection = connection
      @spec = connection.spec
      @delegate = args[:delegate] || Qpid::Delegate::Client.method(:new)
      @control = @spec[:track].enum[:control].value
    end

    def log ; Qpid::logger["qpid.io.ctl"]; end

    def received(seg)
      ssn = @connection.attached[seg.channel]
      unless ssn
        ch = Qpid::Connection::Channel.new(@connection, seg.channel)
      else
        ch = ssn.channel
      end

      if seg.track == @control
        ctl = seg.decode(@spec)
        log.debug("RECV %s", ctl) if log
        attr = ctl.st_type.name
        method(attr).call(ch, ctl)
      elsif ssn.nil?
        ch.session_detached
      else
        ssn.received(seg)
      end
    end

    def connection_close(ch, close)
      @connection.close_code = [close.reply_code, close.reply_text]
      ch.connection_close_ok
      @connection.sock.close_write()
      unless @connection.opened
        @connection.failed = true
        @connection.signal
      end
    end

    def connection_close_ok(ch, close_ok)
      @connection.opened = false
      @connection.signal
    end

    def session_attach(ch, a)
      begin
        @connection.attach(a.name, ch, @delegate, a.force)
        ch.session_attached(a.name)
      rescue Qpid::ChannelBusy
        ch.session_detached(a.name)
      rescue Qpid::SessionBusy
        ch.session_detached(a.name)
      end
    end

    def session_attached(ch, a)
      ch.session.signal
    end

    def session_detach(ch, d)
      #send back the confirmation of detachment before removing the
      #channel from the attached set; this avoids needing to hold the
      #connection lock during the sending of this control and ensures
      #that if the channel is immediately reused for a new session the
      #attach request will follow the detached notification.
      ch.session_detached(d.name)
      ssn = @connection.detach(d.name, ch)
    end

    def session_detached(ch, d)
      @connection.detach(d.name, ch)
    end

    def session_request_timeout(ch, rt)
      ch.session_timeout(rt.timeout)
    end

    def session_command_point(ch, cp)
      ssn = ch.session
      ssn.receiver.next_id = cp.command_id
      ssn.receiver.next_offset = cp.command_offset
    end

    def session_completed(ch, cmp)
      ch.session.sender.has_completed(cmp.commands)
      if cmp.timely_reply
        ch.session_known_completed(cmp.commands)
      end
      ch.session.signal
    end

    def session_known_completed(ch, kn_cmp)
      ch.session.receiver.known_completed(kn_cmp.commands)
    end

    def session_flush(ch, f)
      rcv = ch.session.receiver
      if f.expected
        if rcv.next_id
          exp = Qpid::RangedSet.new(rcv.next_id)
        else
          exp = nil
        end
        ch.session_expected(exp)
      end
      if f.confirmed
        ch.session_confirmed(rcv.completed)
      end
      if f.completed
        ch.session_completed(rcv.completed)
      end
    end

    class Server < Delegate

      def start
        @connection.read_header()
        @connection.write_header(@spec.major, @spec.minor)
        ch = Qpid::Connection::Channel.new(@connection, 0)
        ch.connection_start(:mechanisms => ["ANONYMOUS"])
        ch
      end

      def connection_start_ok(ch, start_ok)
        ch.connection_tune(:channel_max => 65535)
      end

      def connection_tune_ok(ch, tune_ok)
        nil
      end

      def connection_open(ch, open)
        @connection.opened = true
        ch.connection_open_ok()
        @connection.signal
      end
    end

    class Client < Delegate

      # FIXME: Python uses os.name for platform - we don't have an exact
      # analog in Ruby
      PROPERTIES = {"product"  => "qpid python client",
        "version"  => "development",
        "platform" => Config::CONFIG["build_os"],
        "qpid.client_process" => File.basename($0),
        "qpid.client_pid" => Process.pid,
        "qpid.client_ppid" => Process.ppid}


      def initialize(connection, args)
        super(connection)

        result = Sasl::client_init

        @mechanism= args[:mechanism]
        @username = args[:username]
        @password = args[:password]
        @service = args[:service] || "qpidd"
        @min_ssf = args[:min_ssf] || 0
        @max_ssf = args[:max_ssf] || 65535

        @saslConn = Sasl.client_new(@mechanism, @service, args[:host],
                                    @username, @password, @min_ssf, @max_ssf)
      end

      def start
        @connection.write_header(@spec.major, @spec.minor)
        @connection.read_header
      end

      def connection_start(ch, start)
        mech_list = ""
        start.mechanisms.each do |m|
          mech_list += m + " "
        end
        begin
          resp = Sasl.client_start(@saslConn, mech_list)
          @connection.user_id = Sasl.user_id(@saslConn)
          ch.connection_start_ok(:client_properties => PROPERTIES,
                                 :mechanism => resp[2],
                                 :response => resp[1])
        rescue exception
          ch.connection_close(:message => $!.message)
          @connection.failed = true
          @connection.signal
        end
      end

      def connection_secure(ch, secure)
        resp = Sasl.client_step(@saslConn, secure.challenge)
        @connection.user_id = Sasl.user_id(@saslConn)
        ch.connection_secure_ok(:response => resp[1])
      end

      def connection_tune(ch, tune)
        ch.connection_tune_ok(:channel_max => tune.channel_max,
                              :max_frame_size => tune.max_frame_size,
                              :heartbeat => 0)
        ch.connection_open()
        @connection.security_layer_tx = @saslConn
      end

      def connection_open_ok(ch, open_ok)
        @connection.security_layer_rx = @saslConn
        @connection.opened = true
        @connection.signal
      end
    end
  end
end
