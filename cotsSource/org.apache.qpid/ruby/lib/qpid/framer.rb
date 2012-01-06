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
require 'logger'
require 'sasl'

module Qpid

  FIRST_SEG = 0x08
  LAST_SEG = 0x04
  FIRST_FRM = 0x02
  LAST_FRM = 0x01

  class << self
    attr_accessor :raw_logger, :frm_logger
  end

  def self.packed_size(format)
    # FIXME: This is a total copout to simulate Python's
    # struct.calcsize
    ([0]*256).pack(format).size
  end

  class Frame
    attr_reader :payload, :track, :flags, :type, :channel

    # HEADER = "!2BHxBH4x"
    # Python  Meaning         Ruby
    # !       big endian      (implied by format char)
    # 2B      2 uchar         C2
    # H       unsigned short  n
    # x       pad byte        x
    # B       uchar           C
    # H       unsigned short  n
    # 4x      pad byte        x4
    HEADER = "C2nxCnx4"
    HEADER_SIZE = Qpid::packed_size(HEADER)
    MAX_PAYLOAD = 65535 - HEADER_SIZE

    def initialize(flags, type, track, channel, payload)
      if payload.size > MAX_PAYLOAD
        raise ArgumentError, "max payload size exceeded: #{payload.size}"
      end

      @flags = flags
      @type = type
      @track = track
      @channel = channel
      @payload = payload
    end

    def first_segment? ; FIRST_SEG & @flags > 0 ; end

    def last_segment? ; LAST_SEG & @flags > 0 ; end

    def first_frame? ; FIRST_FRM & @flags > 0 ; end

    def last_frame? ; LAST_FRM & @flags > 0 ; end

    def to_s
      fs = first_segment? ? 'S' : '.'
      ls = last_segment? ? 's' : '.'
      ff = first_frame? ? 'F' : '.'
      lf = last_frame? ? 'f' : '.'

      return "%s%s%s%s %s %s %s %s" % [fs, ls, ff, lf,
                                       @type,
                                       @track,
                                       @channel,
                                       @payload.inspect]
    end
  end

  class FramingError < Exception ; end

  class Closed < Exception ; end

  class Framer
    include Packer

    # Python: "!4s4B"
    HEADER = "a4C4"
    HEADER_SIZE = 8

    def raw
      Qpid::raw_logger
    end

    def frm
      Qpid::frm_logger
    end

    def initialize(sock)
      @sock = sock
      @sock.extend(MonitorMixin)
      @tx_buf = ""
      @rx_buf = ""
      @security_layer_tx = nil
      @security_layer_rx = nil
      @maxbufsize = 65535
    end

    attr_reader :sock
    attr_accessor :security_layer_tx, :security_layer_rx

    def aborted? ; false ; end

    def write(buf)
      @tx_buf += buf
    end

    def flush
      @sock.synchronize do
        if @security_layer_tx
          cipher_buf = Sasl.encode(@security_layer_tx, @tx_buf)
          _write(cipher_buf)
        else
          _write(@tx_buf)
        end
        @tx_buf = ""
        frm.debug("FLUSHED") if frm
      end
    rescue
      @sock.close unless @sock.closed?
    end

    def _write(buf)
      while buf && buf.size > 0
        # FIXME: Catch errors
        n = @sock.write(buf)
        raw.debug("SENT #{buf[0, n].inspect}") if raw
        buf[0,n] = ""
        @sock.flush
      end
    end

    def read(n)
      while @rx_buf.size < n
        begin
          s = @sock.recv(@maxbufsize)
          if @security_layer_rx
            s = Sasl.decode(@security_layer_rx, s)
          end
        rescue IOError => e
          raise e if @rx_buf != ""
          @sock.close unless @sock.closed?
          raise Closed
        end
        # FIXME: Catch errors
        if s.nil? or s.size == 0
          @sock.close unless @sock.closed?
          raise Closed
        end
        @rx_buf += s
        raw.debug("RECV #{n}/#{@rx_buf.size} #{s.inspect}") if raw
      end
      data = @rx_buf[0, n]
      @rx_buf = @rx_buf[n, @rx_buf.size - n]
      return data
    end

    def read_header
      unpack(Framer::HEADER, Framer::HEADER_SIZE)
    end

    def write_header(major, minor)
      @sock.synchronize do
        pack(Framer::HEADER, "AMQP", 1, 1, major, minor)
        flush()
      end
    end

    def write_frame(frame)
      @sock.synchronize do
        size = frame.payload.size + Frame::HEADER_SIZE
        track = frame.track & 0x0F
        pack(Frame::HEADER, frame.flags, frame.type, size, track, frame.channel)
        write(frame.payload)
        if frame.last_segment? and frame.last_frame?
          flush()
          frm.debug("SENT #{frame}") if frm
        end
      end
    end

    def read_frame
      flags, type, size, track, channel = unpack(Frame::HEADER, Frame::HEADER_SIZE)
      raise FramingError if (flags & 0xF0 > 0)
      payload = read(size - Frame::HEADER_SIZE)
      frame = Frame.new(flags, type, track, channel, payload)
      frm.debug("RECV #{frame}") if frm
      return frame
    end
  end
end
