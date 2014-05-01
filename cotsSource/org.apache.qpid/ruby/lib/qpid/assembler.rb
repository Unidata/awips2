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

  class << self
    attr_accessor :asm_logger
  end

  class Segment

    attr_reader :type, :payload, :track, :channel
    attr_accessor :id, :offset

    def initialize(first, last, type, track, channel, payload)
      @id = nil
      @offset = nil
      @first = first
      @last = last
      @type = type
      @track = track
      @channel = channel
      @payload = payload
    end

    def first_segment? ; @first ; end

    def last_segment? ; @last ; end

    def decode(spec)
      segs = spec[:segment_type]
      choice = segs.enum.choices[type]
      return method("decode_#{choice.name}").call(spec)
    end

    def decode_control(spec)
      sc = StringCodec.new(spec, payload)
      return sc.read_control()
    end

    def decode_command(spec)
      sc = StringCodec.new(spec, payload)
      hdr, cmd = sc.read_command()
      cmd.id = id
      return hdr, cmd
    end

    def decode_header(spec)
      sc = StringCodec.new(spec, payload)
      values = []
      until sc.encoded.empty?
        values << sc.read_struct32()
      end
      return values
    end

    def decode_body(spec)
      payload
    end

    def append(frame)
      @payload += frame.payload
    end

    def to_s
      f = first_segment? ? 'F' : '.'
      l = last_segment? ? 'L' : '.'
      return "%s%s %s %s %s %s" % [f, l, @type,
                                   @track, @channel, @payload.inspect]
    end

  end

  class Assembler < Framer

    def logger; Qpid::asm_logger; end

    def initialize(sock, max_payload = Frame::MAX_PAYLOAD)
      super(sock)
      @max_payload = max_payload
      @fragments = {}
    end

    def read_segment
      loop do
        frame = read_frame
        key = [frame.channel, frame.track]
        seg = @fragments[key]
        unless seg
          seg = Segment.new(frame.first_segment?,
                            frame.last_segment?,
                            frame.type, frame.track,
                            frame.channel, "")
          @fragments[key] = seg
        end

        seg.append(frame)

        if frame.last_frame?
          @fragments.delete(key)
          logger.debug("RECV #{seg}") if logger
          return seg
        end
      end
    end

    def write_segment(segment)
      remaining = segment.payload

      first = true
      while first or remaining
        payload = remaining[0, @max_payload]
        remaining = remaining[@max_payload, remaining.size]

        flags = 0

        flags |= FIRST_FRM if first
        flags |= LAST_FRM  unless remaining
        flags |= FIRST_SEG if segment.first_segment?
        flags |= LAST_SEG  if segment.last_segment?

        frame = Frame.new(flags, segment.type, segment.track,
                          segment.channel, payload)
        write_frame(frame)

        first = false
      end

      logger.debug("SENT #{segment}") if logger
    end
  end
end
