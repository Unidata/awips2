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

require 'qpid/packer.rb'
require 'iconv'

module Qpid

  class Codec

    include Qpid::Packer

    attr_reader :spec
    
    def initialize(spec = "")
      @spec = spec
    end

    def write_void(v)
      unless v.nil?
        raise Exception.new("void not nil: #{v}")
      end
    end

    def read_void
      return nil
    end

    def write_bit(b)
      unless b
        raise Exception.new("bit is nil: #{b}")
      end
    end

    def read_bit
      return true
    end

    def read_uint8
      return unpack("C", 1)
    end

    def write_uint8(n)
      return pack("C", n)
    end

    def read_int8
      return unpack("c", 1)
    end

    def write_int8(n)
      pack("c", n)
    end

    def read_char
      return unpack("c", 1)
    end

    def write_char(c)
      pack("c")
    end

    def read_boolean
      return read_uint8 != 0
    end

    def write_boolean(b)
      n = 0
      n = 1 if b != 0
      write_uint8(n)
    end

    def read_uint16
      return unpack("n", 2)
    end

    def write_uint16(n)
      pack("n", n)
    end

    def read_int16
      # XXX: holy moly.. pack/unpack doesn't have signed network byte order.  Crazy hackery.
      val = unpack("n", 2)
      val -= 2 ** 16  if val >= 2 ** 15
      return val
    end

    def write_int16(n)
      # XXX: Magically this one works even though it's not signed.
      pack("n", n)
    end

    def read_uint32
      return unpack("N", 4)
    end

    def write_uint32(n)
      pack("N", n)
    end

    def read_int32
      # Again no pack/unpack for signed int
      return unpack("N", 4)
    end

    def write_int32(n)
      # FIXME
      pack("N", n)
    end

    def read_float
      return unpack("g", 4)
    end

    def write_float(n)
      pack("g", n)
    end

    def read_sequence_no
      return read_uint32.to_serial
    end

    def write_sequence_no(n)
      write_uint32(n.value)
    end

    def encode_64bit(num, signed = false)
      b = []

      if num < 0 && signed
        num += 2 ** 64
      end

      (0..7).each do |c|
        d = 7 - c
        b[c] = (num & (0xff << d * 8)) >> d * 8
      end
      pack('C8', *b)
    end


    def decode_64bit(signed = false)
      # Silly ruby pack/unpack does not implement 64 bit network byte order
      # encode/decode.
      a = unpack('C8', 8)
      num = 0
      (0..7).each do |c|
        d = 7 - c
        num |= a[c] << 8 * d
      end

      if signed && num >= 2 ** 63
        num -= 2 ** 64
      end
      return num
    end

    def read_uint64
      return decode_64bit
    end

    def write_uint64(n)
      encode_64bit(n)
    end

    def read_int64
      return decode_64bit(signed = true)
    end

    def write_int64(n)
      encode_64bit(n, signed = true)
    end

    def read_datetime
      return read_uint64
    end

    def write_datetime(n)
      write_uint64(n)
    end

    def read_double
      return unpack("G", 8)
    end

    def write_double(n)
      pack("G", n)
    end

    def read_vbin8
      # XXX
      return read(read_uint8)
    end

    def write_vbin8(b)
      # XXX
      write_uint8(b.length)
      write(b)
    end

    def read_str8
      # FIXME: Check iconv.. I think this will throw if there are odd characters.
      return Iconv.conv("ASCII", "UTF-8", read_vbin8)
    end

    def write_str8(s)
      write_vbin8(Iconv.conv("UTF-8", "ASCII", s))
    end

    def read_str16
      return Iconv.conv("ASCII", "UTF-8", read_vbin16)
    end

    def write_str16(s)
      write_vbin16(Iconv.conv("UTF-8", "ASCII", s))
    end

    def read_vbin16
      # XXX: Using read method?
      return read(read_uint16)
    end

    def write_vbin16(b)
      write_uint16(b.length)
      write(b)
    end

    def read_sequence_set
      # FIXME: Need datatypes
      result = RangedSet.new
      size = read_uint16
      nranges = size / 8
      nranges.times do |i|
        lower = read_sequence_no
        upper = read_sequence_no
        result.add(lower, upper)
      end
      return result
    end

    def write_sequence_set(ss)
      size = 8 * ss.ranges.length
      write_uint16(size)
      ss.ranges.each do |range|
        write_sequence_no(range.lower)
        write_sequence_no(range.upper)
      end
    end

    def read_vbin32
      return read(read_uint32)
    end

    def write_vbin32(b)
      write_uint32(b.length)
      write(b)
    end

    def write_map(m)
      sc = StringCodec.new(@spec)
      unless m.nil?
        sc.write_uint32(m.size)
        m.each do |k, v|
          unless type = @spec.encoding(v.class)
            raise Exception.new("no encoding for: #{v.class}")
          end
          sc.write_str8(k)
          sc.write_uint8(type.code)
          type.encode(sc, v)
        end
      end
      write_vbin32(sc.encoded)
    end

    def read_map
      sc = StringCodec.new(@spec, read_vbin32)
      return nil unless sc.encoded
      count = sc.read_uint32
      result = nil
      if count
        result = {}
        until sc.encoded.empty?
          k = sc.read_str8
          code = sc.read_uint8
          type = @spec.types[code]
          v = type.decode(sc)
          result[k] = v
        end
      end
      return result
    end

    def write_array(a)
      sc = StringCodec.new(@spec)
      unless a.nil?
        if a.length > 0
          type = @spec.encoding(a[0].class)
        else
          type = @spec.encoding(nil.class)
        end
        sc.write_uint8(type.code)
        sc.write_uint32(a.size)
        a.each { |o| type.encode(sc, o) }
      end
      write_vbin32(sc.encoded)
    end

    def read_array
      sc = StringCodec.new(@spec, read_vbin32)
      return nil if not sc.encoded
      type = @spec.types[sc.read_uint8]
      count = sc.read_uint32
      result = nil
      if count
        result = []
        count.times { |i| result << (type.decode(sc)) }
      end
      return result
    end

    def write_list(l)
      sc = StringCodec.new(@spec)
      unless l.nil?
        sc.write_uint32(l.length)
        l.each do |o|
          type = @spec.encoding(o.class)
          sc.write_uint8(type.code)
          type.encode(sc, o)
        end
      end
      write_vbin32(sc.encoded)
    end

    def read_list
      sc = StringCodec.new(@spec, read_vbin32)
      return nil if not sc.encoded
      count = sc.read_uint32
      result = nil
      if count
        result = []
        count.times do |i|
          type = @spec.types[sc.read_uint8]
          result << type.decode(sc)
        end
      end
      return result
    end

    def read_struct32
      size = read_uint32
      code = read_uint16
      type = @spec.structs[code]
      # XXX: BLEH!
      fields = type.decode_fields(self)
      return Qpid::struct(type, fields)
    end

    def write_struct32(value)
      type = value.st_type
      sc = StringCodec.new(@spec)
      sc.write_uint16(type.code)
      type.encode_fields(sc, value)
      write_vbin32(sc.encoded)
    end

    def read_control
      cntrl = @spec.controls[read_uint16]
      return Qpid::struct(cntrl, cntrl.decode_fields(self))
    end

    def write_control(ctrl)
      type = ctrl.st_type
      write_uint16(type.code)
      type.encode_fields(self, ctrl)
    end

    def read_command
      type = @spec.commands[read_uint16]
      hdr = @spec[:header].decode(self)
      cmd = Qpid::struct(type, type.decode_fields(self))
      return hdr, cmd
    end

    def write_command(hdr, cmd)
      type = cmd.st_type
      write_uint16(type.code)
      hdr.st_type.encode(self, hdr)
      type.encode_fields(self, cmd)
    end

    def read_size(width)
      if width > 0
        return send(:"read_uint#{width * 8}")
      end
    end

    def write_size(width, n)
      if width > 0
        send(:"write_uint#{width * 8}", n)
      end
    end

    def read_uuid
      return unpack("a16", 16)
    end

    def write_uuid(s)
      pack("a16", s)
    end

    def read_bin128
      return unpack("a16", 16)
    end

    def write_bin128(b)
      pack("a16", b)
    end

  end

  class StringCodec < Codec

    def initialize(spec, encoded = "")
      @spec = spec
      @encoded = encoded
    end

    attr_reader :encoded

    def write(s)
      @encoded += s
    end

    def read(n)
      return "" if n.nil?
      result = @encoded[0...n]
      @encoded = @encoded[n...@encoded.size] || ""
      return result
    end
  end
end
