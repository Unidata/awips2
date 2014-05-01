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

  def self.struct(type, *args)
    # FIXME: This is fragile; the last arg could be a hash,
    # without being hte keywords
    kwargs = {}
    kwargs = args.pop if args.any? && args[-1].is_a?(Hash)

    if args.size > type.fields.size
      raise TypeError,
      "%s() takes at most %d arguments (%d given)" %
        [type.name, type.fields.size, args.size]
    end

    attrs = type.fields.inject({}) do |attrs, field|
      if args.any?
        attrs[field.name] = args.shift
        if kwargs.key?(field.name)
          raise TypeError,
          "%s() got multiple values for keyword argument '%s'" %
            [type.name, field.name]
        end
      elsif kwargs.key?(field.name)
        attrs[field.name] = kwargs.delete(field.name)
      else
        attrs[field.name] = field.default
      end
      attrs
    end

    unless kwargs.empty?
      unexpected = kwargs.keys[0]
      raise TypeError,
      "%s() got an unexpected keyword argument '%s'" %
        [type.name, unexpected]
    end

    attrs[:st_type] = type
    attrs[:id] = nil

    name = "Qpid_" + type.name.to_s.capitalize
    unless ::Struct.const_defined?(name)
      vars = type.fields.collect { |f| f.name } << :st_type << :id
      ::Struct.new(name, *vars)
    end
    st = ::Struct.const_get(name)

    result = st.new
    attrs.each { |k, v| result[k] = v }
    return result
  end

  class Message

    attr_accessor :headers, :body, :id

    def initialize(*args)
      @body = nil
      @headers = nil

      @body = args.pop unless args.empty?
      @headers = args unless args.empty?

      @id = nil
    end

    def has(name)
      return ! get(name).nil?
    end

    def get(name)
      if @headers
        name = name.to_sym
        @headers.find { |h| h.st_type.name == name }
      end
    end

    def set(header)
      @headers ||= []
      if h = @headers.find { |h| h.st_type == header.st_type }
        ind = @headers.index(h)
        @headers[ind] = header
      else
        @headers << header
      end
    end

    def clear(name)
      if @headers
        name = name.to_sym
        @headers.delete_if { |h| h.st_type.name == name }
      end
    end

    # FIXME: Not sure what to do here
    # Ruby doesn't have a notion of a evaluable string representation
    # def __repr__(self):
    #     args = []
    #     if self.headers:
    #       args.extend(map(repr, self.headers))
    #     if self.body:
    #       args.append(repr(self.body))
    #     if self.id is not None:
    #       args.append("id=%s" % self.id)
    #     return "Message(%s)" % ", ".join(args)
    #   end
  end

  class ::Object

    def to_serial
      Qpid::Serial.new(self)
    end
  end

  class Serial

    include Comparable

    attr_accessor :value

    def initialize(value)
      @value = value & 0xFFFFFFFF
    end

    def hash
      @value.hash
    end

    def to_serial
      self
    end

    def eql?(other)
      other = other.to_serial
      value.eql?(other.value)
    end

    def <=>(other)
      return 1 if other.nil?

      other = other.to_serial

      delta = (value - other.value) & 0xFFFFFFFF
      neg = delta & 0x80000000
      mag = delta & 0x7FFFFFFF

      return (neg>0) ? -mag : mag
    end

    def +(other)
      result = other.to_serial
      result.value += value
      return result
    end

    def -(other)
      result = other.to_serial
      result.value = value - result.value
      return result
    end

    def succ
      Serial.new(value + 1)
    end

    # FIXME: Not sure what to do here
    # Ruby doesn't have a notion of a evaluable string representation
    # def __repr__(self):
    #         return "serial(%s)" % self.value
    # end

    def to_s
      value.to_s
    end

  end

  # The Python class datatypes.Range is emulated by the standard
  # Range class with a few additions
  class ::Range

    alias :lower :begin
    alias :upper :end

    def touches(r)
      # XXX: are we doing more checks than we need?
      return (r.include?(lower - 1) ||
              r.include?(upper + 1) ||
              include?(r.lower - 1) ||
              include?(r.upper + 1) ||
              r.include?(lower)     ||
              r.include?(upper)     ||
              include?(r.lower)     ||
              include?(r.upper))
    end

    def span(r)
      Range.new([lower, r.lower].min, [upper, r.upper].max)
    end

    def intersect(r)
      l = [lower, r.lower].max
      u = [upper, r.upper].min
      return l > u ? nil : Range.new(l, u)
    end

  end

  class RangedSet

    include Enumerable

    attr_accessor :ranges

    def initialize(*args)
      @ranges = []
      args.each { |n| add(n) }
    end

    def each(&block)
      ranges.each { |r| yield(r) }
    end

    def include?(n)
      if (n.is_a?(Range))
        super(n)
      else
        ranges.find { |r| r.include?(n) }
      end
    end

    def add_range(range)
      ranges.delete_if do |r|
        if range.touches(r)
          range = range.span(r)
          true
        else
          false
        end
      end
      ranges << range
    end

    def add(lower, upper = nil)
      upper = lower if upper.nil?
      add_range(Range.new(lower, upper))
    end

    def to_s
      repr = ranges.sort { |a,b| b.lower <=> a.lower }.
        map { |r| r.to_s }.join(",")
      "<RangedSet: {#{repr}}"
    end
  end

  class Future
    def initialize(initial=nil, exception=Exception)
      @value = initial
      @error = nil
      @set = Util::Event.new
      @exception = exception
    end

    def error(error)
      @error = error
      @set.set
    end

    def set(value)
      @value = value
      @set.set
    end

    def get(timeout=nil)
      @set.wait(timeout)
      unless @error.nil?
        raise @exception.new(@error)
      end
      @value
    end
  end

  class UUID
    include Comparable

    attr_accessor :bytes

    def initialize(bytes)
      @bytes = bytes
    end

    def <=>(other)
      if other.respond_to?(:bytes)
        return bytes <=> other.bytes
      else
        raise NotImplementedError
      end
    end

    def to_s
      UUID::format(bytes)
    end

    # FIXME: Not sure what to do here
    # Ruby doesn't have a notion of a evaluable string representation
    #   def __repr__(self):
    #     return "UUID(%r)" % str(self)
    #   end

    def self.random_uuid
      bytes = (1..16).collect { |i| rand(256) }

      # From RFC4122, the version bits are set to 0100
      bytes[7] &= 0x0F
      bytes[7] |= 0x40

      # From RFC4122, the top two bits of byte 8 get set to 01
      bytes[8] &= 0x3F
      bytes[8] |= 0x80
      return bytes.pack("C16")
    end

    def self.uuid4
      UUID.new(random_uuid)
    end

    def self.format(s)
      # Python format !LHHHHL
      # big-endian, ulong, ushort x 4, ulong
      "%08x-%04x-%04x-%04x-%04x%08x" % s.unpack("NnnnnN")
    end
  end
end
