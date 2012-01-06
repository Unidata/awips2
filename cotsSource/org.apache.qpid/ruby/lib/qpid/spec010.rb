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

require "qpid/spec"
require 'pathname'
require 'fileutils'

module Qpid::Spec010

  include Qpid::Spec

  # XXX: workaround for ruby bug/missfeature
  Reference = Reference
  Loader = Loader

  class Spec

    ENCODINGS = {
      String => "str16",
      Fixnum => "int64",
      Bignum => "int64",
      Float => "float",
      NilClass => "void",
      Array => "list",
      Hash => "map"
    }

    fields(:major, :minor, :port, :children)

    def init()
      @controls = {}
      @commands = {}
      @structs = {}
      @types = {}
      children.each {|c|
        case c
        when Control
          @controls[c.code] = c
        when Command
          @commands[c.code] = c
        when Struct
          @structs[c.code] = c
        when Type
          @types[c.code] = c unless c.code.nil?
        end
      }
    end

    attr_reader :controls, :commands, :structs, :types

    def [](key)
      return @children[key]
    end

    def encoding(klass)
      if ENCODINGS.has_key?(klass)
        return self[ENCODINGS[klass]]
      end
      for base in klass.__bases__
        result = encoding(base)
        return result unless result.nil?
      end
    end

    def inspect; "spec"; end
  end

  class Constant

    fields(:name, :value)

    attr :parent, true

  end

  class Type

    fields(:name, :code, :fixed, :variable)

    attr :parent, true

    def present?(value)
      if @fixed == 0
        return value
      else
        return !value.nil?
      end
    end

    def encode(codec, value)
      codec.send("write_#{name}", value)
    end

    def decode(codec)
      return codec.send("read_#{name}")
    end

    def inspect; name; end

  end

  class Domain < Type

    fields(:name, :type, :enum)

    attr :parent, true

    def encode(codec, value)
      @type.encode(codec, value)
    end

    def decode(codec)
      return @type.decode(codec)
    end

  end

  class Enum
    fields(:choices)

    def [](choice)
      case choice
      when String
        choice = choice.to_sym
        return choices.find { |c| c.name == choice }
      when Symbol
        return choices.find { |c| c.name == choice }
      else
        return choices.find { |c| c.value == choice }
      end
    end

    def method_missing(name, *args)
      raise ArgumentError.new("wrong number of arguments") unless  args.empty?
      return self[name].value
    end

  end

  class Choice
    fields(:name, :value)
  end

  class Composite

    fields(:name, :code, :size, :pack, :fields)

    attr :parent, true

    # Python calls this 'new', but that has special meaning in Ruby
    def create(*args)
      return Qpid::struct(self, *args)
    end

    def decode(codec)
      codec.read_size(@size)
      codec.read_uint16() unless @code.nil?
      return Qpid::struct(self, self.decode_fields(codec))
    end

    def decode_fields(codec)
      flags = 0
      pack.times {|i| flags |= (codec.read_uint8() << 8*i)}

      result = {}

      fields.each_index {|i|
        f = @fields[i]
        if flags & (0x1 << i) != 0
          result[f.name] = f.type.decode(codec)
        else
          result[f.name] = nil
        end
      }

      return result
    end

    def encode(codec, value)
      sc = Qpid::StringCodec.new(@spec)
      sc.write_uint16(@code) unless @code.nil?
      encode_fields(sc, value)
      codec.write_size(@size, sc.encoded.size)
      codec.write(sc.encoded)
    end

    def encode_fields(codec, values)
      # FIXME: This could be written cleaner using select
      # instead of flags
      flags = 0
      fields.each_index do |i|
        f = fields[i]
        flags |= (0x1 << i) if f.type.present?(values[f.name])
      end

      pack.times { |i| codec.write_uint8((flags >> 8*i) & 0xFF) }

      fields.each_index do |i|
        f = fields[i]
        f.type.encode(codec, values[f.name]) if flags & (0x1 << i) != 0
      end
    end

    def inspect; name; end

  end

  class Field

    fields(:name, :type, :exceptions)

    def default()
      return nil
    end

  end

  class Struct < Composite

    def present?(value)
      return !value.nil?
    end

  end

  class Action < Composite; end

  class Control < Action

    def segment_type
      @parent[:segment_type].enum[:control].value
    end

    def track
      @parent[:track].enum[:control].value
    end

  end

  class Command < Action

    attr_accessor :payload, :result

    def segment_type
      @parent["segment_type"].enum["command"].value
    end

    def track
      @parent["track"].enum["command"].value
    end

  end

  class Doc
    fields(:type, :title, :text)
  end

  class Loader010 < Loader

    def initialize()
      super()
    end

    def klass
      cls = element
      until cls.nil?
        break if cls.name == "class"
        cls = cls.parent
      end
      return cls
    end

    def scope
      if element.name == "struct"
        return nil
      else
        return class_name
      end
    end

    def class_name
      cls = klass
      if cls.nil?
        return nil
      else
        return parse_name(cls.attributes["name"].strip)
      end
    end

    def class_code
      cls = klass
      if cls.nil?
        return 0
      else
        return parse_int(cls.attributes["code"].strip)
      end
    end

    def parse_decl(value)
      name = parse_name(value)

      s = scope
      if s.nil?
        return name
      else
        return :"#{s}_#{name}"
      end
    end

    def parse_code(value)
      c = parse_int(value)
      if c.nil?
        return nil
      else
        return c | (class_code << 8)
      end
    end

    def parse_type(value)
      name = parse_name(value.sub(".", "_"))
      cls = class_name
      return Reference.new {|spec|
        candidates = [name]
        candidates << :"#{cls}_#{name}" unless cls.nil?
        for c in candidates
          child = spec[c]
          break unless child.nil?
        end
        if child.nil?
          raise Exception.new("unresolved type: #{name}")
  else
    child
  end
}
    end

    def load_amqp()
      children = nil

      for s in ["constant", "type", "domain", "struct", "control",
                  "command"]
        ch = load(s)
        if children.nil?
          children = ch
        else
          children += ch
        end
        children += load("class/#{s}")
      end
      children += load("class/command/result/struct")
      Spec.new(attr("major", :int), attr("minor", :int), attr("port", :int),
               children)
    end

    def load_constant()
      Constant.new(attr("name", :decl), attr("value", :int))
    end

    def load_type()
      Type.new(attr("name", :decl), attr("code", :code),
               attr("fixed-width", :int), attr("variable-width", :int))
    end

    def load_domain()
      Domain.new(attr("name", :decl), attr("type", :type), load("enum").first)
    end

    def load_enum()
      Enum.new(load("choice"))
    end

    def load_choice()
      Choice.new(attr("name", :name), attr("value", :int))
    end

    def load_field()
      Field.new(attr("name", :name), attr("type", :type))
    end

    def load_struct()
      Struct.new(attr("name", :decl), attr("code", :code), attr("size", :int),
                 attr("pack", :int), load("field"))
    end

    def load_action(cls)
      cls.new(attr("name", :decl), attr("code", :code), 0, 2, load("field"))
    end

    def load_control()
      load_action(Control)
    end

    def load_command()
      result = attr("type", :type, nil, "result")
      result = attr("name", :type, nil, "result/struct") if result.nil?
      segs = load("segments")
      cmd = load_action(Command)
      cmd.result = result
      cmd.payload = !segs.empty?
      return cmd
    end

    def load_result()
      true
    end

    def load_segments()
      true
    end

  end

  def self.spec_cache(specfile)
      File::join(File::dirname(__FILE__), "spec_cache",
                 File::basename(specfile, ".xml") + ".rb_marshal")
  end

  # XXX: could be shared
  def self.load(spec = nil)
    return spec if spec.is_a?(Qpid::Spec010::Spec)
    if spec.nil?
      # FIXME: Need to add a packaging setup in here so we know where
      # the installed spec is going to be.
      specfile = nil
      if ENV['AMQP_SPEC']
        specfile = ENV['AMQP_SPEC']
      else
        require "qpid/config"
        specfile = Qpid::Config.amqp_spec
      end
    else
      specfile = spec
    end

    specfile_cache = spec_cache(specfile)
    # FIXME: Check that cache is newer than specfile
    if File::exist?(specfile_cache)
      begin
        spec = File::open(specfile_cache, "r") do |f|
          Marshal::load(f)
        end
        return spec
      rescue
        # Ignore, will load from XML
      end
    end

    doc = File::open(specfile, "r") { |f| Document.new(f) }
    spec = Loader010.new().load(doc.root)
    spec.traverse! do |o|
      if o.is_a?(Reference)
        o.resolve(spec)
      else
        o
      end
    end

    spec.children.each { |c| c.parent = spec }

    begin
      FileUtils::mkdir_p(File::dirname(specfile_cache))
      File::open(specfile_cache, "w") { |f| Marshal::dump(spec, f) }
    rescue
      # Ignore, we are fine without the cached spec
    end
    return spec
  end

end
