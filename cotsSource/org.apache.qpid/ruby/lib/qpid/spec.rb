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

require "set"
require "rexml/document"
require "qpid/fields"
require "qpid/traverse"

module Qpid
  module Spec

    include REXML

    class Container < Array

      def initialize()
        @cache = {}
      end

      def [](key)
        return @cache[key] if @cache.include?(key)
        value = do_lookup(key)
        @cache[key] = value
        return value
      end

      def do_lookup(key)
        case key
        when String
          return find {|x| x.name == key.intern()}
        when Symbol
          return find {|x| x.name == key}
        else
          return slice(key)
        end
      end

      def +(other)
        copy = clone()
        copy.concat(other)
        return copy
      end

    end

    class Reference

      fields(:name)

      def init(&block)
        @resolver = block
      end

      def resolve(*args)
        @resolver.call(*args)
      end

    end

    class Loader

      def initialize()
        @stack = []
      end

      def container()
        return Container.new()
      end

      def load(obj)
        case obj
        when String
          elem = @stack[-1]
          result = container()
          elem.elements.each(obj) {|e|
            @index = result.size
            result << load(e)
          }
          @index = nil
          return result
        else
          elem = obj
          @stack << elem
          begin
            result = send(:"load_#{elem.name}")
          ensure
            @stack.pop()
          end
          return result
        end
      end

      def element
        @stack[-1]
      end

      def text
        element.text
      end

      def attr(name, type = :string, default = nil, path = nil)
        if path.nil?
          elem = element
        else
          elem = nil
          element.elements.each(path) {|elem|}
          if elem.nil?
            return default
          end
        end

        value = elem.attributes[name]
        value = value.strip() unless value.nil?
        if value.nil?
          default
        else
          send(:"parse_#{type}", value)
        end
      end

      def parse_int(value)
        if value.nil?
          return nil
        else
          value.to_i(0)
        end
      end

      TRUE = ["yes", "true", "1"].to_set
      FALSE = ["no", "false", "0", nil].to_set

      def parse_bool(value)
        if TRUE.include?(value)
          true
        elsif FALSE.include?(value)
          false
        else
          raise Exception.new("parse error, expecting boolean: #{value}")
        end
      end

      def parse_string(value)
        value.to_s
      end

      def parse_symbol(value)
        value.intern() unless value.nil?
      end

      REPLACE = {" " => "_", "-" => "_"}
      KEYWORDS = {"global" => "global_", "return" => "return_"}

      def parse_name(value)
        return if value.nil?

        REPLACE.each do |k, v|
          value = value.gsub(k, v)
        end

        value = KEYWORDS[value] if KEYWORDS.has_key? value
        return value.intern()
      end

    end

  end
end
