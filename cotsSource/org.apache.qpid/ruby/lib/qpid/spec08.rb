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

module Qpid08

  module Spec

    include Qpid::Spec

    # XXX: workaround for ruby bug/missfeature
    Reference = Reference

    class Root
      fields(:major, :minor, :classes, :constants, :domains)

      def find_method(name)
        classes.each do |c|
          c.methods.each do |m|
            if name == m.qname
              return m
            end
          end
        end

        return nil
      end
    end

    class Constant
      fields(:name, :id, :type, :docs)
    end

    class Domain
      fields(:name, :type)
    end

    class Class
      fields(:name, :id, :handler, :fields, :methods, :docs)
    end

    class Method
      fields(:name, :id, :content?, :responses, :synchronous?, :fields,
             :docs)

      def init()
        @response = false
      end

      attr :parent, true

      def response?; @response end
      def response=(b); @response = b end

      def qname
        :"#{parent.name}_#{name}"
      end
    end

    class Field
      fields(:name, :id, :type, :docs)

      def default
        case type
        when :bit then false
        when :octet, :short, :long, :longlong then 0
        when :shortstr, :longstr then ""
        when :table then {}
        end
      end
    end

    class Doc
      fields(:type, :text)
    end

    class Container08 < Container
      def do_lookup(key)
        case key
        when Integer
          return find {|x| x.id == key}
        else
          return super(key)
        end
      end
    end

    class Loader08 < Loader

      def container()
        return Container08.new()
      end

      def load_amqp()
        Root.new(attr("major", :int), attr("minor", :int), load("class"),
                 load("constant"), load("domain"))
      end

      def load_class()
        Class.new(attr("name", :name), attr("index", :int), attr("handler", :name),
                  load("field"), load("method"), load("doc"))
      end

      def load_method()
        Method.new(attr("name", :name), attr("index", :int),
                   attr("content", :bool), load("response"),
                   attr("synchronous", :bool), load("field"), load("docs"))
      end

      def load_response()
        name = attr("name", :name)
        Reference.new {|spec, klass|
          response = klass.methods[name]
          if response.nil?
            raise Exception.new("no such method: #{name}")
          end
          response
        }
      end

      def load_field()
        type = attr("type", :name)
        if type.nil?
          domain = attr("domain", :name)
          type = Reference.new {|spec, klass|
            spec.domains[domain].type
          }
        end
        Field.new(attr("name", :name), @index, type, load("docs"))
      end

      def load_constant()
        Constant.new(attr("name", :name), attr("value", :int), attr("class", :name),
                     load("doc"))
      end

      def load_domain()
        Domain.new(attr("name", :name), attr("type", :name))
      end

      def load_doc()
        Doc.new(attr("type", :symbol), text)
      end

    end

    def self.load(spec)
      case spec
      when String
        spec = File.new(spec)
      end
      doc = Document.new(spec)
      spec = Loader08.new().load(doc.root)
      spec.classes.each do |klass|
        klass.traverse! do |o|
          case o
          when Reference
            o.resolve(spec, klass)
          else
            o
          end
        end
        klass.methods.each do |m|
          m.parent = klass
          m.responses.each do |r|
            r.response = true
          end
        end
      end
      return spec
    end
  end
end
