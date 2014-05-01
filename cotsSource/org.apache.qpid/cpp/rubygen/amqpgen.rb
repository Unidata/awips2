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
# Generic AMQP code generation library.
#
# TODO aconway 2008-02-21:
#
# The amqp_attr_reader and amqp_child_reader for each Amqp* class
# should correspond exactly to ampq.dtd.  Currently they are more
# permissive so we can parse 0-10 preview and 0-10 final XML.
#
# Code marked with "# preview" should be removed/modified when final  0-10
# is complete and we are ready to remove preview-related code.
#

require 'delegate'
require 'rexml/document'
require 'pathname'
require 'set'
include REXML

# Handy String functions for converting names.
class String
  # Convert to CapitalizedForm.
  def caps() gsub( /(^|\W)(\w)/ ) { |m| $2.upcase } end

  # Convert to underbar_separated_form.
  def bars() tr('- .','_'); end

  # Convert to ALL_UPPERCASE_FORM
  def shout() bars.upcase;  end

  # Convert to lowerCaseCapitalizedForm
  def lcaps() gsub( /\W(\w)/ ) { |m| $1.upcase } end

  def plural() self + (/[xs]$/ === self ? 'es' : 's'); end
end

# Sort an array by name.
module Enumerable
  def sort_by_name() sort { |a,b| a.name <=> b.name }; end
end

# Add functions similar to attr_reader for AMQP attributes/children.
# Symbols that are ruby Object function names (e.g. class) get
# an "_" suffix.
class Module
  # Add trailing _ to avoid conflict with Object methods.
  def mangle(sym)
    (Object.method_defined? sym) ? (sym.to_s+"_").intern : sym
  end

  # Add attribute reader for XML attribute.
  def amqp_attr_reader(*attrs)
    attrs.each { |a|
      case a
      when Symbol
        define_method(mangle(a)) {
          @amqp_attr_reader||={ }
          @amqp_attr_reader[a] ||= xml.attributes[a.to_s]
        }
      when Hash
        a.each { |attr, default|
          define_method(mangle(attr)) {
            @amqp_attr_reader||={ }
            value = xml.attributes[attr.to_s]
            if value
              @amqp_attr_reader[attr] ||= value
            else
              @amqp_attr_reader[attr] ||= default
            end
          }
        }
      end
    }
  end

  # Add 2 child readers:
  # elname(name) == child('elname',name)
  # elnames() == children('elname')
  def amqp_child_reader(*element_names)
    element_names.each { |e|
      define_method(mangle(e)) { |name| child(e.to_s, name) }
      define_method(mangle(e.to_s.plural)) { children(e.to_s) } }
  end

  # When there can only be one child instance 
  def amqp_single_child_reader(*element_names)
    element_names.each { |e|
      define_method(mangle(e)) { children(e.to_s)[0] } }
  end
end

# An AmqpElement contains an XML element and provides a convenient
# API to access AMQP data.
# 
# NB: AmqpElements cache values from XML, they assume that
# the XML model does not change after the AmqpElement has
# been created.
class AmqpElement

  def wrap(xml)
    return nil if ["assert","rule"].include? xml.name
    eval("Amqp"+xml.name.caps).new(xml, self) or raise "nil wrapper"
  end

  public
  
  def initialize(xml, parent)
    @xml, @parent=xml, parent
    @children=xml.elements.map { |e| wrap e }.compact
    @cache_child={}
    @cache_child_named={}
    @cache_children={}
    @cache_children[nil]=@children
  end

  attr_reader :parent, :xml, :children, :doc
  amqp_attr_reader :name, :label

  # List of children of type elname, or all children if elname
  # not specified.
  def children(elname=nil)
    if elname
      @cache_children[elname] ||= @children.select { |c| elname==c.xml.name }
    else
      @children
    end
  end

  def each_descendant(&block)
    yield self
    @children.each { |c| c.each_descendant(&block) }
  end

  def collect_all(amqp_type)
    collect=[]
    each_descendant { |d| collect << d if d.is_a? amqp_type }
    collect
  end

  # Look up child of type elname with attribute name.
  def child(elname, name)
    @cache_child[[elname,name]] ||= children(elname).find { |c| c.name==name }
  end

  # Look up any child with name
  def child_named(name)
    @cache_child_named[name] ||= @children.find { |c| c.name==name }
  end
  
  # The root <amqp> element.
  def root() @root ||=parent ? parent.root : self; end

  def to_s() "#<#{self.class}(#{fqname})>"; end
  def inspect() to_s; end

  # Text of doc child if there is one.
  def doc() d=xml.elements["doc"]; d and d.text; end

  def fqname()
    throw "fqname: #{self} #{parent.fqname} has no name" unless name
    p=parent && parent.fqname
    p ? p+"."+name : name;
  end

  def containing_class()
    return self if is_a? AmqpClass
    return parent && parent.containing_class
  end

  # 0-10 array domains are missing element type information, add it here.
  ArrayTypes={
    "str16-array" => "str-16",
    "amqp-host-array" => "connection.amqp-host-url",
    "command-fragments" => "session.command-fragment",
    "in-doubt" => "dtx.xid",
    "tx-publish" => "str-8",
    "queues" => "str-8"
  }

  def array_type(name)
    return  ArrayTypes[name] if ArrayTypes[name]
    raise "Missing ArrayType entry for " + name
  end
  
end

class AmqpResponse < AmqpElement
  def initialize(xml, parent) super; end
  def fqname() (parent ? parent.dotted_name+"." : "") + "response"; end  
end

class AmqpDoc < AmqpElement
  def initialize(xml,parent) super; end
  def text() @xml.text end
end

class AmqpChoice < AmqpElement
  def initialize(xml,parent) super; end
  amqp_attr_reader :name, :value
end

class AmqpEnum < AmqpElement
  def initialize(xml,parent) super; end
  amqp_child_reader :choice
end

class AmqpDomain < AmqpElement
  def initialize(xml, parent)
    super
    root.used_by[uses].push(fqname) if uses and uses.index('.') 
  end
  
  amqp_attr_reader :type
  amqp_single_child_reader :struct # preview
  amqp_single_child_reader :enum

  def uses() type_=="array" ? ArrayTypes[name] : type_; end
end

class AmqpException < AmqpElement
  def initialize(xml, amqp) super; end;
  amqp_attr_reader :error_code
end

class AmqpField < AmqpElement
  def initialize(xml, amqp)
    super;
    root.used_by[type_].push(parent.fqname) if  type_ and type_.index('.')
  end
  amqp_single_child_reader :struct # preview
  amqp_child_reader :exception
  amqp_attr_reader :type, :default, :code, :required
end

class AmqpChassis < AmqpElement # preview
  def initialize(xml, parent) super; end
  amqp_attr_reader :implement
end

class AmqpConstant < AmqpElement
  def initialize(xml, parent) super; end
  amqp_attr_reader :value, :class
end

class AmqpResult < AmqpElement
  def initialize(xml, parent) super; end
  amqp_single_child_reader :struct # preview
  amqp_attr_reader :type
  def name() "result"; end
end

class AmqpEntry < AmqpElement
  def initialize(xml,parent) super; end
  amqp_attr_reader :type
end

class AmqpHeader < AmqpElement
  def initialize(xml,parent) super; end
  amqp_child_reader :entry
  amqp_attr_reader :required
end

class AmqpBody < AmqpElement
  def initialize(xml,parent) super; end
  amqp_attr_reader :required  
end

class AmqpSegments < AmqpElement
  def initialize(xml,parent) super; end
  amqp_child_reader :header, :body
end

class AmqpStruct < AmqpElement
  def initialize(xml, parent) super; end
  amqp_attr_reader :type        # preview
  amqp_attr_reader :size, :code, :pack 
  amqp_child_reader :field

  def result?() parent.xml.name == "result"; end
  def domain?() parent.xml.name == "domain"; end
end

class AmqpMethod < AmqpElement
  def initialize(xml, parent) super; end

  amqp_attr_reader :content, :index, :synchronous
  amqp_child_reader :field, :chassis,:response
  amqp_single_child_reader :result

  def on_chassis?(chassis) child("chassis", chassis); end
  def on_client?() on_chassis? "client"; end
  def on_server?() on_chassis? "server"; end
end

# preview: Map command/control to preview method.
class AmqpFakeMethod < AmqpMethod
  def initialize(action)
    super(action.xml, action.parent);
    @action=action
  end

  def content() return "1" if @action.is_a? AmqpCommand and @action.segments end
  def index() @action.code end
  def code() @action.code end
  def synchronous() end     
  def on_chassis?(chassis)
    @action.received_by?(chassis)
  end
  def pack() "2" end              # Encode  pack=2, size=4 struct
  def size() "4" end
end

class AmqpImplement < AmqpElement
  def initialize(xml,amqp) super; end
  amqp_attr_reader :handle, :send
end

class AmqpRole < AmqpElement
  def initialize(xml,amqp) super; end
  amqp_attr_reader :implement
end

# Base class for command and control.
class AmqpAction < AmqpElement
  def initialize(xml,amqp) super; end
  amqp_child_reader :implement, :field, :response
  amqp_attr_reader :code
  def implement?(role)
    # we can't use xpath for this because it triggers a bug in some
    # versions of ruby, including version 1.8.6.110
    xml.elements.each {|el|
      return true if el.name == "implement" and el.attributes["role"] == role
    }
    return false
  end
  def received_by?(client_or_server)
    return (implement?(client_or_server) or implement?("sender") or implement?("receiver"))
  end
  def pack() "2" end
  def size() "4" end              # Encoded as a size 4 Struct
end

class AmqpControl < AmqpAction
  def initialize(xml,amqp) super; end
end

class AmqpCommand < AmqpAction
  def initialize(xml,amqp) super; end
  amqp_child_reader :exception
  amqp_single_child_reader :result, :segments
end

class AmqpClass < AmqpElement
  def initialize(xml,amqp) super; end

  amqp_attr_reader :index       # preview
  
  amqp_child_reader :struct, :domain, :control, :command, :role, :method
  amqp_attr_reader :code

  def actions() controls+commands;   end

  # preview - command/control as methods
  def methods_()
    return (controls + commands).map { |a| AmqpFakeMethod.new(a) }
  end

  def method(name)
    a = (command(name) or control(name))
    return AmqpFakeMethod.new(a) 
  end

  # chassis should be "client" or "server"
  def methods_on(chassis)       # preview
    @methods_on ||= { }
    @methods_on[chassis] ||= methods_.select { |m| m.on_chassis? chassis }
  end

  # FIXME aconway 2008-04-11: 
  def l4?()                     # preview
    !["connection", "session", "execution"].include?(name) && !control?
  end

  # FIXME aconway 2008-04-11: 
  def control?()
    ["connection", "session"].include?(name)
  end
end

class AmqpType < AmqpElement
  def initialize(xml,amqp) super; end
  amqp_attr_reader :code, :fixed_width, :variable_width
end

class AmqpXref < AmqpElement
  def initialize(xml,amqp) super; end
end

# AMQP root element.
class AmqpRoot < AmqpElement
  amqp_attr_reader :major, :minor, :port, :comment
  amqp_child_reader :doc, :type, :struct, :domain, :constant, :class

  def get_root(x)
    case x
    when Element then x
    when Document then x.root
    else Document.new(x).root
    end
  end

  # Initialize with output directory and spec files from ARGV.
  def initialize(*specs)
    raise "No XML spec files." if specs.empty?
    xml=get_root(specs.shift)
    specs.each { |s| xml_merge(xml, get_root(s)) }
    @used_by=Hash.new{ |h,k| h[k]=[] }
    super(xml, nil)
  end

  attr_reader :used_by
  
  def merge(root) xml_merge(xml, root.xml); end
  
  def version() major + "-" + minor; end

  def methods_() classes.map { |c| c.methods_ }.flatten; end

  #preview
  # Return all methods on chassis for all classes.
  def methods_on(chassis)
    @methods_on ||= { }
    @methods_on[chassis] ||= classes.map { |c| c.methods_on(chassis) }.flatten
  end

  def fqname() nil; end

  private
  
  # Merge contents of elements.
  def xml_merge(to,from)
    from.elements.each { |from_child|
      tag,name = from_child.name, from_child.attributes["name"]
      to_child=to.elements["./#{tag}[@name='#{name}']"]
      to_child ? xml_merge(to_child, from_child) : to.add(from_child.deep_clone) }
  end
end

# Collect information about generated files.
class GenFiles
  @@files = Set.new
  @@public_api = []
  def GenFiles.add(f) @@files.add(f); end
  def GenFiles.get() @@files; end
  def GenFiles.public_api(file) @@public_api << file; end
  def GenFiles.public_api?(file) @@public_api.find { |f| f == file }; end
end

# Base class for code generators.
# Supports setting a per-line prefix, useful for e.g. indenting code.
# 
class Generator
  # Takes directory for output or "-", meaning print file names that
  # would be generated.
  def initialize (outdir, amqp)
    @outdir=outdir[0]
    @apidir=outdir[1]
    @amqp=amqp
    raise "outdir is not an array" unless outdir.class == Array
    @prefix=['']                # For indentation or comments.
    @indentstr='    '           # One indent level.
    @outdent=2
  end

  # Declare next file to be public API
  def public_api(file) GenFiles.public_api(file); end
  
  # Create a new file, set @out. 
  def file(file, &block)
    GenFiles.add(file)
    dir = GenFiles.public_api?(file) ? @apidir : @outdir
    if (dir != "-")         
      @path=Pathname.new "#{dir}/#{file}"
      @path.parent.mkpath
      @out=String.new           # Generate in memory first
      yield if block
      if @path.exist? and @path.read == @out  
        puts "Skipped #{@path} - unchanged" # Dont generate if unchanged
      else
        @path.open('w') { |f| f << @out }
        puts "Generated #{@path}"
      end
    end
  end

  # Append multi-line string to generated code, prefixing each line.
  def gen(str)
    str.each_line { |line|
      @out << @prefix.last unless @midline
      @out << line
      @midline = nil
    }
    # Note if we stopped mid-line
    @midline = /[^\n]\z/ === str
  end

  # Append str + '\n' to generated code.
  def genl(str="") gen str+"\n"; end

  # Generate code with added prefix.
  def prefix(add, &block)
    @prefix.push @prefix.last+add
    if block then yield; endprefix; end
  end

  def endprefix()
    @prefix.pop
  end
  
  # Generate indented code
  def indent(n=1,&block) prefix(@indentstr * n,&block); end
  alias :endindent :endprefix

  # Generate outdented code
  def outdent(&block)
    @prefix.push @prefix.last[0...-2]
    if block then yield; endprefix; end
  end
  alias :endoutdent :endprefix
  
  attr_accessor :out
end

