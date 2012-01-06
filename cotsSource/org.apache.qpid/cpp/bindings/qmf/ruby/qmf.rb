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

require 'qmfengine'
require 'thread'
require 'socket'
require 'monitor'

module Qmf

  # Pull all the TYPE_* constants into Qmf namespace.  Maybe there's an easier way?
  Qmfengine.constants.each do |c|
    if c.index('TYPE_') == 0 or c.index('ACCESS_') == 0 or c.index('DIR_') == 0 or c.index('CLASS_') == 0
      const_set(c, Qmfengine.const_get(c))
    end
  end

  ##==============================================================================
  ## CONNECTION
  ##==============================================================================

  class ConnectionSettings
    attr_reader :impl

    def initialize(url = nil)
      if url
        @impl = Qmfengine::ConnectionSettings.new(url)
      else
        @impl = Qmfengine::ConnectionSettings.new()
      end
    end

    def set_attr(key, val)
      if val.class == String
        v = Qmfengine::Value.new(TYPE_LSTR)
        v.setString(val)
      elsif val.class == TrueClass or val.class == FalseClass
        v = Qmfengine::Value.new(TYPE_BOOL)
        v.setBool(val)
      elsif val.class == Fixnum
        v = Qmfengine::Value.new(TYPE_UINT32)
        v.setUint(val)
      else
        raise ArgumentError, "Value for attribute '#{key}' has unsupported type: #{val.class}"
      end

      good = @impl.setAttr(key, v)
      raise "Invalid attribute '#{key}'" unless good
    end

    def method_missing(name_in, *args)
      name = name_in.to_s
      if name[name.length - 1] == 61
        attr = name[0..name.length - 2]
        set_attr(attr, args[0])
        return
      end

      super.method_missing(name_in, args)
    end
  end

  class ConnectionHandler
    def conn_event_connected(); end
    def conn_event_disconnected(error); end
    def conn_event_visit(); end
    def sess_event_session_closed(context, error); end
    def sess_event_recv(context, message); end
  end

  class Connection
    include MonitorMixin

    attr_reader :impl

    def initialize(settings)
      super()
      @impl = Qmfengine::ResilientConnection.new(settings.impl)
      @sockEngine, @sock = Socket::socketpair(Socket::PF_UNIX, Socket::SOCK_STREAM, 0)
      @impl.setNotifyFd(@sockEngine.fileno)
      @new_conn_handlers = []
      @conn_handlers_to_delete = []
      @conn_handlers = []
      @connected = nil

      @thread = Thread.new do
        run
      end
    end

    def connected?
      @connected
    end

    def kick
      @sockEngine.write(".")
      @sockEngine.flush
    end

    def add_conn_handler(handler)
      synchronize do
        @new_conn_handlers << handler
      end
      kick
    end

    def del_conn_handler(handler)
      synchronize do
        @conn_handlers_to_delete << handler
      end
      kick
    end

    def run()
      eventImpl = Qmfengine::ResilientConnectionEvent.new
      new_handlers = nil
      del_handlers = nil
      bt_count = 0

      while :true
        @sock.read(1)

        synchronize do
          new_handlers = @new_conn_handlers
          del_handlers = @conn_handlers_to_delete
          @new_conn_handlers = []
          @conn_handlers_to_delete = []
        end

        new_handlers.each do |nh|
          @conn_handlers << nh
          nh.conn_event_connected() if @connected
        end
        new_handlers = nil

        del_handlers.each do |dh|
          d = @conn_handlers.delete(dh)
        end
        del_handlers = nil

        valid = @impl.getEvent(eventImpl)
        while valid
          begin
            case eventImpl.kind
            when Qmfengine::ResilientConnectionEvent::CONNECTED
              @connected = :true
              @conn_handlers.each { |h| h.conn_event_connected() }
            when Qmfengine::ResilientConnectionEvent::DISCONNECTED
              @connected = nil
              @conn_handlers.each { |h| h.conn_event_disconnected(eventImpl.errorText) }
            when Qmfengine::ResilientConnectionEvent::SESSION_CLOSED
              eventImpl.sessionContext.handler.sess_event_session_closed(eventImpl.sessionContext, eventImpl.errorText)
            when Qmfengine::ResilientConnectionEvent::RECV
              eventImpl.sessionContext.handler.sess_event_recv(eventImpl.sessionContext, eventImpl.message)
            end
          rescue Exception => ex
            puts "Event Exception: #{ex}"
            if bt_count < 2
              puts ex.backtrace
              bt_count += 1
            end
          end
          @impl.popEvent
          valid = @impl.getEvent(eventImpl)
        end
        @conn_handlers.each { |h| h.conn_event_visit }
      end
    end
  end

  class Session
    attr_reader :handle, :handler

    def initialize(conn, label, handler)
      @conn = conn
      @label = label
      @handler = handler
      @handle = Qmfengine::SessionHandle.new
      result = @conn.impl.createSession(label, self, @handle)
    end

    def destroy()
      @conn.impl.destroySession(@handle)
    end
  end

  ##==============================================================================
  ## OBJECTS
  ##==============================================================================

  class QmfObject
    include MonitorMixin
    attr_reader :impl, :object_class
    def initialize(cls, kwargs={})
      super()
      @cv = new_cond
      @sync_count = 0
      @sync_result = nil
      @allow_sets = :false
      @broker = kwargs[:broker] if kwargs.include?(:broker)

      if cls:
        @object_class = cls
        @impl = Qmfengine::Object.new(@object_class.impl)
      elsif kwargs.include?(:impl)
        @impl = Qmfengine::Object.new(kwargs[:impl])
        @object_class = SchemaObjectClass.new(nil, nil, :impl => @impl.getClass)
      end
    end

    def object_id
      return ObjectId.new(@impl.getObjectId)
    end

    def properties
      list = []
      @object_class.properties.each do |prop|
        list << [prop, get_attr(prop.name)]
      end
      return list
    end

    def statistics
      list = []
      @object_class.statistics.each do |stat|
        list << [stat, get_attr(stat.name)]
      end
      return list
    end

    def get_attr(name)
      val = value(name)
      case val.getType
      when TYPE_UINT8, TYPE_UINT16, TYPE_UINT32 then val.asUint
      when TYPE_UINT64 then val.asUint64
      when TYPE_SSTR, TYPE_LSTR then val.asString
      when TYPE_ABSTIME then val.asInt64
      when TYPE_DELTATIME then val.asUint64
      when TYPE_REF then ObjectId.new(val.asObjectId)
      when TYPE_BOOL then val.asBool
      when TYPE_FLOAT then val.asFloat
      when TYPE_DOUBLE then val.asDouble
      when TYPE_UUID then val.asUuid
      when TYPE_INT8, TYPE_INT16, TYPE_INT32 then val.asInt
      when TYPE_INT64 then val.asInt64
      when TYPE_MAP
      when TYPE_OBJECT
      when TYPE_LIST
      when TYPE_ARRAY
      end
    end

    def set_attr(name, v)
      val = value(name)
      case val.getType
      when TYPE_UINT8, TYPE_UINT16, TYPE_UINT32 then val.setUint(v)
      when TYPE_UINT64 then val.setUint64(v)
      when TYPE_SSTR, TYPE_LSTR then v ? val.setString(v) : val.setString('')
      when TYPE_ABSTIME then val.setInt64(v)
      when TYPE_DELTATIME then val.setUint64(v)
      when TYPE_REF then val.setObjectId(v.impl)
      when TYPE_BOOL then v ? val.setBool(v) : val.setBool(0)
      when TYPE_FLOAT then val.setFloat(v)
      when TYPE_DOUBLE then val.setDouble(v)
      when TYPE_UUID then val.setUuid(v)
      when TYPE_INT8, TYPE_INT16, TYPE_INT32 then val.setInt(v)
      when TYPE_INT64 then val.setInt64(v)
      when TYPE_MAP
      when TYPE_OBJECT
      when TYPE_LIST
      when TYPE_ARRAY
      end
    end

    def [](name)
      get_attr(name)
    end

    def []=(name, value)
      set_attr(name, value)
    end

    def inc_attr(name, by=1)
      set_attr(name, get_attr(name) + by)
    end

    def dec_attr(name, by=1)
      set_attr(name, get_attr(name) - by)
    end

    def method_missing(name_in, *args)
      #
      # Convert the name to a string and determine if it represents an
      # attribute assignment (i.e. "attr=")
      #
      name = name_in.to_s
      attr_set = (name[name.length - 1] == 61)
      name = name[0..name.length - 2] if attr_set
      raise "Sets not permitted on this object" if attr_set && !@allow_sets

      #
      # If the name matches a property name, set or return the value of the property.
      #
      @object_class.properties.each do |prop|
        if prop.name == name
          if attr_set
            return set_attr(name, args[0])
          else
            return get_attr(name)
          end
        end
      end

      #
      # Do the same for statistics
      #
      @object_class.statistics.each do |stat|
        if stat.name == name
          if attr_set
            return set_attr(name, args[0])
          else
            return get_attr(name)
          end
        end
      end

      #
      # If we still haven't found a match for the name, check to see if
      # it matches a method name.  If so, marshall the arguments and invoke
      # the method.
      #
      @object_class.methods.each do |method|
        if method.name == name
          raise "Sets not permitted on methods" if attr_set
          timeout = 30
          synchronize do
            @sync_count = 1
            @impl.invokeMethod(name, _marshall(method, args), self)
            @broker.conn.kick if @broker
            unless @cv.wait(timeout) { @sync_count == 0 }
              raise "Timed out waiting for response"
            end
          end

          return @sync_result
        end
      end

      #
      # This name means nothing to us, pass it up the line to the parent
      # class's handler.
      #
      super.method_missing(name_in, args)
    end

    def _method_result(result)
      synchronize do
        @sync_result = result
        @sync_count -= 1
        @cv.signal
      end
    end

    #
    # Convert a Ruby array of arguments (positional) into a Value object of type "map".
    #
    private
    def _marshall(schema, args)
      map = Qmfengine::Value.new(TYPE_MAP)
      schema.arguments.each do |arg|
        if arg.direction == DIR_IN || arg.direction == DIR_IN_OUT
          map.insert(arg.name, Qmfengine::Value.new(arg.typecode))
        end
      end

      marshalled = Arguments.new(map)
      idx = 0
      schema.arguments.each do |arg|
        if arg.direction == DIR_IN || arg.direction == DIR_IN_OUT
          marshalled[arg.name] = args[idx] unless args[idx] == nil
          idx += 1
        end
      end

      return marshalled.map
    end

    private
    def value(name)
      val = @impl.getValue(name.to_s)
      if val.nil?
        raise ArgumentError, "Attribute '#{name}' not defined for class #{@object_class.impl.getClassKey.getPackageName}:#{@object_class.impl.getClassKey.getClassName}"
      end
      return val
    end
  end

  class AgentObject < QmfObject
    def initialize(cls, kwargs={})
      super(cls, kwargs)
      @allow_sets = :true
    end

    def destroy
      @impl.destroy
    end

    def set_object_id(oid)
      @impl.setObjectId(oid.impl)
    end
  end

  class ConsoleObject < QmfObject
    attr_reader :current_time, :create_time, :delete_time

    def initialize(cls, kwargs={})
      super(cls, kwargs)
    end

    def update()
      raise "No linkage to broker" unless @broker
      newer = @broker.console.objects(Query.new(:object_id => object_id))
      raise "Expected exactly one update for this object" unless newer.size == 1
      merge_update(newer[0])
    end

    def merge_update(new_object)
      @impl.merge(new_object.impl)
    end

    def deleted?()
      @impl.isDeleted
    end

    def key()
    end
  end

  class ObjectId
    attr_reader :impl, :agent_key
    def initialize(impl=nil)
      if impl
        @impl = Qmfengine::ObjectId.new(impl)
      else
        @impl = Qmfengine::ObjectId.new
      end
      @agent_key = "#{@impl.getBrokerBank}.#{@impl.getAgentBank}"
    end

    def object_num_high
      @impl.getObjectNumHi
    end

    def object_num_low
      @impl.getObjectNumLo
    end

    def ==(other)
      return @impl == other.impl
    end

    def to_s
      @impl.str
    end
  end

  class Arguments
    attr_reader :map
    def initialize(map)
      @map = map
      @by_hash = {}
      key_count = @map.keyCount
      a = 0
      while a < key_count
        @by_hash[@map.key(a)] = by_key(@map.key(a))
        a += 1
      end
    end

    def [] (key)
      return @by_hash[key]
    end

    def []= (key, value)
      @by_hash[key] = value
      set(key, value)
    end

    def each
      @by_hash.each { |k, v| yield(k, v) }
    end

    def method_missing(name, *args)
      if @by_hash.include?(name.to_s)
        return @by_hash[name.to_s]
      end

      super.method_missing(name, args)
    end

    def by_key(key)
      val = @map.byKey(key)
      case val.getType
      when TYPE_UINT8, TYPE_UINT16, TYPE_UINT32 then val.asUint
      when TYPE_UINT64                          then val.asUint64
      when TYPE_SSTR, TYPE_LSTR                 then val.asString
      when TYPE_ABSTIME                         then val.asInt64
      when TYPE_DELTATIME                       then val.asUint64
      when TYPE_REF                             then ObjectId.new(val.asObjectId)
      when TYPE_BOOL                            then val.asBool
      when TYPE_FLOAT                           then val.asFloat
      when TYPE_DOUBLE                          then val.asDouble
      when TYPE_UUID                            then val.asUuid
      when TYPE_INT8, TYPE_INT16, TYPE_INT32    then val.asInt
      when TYPE_INT64                           then val.asInt64
      when TYPE_MAP
      when TYPE_OBJECT
      when TYPE_LIST
      when TYPE_ARRAY
      end
    end

    def set(key, value)
      val = @map.byKey(key)
      case val.getType
      when TYPE_UINT8, TYPE_UINT16, TYPE_UINT32 then val.setUint(value)
      when TYPE_UINT64 then val.setUint64(value)
      when TYPE_SSTR, TYPE_LSTR then value ? val.setString(value) : val.setString('')
      when TYPE_ABSTIME then val.setInt64(value)
      when TYPE_DELTATIME then val.setUint64(value)
      when TYPE_REF then val.setObjectId(value.impl)
      when TYPE_BOOL then value ? val.setBool(value) : val.setBool(0)
      when TYPE_FLOAT then val.setFloat(value)
      when TYPE_DOUBLE then val.setDouble(value)
      when TYPE_UUID then val.setUuid(value)
      when TYPE_INT8, TYPE_INT16, TYPE_INT32 then val.setInt(value)
      when TYPE_INT64 then val.setInt64(value)
      when TYPE_MAP
      when TYPE_OBJECT
      when TYPE_LIST
      when TYPE_ARRAY
      end
    end
  end

  class MethodResponse
    def initialize(impl)
      @impl = Qmfengine::MethodResponse.new(impl)
    end

    def status
      @impl.getStatus
    end

    def exception
      @impl.getException
    end

    def text
      exception.asString
    end

    def args
      Arguments.new(@impl.getArgs)
    end

    def method_missing(name, *extra_args)
      args.__send__(name, extra_args)
    end
  end

  ##==============================================================================
  ## QUERY
  ##==============================================================================

  class Query
    attr_reader :impl
    def initialize(kwargs = {})
      if kwargs.include?(:impl)
        @impl = Qmfengine::Query.new(kwargs[:impl])
      else
        package = ''
        if kwargs.include?(:key)
          @impl = Qmfengine::Query.new(kwargs[:key])
        elsif kwargs.include?(:object_id)
          @impl = Qmfengine::Query.new(kwargs[:object_id].impl)
        else
          package = kwargs[:package] if kwargs.include?(:package)
          if kwargs.include?(:class)
            @impl = Qmfengine::Query.new(kwargs[:class], package)
          else
            raise ArgumentError, "Invalid arguments, use :key, :object_id or :class[,:package]"
          end
        end
      end
    end

    def package_name
      @impl.getPackage
    end

    def class_name
      @impl.getClass
    end

    def object_id
      objid = @impl.getObjectId
      if objid.class == NilClass
        return nil
      end
      return ObjectId.new(objid)
    end
  end

  ##==============================================================================
  ## SCHEMA
  ##==============================================================================

  class SchemaArgument
    attr_reader :impl
    def initialize(name, typecode, kwargs={})
      if kwargs.include?(:impl)
        @impl = kwargs[:impl]
      else
        @impl = Qmfengine::SchemaArgument.new(name, typecode)
        @impl.setDirection(kwargs[:dir]) if kwargs.include?(:dir)
        @impl.setUnit(kwargs[:unit])     if kwargs.include?(:unit)
        @impl.setDesc(kwargs[:desc])     if kwargs.include?(:desc)
      end
    end

    def name
      @impl.getName
    end

    def direction
      @impl.getDirection
    end

    def typecode
      @impl.getType
    end

    def to_s
      name
    end
  end

  class SchemaMethod
    attr_reader :impl, :arguments
    def initialize(name, kwargs={})
      @arguments = []
      if kwargs.include?(:impl)
        @impl = kwargs[:impl]
        arg_count = @impl.getArgumentCount
        for i in 0...arg_count
          @arguments << SchemaArgument.new(nil, nil, :impl => @impl.getArgument(i))
        end
      else
        @impl = Qmfengine::SchemaMethod.new(name)
        @impl.setDesc(kwargs[:desc]) if kwargs.include?(:desc)
      end
    end

    def add_argument(arg)
      @arguments << arg
      @impl.addArgument(arg.impl)
    end

    def name
      @impl.getName
    end

    def to_s
      name
    end
  end

  class SchemaProperty
    attr_reader :impl
    def initialize(name, typecode, kwargs={})
      if kwargs.include?(:impl)
        @impl = kwargs[:impl]
      else
        @impl = Qmfengine::SchemaProperty.new(name, typecode)
        @impl.setAccess(kwargs[:access])     if kwargs.include?(:access)
        @impl.setIndex(kwargs[:index])       if kwargs.include?(:index)
        @impl.setOptional(kwargs[:optional]) if kwargs.include?(:optional)
        @impl.setUnit(kwargs[:unit])         if kwargs.include?(:unit)
        @impl.setDesc(kwargs[:desc])         if kwargs.include?(:desc)
      end
    end

    def name
      @impl.getName
    end

    def to_s
      name
    end
  end

  class SchemaStatistic
    attr_reader :impl
    def initialize(name, typecode, kwargs={})
      if kwargs.include?(:impl)
        @impl = kwargs[:impl]
      else
        @impl = Qmfengine::SchemaStatistic.new(name, typecode)
        @impl.setUnit(kwargs[:unit]) if kwargs.include?(:unit)
        @impl.setDesc(kwargs[:desc]) if kwargs.include?(:desc)
      end
    end

    def name
      @impl.getName
    end

    def to_s
      name
    end
  end

  class SchemaClassKey
    attr_reader :impl
    def initialize(i)
      @impl = Qmfengine::SchemaClassKey.new(i)
    end

    def package_name
      @impl.getPackageName
    end

    def class_name
      @impl.getClassName
    end

    def to_s
      @impl.asString
    end
  end

  class SchemaObjectClass
    attr_reader :impl, :properties, :statistics, :methods
    def initialize(package, name, kwargs={})
      @properties = []
      @statistics = []
      @methods = []
      if kwargs.include?(:impl)
        @impl = kwargs[:impl]

        @impl.getPropertyCount.times do |i|
          @properties << SchemaProperty.new(nil, nil, :impl => @impl.getProperty(i))
        end

        @impl.getStatisticCount.times do |i|
          @statistics << SchemaStatistic.new(nil, nil, :impl => @impl.getStatistic(i))
        end
          
        @impl.getMethodCount.times do |i|
          @methods << SchemaMethod.new(nil, :impl => @impl.getMethod(i))
        end
      else
        @impl = Qmfengine::SchemaObjectClass.new(package, name)
      end
    end

    def add_property(prop)
      @properties << prop
      @impl.addProperty(prop.impl)
    end

    def add_statistic(stat)
      @statistics << stat
      @impl.addStatistic(stat.impl)
    end

    def add_method(meth)
      @methods << meth
      @impl.addMethod(meth.impl)
    end

    def class_key
      SchemaClassKey.new(@impl.getClassKey)
    end

    def package_name
      @impl.getClassKey.getPackageName
    end

    def class_name
      @impl.getClassKey.getClassName
    end
  end

  class SchemaEventClass
    attr_reader :impl, :arguments
    def initialize(package, name, kwargs={})
      @arguments = []
      if kwargs.include?(:impl)
        @impl = kwargs[:impl]
        @impl.getArgumentCount.times do |i|
          @arguments << SchemaArgument.new(nil, nil, :impl => @impl.getArgument(i))
        end
      else
        @impl = Qmfengine::SchemaEventClass.new(package, name)
        @impl.setDesc(kwargs[:desc]) if kwargs.include?(:desc)
      end
    end

    def add_argument(arg)
      @arguments << arg
      @impl.addArgument(arg.impl)
    end

    def name
      @impl.getClassKey.getClassName
    end
  end

  ##==============================================================================
  ## CONSOLE
  ##==============================================================================

  class ConsoleHandler
    def agent_added(agent); end
    def agent_deleted(agent); end
    def new_package(package); end
    def new_class(class_key); end
    def object_update(object, hasProps, hasStats); end
    def event_received(event); end
    def agent_heartbeat(agent, timestamp); end
    def method_response(resp); end
    def broker_info(broker); end
  end

  class Console
    include MonitorMixin
    attr_reader :impl

    def initialize(handler = nil, kwargs={})
      super()
      @handler = handler
      @impl = Qmfengine::Console.new
      @event = Qmfengine::ConsoleEvent.new
      @broker_list = []
      @cv = new_cond
      @sync_count = nil
      @sync_result = nil
      @select = []
      @bt_count = 0
      @cb_cond = new_cond
      @cb_thread = Thread.new do
        run_cb_thread
      end
    end

    def add_connection(conn)
      broker = Broker.new(self, conn)
      synchronize { @broker_list << broker }
      return broker
    end

    def del_connection(broker)
      broker.shutdown
      synchronize { @broker_list.delete(broker) }
    end

    def packages()
      plist = []
      count = @impl.packageCount
      for i in 0...count
        plist << @impl.getPackageName(i)
      end
      return plist
    end

    def classes(package, kind=CLASS_OBJECT)
      clist = []
      count = @impl.classCount(package)
      for i in 0...count
        key = @impl.getClass(package, i)
        class_kind = @impl.getClassKind(key)
        if class_kind == kind
          if kind == CLASS_OBJECT
            clist << SchemaObjectClass.new(nil, nil, :impl => @impl.getObjectClass(key))
          elsif kind == CLASS_EVENT
            clist << SchemaEventClass.new(nil, nil, :impl => @impl.getEventClass(key))
          end
        end
      end

      return clist
    end

    def bind_package(package)
      @impl.bindPackage(package)
    end

    def bind_class(kwargs = {})
      if kwargs.include?(:key)
        @impl.bindClass(kwargs[:key])
      elsif kwargs.include?(:package)
        package = kwargs[:package]
        if kwargs.include?(:class)
          @impl.bindClass(package, kwargs[:class])
        else
          @impl.bindClass(package)
        end
      else
        raise ArgumentError, "Invalid arguments, use :key or :package[,:class]"
      end
    end

    def agents(broker = nil)
      blist = []
      if broker
        blist << broker
      else
        synchronize { blist = @broker_list }
      end

      agents = []
      blist.each do |b|
        count = b.impl.agentCount
        for idx in 0...count
          agents << AgentProxy.new(b.impl.getAgent(idx), b)
        end
      end

      return agents
    end

    def objects(query, kwargs = {})
      timeout = 30
      agent = nil
      kwargs.merge!(query) if query.class == Hash

      if kwargs.include?(:timeout)
        timeout = kwargs[:timeout]
        kwargs.delete(:timeout)
      end

      if kwargs.include?(:agent)
        agent = kwargs[:agent]
        kwargs.delete(:agent)
      end

      query = Query.new(kwargs) if query.class == Hash

      @select = []
      kwargs.each do |k,v|
        @select << [k, v] if k.is_a?(String)
      end

      synchronize do
        @sync_count = 1
        @sync_result = []
        broker = nil
        synchronize { broker = @broker_list[0] }
        broker.send_query(query.impl, nil, agent)
        unless @cv.wait(timeout) { @sync_count == 0 }
          raise "Timed out waiting for response"
        end

        return @sync_result
      end
    end

    # Return one and only one object or nil.
    def object(query, kwargs = {})
      objs = objects(query, kwargs)
      return objs.length == 1 ? objs[0] : nil
    end

    # Return the first of potentially many objects.
    def first_object(query, kwargs = {})
      objs = objects(query, kwargs)
      return objs.length > 0 ? objs[0] : nil
    end

    # Check the object against select to check for a match
    def select_match(object)
      @select.each do |key, value|
        object.properties.each do |prop, propval|
          if key == prop.name && value != propval
            return nil
          end
        end
      end
      return :true
    end

    def _get_result(list, context)
      synchronize do
        list.each do |item|
          @sync_result << item if select_match(item)
        end
        @sync_count -= 1
        @cv.signal
      end
    end

    def start_sync(query)
    end

    def touch_sync(sync)
    end

    def end_sync(sync)
    end

    def run_cb_thread
      while :true
        synchronize { @cb_cond.wait(1) }
        begin
          count = do_console_events
        end until count == 0
      end
    end

    def start_console_events
      synchronize { @cb_cond.signal }
    end

    def do_console_events
      count = 0
      valid = @impl.getEvent(@event)
      while valid
        count += 1
        begin
          case @event.kind
          when Qmfengine::ConsoleEvent::AGENT_ADDED
            @handler.agent_added(AgentProxy.new(@event.agent, nil)) if @handler
          when Qmfengine::ConsoleEvent::AGENT_DELETED
            @handler.agent_deleted(AgentProxy.new(@event.agent, nil)) if @handler
          when Qmfengine::ConsoleEvent::NEW_PACKAGE
            @handler.new_package(@event.name) if @handler
          when Qmfengine::ConsoleEvent::NEW_CLASS
            @handler.new_class(SchemaClassKey.new(@event.classKey)) if @handler
          when Qmfengine::ConsoleEvent::OBJECT_UPDATE
            @handler.object_update(ConsoleObject.new(nil, :impl => @event.object), @event.hasProps, @event.hasStats) if @handler
          when Qmfengine::ConsoleEvent::EVENT_RECEIVED
          when Qmfengine::ConsoleEvent::AGENT_HEARTBEAT
            @handler.agent_heartbeat(AgentProxy.new(@event.agent, nil), @event.timestamp) if @handler
          when Qmfengine::ConsoleEvent::METHOD_RESPONSE
          end
        rescue Exception => ex
          puts "Exception caught in callback: #{ex}"
          if @bt_count < 2
            puts ex.backtrace
            @bt_count += 1
          end
        end
        @impl.popEvent
        valid = @impl.getEvent(@event)
      end
      return count
    end
  end

  class AgentProxy
    attr_reader :impl, :broker, :label, :key

    def initialize(impl, broker)
      @impl = Qmfengine::AgentProxy.new(impl)
      @broker = broker
      @label = @impl.getLabel
      @key = "#{@impl.getBrokerBank}.#{@impl.getAgentBank}"
    end
  end

  class Broker < ConnectionHandler
    include MonitorMixin
    attr_reader :impl, :conn, :console, :broker_bank

    def initialize(console, conn)
      super()
      @broker_bank = 1
      @console = console
      @conn = conn
      @session = nil
      @cv = new_cond
      @stable = nil
      @event = Qmfengine::BrokerEvent.new
      @xmtMessage = Qmfengine::Message.new
      @impl = Qmfengine::BrokerProxy.new(@console.impl)
      @console.impl.addConnection(@impl, self)
      @conn.add_conn_handler(self)
      @operational = :true
    end

    def shutdown()
      @console.impl.delConnection(@impl)
      @conn.del_conn_handler(self)
      @operational = :false
    end

    def wait_for_stable(timeout = nil)
      synchronize do
        return if @stable
        if timeout
          unless @cv.wait(timeout) { @stable }
            raise "Timed out waiting for broker connection to become stable"
          end
        else
          while not @stable
            @cv.wait
          end
        end
      end
    end

    def send_query(query, ctx, agent)
      agent_impl = agent.impl if agent
      @impl.sendQuery(query, ctx, agent_impl)
      @conn.kick
    end

    def do_broker_events()
      count = 0
      valid = @impl.getEvent(@event)
      while valid
        count += 1
        case @event.kind
        when Qmfengine::BrokerEvent::BROKER_INFO
        when Qmfengine::BrokerEvent::DECLARE_QUEUE
          @conn.impl.declareQueue(@session.handle, @event.name)
        when Qmfengine::BrokerEvent::DELETE_QUEUE
          @conn.impl.deleteQueue(@session.handle, @event.name)
        when Qmfengine::BrokerEvent::BIND
          @conn.impl.bind(@session.handle, @event.exchange, @event.name, @event.bindingKey)
        when Qmfengine::BrokerEvent::UNBIND
          @conn.impl.unbind(@session.handle, @event.exchange, @event.name, @event.bindingKey)
        when Qmfengine::BrokerEvent::SETUP_COMPLETE
          @impl.startProtocol
        when Qmfengine::BrokerEvent::STABLE
          synchronize do
            @stable = :true
            @cv.signal
          end
        when Qmfengine::BrokerEvent::QUERY_COMPLETE
          result = []
          for idx in 0...@event.queryResponse.getObjectCount
            result << ConsoleObject.new(nil, :impl => @event.queryResponse.getObject(idx), :broker => self)
          end
          @console._get_result(result, @event.context)
        when Qmfengine::BrokerEvent::METHOD_RESPONSE
          obj = @event.context
          obj._method_result(MethodResponse.new(@event.methodResponse))
        end
        @impl.popEvent
        valid = @impl.getEvent(@event)
      end
      return count
    end

    def do_broker_messages()
      count = 0
      valid = @impl.getXmtMessage(@xmtMessage)
      while valid
        count += 1
        @conn.impl.sendMessage(@session.handle, @xmtMessage)
        @impl.popXmt
        valid = @impl.getXmtMessage(@xmtMessage)
      end
      return count
    end

    def do_events()
      begin
        @console.start_console_events
        bcnt = do_broker_events
        mcnt = do_broker_messages
      end until bcnt == 0 and mcnt == 0
    end

    def conn_event_connected()
      puts "Console Connection Established..."
      @session = Session.new(@conn, "qmfc-%s.%d" % [Socket.gethostname, Process::pid], self)
      @impl.sessionOpened(@session.handle)
      do_events
    end

    def conn_event_disconnected(error)
      puts "Console Connection Lost"
    end

    def conn_event_visit
      do_events
    end

    def sess_event_session_closed(context, error)
      puts "Console Session Lost"
      @impl.sessionClosed()
    end

    def sess_event_recv(context, message)
      puts "Unexpected RECV Event" if not @operational
      @impl.handleRcvMessage(message)
      do_events
    end
  end

  ##==============================================================================
  ## AGENT
  ##==============================================================================

  class AgentHandler
    def get_query(context, query, userId); end
    def method_call(context, name, object_id, args, userId); end
  end

  class Agent < ConnectionHandler
    def initialize(handler, label="")
      if label == ""
        @agentLabel = "rb-%s.%d" % [Socket.gethostname, Process::pid]
      else
        @agentLabel = label
      end
      @conn = nil
      @handler = handler
      @impl = Qmfengine::Agent.new(@agentLabel)
      @event = Qmfengine::AgentEvent.new
      @xmtMessage = Qmfengine::Message.new
    end

    def set_connection(conn)
      @conn = conn
      @conn.add_conn_handler(self)
    end

    def register_class(cls)
      @impl.registerClass(cls.impl)
    end

    def alloc_object_id(low = 0, high = 0)
      ObjectId.new(@impl.allocObjectId(low, high))
    end

    def query_response(context, object)
      @impl.queryResponse(context, object.impl)
    end

    def query_complete(context)
      @impl.queryComplete(context)
    end

    def method_response(context, status, text, arguments)
      @impl.methodResponse(context, status, text, arguments.map)
    end

    def do_agent_events()
      count = 0
      valid = @impl.getEvent(@event)
      while valid
        count += 1
        case @event.kind
        when Qmfengine::AgentEvent::GET_QUERY
          @handler.get_query(@event.sequence, Query.new(:impl => @event.query), @event.authUserId)
        when Qmfengine::AgentEvent::START_SYNC
        when Qmfengine::AgentEvent::END_SYNC
        when Qmfengine::AgentEvent::METHOD_CALL
          args = Arguments.new(@event.arguments)
          @handler.method_call(@event.sequence, @event.name, ObjectId.new(@event.objectId),
                               args, @event.authUserId)
        when Qmfengine::AgentEvent::DECLARE_QUEUE
          @conn.impl.declareQueue(@session.handle, @event.name)
        when Qmfengine::AgentEvent::DELETE_QUEUE
          @conn.impl.deleteQueue(@session.handle, @event.name)
        when Qmfengine::AgentEvent::BIND
          @conn.impl.bind(@session.handle, @event.exchange, @event.name, @event.bindingKey)
        when Qmfengine::AgentEvent::UNBIND
          @conn.impl.unbind(@session.handle, @event.exchange, @event.name, @event.bindingKey)
        when Qmfengine::AgentEvent::SETUP_COMPLETE
          @impl.startProtocol()
        end
        @impl.popEvent
        valid = @impl.getEvent(@event)
      end
      return count
    end

    def do_agent_messages()
      count = 0
      valid = @impl.getXmtMessage(@xmtMessage)
      while valid
        count += 1
        @conn.impl.sendMessage(@session.handle, @xmtMessage)
        @impl.popXmt
        valid = @impl.getXmtMessage(@xmtMessage)
      end
      return count
    end

    def do_events()
      begin
        ecnt = do_agent_events
        mcnt = do_agent_messages
      end until ecnt == 0 and mcnt == 0
    end

    def conn_event_connected()
      puts "Agent Connection Established..."
      @session = Session.new(@conn, "qmfa-%s.%d" % [Socket.gethostname, Process::pid], self)
      @impl.newSession
      do_events
    end

    def conn_event_disconnected(error)
      puts "Agent Connection Lost"
    end

    def conn_event_visit
      do_events
    end

    def sess_event_session_closed(context, error)
      puts "Agent Session Lost"
    end

    def sess_event_recv(context, message)
      @impl.handleRcvMessage(message)
      do_events
    end
  end
end
