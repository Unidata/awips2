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

# Console API for Qpid Management Framework

require 'socket'
require 'monitor'
require 'thread'
require 'uri'
require 'time'

module Qpid::Qmf

  # To access the asynchronous operations, a class must be derived from
  # Console with overrides of any combination of the available methods.
  class Console

    # Invoked when a connection is established to a broker
    def broker_connected(broker); end

    # Invoked when the connection to a broker is lost
    def broker_disconnected(broker); end

    # Invoked when a QMF package is discovered
    def new_package(name); end

    # Invoked when a new class is discovered.  Session.getSchema can be
    # used to obtain details about the class
    def new_class(kind, klass_key); end

    # Invoked when a QMF agent is discovered
    def new_agent(agent); end

    # Invoked when a QMF agent disconects
    def del_agent(agent); end

    # Invoked when an object is updated
    def object_props(broker, record); end

    # Invoked when an object is updated
    def object_stats(broker, record); end

    # Invoked when an event is raised
    def event(broker, event); end

    # Invoked when an agent heartbeat is received.
    def heartbeat(agent, timestamp); end

    # Invoked when the connection sequence reaches the point where broker information is available.
    def broker_info(broker); end

    # Invoked when a method response from an asynchronous method call is received.
    def method_response(broker, seq, response); end
  end

  class BrokerURL

    attr_reader :host, :port, :auth_name, :auth_pass

    def initialize(text)
      uri = URI.parse(text)

      @host = uri.host
      @port = uri.port ? uri.port : 5672
      @auth_name = uri.user
      @auth_pass = uri.password

      return uri
    end

    def name
      "#{@host}:#{@port}"
    end

    def match(host, port)
      # FIXME: Unlcear what the Python code is actually checking for
      # here, especially since HOST can resolve to multiple IP's
      @port == port &&
        (host == @host || ipaddr(host, port) == ipaddr(@host, @port))
    end

    private
    def ipaddr(host, port)
      s = Socket::getaddrinfo(host, port,
                              Socket::AF_INET, Socket::SOCK_STREAM)
      s[0][2]
    end
  end

  # An instance of the Session class represents a console session running
  # against one or more QMF brokers.  A single instance of Session is
  # needed to interact with the management framework as a console.
  class Session
    CONTEXT_SYNC     = 1
    CONTEXT_STARTUP  = 2
    CONTEXT_MULTIGET = 3

    DEFAULT_GET_WAIT_TIME = 60

    include MonitorMixin

    attr_reader :binding_key_list, :select, :seq_mgr, :console, :packages

    # Initialize a session.  If the console argument is provided, the
    # more advanced asynchronous features are available.  If console is
    # defaulted, the session will operate in a simpler, synchronous
    # manner. The rcvObjects, rcvEvents, and rcvHeartbeats arguments
    # are meaningful only if 'console' is provided.  They control
    # whether object updates, events, and agent-heartbeats are
    # subscribed to.  If the console is not interested in receiving one
    # or more of the above, setting the argument to False will reduce
    # tha bandwidth used by the API. If manageConnections is set to
    # True, the Session object will manage connections to the brokers.
    # This means that if a broker is unreachable, it will retry until a
    # connection can be established.  If a connection is lost, the
    # Session will attempt to reconnect.
    #
    # If manageConnections is set to False, the user is responsible for
    # handing failures. In this case, an unreachable broker will cause
    # addBroker to raise an exception. If userBindings is set to False
    # (the default) and rcvObjects is True, the console will receive
    # data for all object classes.  If userBindings is set to True, the
    # user must select which classes the console shall receive by
    # invoking the bindPackage or bindClass methods. This allows the
    # console to be configured to receive only information that is
    # relavant to a particular application.  If rcvObjects id False,
    # userBindings has no meaning.
    #
    # Accept a hash of parameters, where keys can be :console,
    # :rcv_objects, :rcv_events, :rcv_heartbeats, :manage_connections,
    # and :user_bindings
    def initialize(kwargs = {})
      super()
      @console           = kwargs[:console] || nil
      @brokers           = []
      @packages          = {}
      @seq_mgr           = SequenceManager.new
      @cv                = new_cond
      @sync_sequence_list = []
      @result         = []
      @select         = []
      @error             = nil
      @rcv_objects       = kwargs[:rcv_objects] == nil ? true : kwargs[:rcv_objects]
      @rcv_events        = kwargs[:rcv_events]  == nil ? true : kwargs[:rcv_events]
      @rcv_heartbeats    = kwargs[:rcv_heartbeats] == nil ? true : kwargs[:rcv_heartbeats]
      @user_bindings     = kwargs[:user_bindings] == nil ? false : kwargs[:user_bindings]
      unless @console
        @rcv_objects    = false
        @rcv_events     = false
        @rcv_heartbeats = false
      end
      @binding_key_list    = binding_keys
      @manage_connections  = kwargs[:manage_connections] || false

      if @user_bindings && ! @rcv_objects
        raise ArgumentError, "user_bindings can't be set unless rcv_objects is set and a console is provided"
      end

    end

    def to_s
      "QMF Console Session Manager (brokers: #{@brokers.size})"
    end

    def managedConnections?
      return @manage_connections
    end

    # Connect to a Qpid broker.  Returns an object of type Broker
    #
    #  To supply a username for authentication, use the URL syntax:
    #
    #      amqp://username@hostname:port
    #
    #  If the broker needs a password for the client, an interactive prompt will be
    #  provided to the user.
    #
    #  To supply a username and a password, use
    #
    #      amqp://username:password@hostname:port
    #
    #  The following keyword arguments may be used to control authentication:
    #
    #      :mechanism  - SASL mechanism (i.e. "PLAIN", "GSSAPI", "ANONYMOUS", etc.
    #                  - defaults to unspecified (the system chooses for you)
    #      :service    - SASL service name (i.e. the kerberos principal of the broker)
    #                  - defaults to "qpidd"
    #      :min_ssf    - Minimum Security Strength Factor for SASL security layers
    #                  - defaults to 0
    #      :max_ssf    - Maximum Security Strength Factor for SASL security layers
    #                  - defaults to 65535
    #
    def add_broker(target = "amqp://localhost", kwargs = {})
      url = BrokerURL.new(target)
      broker = Broker.new(self, url.host, url.port, url.auth_name, url.auth_pass, kwargs)
      unless broker.connected? || @manage_connections
        raise broker.error
      end

      @brokers << broker
      objects(:broker => broker, :class => "agent") unless @manage_connections
      return broker
    end

    # Disconnect from a broker.  The 'broker' argument is the object
    # returned from the addBroker call
    def del_broker(broker)
      broker.shutdown
      @brokers.delete(broker)
    end

    # Get the list of known classes within a QMF package
    def classes(package_name)
      list = []
      @brokers.each { |broker| broker.wait_for_stable }
      if @packages.include?(package_name)
        # FIXME What's the actual structure of @packages[package_name]
        @packages[package_name].each do |key, schema_class|
          list << schema_class.klass_key
        end
      end
      return list
    end

    # Get the schema for a QMF class
    def schema(klass_key)
      @brokers.each { |broker| broker.wait_for_stable }
      if @packages.include?(klass_key.package)
        @packages[klass_key.package][ [klass_key.klass_name, klass_key.hash] ]
      end
    end

    def bind_package(package_name)
      unless @user_bindings && @rcv_objects
        raise "userBindings option not set for Session"
      end
      @brokers.each do |broker|
        args = { :exchange => "qpid.management",
          :queue => broker.topic_name,
          :binding_key => "console.obj.*.*.#{package_name}.#" }
        broker.amqp_session.exchange_bind(args)
      end
    end

    def bind_class(package_name, class_name)
      unless @user_bindings && @rcv_objects
        raise "userBindings option not set for Session"
      end
      @brokers.each do |broker|
        args = { :exchange => "qpid.management",
          :queue => broker.topic_name,
          :binding_key=> "console.obj.*.*.#{package_name}.#{class_name}.#" }
        broker.amqp_session.exchange_bind(args)
      end
    end

    def bind_class_key(klass_key)
      unless @user_bindings && @rcv_objects
        raise "userBindings option not set for Session"
      end
      pname, cname, hash = klass_key.to_a()
      @brokers.each do |broker|
        args = { :exchange => "qpid.management",
          :queue => broker.topic_name,
          :binding_key => "console.obj.*.*.#{pname}.#{cname}.#" }
        broker.amqp_session.exchange_bind(args)
      end
    end

    # Get a list of currently known agents
    def agents(broker=nil)
      broker_list = []
      if broker.nil?
        broker_list = @brokers.dup
      else
        broker_list << broker
      end
      broker_list.each { |b| b.wait_for_stable }
      agent_list = []
      broker_list.each { |b| agent_list += b.agents }
      return agent_list
    end

    # Get a list of objects from QMF agents.
    # All arguments are passed by name(keyword).
    #
    # The class for queried objects may be specified in one of the
    # following ways:
    # :schema => <schema> - supply a schema object returned from getSchema.
    # :key => <key>       - supply a klass_key from the list returned by getClasses.
    # :class => <name>    - supply a class name as a string.  If the class name exists
    #                       in multiple packages, a _package argument may also be supplied.
    # :object_id = <id>   - get the object referenced by the object-id
    #
    # If objects should be obtained from only one agent, use the following argument.
    # Otherwise, the query will go to all agents.
    #
    # :agent = <agent> - supply an agent from the list returned by getAgents.
    #
    # If the get query is to be restricted to one broker (as opposed to
    # all connected brokers), add the following argument:
    #
    # :broker = <broker> - supply a broker as returned by addBroker.
    #
    # The default timeout for this synchronous operation is 60 seconds.  To change the timeout,
    # use the following argument:
    #
    # :timeout = <time in seconds>
    #
    # If additional arguments are supplied, they are used as property
    # selectors, as long as their keys are strings.  For example, if
    # the argument "name" => "test" is supplied, only objects whose
    # "name" property is "test" will be returned in the result.
    def objects(kwargs)
      if kwargs.include?(:broker)
        broker_list = []
        broker_list << kwargs[:broker]
      else
        broker_list = @brokers
      end
      broker_list.each { |broker|
        broker.wait_for_stable
        if kwargs[:package] != "org.apache.qpid.broker" or kwargs[:class] != "agent"
          objects(:agent => broker.agent(1,0), :package => "org.apache.qpid.broker", :class => "agent") if broker.connected?
        end
      }

      agent_list = []
      if kwargs.include?(:agent)
        agent = kwargs[:agent]
        unless broker_list.include?(agent.broker)
          raise ArgumentError, "Supplied agent is not accessible through the supplied broker"
        end
        agent_list << agent if agent.broker.connected?
      else
        if kwargs.include?(:object_id)
          oid = kwargs[:object_id]
          broker_list.each { |broker|
            broker.agents.each { |agent|
              if oid.broker_bank == agent.broker_bank && oid.agent_bank == agent.agent_bank
                agent_list << agent if agent.broker.connected?
              end
            }
          }
        else
          broker_list.each { |broker|
            agent_list += broker.agents if broker.connected?
          }
        end
      end

      cname = nil
      if kwargs.include?(:schema)
        # FIXME: What kind of object is kwargs[:schema]
        pname, cname, hash = kwargs[:schema].getKey().to_a
      elsif kwargs.include?(:key)
        pname, cname, hash = kwargs[:key].to_a
      elsif kwargs.include?(:class)
        pname, cname, hash = [kwargs[:package], kwargs[:class], nil]
      end
      if cname.nil? && ! kwargs.include?(:object_id)
        raise ArgumentError,
        "No class supplied, use :schema, :key, :class, or :object_id' argument"
      end

      map = {}
      @select = []
      if kwargs.include?(:object_id)
        map["_objectid"] = kwargs[:object_id].to_s
      else
        map["_class"] = cname
        map["_package"] = pname if pname
        map["_hash"]    = hash if hash
        kwargs.each do |k,v|
          @select << [k, v] if k.is_a?(String)
        end
      end

      @result = []
      agent_list.each do |agent|
        broker = agent.broker
        send_codec = Qpid::StringCodec.new(broker.conn.spec)
        seq = nil
        synchronize do
          seq = @seq_mgr.reserve(CONTEXT_MULTIGET)
          @sync_sequence_list << seq
        end
        broker.set_header(send_codec, ?G, seq)
        send_codec.write_map(map)
        bank_key = "%d.%d" % [broker.broker_bank, agent.agent_bank]
        smsg = broker.message(send_codec.encoded, "agent.#{bank_key}")
        broker.emit(smsg)
      end

      timeout = false
      if kwargs.include?(:timeout)
        wait_time = kwargs[:timeout]
      else
        wait_time = DEFAULT_GET_WAIT_TIME
      end
      synchronize do
        unless @cv.wait_for(wait_time) { @sync_sequence_list.empty? || @error }
          @sync_sequence_list.each do |pending_seq|
            @seq_mgr.release(pending_seq)
          end
          @sync_sequence_list = []
          timeout = true
        end
      end

      if @error
        errorText = @error
        @error = nil
        raise errorText
      end

      if @result.empty? && timeout
        raise "No agent responded within timeout period"
      end
      @result
    end

    # Return one and only one object or nil.
    def object(kwargs)
      objs = objects(kwargs)
      return objs.length == 1 ? objs[0] : nil
    end

    # Return the first of potentially many objects.
    def first_object(kwargs)
      objs = objects(kwargs)
      return objs.length > 0 ? objs[0] : nil
    end

    def set_event_filter(kwargs); end

    def handle_broker_connect(broker); end

    def handle_broker_resp(broker, codec, seq)
      broker.broker_id = codec.read_uuid
      @console.broker_info(broker) if @console

      # Send a package request
      # (effectively inc and dec outstanding by not doing anything)
      send_codec = Qpid::StringCodec.new(broker.conn.spec)
      seq = @seq_mgr.reserve(CONTEXT_STARTUP)
      broker.set_header(send_codec, ?P, seq)
      smsg = broker.message(send_codec.encoded)
      broker.emit(smsg)
    end

    def handle_package_ind(broker, codec, seq)
      pname = codec.read_str8
      new_package = false
      synchronize do
        new_package = ! @packages.include?(pname)
        @packages[pname] = {} if new_package
      end
      @console.new_package(pname) if @console

      # Send a class request
      broker.inc_outstanding
      send_codec = Qpid::StringCodec.new(broker.conn.spec)
      seq = @seq_mgr.reserve(CONTEXT_STARTUP)
      broker.set_header(send_codec, ?Q, seq)
      send_codec.write_str8(pname)
      smsg = broker.message(send_codec.encoded)
      broker.emit(smsg)
    end

    def handle_command_complete(broker, codec, seq)
      code = codec.read_uint32
      text = codec.read_str8
      context = @seq_mgr.release(seq)
      if context == CONTEXT_STARTUP
        broker.dec_outstanding
      elsif context == CONTEXT_SYNC && seq == broker.sync_sequence
        broker.sync_done
      elsif context == CONTEXT_MULTIGET && @sync_sequence_list.include?(seq)
        synchronize do
          @sync_sequence_list.delete(seq)
          @cv.signal if @sync_sequence_list.empty?
        end
      end
    end

    def handle_class_ind(broker, codec, seq)
      kind  = codec.read_uint8
      classKey = ClassKey.new(codec)
      unknown = false

      synchronize do
        return unless @packages.include?(classKey.package)
        unknown = true unless @packages[classKey.package].include?([classKey.klass_name, classKey.hash])
      end


      if unknown
        # Send a schema request for the unknown class
        broker.inc_outstanding
        send_codec = Qpid::StringCodec.new(broker.conn.spec)
        seq = @seq_mgr.reserve(CONTEXT_STARTUP)
        broker.set_header(send_codec, ?S, seq)
        classKey.encode(send_codec)
        smsg = broker.message(send_codec.encoded)
        broker.emit(smsg)
      end
    end

    def handle_method_resp(broker, codec, seq)
      code = codec.read_uint32
      text = codec.read_str16
      out_args = {}
      pair = @seq_mgr.release(seq)
      return unless pair
      method, synchronous = pair
      if code == 0
        method.arguments.each do |arg|
          if arg.dir.index(?O)
            out_args[arg.name] = decode_value(codec, arg.type)
          end
        end
      end
      result = MethodResult.new(code, text, out_args)
      if synchronous:
          broker.synchronize do
          broker.sync_result = MethodResult.new(code, text, out_args)
          broker.sync_done
        end
      else
        @console.method_response(broker, seq, result) if @console
      end
    end

    def handle_heartbeat_ind(broker, codec, seq, msg)
      if @console
        broker_bank = 1
        agent_bank = 0
        dp = msg.get("delivery_properties")
        if dp
          key = dp["routing_key"]
          key_elements = key.split(".")
          if key_elements.length == 4
            broker_bank = key_elements[2].to_i
            agent_bank = key_elements[3].to_i
          end
        end
        agent = broker.agent(broker_bank, agent_bank)
        timestamp = codec.read_uint64
        @console.heartbeat(agent, timestamp) if agent
      end
    end

    def handle_event_ind(broker, codec, seq)
      if @console
        event = Event.new(self, broker, codec)
        @console.event(broker, event)
      end
    end

    def handle_schema_resp(broker, codec, seq)
      kind  = codec.read_uint8
      classKey = ClassKey.new(codec)
      klass = SchemaClass.new(self, kind, classKey, codec)
      synchronize { @packages[classKey.package][ [classKey.klass_name, classKey.hash] ] = klass }

      @seq_mgr.release(seq)
      broker.dec_outstanding
      @console.new_class(kind, classKey) if @console
    end

    def handle_content_ind(broker, codec, seq, prop=false, stat=false)
      klass_key = ClassKey.new(codec)
      pname, cname, hash = klass_key.to_a() ;

      schema = nil
      synchronize do
        return unless @packages.include?(klass_key.package)
        return unless @packages[klass_key.package].include?([klass_key.klass_name, klass_key.hash])
        schema = @packages[klass_key.package][ [klass_key.klass_name, klass_key.hash] ]
      end


      object = Qpid::Qmf::Object.new(self, broker, schema, codec, prop, stat)
      if pname == "org.apache.qpid.broker" && cname == "agent" && prop
        broker.update_agent(object)
      end

      synchronize do
        if @sync_sequence_list.include?(seq)
          if object.timestamps()[2] == 0 && select_match(object)
            @result << object
          end
          return
        end
      end

      @console.object_props(broker, object) if @console && @rcv_objects && prop
      @console.object_stats(broker, object) if @console && @rcv_objects && stat
    end

    def handle_broker_disconnect(broker); end

    def handle_error(error)
      synchronize do
        @error = error if @sync_sequence_list.length > 0
        @sync_sequence_list = []
        @cv.signal
      end
    end

    # Decode, from the codec, a value based on its typecode
    def decode_value(codec, typecode)
      case typecode
      when 1:  data = codec.read_uint8      # U8
      when 2:  data = codec.read_uint16     # U16
      when 3:  data = codec.read_uint32     # U32
      when 4:  data = codec.read_uint64     # U64
      when 6:  data = codec.read_str8       # SSTR
      when 7:  data = codec.read_str16      # LSTR
      when 8:  data = codec.read_int64      # ABSTIME
      when 9:  data = codec.read_uint64     # DELTATIME
      when 10: data = ObjectId.new(codec)   # REF
      when 11: data = codec.read_uint8 != 0 # BOOL
      when 12: data = codec.read_float      # FLOAT
      when 13: data = codec.read_double     # DOUBLE
      when 14: data = codec.read_uuid       # UUID
      when 15: data = codec.read_map        # FTABLE
      when 16: data = codec.read_int8       # S8
      when 17: data = codec.read_int16      # S16
      when 18: data = codec.read_int32      # S32
      when 19: data = codec.read_int64      # S64
      when 20:                              # Object
        inner_type_code = codec.read_uint8()
        if (inner_type_code == 20)
            classKey = ClassKey.new(codec)
            innerSchema = schema(classKey)
            data = Object.new(self, @broker, innerSchema, codec, true, true, false)  if innerSchema
        else
            data = decode_value(codec, inner_type_code)
        end
      when 21:
          data = []
          rec_codec = Qpid::StringCodec.new(codec.spec, codec.read_vbin32())
          count = rec_codec.read_uint32()
          while count > 0 do
            type = rec_codec.read_uint8()
            data << (decode_value(rec_codec,type))
            count -= 1
          end
      when 22:
          data = []
          rec_codec = Qpid::StringCodec.new(codec.spec, codec.read_vbin32())
          count = rec_codec.read_uint32()
          type = rec_codec.read_uint8()
          while count > 0 do
            data << (decode_value(rec_codec,type))
            count -= 1
          end
      else
        raise ArgumentError, "Invalid type code: #{typecode} - #{typecode.inspect}"
      end
      return data
    end

    ENCODINGS = {
      String => 6,
      Fixnum => 18,
      Bignum => 19,
      Float => 12,
      Array => 21,
      Hash => 15
    }

    def encoding(object)
      klass = object.class
      if ENCODINGS.has_key?(klass)
        return ENCODINGS[klass]
      end
      for base in klass.__bases__
        result = encoding(base)
        return result unless result.nil?
      end
    end

    # Encode, into the codec, a value based on its typecode
    def encode_value(codec, value, typecode)
      # FIXME: Python does a lot of magic type conversions
      # We just assume that value has the right type; this is safer
      # than coercing explicitly, since Array::pack will complain
      # loudly about various type errors
      case typecode
      when 1:  codec.write_uint8(value)         # U8
      when 2:  codec.write_uint16(value)        # U16
      when 3:  codec.write_uint32(value)        # U32
      when 4:  codec.write_uint64(value)        # U64
      when 6:  codec.write_str8(value)          # SSTR
      when 7:  codec.write_str16(value)         # LSTR
      when 8:  codec.write_int64(value)         # ABSTIME
      when 9:  codec.write_uint64(value)        # DELTATIME
      when 10: value.encode(codec)              # REF
      when 11: codec.write_uint8(value ? 1 : 0) # BOOL
      when 12: codec.write_float(value)         # FLOAT
      when 13: codec.write_double(value)        # DOUBLE
      when 14: codec.write_uuid(value)          # UUID
      when 15: codec.write_map(value)           # FTABLE
      when 16: codec.write_int8(value)          # S8
      when 17: codec.write_int16(value)         # S16
      when 18: codec.write_int32(value)         # S32
      when 19: codec.write_int64(value)         # S64
      when 20: value.encode(codec)
      when 21:                                  # List
        send_codec = Qpid::StringCodec.new(codec.spec)
        encode_value(send_codec, value.size, 3)
        value.each do v
            ltype = encoding(v)
            encode_value(send_codec,ltype,1)
            encode_value(send_codec,v,ltype)
        end
        codec.write_vbin32(send_codec.encoded)
      when 22:                                  # Array
        send_codec = Qpid::StringCodec.new(codec.spec)
        encode_value(send_codec, value.size, 3)
        if value.size > 0
            ltype = encoding(value[0])
            encode_value(send_codec,ltype,1)
            value.each do v
                encode_value(send_codec,v,ltype)
            end
        end
        codec.write_vbin32(send_codec.encoded)
      else
        raise ValueError, "Invalid type code: %d" % typecode
      end
    end

    def display_value(value, typecode)
      case typecode
      when 1:  return value.to_s
      when 2:  return value.to_s
      when 3:  return value.to_s
      when 4:  return value.to_s
      when 6:  return value.to_s
      when 7:  return value.to_s
      when 8:  return strftime("%c", gmtime(value / 1000000000))
      when 9:  return value.to_s
      when 10: return value.to_s
      when 11: return value ? 'T' : 'F'
      when 12: return value.to_s
      when 13: return value.to_s
      when 14: return Qpid::UUID::format(value)
      when 15: return value.to_s
      when 16: return value.to_s
      when 17: return value.to_s
      when 18: return value.to_s
      when 19: return value.to_s
      when 20: return value.to_s
      when 21: return value.to_s
      when 22: return value.to_s
      else
        raise ValueError, "Invalid type code: %d" % typecode
      end
    end

    private

    def binding_keys
      key_list = []
      key_list << "schema.#"
      if @rcv_objects && @rcv_events && @rcv_heartbeats &&
          ! @user_bindings
        key_list << "console.#"
      else
        if @rcv_objects && ! @user_bindings
          key_list << "console.obj.#"
        else
          key_list << "console.obj.*.*.org.apache.qpid.broker.agent"
        end
        key_list << "console.event.#" if @rcv_events
        key_list << "console.heartbeat.#" if @rcv_heartbeats
      end
      return key_list
    end

    # Check the object against select to check for a match
    def select_match(object)
      select.each do |key, value|
        object.properties.each do |prop, propval|
          return false if key == prop.name && value != propval
        end
      end
      return true
    end

  end

  class Package
    attr_reader :name

    def initialize(name)
      @name = name
    end
  end

  # A ClassKey uniquely identifies a class from the schema.
  class ClassKey
    attr_reader :package, :klass_name, :hash

    def initialize(package="", klass_name="", hash=0)
      if (package.kind_of?(Qpid::Codec))
        @package = package.read_str8()
        @klass_name = package.read_str8()
        @hash = package.read_bin128()
      else
        @package = package
        @klass_name = klass_name
        @hash = hash
      end
    end

    def encode(codec)
        codec.write_str8(@package)
        codec.write_str8(@klass_name)
        codec.write_bin128(@hash)
    end

    def to_a()
        return [@package, @klass_name, @hash]
    end

    def hash_string()
      "%08x-%08x-%08x-%08x" % hash.unpack("NNNN")
    end

    def to_s()
        return "#{@package}:#{@klass_name}(#{hash_string()})"
    end
  end

  class SchemaClass

    CLASS_KIND_TABLE = 1
    CLASS_KIND_EVENT = 2

    attr_reader :klass_key, :arguments, :super_klass_key

    def initialize(session, kind, key, codec)
      @session = session
      @kind = kind
      @klass_key = key
      @super_klass_key = nil
      @properties = []
      @statistics = []
      @methods = []
      @arguments = []

      has_supertype = 0 #codec.read_uint8
      if @kind == CLASS_KIND_TABLE
        prop_count   = codec.read_uint16
        stat_count   = codec.read_uint16
        method_count = codec.read_uint16
        if has_supertype == 1
          @super_klass_key = ClassKey.new(codec)
        end
        prop_count.times { |idx|
          @properties << SchemaProperty.new(codec) }
        stat_count.times { |idx|
          @statistics << SchemaStatistic.new(codec) }
        method_count.times { |idx|
          @methods<< SchemaMethod.new(codec) }
      elsif @kind == CLASS_KIND_EVENT
        arg_count = codec.read_uint16
        arg_count.times { |idx|
          sa = SchemaArgument.new(codec, false)
          @arguments << sa
        }
      end
    end

    def is_table?
        @kind == CLASS_KIND_TABLE
    end

    def is_event?
        @kind == CLASS_KIND_EVENT
    end

    def properties(include_inherited = true)
        returnValue = @properties
        if !@super_klass_key.nil? && include_inherited
            returnValue = @properties + @session.schema(@super_klass_key).properties
        end
        return returnValue
    end

    def statistics(include_inherited = true)
        returnValue = @statistics
        if !@super_klass_key.nil? && include_inherited
            returnValue = @statistics + @session.schema(@super_klass_key).statistics
        end
        return returnValue
    end

    def methods(include_inherited = true)
        returnValue = @methods
        if !@super_klass_key.nil? && include_inherited
            returnValue = @methods + @session.schema(@super_klass_key).methods
        end
        return returnValue
    end

    def to_s
      if @kind == CLASS_KIND_TABLE
        kind_str = "Table"
      elsif @kind == CLASS_KIND_EVENT
        kind_str = "Event"
      else
        kind_str = "Unsupported"
      end
      "#{kind_str} Class: #{klass_key.to_s}" 
    end
  end

  class SchemaProperty

    attr_reader :name, :type, :access, :index, :optional,
    :unit, :min, :max, :maxlen, :desc, :refClass, :refPackage

    def initialize(codec)
      map = codec.read_map
      @name     = map["name"]
      @type     = map["type"]
      @access   = map["access"]
      @index    = map["index"] != 0
      @optional = map["optional"] != 0
      @unit     = map["unit"]
      @min      = map["min"]
      @max      = map["max"]
      @maxlen   = map["maxlen"]
      @desc     = map["desc"]
      @refClass = map["refClass"]
      @refPackage = map["refPackage"]      
    end

    def to_s
      @name
    end
  end

  class SchemaStatistic

    attr_reader :name, :type, :unit, :desc, :refClass, :refPackage

    def initialize(codec)
      map = codec.read_map
      @name     = map["name"]
      @type     = map["type"]
      @unit     = map["unit"]
      @desc     = map["desc"]
      @refClass = map["refClass"]
      @refPackage = map["refPackage"]            
    end

    def to_s
      @name
    end
  end

  class SchemaMethod

    attr_reader :name, :desc, :arguments

    def initialize(codec)
      map = codec.read_map
      @name = map["name"]
      arg_count  = map["argCount"]
      @desc = map["desc"]
      @arguments = []
      arg_count.times { |idx|
        @arguments << SchemaArgument.new(codec, true)
      }
    end

    def to_s
      result = @name + "("
      first = true
      result += @arguments.select { |arg| arg.dir.index(?I) }.join(", ")
      result += ")"
      return result
    end
  end

  class SchemaArgument

    attr_reader :name, :type, :dir, :unit, :min, :max, :maxlen
    attr_reader :desc, :default, :refClass, :refPackage

    def initialize(codec, method_arg)
      map = codec.read_map
      @name    = map["name"]
      @type    = map["type"]
      @dir     = map["dir"].upcase if method_arg
      @unit    = map["unit"]
      @min     = map["min"]
      @max     = map["max"]
      @maxlen  = map["maxlen"]
      @desc    = map["desc"]
      @default = map["default"]
      @refClass = map["refClass"]
      @refPackage = map["refPackage"]            
    end
  end

  # Object that represents QMF object identifiers
  class ObjectId

    include Comparable

    attr_reader :first, :second

    def initialize(codec, first=0, second=0)
      if codec
        @first  = codec.read_uint64
        @second = codec.read_uint64
      else
        @first = first
        @second = second
      end
    end

    def <=>(other)
      return 1 unless other.is_a?(ObjectId)
      return -1 if first < other.first
      return 1  if first > other.first
      return second <=> other.second
    end

    def to_s
      "%d-%d-%d-%d-%d" % [flags, sequence, broker_bank, agent_bank, object]
    end

    def index
      [first, second]
    end

    def flags
      (first & 0xF000000000000000) >> 60
    end

    def sequence
      (first & 0x0FFF000000000000) >> 48
    end

    def broker_bank
      (first & 0x0000FFFFF0000000) >> 28
    end

    def agent_bank
      first & 0x000000000FFFFFFF
    end

    def object
      second
    end

    def durable?
      sequence == 0
    end

    def encode(codec)
      codec.write_uint64(first)
      codec.write_uint64(second)
    end
  end

  class Object

    DEFAULT_METHOD_WAIT_TIME = 60

    attr_reader :object_id, :schema, :properties, :statistics,
    :current_time, :create_time, :delete_time, :broker

    def initialize(session, broker, schema, codec, prop, stat, managed=true)
      @session = session
      @broker  = broker
      @schema  = schema
      if managed
          @current_time = codec.read_uint64
          @create_time  = codec.read_uint64
          @delete_time  = codec.read_uint64
          @object_id   = ObjectId.new(codec)
      end
      @properties  = []
      @statistics  = []
      if prop
        missing = parse_presence_masks(codec, schema)
        schema.properties.each do |property|
          v = nil
          unless missing.include?(property.name)
            v = @session.decode_value(codec, property.type)
          end
          @properties << [property, v]
        end
      end

      if stat
        schema.statistics.each do |statistic|
          s = @session.decode_value(codec, statistic.type)
          @statistics << [statistic, s]
        end
      end
    end

    def klass_key
      @schema.klass_key
    end


    def methods
      @schema.methods
    end

    # Return the current, creation, and deletion times for this object
    def timestamps
      return [@current_time, @create_time, @delete_time]
    end

    # Return a string describing this object's primary key
    def index
      @properties.select { |property, value|
        property.index
      }.collect { |property,value|
        @session.display_value(value, property.type) }.join(":")
    end

    # Replace properties and/or statistics with a newly received update
    def merge_update(newer)
      unless object_id == newer.object_id
        raise "Objects with different object-ids"
      end
      @properties = newer.properties unless newer.properties.empty?
      @statistics = newer.statistics unless newer.statistics.empty?
    end

    def update
      obj = @session.object(:object_id => @object_id, :broker => @broker)
      if obj
        merge_update(obj)
      else
        raise "Underlying object no longer exists."
      end
    end

    def to_s
      @schema.klass_key.to_s
    end

    # This must be defined because ruby has this (deprecated) method built in.
    def id
      method_missing(:id)
    end

    # Same here..
    def type
      method_missing(:type)
    end

    def name
      method_missing(:name)
    end

    def method_missing(name, *args)
      name = name.to_s

      if method = @schema.methods.find { |method| name == method.name }
        return invoke(method, name, args)
      end

      @properties.each do |property, value|
        return value if name == property.name
        if name == "_#{property.name}_" && property.type == 10
          # Dereference references
          deref = @session.objects(:object_id => value, :broker => @broker)
          return nil unless deref.size == 1
          return deref[0]
        end
      end
      @statistics.each do |statistic, value|
        if name == statistic.name
          return value
        end
      end
      raise "Type Object has no attribute '#{name}'"
    end

        def encode(codec)
            codec.write_uint8(20)
            @schema.klass_key.encode(codec)

            # emit presence masks for optional properties
            mask = 0
            bit  = 0
            schema.properties.each do |property|
            if prop.optional
                bit = 1 if bit == 0
                mask |= bit if value
                bit = bit << 1
                if bit == 256
                    bit = 0
                    codec.write_uint8(mask)
                    mask = 0
                end
                codec.write_uint8(mask) if bit != 0
            end
        end

        # encode properties
        @properties.each do |property, value|
          @session.encode_value(codec, value, prop.type) if value
        end

        # encode statistics
        @statistics.each do |statistic, value|
          @session.encode_value(codec, value, stat.type)
        end
    end

    private

    def send_method_request(method, name, args, synchronous = false, time_wait = nil)
      @schema.methods.each do |schema_method|
        if name == schema_method.name
          send_codec = Qpid::StringCodec.new(@broker.conn.spec)
          seq = @session.seq_mgr.reserve([schema_method, synchronous])
          @broker.set_header(send_codec, ?M, seq)
          @object_id.encode(send_codec)
          @schema.klass_key.encode(send_codec)
          send_codec.write_str8(name)

          formals = method.arguments.select { |arg| arg.dir.index(?I) }
          count = method.arguments.select { |arg| arg.dir.index(?I) }.size
          unless formals.size == args.size
            raise "Incorrect number of arguments: expected #{formals.size}, got #{args.size}"
          end

          formals.zip(args).each do |formal, actual|
            @session.encode_value(send_codec, actual, formal.type)
          end

          ttl = time_wait ? time_wait * 1000 : nil
          smsg = @broker.message(send_codec.encoded,
                                 "agent.#{object_id.broker_bank}.#{object_id.agent_bank}", ttl=ttl)
          @broker.sync_start if synchronous
          @broker.emit(smsg)

          return seq
        end
      end
    end

    def invoke(method, name, args)
      kwargs = args[args.size - 1]
      sync = true
      timeout = DEFAULT_METHOD_WAIT_TIME

      if kwargs.class == Hash
        if kwargs.include?(:timeout)
          timeout = kwargs[:timeout]
        end

        if kwargs.include?(:async)
          sync = !kwargs[:async]
        end
        args.pop
      end

      seq = send_method_request(method, name, args, synchronous = sync)
      if seq
        return seq unless sync
        unless @broker.wait_for_sync_done(timeout)
          @session.seq_mgr.release(seq)
          raise "Timed out waiting for method to respond"
        end

        if @broker.error
          error_text = @broker.error
          @broker.error = nil
          raise error_text
        end

        return @broker.sync_result
      end
      raise "Invalid Method (software defect) [#{name}]"
    end

    def parse_presence_masks(codec, schema)
      exclude_list = []
      bit = 0
      schema.properties.each do |property|
        if property.optional
          if bit == 0
            mask = codec.read_uint8
            bit = 1
          end
          if (mask & bit) == 0
            exclude_list << property.name
          end
          bit *= 2
          bit = 0 if bit == 256
        end
      end
      return exclude_list
    end
  end

  class MethodResult

    attr_reader :status, :text, :out_args

    def initialize(status, text, out_args)
      @status   = status
      @text     = text
      @out_args = out_args
    end

    def method_missing(name)
      name = name.to_s()
      if @out_args.include?(name)
        return @out_args[name]
      else
        raise "Unknown method result arg #{name}"
      end
    end

    def to_s
      argsString = ""
      padding = ""
      out_args.each do |key,value|
        argsString += padding
        padding = " " if padding == ""
        argsString += key.to_s
        argsString += " => "
        argsString += value.to_s()
      end
      "MethodResult(Msg: '#{text}' Status: #{status} Return: [#{argsString}])"
    end
  end

  class ManagedConnection

    DELAY_MIN = 1
    DELAY_MAX = 128
    DELAY_FACTOR = 2
    include MonitorMixin

    def initialize(broker)
      super()
      @broker = broker
      @cv = new_cond
      @is_cancelled = false
    end

    # Main body of the running thread.
    def start
      @thread = Thread.new {
        delay = DELAY_MIN
        while true
          begin
            @broker.try_to_connect
            synchronize do
              while !@is_cancelled and @broker.connected?
                @cv.wait
                Thread.exit if @is_cancelled
                delay = DELAY_MIN
              end
            end

          rescue
            delay *= DELAY_FACTOR if delay < DELAY_MAX
          end

          synchronize do
            @cv.wait(delay)
            Thread.exit if @is_cancelled
          end
        end
      }
    end

    # Tell this thread to stop running and return.
    def stop
      synchronize do
        @is_cancelled = true
        @cv.signal
      end
    end

    # Notify the thread that the connection was lost.
    def disconnected
      synchronize do
        @cv.signal
      end
    end

    def join
      @thread.join
    end
  end

  class Broker

    SYNC_TIME = 60
    @@next_seq = 1

    include MonitorMixin

    attr_accessor :error

    attr_reader :amqp_session_id, :amqp_session, :conn, :broker_bank, :topic_name

    attr_accessor :broker_id, :sync_result

    def initialize(session, host, port, auth_name, auth_pass, kwargs)
      super()

      # For debugging..
      Thread.abort_on_exception = true

      @session  = session
      @host     = host
      @port     = port
      @auth_name = auth_name
      @auth_pass = auth_pass
      @user_id = nil
      @auth_mechanism = kwargs[:mechanism]
      @auth_service = kwargs[:service]
      @broker_bank = 1
      @topic_bound = false
      @cv = new_cond
      @error = nil
      @broker_id = nil
      @is_connected = false
      @amqp_session_id = "%s.%d.%d" % [Socket.gethostname, Process::pid, @@next_seq]
      @@next_seq += 1
      @conn = nil
      if @session.managedConnections?
        @thread = ManagedConnection.new(self)
        @thread.start
      else
        @thread = nil
        try_to_connect
      end
    end

    def connected?
      @is_connected
    end

    def agent(broker_bank, agent_bank)
      bank_key = "%d.%d" % [broker_bank, agent_bank]
      return @agents[bank_key]
    end

    # Get the list of agents reachable via this broker
    def agents
      @agents.values
    end

    def url
      "#{@host}:#{@port}"
    end

    def to_s
      if connected?
        "Broker connected at: #{url}"
      else
        "Disconnected Broker"
      end
    end

    def wait_for_sync_done(timeout=nil)
      wait_time = timeout ? timeout : SYNC_TIME
      synchronize do
        return @cv.wait_for(wait_time) { ! @sync_in_flight || @error }
      end
    end

    def wait_for_stable
      synchronize do
        return unless connected?
        return if @reqs_outstanding == 0
        @sync_in_flight = true
        unless @cv.wait_for(SYNC_TIME) { @reqs_outstanding == 0 }
          raise "Timed out waiting for broker to synchronize"
        end
      end
    end

    # Compose the header of a management message
    def set_header(codec, opcode, seq=0)
      codec.write_uint8(?A)
      codec.write_uint8(?M)
      codec.write_uint8(?2)
      codec.write_uint8(opcode)
      codec.write_uint32(seq)
    end

    def message(body, routing_key="broker", ttl=nil)
      dp = @amqp_session.delivery_properties
      dp.routing_key = routing_key
      dp.ttl = ttl if ttl
      mp = @amqp_session.message_properties
      mp.content_type = "x-application/qmf"
      mp.reply_to = amqp_session.reply_to("amq.direct", @reply_name)
      #mp.user_id = @user_id if @user_id
      return Qpid::Message.new(dp, mp, body)
    end

    def emit(msg, dest="qpid.management")
      @amqp_session.message_transfer(:destination => dest,
                                     :message => msg)
    end

    def inc_outstanding
      synchronize { @reqs_outstanding += 1 }
    end

    def dec_outstanding
      synchronize do
        @reqs_outstanding -= 1
        if @reqs_outstanding == 0 && ! @topic_bound
          @topic_bound = true
          @session.binding_key_list.each do |key|
            args = {
              :exchange => "qpid.management",
              :queue => @topic_name,
              :binding_key => key }
            @amqp_session.exchange_bind(args)
          end
        end
        if @reqs_outstanding == 0 && @sync_in_flight
          sync_done
        end
      end
    end

    def sync_start
      synchronize { @sync_in_flight = true }
    end

    def sync_done
      synchronize do
        @sync_in_flight = false
        @cv.signal
      end
    end

    def update_agent(obj)
      bank_key = "%d.%d" % [obj.brokerBank, obj.agentBank]
      if obj.delete_time == 0
        unless @agents.include?(bank_key)
          agent = Agent.new(self, obj.agentBank, obj.label)
          @agents[bank_key] = agent
          @session.console.new_agent(agent) if @session.console
        end
      else
        agent = @agents.delete(bank_key)
        @session.console.del_agent(agent) if agent && @session.console
      end
    end

    def shutdown
      if @thread
        @thread.stop
        @thread.join
      end
      if connected?
        @amqp_session.incoming("rdest").stop
        if @session.console
          @amqp_session.incoming("tdest").stop
        end
        @amqp_session.close
        @is_connected = false
      end
    end

    def try_to_connect
      @agents = {}
      @agents["1.0"] = Agent.new(self, 0, "BrokerAgent")
      @topic_bound = false
      @sync_in_flight = false
      @sync_request = 0
      @sync_result = nil
      @reqs_outstanding = 1

      # FIXME: Need sth for Qpid::Util::connect

      @conn = Qpid::Connection.new(TCPSocket.new(@host, @port),
                                   :mechanism => @auth_mechanism,
                                   :username => @auth_name,
                                   :password => @auth_pass,
                                   :host => @host,
                                   :service => @auth_service)
      @conn.start
      @user_id = @conn.user_id
      @reply_name = "reply-%s" % amqp_session_id
      @amqp_session = @conn.session(@amqp_session_id)
      @amqp_session.auto_sync = true

      @amqp_session.queue_declare(:queue => @reply_name,
                                  :exclusive => true,
                                  :auto_delete => true)

      @amqp_session.exchange_bind(:exchange => "amq.direct",
                                  :queue => @reply_name,
                                  :binding_key => @reply_name)
      @amqp_session.message_subscribe(:queue => @reply_name,
                                      :destination => "rdest",
                                      :accept_mode => @amqp_session.message_accept_mode.none,
                                      :acquire_mode => @amqp_session.message_acquire_mode.pre_acquired)
      q = @amqp_session.incoming("rdest")
      q.exc_listen(& method(:exception_cb))
      q.listen(& method(:reply_cb))
      @amqp_session.message_set_flow_mode(:destination => "rdest",
                                          :flow_mode => 1)
      @amqp_session.message_flow(:destination => "rdest",
                                 :unit => 0,
                                 :value => 0xFFFFFFFF)
      @amqp_session.message_flow(:destination => "rdest",
                                 :unit => 1,
                                 :value => 0xFFFFFFFF)

      @topic_name = "topic-#{@amqp_session_id}"
      @amqp_session.queue_declare(:queue => @topic_name,
                                  :exclusive => true,
                                  :auto_delete => true)
      @amqp_session.message_subscribe(:queue => @topic_name,
                                      :destination => "tdest",
                                      :accept_mode => @amqp_session.message_accept_mode.none,
                                      :acquire_mode => @amqp_session.message_acquire_mode.pre_acquired)
      @amqp_session.incoming("tdest").listen(& method(:reply_cb))
      @amqp_session.message_set_flow_mode(:destination => "tdest",
                                          :flow_mode => 1)
      @amqp_session.message_flow(:destination => "tdest",
                                 :unit => 0,
                                 :value => 0xFFFFFFFF)
      @amqp_session.message_flow(:destination => "tdest",
                                 :unit => 1,
                                 :value => 0xFFFFFFFF)

      @is_connected = true
      @session.handle_broker_connect(self)

      codec = Qpid::StringCodec.new(@conn.spec)
      set_header(codec, ?B)
      msg = message(codec.encoded)
      emit(msg)
    end

    private

    # Check the header of a management message and extract the opcode and
    # class
    def check_header(codec)
      begin
        return [nil, nil] unless codec.read_uint8 == ?A
        return [nil, nil] unless codec.read_uint8 == ?M
        return [nil, nil] unless codec.read_uint8 == ?2
        opcode = codec.read_uint8
        seq    = codec.read_uint32
        return [opcode, seq]
      rescue
        return [nil, nil]
      end
    end

    def reply_cb(msg)
      codec = Qpid::StringCodec.new(@conn.spec, msg.body)
      loop do
        opcode, seq = check_header(codec)
        return unless opcode
        case opcode
        when ?b: @session.handle_broker_resp(self, codec, seq)
        when ?p: @session.handle_package_ind(self, codec, seq)
        when ?z: @session.handle_command_complete(self, codec, seq)
        when ?q: @session.handle_class_ind(self, codec, seq)
        when ?m: @session.handle_method_resp(self, codec, seq)
        when ?h: @session.handle_heartbeat_ind(self, codec, seq, msg)
        when ?e: @session.handle_event_ind(self, codec, seq)
        when ?s: @session.handle_schema_resp(self, codec, seq)
        when ?c: @session.handle_content_ind(self, codec, seq, true, false)
        when ?i: @session.handle_content_ind(self, codec, seq, false, true)
        when ?g: @session.handle_content_ind(self, codec, seq, true, true)
        else
          raise "Unexpected opcode #{opcode.inspect}"
        end
      end
    end

    def exception_cb(data)
      @is_connected = false
      @error = data
      synchronize { @cv.signal if @sync_in_flight }
      @session.handle_error(@error)
      @session.handle_broker_disconnect(self)
      @thread.disconnected if @thread
    end
  end

  class Agent
    attr_reader :broker, :agent_bank, :label

    def initialize(broker, agent_bank, label)
      @broker = broker
      @agent_bank = agent_bank
      @label  = label
    end

    def broker_bank
      @broker.broker_bank
    end

    def to_s
      "Agent at bank %d.%d (%s)" % [@broker.broker_bank, @agent_bank, @label]
    end
  end

  class Event

    attr_reader :klass_key, :arguments, :timestamp, :name, :schema

    def initialize(session, broker, codec)
      @session = session
      @broker  = broker
      @klass_key = ClassKey.new(codec)
      @timestamp = codec.read_int64
      @severity = codec.read_uint8
      @schema = nil

      pname, cname, hash = @klass_key.to_a()
      session.packages.keys.each do |pname|
        k = [cname, hash]
        if session.packages[pname].include?(k)
          @schema = session.packages[pname][k]
          @arguments = {}
          @schema.arguments.each do |arg|
            v = session.decode_value(codec, arg.type)
            @arguments[arg.name] = v
          end
        end
      end
    end

    def to_s
      return "<uninterpretable>" unless @schema
      t = Time.at(self.timestamp / 1000000000)
      out = t.strftime("%c")
      out += " " + sev_name + " " + @klass_key.package + ":" + @klass_key.klass_name
      out += " broker=" + @broker.url
      @schema.arguments.each do |arg|
        out += " " + arg.name + "=" + @session.display_value(@arguments[arg.name], arg.type)
      end
      return out
    end

    def sev_name
      case @severity
      when 0 : return "EMER "
      when 1 : return "ALERT"
      when 2 : return "CRIT "
      when 3 : return "ERROR"
      when 4 : return "WARN "
      when 5 : return "NOTIC"
      when 6 : return "INFO "
      when 7 : return "DEBUG"
      else
        return "INV-%d" % @severity
      end
    end

  end

  # Manage sequence numbers for asynchronous method calls
  class SequenceManager
    include MonitorMixin

    def initialize
      super()
      @sequence = 0
      @pending  = {}
    end

    # Reserve a unique sequence number
    def reserve (data)
      synchronize do
        result = @sequence
        @sequence += 1
        @pending[result] = data
        return result
      end
    end

    # Release a reserved sequence number
    def release (seq)
      synchronize { @pending.delete(seq) }
    end
  end

  class DebugConsole < Console

    def broker_connected(broker)
      puts "brokerConnected #{broker}"
    end

    def broker_disconnected(broker)
      puts "brokerDisconnected #{broker}"
    end

    def new_package(name)
      puts "newPackage #{name}"
    end

    def new_class(kind, klass_key)
      puts "newClass #{kind} #{klass_key}"
    end

    def new_agent(agent)
      puts "new_agent #{agent}"
    end

    def del_agent(agent)
      puts "delAgent #{agent}"
    end

    def object_props(broker, record)
      puts "objectProps #{record}"
    end

    def object_stats(broker, record)
      puts "objectStats #{record}"
    end

    def event(broker, event)
      puts "event #{event}"
    end

    def heartbeat(agent, timestamp)
      puts "heartbeat #{agent}"
    end

    def broker_info(broker)
      puts "brokerInfo #{broker}"
    end
  end

  module XML
      TYPES = {
        1 => "uint8",
        2 => "uint16",
        3 => "uint32",
        4 => "uint64",
        5 => "bool",
        6 => "short-stirng",
        7 => "long-string",
        8 => "abs-time",
        9 => "delta-time",
        10 => "reference",
        11 => "boolean",
        12 => "float",
        13 => "double",
        14 => "uuid",
        15 => "field-table",
        16 => "int8",
        17 => "int16",
        18 => "int32",
        19 => "int64",
        20 => "object",
        21 => "list",
        22 => "array"  
      }

      ACCESS_MODES = {
        1 => "RC",
        2 => "RW",
        3 => "RO"
      }

      def common_attributes(item)
        attr_string = ""
        attr_string << " desc='#{item.desc}'" if item.desc
        attr_string << " desc='#{item.desc}'" if item.desc
        attr_string << " refPackage='#{item.refPackage}'" if item.refPackage
        attr_string << " refClass='#{item.refClass}'" if item.refClass
        attr_string << " unit='#{item.unit}'" if item.unit
        attr_string << " min='#{item.min}'" if item.min
        attr_string << " max='#{item.max}'" if item.max
        attr_string << " maxlen='#{item.maxlen}'" if item.maxlen
        return attr_string
      end

      module_function :common_attributes      
      
      def schema_xml(session, *packages)
        schema = "<schemas>\n"
        packages.each do |package|
            schema << "\t<schema package='#{package}'>\n"
            session.classes(package).each do |klass_key|
                klass = session.schema(klass_key)
                if klass.is_table?
                    if klass.super_klass_key
                        schema << "\t\t<class name='#{klass.klass_key.klass_name}' hash='#{klass.klass_key.hash_string}' extends='#{klass.super_klass_key.to_s}'>\n"
                    else
                        schema << "\t\t<class name='#{klass.klass_key.klass_name}' hash='#{klass.klass_key.hash_string}'>\n"
                    end
                    klass.properties(false).each do |property|
                        schema << "\t\t\t<property name='#{property.name}' type='#{TYPES[property.type]}' access='#{ACCESS_MODES[property.access]}' optional='#{property.optional ? "True" : "False"}'#{common_attributes(property)}/>\n"
                    end
                    klass.methods(false).each do |method|
                        schema << "\t\t\t<method name='#{method.name}'>\n"
                        method.arguments.each do |arg|
                            schema << "\t\t\t\t<arg name='#{arg.name}' dir='#{arg.dir}' type='#{TYPES[arg.type]}'#{common_attributes(arg)}/>\n" 
                        end
                        schema << "\t\t\t</method>\n"                        
                    end
                    schema << "\t\t</class>\n"
                else
                    schema << "\t\t<event name='#{klass.klass_key.klass_name}' hash='#{klass.klass_key.hash_string}'>\n"
                    klass.arguments.each do |arg|
                        schema << "\t\t\t<arg name='#{arg.name}'type='#{TYPES[arg.type]}'#{common_attributes(arg)}/>\n" 
                    end                    
                    schema << "\t\t</event>\n"                    
                end
            end
            schema << "\t</package>\n"
        end    
        schema << "</schema>"
      end

      module_function :schema_xml
  end
  
end
