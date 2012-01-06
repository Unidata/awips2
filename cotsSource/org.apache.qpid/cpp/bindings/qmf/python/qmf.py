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
import sys
import socket
import os
import logging
from threading import Thread
from threading import RLock
from threading import Condition
import qmfengine
from qmfengine import (ACCESS_READ_CREATE, ACCESS_READ_ONLY, ACCESS_READ_WRITE)
from qmfengine import (CLASS_EVENT, CLASS_OBJECT)
from qmfengine import (DIR_IN, DIR_IN_OUT, DIR_OUT)
from qmfengine import (TYPE_ABSTIME, TYPE_ARRAY, TYPE_BOOL, TYPE_DELTATIME,
                       TYPE_DOUBLE, TYPE_FLOAT, TYPE_INT16, TYPE_INT32, TYPE_INT64,
                       TYPE_INT8, TYPE_LIST, TYPE_LSTR, TYPE_MAP, TYPE_OBJECT, 
                       TYPE_REF, TYPE_SSTR, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64, 
                       TYPE_UINT8, TYPE_UUID)
from qmfengine import (O_EQ, O_NE, O_LT, O_LE, O_GT, O_GE, O_RE_MATCH, O_RE_NOMATCH,
                       E_NOT, E_AND, E_OR, E_XOR)

  ##==============================================================================
  ## CONNECTION
  ##==============================================================================

class ConnectionSettings(object):
  #attr_reader :impl
    def __init__(self, url=None):
        if url:
            self.impl = qmfengine.ConnectionSettings(url)
        else:
            self.impl = qmfengine.ConnectionSettings()


    def set_attr(self, key, val):
        if type(val) == str:
            _v = qmfengine.Value(TYPE_LSTR)
            _v.setString(val)
        elif type(val) == int:
            _v = qmfengine.Value(TYPE_UINT32)
            _v.setUint(val)
        elif type(val) == bool:
            _v = qmfengine.Value(TYPE_BOOL)
            _v.setBool(val)
        else:
            raise Exception("Argument error: value for attribute '%s' has unsupported type: %s" % ( key, type(val)))

        good = self.impl.setAttr(key, _v)
        if not good:
            raise Exception("Argument error: unsupported attribute '%s'" % key )


    def get_attr(self, key):
        _v = self.impl.getAttr(key)
        if _v.isString():
            return _v.asString()
        elif _v.isUint():
            return _v.asUint()
        elif _v.isBool():
            return _v.asBool()
        else:
            raise Exception("Argument error: value for attribute '%s' has unsupported type: %s" % ( key, str(_v.getType())))


    def __getattr__(self, name):
        return self.get_attr(name)


    def __setattr__(self, name, value):
        if name == "impl":
            return super.__setattr__(self, name, value)
        return self.set_attr(name, value)



class ConnectionHandler:
    def conn_event_connected(self): None
    def conn_event_disconnected(self, error): None
    def conn_event_visit(self): None
    def sess_event_session_closed(self, context, error): None
    def sess_event_recv(self, context, message): None



class Connection(Thread):
    def __init__(self, settings):
        Thread.__init__(self)
        self._lock = RLock()
        self.impl = qmfengine.ResilientConnection(settings.impl)
        self._sockEngine, self._sock = socket.socketpair(socket.AF_UNIX, socket.SOCK_STREAM)
        self.impl.setNotifyFd(self._sockEngine.fileno())
        self._new_conn_handlers = []
        self._conn_handlers_to_delete = []
        self._conn_handlers = []
        self._connected = False
        self._operational = True
        self.start()


    def destroy(self, timeout=None):
        logging.debug("Destroying Connection...")
        self._operational = False
        self.kick()
        self.join(timeout)
        logging.debug("... Conn Destroyed!" )
        if self.isAlive():
            logging.error("Error: Connection thread '%s' is hung..." % self.getName())


    def connected(self):
        return self._connected


    def kick(self):
        self._sockEngine.send(".")
        # self._sockEngine.flush()  Not available with python?


    def add_conn_handler(self, handler):
        self._lock.acquire()
        try:
            self._new_conn_handlers.append(handler)
        finally:
            self._lock.release()
        self.kick()
    
    
    def del_conn_handler(self, handler):
        self._lock.acquire()
        try:
            self._conn_handlers_to_delete.append(handler)
        finally:
            self._lock.release()
        self.kick()


    def run(self):
        eventImpl = qmfengine.ResilientConnectionEvent()
        new_handlers = []
        del_handlers = []
        bt_count = 0
        
        while self._operational:
            logging.debug("Connection thread waiting for socket data...")
            self._sock.recv(1)
            
            self._lock.acquire()
            try:
                new_handlers = self._new_conn_handlers
                del_handlers = self._conn_handlers_to_delete
                self._new_conn_handlers = []
                self._conn_handlers_to_delete = []
            finally:
                self._lock.release()
            
            for nh in new_handlers:
                self._conn_handlers.append(nh)
                if self._connected:
                    nh.conn_event_connected()
            new_handlers = []

            for dh in del_handlers:
                if dh in self._conn_handlers:
                    self._conn_handlers.remove(dh)
            del_handlers = []
            
            valid = self.impl.getEvent(eventImpl)
            while valid:
                try:
                    if eventImpl.kind == qmfengine.ResilientConnectionEvent.CONNECTED:
                        logging.debug("Connection thread: CONNECTED event received.")
                        self._connected = True
                        for h in self._conn_handlers:
                            h.conn_event_connected()
                        
                    elif eventImpl.kind == qmfengine.ResilientConnectionEvent.DISCONNECTED:
                        logging.debug("Connection thread: DISCONNECTED event received.")
                        self._connected = False
                        for h in self._conn_handlers:
                            h.conn_event_disconnected(eventImpl.errorText)
                        
                    elif eventImpl.kind == qmfengine.ResilientConnectionEvent.SESSION_CLOSED:
                        logging.debug("Connection thread: SESSION_CLOSED event received.")
                        eventImpl.sessionContext.handler.sess_event_session_closed(eventImpl.sessionContext, eventImpl.errorText)
                        
                    elif eventImpl.kind == qmfengine.ResilientConnectionEvent.RECV:
                        logging.debug("Connection thread: RECV event received.")
                        eventImpl.sessionContext.handler.sess_event_recv(eventImpl.sessionContext, eventImpl.message)
                    else:
                        logging.debug("Connection thread received unknown event: '%s'" % str(eventImpl.kind))
                        
                except:
                    import traceback
                    logging.error( "Exception occurred during Connection event processing:" )
                    logging.error( str(sys.exc_info()) )
                    if bt_count < 2:
                        traceback.print_exc()
                        traceback.print_stack()
                        bt_count += 1
                
                self.impl.popEvent()
                valid = self.impl.getEvent(eventImpl)
                
            for h in self._conn_handlers:
                h.conn_event_visit()

        logging.debug("Shutting down Connection thread")



class Session:
    def __init__(self, conn, label, handler):
        self._conn = conn
        self._label = label
        self.handler = handler
        self.handle = qmfengine.SessionHandle()
        result = self._conn.impl.createSession(label, self, self.handle)
    
    
    def destroy(self):
        self._conn.impl.destroySession(self.handle)



  ##==============================================================================
  ## OBJECTS
  ##==============================================================================

class QmfObject(object):
    # attr_reader :impl, :object_class
    def __init__(self, cls, kwargs={}):
        self._cv = Condition()
        self._sync_count = 0
        self._sync_result = None
        self._allow_sets = False
        if kwargs.has_key("broker"):
            self._broker = kwargs["broker"]
        else:
            self._broker = None
        if cls:
            self.object_class = cls
            self.impl = qmfengine.Object(self.object_class.impl)
        elif kwargs.has_key("impl"):
            self.impl = qmfengine.Object(kwargs["impl"])
            self.object_class = SchemaObjectClass(None,
                                                  None,
                                                  {"impl":self.impl.getClass()})
        else:
            raise Exception("Argument error: required parameter ('impl') not supplied")
    
    
    def destroy(self):
        self.impl.destroy()
    
    
    def object_id(self):
        return ObjectId(self.impl.getObjectId())
    
    
    def set_object_id(self, oid):
        self.impl.setObjectId(oid.impl)


    def properties(self):
        list = []
        for prop in self.object_class.properties:
            list.append([prop, self.get_attr(prop.name())])
        return list


    def statistics(self):
        list = []
        for stat in self.object_class.statistics:
            list.append([stat, self.get_attr(stat.name())])
        return list
    
    
    def get_attr(self, name):
        val = self._value(name)
        vType = val.getType()
        if vType == TYPE_UINT8:  return val.asUint()
        elif vType == TYPE_UINT16:  return val.asUint()
        elif vType == TYPE_UINT32:  return val.asUint()
        elif vType == TYPE_UINT64:  return val.asUint64()
        elif vType == TYPE_SSTR:  return val.asString()
        elif vType == TYPE_LSTR:  return val.asString()
        elif vType == TYPE_ABSTIME:  return val.asInt64()
        elif vType == TYPE_DELTATIME:  return val.asUint64()
        elif vType == TYPE_REF:  return ObjectId(val.asObjectId())
        elif vType == TYPE_BOOL:  return val.asBool()
        elif vType == TYPE_FLOAT:  return val.asFloat()
        elif vType == TYPE_DOUBLE:  return val.asDouble()
        elif vType == TYPE_UUID:  return val.asUuid()
        elif vType == TYPE_INT8:  return val.asInt()
        elif vType == TYPE_INT16:  return val.asInt()
        elif vType == TYPE_INT32:  return val.asInt()
        elif vType == TYPE_INT64:  return val.asInt64()
        else:
            # when TYPE_MAP
            # when TYPE_OBJECT
            # when TYPE_LIST
            # when TYPE_ARRAY
            logging.error( "Unsupported type for get_attr? '%s'" % str(val.getType()) )
            return None
    
    
    def set_attr(self, name, v):
        val = self._value(name)
        vType = val.getType()
        if vType == TYPE_UINT8: return val.setUint(v)
        elif vType == TYPE_UINT16: return val.setUint(v)
        elif vType == TYPE_UINT32: return val.setUint(v)
        elif vType == TYPE_UINT64: return val.setUint64(v)
        elif vType == TYPE_SSTR:
            if v: return val.setString(v)
            else: return val.setString('')
        elif vType == TYPE_LSTR:
            if v: return val.setString(v) 
            else: return val.setString('')
        elif vType == TYPE_ABSTIME: return val.setInt64(v)
        elif vType == TYPE_DELTATIME: return val.setUint64(v)
        elif vType == TYPE_REF: return val.setObjectId(v.impl)
        elif vType == TYPE_BOOL: return val.setBool(v)
        elif vType == TYPE_FLOAT: return val.setFloat(v)
        elif vType == TYPE_DOUBLE: return val.setDouble(v)
        elif vType == TYPE_UUID: return val.setUuid(v)
        elif vType == TYPE_INT8: return val.setInt(v)
        elif vType == TYPE_INT16: return val.setInt(v)
        elif vType == TYPE_INT32: return val.setInt(v)
        elif vType == TYPE_INT64: return val.setInt64(v)
        else:
            # when TYPE_MAP
            # when TYPE_OBJECT
            # when TYPE_LIST
            # when TYPE_ARRAY
            logging.error("Unsupported type for get_attr? '%s'" % str(val.getType()))
            return None
    
    
    def __getitem__(self, name):
        return self.get_attr(name)
    
    
    def __setitem__(self, name, value):
        self.set_attr(name, value)
    
    
    def inc_attr(self, name, by=1):
        self.set_attr(name, self.get_attr(name) + by)
    
    
    def dec_attr(self, name, by=1):
        self.set_attr(name, self.get_attr(name) - by)
    
    
    def __setattr__(self, name, value):
        #
        # Ignore the internal attributes, set them normally...
        #
        if (name[0] == '_' or
            name == 'impl' or
            name == 'object_class'):
            return super.__setattr__(self, name, value)

        if not self._allow_sets:
            raise Exception("'Set' operations not permitted on this object")
        #
        # If the name matches a property name, set the value of the property.
        #
        # print "set name=%s" % str(name)
        for prop in self.object_class.properties:
            if prop.name() == name:
                return self.set_attr(name, value)
        #
        # otherwise, check for a statistic set...
        #
        for stat in self.object_class.statistics:
            if stat.name() == name:
                return self.set_attr(name, value)

        # unrecognized name?  should I raise an exception?
        super.__setattr__(self, name, value)


    def __getattr__(self, name, *args):
        #
        # If the name matches a property name, return the value of the property.
        #
        for prop in self.object_class.properties:
            if prop.name() == name:
                return self.get_attr(name)
        #
        # Do the same for statistics
        #
        for stat in self.object_class.statistics:
            if stat.name() == name:
                return self.get_attr(name)
        #
        # If we still haven't found a match for the name, check to see if
        # it matches a method name.  If so, marshall up the arguments into
        # a map, and invoke the method.
        #
        for method in self.object_class.methods:
            if method.name() == name:
                argMap = self._marshall(method, args)
                return lambda name, argMap : self._invokeMethod(name, argMap)

        #
        # This name means nothing to us, pass it up the line to the parent
        # class's handler.
        #
        # print "__getattr__=%s" % str(name)
        super.__getattr__(self, name)


    def _invokeMethod(self, name, argMap):
        """
        Private: Helper function that invokes an object's method, and waits for the result.
        """
        self._cv.acquire()
        try:
            timeout = 30
            self._sync_count = 1
            self.impl.invokeMethod(name, argMap, self)
            if self._broker:
                self._broker.conn.kick()
            self._cv.wait(timeout)
            if self._sync_count == 1:
                raise Exception("Timed out: waiting for response to method call.")
        finally:
            self._cv.release()

        return self._sync_result


    def _method_result(self, result):
        """
        Called to return the result of a method call on an object
        """
        self._cv.acquire();
        try:
            self._sync_result = result
            self._sync_count -= 1
            self._cv.notify()
        finally:
            self._cv.release()


    def _marshall(schema, args):
        '''
        Private: Convert a list of arguments (positional) into a Value object of type "map".
        Used to create the argument parameter for an object's method invokation.
        '''
        # Build a map of the method's arguments
        map = qmfengine.Value(TYPE_MAP)
        for arg in schema.arguments:
            if arg.direction == DIR_IN or arg.direction == DIR_IN_OUT:
                map.insert(arg.name, qmfengine.Value(arg.typecode))

        # install each argument's value into the map
        marshalled = Arguments(map)
        idx = 0
        for arg in schema.arguments:
            if arg.direction == DIR_IN or arg.direction == DIR_IN_OUT:
                if args[idx]:
                    marshalled[arg.name] = args[idx]
                idx += 1

        return marshalled.map


    def _value(self, name):
        val = self.impl.getValue(name)
        if not val:
            raise Exception("Argument error: attribute named '%s' not defined for package %s, class %s" % 
                            (name,
                             self.object_class.impl.getClassKey().getPackageName(),
                             self.object_class.impl.getClassKey().getClassName()))
        return val



class AgentObject(QmfObject):
    def __init__(self, cls, kwargs={}):
        QmfObject.__init__(self, cls, kwargs)
        self._allow_sets = True


    def destroy(self):
        self.impl.destroy()


    def set_object_id(self, oid):
        self.impl.setObjectId(oid.impl)



class ConsoleObject(QmfObject):
    # attr_reader :current_time, :create_time, :delete_time
    def __init__(self, cls, kwargs={}):
        QmfObject.__init__(self, cls, kwargs)
    
    
    def update(self):
        if not self._broker:
            raise Exception("No linkage to broker")
        newer = self._broker.console.objects(Query({"object_id":object_id}))
        if newer.size != 1:
            raise Exception("Expected exactly one update for this object, %d present" % int(newer.size))
        self.merge_update(newer[0])


    def merge_update(self, newObject):
        self.impl.merge(new_object.impl)


    def is_deleted(self):
        return self.impl.isDeleted()


    def key(self): pass



class ObjectId:
    def __init__(self, impl=None):
        if impl:
            self.impl = impl
        else:
            self.impl = qmfengine.ObjectId()
        self.agent_key = "%d.%d" % (self.impl.getBrokerBank(), self.impl.getAgentBank())
    
    
    def object_num_high(self):
        return self.impl.getObjectNumHi()
    
    
    def object_num_low(self):
        return self.impl.getObjectNumLo()
    
    
    def agent_key(self):
        self.agent_key

    def __eq__(self, other):
        if not isinstance(other, self.__class__): return False
        return self.impl == other.impl
    
    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        return self.impl.str()



class Arguments(object):
    def __init__(self, map):
        self.map = map
        self._by_hash = {}
        key_count = self.map.keyCount()
        a = 0
        while a < key_count:
            self._by_hash[self.map.key(a)] = self.by_key(self.map.key(a))
            a += 1
    
    
    def __getitem__(self, key):
        return self._by_hash[key]
    
    
    def __setitem__(self, key, value):
        self._by_hash[key] = value
        self.set(key, value)
    
    
    def __iter__(self):
        return self._by_hash.__iter__


    def __getattr__(self, name):
        if name in self._by_hash:
            return self._by_hash[name]
        return super.__getattr__(self, name)


    def __setattr__(self, name, value):
        #
        # ignore local data members
        #
        if (name[0] == '_' or
            name == 'map'):
            return super.__setattr__(self, name, value)

        if name in self._by_hash:
            self._by_hash[name] = value
            return self.set(name, value)

        return super.__setattr__(self, name, value)


    def by_key(self, key):
        val = self.map.byKey(key)
        vType = val.getType()
        if vType == TYPE_UINT8: return val.asUint()
        elif vType == TYPE_UINT16: return val.asUint()
        elif vType == TYPE_UINT32: return val.asUint()
        elif vType == TYPE_UINT64: return val.asUint64()
        elif vType == TYPE_SSTR: return val.asString()
        elif vType == TYPE_LSTR: return val.asString()
        elif vType == TYPE_ABSTIME:   return val.asInt64()
        elif vType == TYPE_DELTATIME: return val.asUint64()
        elif vType == TYPE_REF:  return ObjectId(val.asObjectId())
        elif vType == TYPE_BOOL: return val.asBool()
        elif vType == TYPE_FLOAT:  return val.asFloat()
        elif vType == TYPE_DOUBLE: return val.asDouble()
        elif vType == TYPE_UUID: return val.asUuid()
        elif vType == TYPE_INT8: return val.asInt()
        elif vType == TYPE_INT16: return val.asInt()
        elif vType == TYPE_INT32: return val.asInt()
        elif vType == TYPE_INT64: return val.asInt64()
        else:
            # when TYPE_MAP
            # when TYPE_OBJECT
            # when TYPE_LIST
            # when TYPE_ARRAY
            logging.error( "Unsupported Type for Get? '%s'" % str(val.getType()))
            return None
    
    
    def set(self, key, value):
        val = self.map.byKey(key)
        vType = val.getType()
        if vType == TYPE_UINT8: return val.setUint(value)
        elif vType == TYPE_UINT16: return val.setUint(value)
        elif vType == TYPE_UINT32: return val.setUint(value)
        elif vType == TYPE_UINT64: return val.setUint64(value)
        elif vType == TYPE_SSTR: 
            if value:
                return val.setString(value)
            else:
                return val.setString('')
        elif vType == TYPE_LSTR:
            if value:
                return val.setString(value)
            else:
                return val.setString('')
        elif vType == TYPE_ABSTIME: return val.setInt64(value)
        elif vType == TYPE_DELTATIME: return val.setUint64(value)
        elif vType == TYPE_REF: return val.setObjectId(value.impl)
        elif vType == TYPE_BOOL: return val.setBool(value)
        elif vType == TYPE_FLOAT: return val.setFloat(value)
        elif vType == TYPE_DOUBLE: return val.setDouble(value)
        elif vType == TYPE_UUID: return val.setUuid(value)
        elif vType == TYPE_INT8: return val.setInt(value)
        elif vType == TYPE_INT16: return val.setInt(value)
        elif vType == TYPE_INT32: return val.setInt(value)
        elif vType == TYPE_INT64: return val.setInt64(value)
        else:
            # when TYPE_MAP
            # when TYPE_OBJECT
            # when TYPE_LIST
            # when TYPE_ARRAY
            logging.error("Unsupported Type for Set? '%s'" % str(val.getType()))
            return None



class MethodResponse(object):
    def __init__(self, impl):
        self.impl = qmfengine.MethodResponse(impl)


    def status(self):
        return self.impl.getStatus()


    def exception(self):
        return self.impl.getException()


    def text(self):
        return exception().asString()


    def args(self):
        return Arguments(self.impl.getArgs())


    def __getattr__(self, name):
        myArgs = self.args()
        return myArgs.__getattr__(name)


    def __setattr__(self, name, value):
        if name == 'impl':
            return super.__setattr__(self, name, value)

        myArgs = self.args()
        return myArgs.__setattr__(name, value)



  ##==============================================================================
  ## QUERY
  ##==============================================================================


class Query:
    def __init__(self, kwargs={}):
        if "impl" in kwargs:
            self.impl = kwargs["impl"]
        else:
            package = ''
            if "key" in kwargs:
                # construct using SchemaClassKey:
                self.impl = qmfengine.Query(kwargs["key"])
            elif "object_id" in kwargs:
                self.impl = qmfengine.Query(kwargs["object_id"].impl)
            else:
                if "package" in kwargs:
                    package = kwargs["package"]
                if "class" in kwargs:
                    self.impl = qmfengine.Query(kwargs["class"], package)
                else:
                    raise Exception("Argument error: invalid arguments, use 'key', 'object_id' or 'class'[,'package']")


    def package_name(self): return self.impl.getPackage()
    def class_name(self): return self.impl.getClass()
    def object_id(self):
        _objid = self.impl.getObjectId()
        if _objid:
            return ObjectId(_objid)
        else:
            return None


  ##==============================================================================
  ## SCHEMA
  ##==============================================================================



class SchemaArgument:
    #attr_reader :impl
    def __init__(self, name, typecode, kwargs={}):
        if "impl" in kwargs:
            self.impl = kwargs["impl"]
        else:
            self.impl = qmfengine.SchemaArgument(name, typecode)
            if kwargs.has_key("dir"):  self.impl.setDirection(kwargs["dir"])
            if kwargs.has_key("unit"): self.impl.setUnit(kwargs["unit"])
            if kwargs.has_key("desc"): self.impl.setDesc(kwargs["desc"])


    def name(self):
        return self.impl.getName()


    def direction(self):
        return self.impl.getDirection()


    def typecode(self):
        return self.impl.getType()


    def __repr__(self):
        return self.name()



class SchemaMethod:
    # attr_reader :impl, arguments
    def __init__(self, name, kwargs={}):
        self.arguments = []
        if "impl" in kwargs:
            self.impl = kwargs["impl"]
            for i in range(self.impl.getArgumentCount()):
                self.arguments.append(SchemaArgument(None,None,{"impl":self.impl.getArgument(i)}))
        else:
            self.impl = qmfengine.SchemaMethod(name)
            if kwargs.has_key("desc"): self.impl.setDesc(kwargs["desc"])
    
    
    def add_argument(self, arg):
        self.arguments.append(arg)
        self.impl.addArgument(arg.impl)

    def name(self):
        return self.impl.getName()

    def __repr__(self):
        return self.name()



class SchemaProperty:
    #attr_reader :impl
    def __init__(self, name, typecode, kwargs={}):
        if "impl" in kwargs:
            self.impl = kwargs["impl"]
        else:
            self.impl = qmfengine.SchemaProperty(name, typecode)
            if kwargs.has_key("access"):   self.impl.setAccess(kwargs["access"])
            if kwargs.has_key("index"):    self.impl.setIndex(kwargs["index"])
            if kwargs.has_key("optional"): self.impl.setOptional(kwargs["optional"])
            if kwargs.has_key("unit"):     self.impl.setUnit(kwargs["unit"])
            if kwargs.has_key("desc"):     self.impl.setDesc(kwargs["desc"])
    
    
    def name(self):
        return self.impl.getName()

    def __repr__(self):
        return self.name()



class SchemaStatistic:
    # attr_reader :impl
    def __init__(self, name, typecode, kwargs={}):
        if "impl" in kwargs:
            self.impl = kwargs["impl"]
        else:
            self.impl = qmfengine.SchemaStatistic(name, typecode)
            if kwargs.has_key("unit"): self.impl.setUnit(kwargs["unit"])
            if kwargs.has_key("desc"): self.impl.setDesc(kwargs["desc"])


    def name(self):
        return self.impl.getName()

    def __repr__(self):
        return self.name()



class SchemaClassKey:
    #attr_reader :impl
    def __init__(self, i):
        self.impl = i
    
    
    def package_name(self):
        return self.impl.getPackageName()
    
    
    def class_name(self):
        return self.impl.getClassName()

    def __repr__(self):
        return self.impl.asString()



class SchemaObjectClass:
    # attr_reader :impl, :properties, :statistics, :methods
    def __init__(self, package, name, kwargs={}):
        self.properties = []
        self.statistics = []
        self.methods = []
        if "impl" in kwargs:
            self.impl = kwargs["impl"]

            for i in range(self.impl.getPropertyCount()):
                self.properties.append(SchemaProperty(None, None, {"impl":self.impl.getProperty(i)}))

            for i in range(self.impl.getStatisticCount()):
                self.statistics.append(SchemaStatistic(None, None, {"impl":self.impl.getStatistic(i)}))

            for i in range(self.impl.getMethodCount()):
                self.methods.append(SchemaMethod(None, {"impl":self.impl.getMethod(i)}))
        else:
            self.impl = qmfengine.SchemaObjectClass(package, name)
    
    
    def add_property(self, prop):
        self.properties.append(prop)
        self.impl.addProperty(prop.impl)
    
    
    def add_statistic(self, stat):
        self.statistics.append(stat)
        self.impl.addStatistic(stat.impl)
    
    
    def add_method(self, meth):
        self.methods.append(meth)
        self.impl.addMethod(meth.impl)
    
    
    def class_key(self):
        return SchemaClassKey(self.impl.getClassKey())


    def package_name(self):
        return self.impl.getClassKey().getPackageName()


    def class_name(self):
        return self.impl.getClassKey().getClassName()



class SchemaEventClass:
    # attr_reader :impl :arguments
    def __init__(self, package, name, kwargs={}):
      self.arguments = []
      if "impl" in kwargs:
          self.impl = kwargs["impl"]
          for i in range(self.impl.getArgumentCount()):
              self.arguments.append(SchemaArgument(nil, nil, {"impl":self.impl.getArgument(i)}))
      else:
          self.impl = qmfengine.SchemaEventClass(package, name)
          if kwargs.has_key("desc"): self.impl.setDesc(kwargs["desc"])
    
    
    def add_argument(self, arg):
        self.arguments.append(arg)
        self.impl.addArgument(arg.impl)


    def name(self):
        return self.impl.getClassKey().getClassName()


  ##==============================================================================
  ## CONSOLE
  ##==============================================================================



class ConsoleHandler:
    def agent_added(self, agent): pass
    def agent_deleted(self, agent): pass
    def new_package(self, package): pass
    def new_class(self, class_key): pass
    def object_update(self, obj, hasProps, hasStats): pass
    def event_received(self, event): pass
    def agent_heartbeat(self, agent, timestamp): pass
    def method_response(self, resp): pass
    def broker_info(self, broker): pass



class Console(Thread):
    #   attr_reader :impl
    def __init__(self, handler=None, kwargs={}):
        Thread.__init__(self)
        self._handler = handler
        self.impl = qmfengine.Console()
        self._event = qmfengine.ConsoleEvent()
        self._broker_list = []
        self._cv = Condition()
        self._sync_count = 0
        self._sync_result = None
        self._select = {}
        self._cb_cond = Condition()
        self._operational = True
        self.start()


    def destroy(self, timeout=None):
        logging.debug("Destroying Console...")
        self._operational = False
        self.start_console_events()  # wake thread up
        self.join(timeout)
        logging.debug("... Console Destroyed!")
        if self.isAlive():
            logging.error( "Console thread '%s' is hung..." % self.getName() )

    
    def add_connection(self, conn):
        broker = Broker(self, conn)
        self._cv.acquire()
        try:
            self._broker_list.append(broker)
        finally:
            self._cv.release()
        return broker
    
    
    def del_connection(self, broker):
        logging.debug("shutting down broker...")
        broker.shutdown()
        logging.debug("...broker down.")
        self._cv.acquire()
        try:
            self._broker_list.remove(broker)
        finally:
            self._cv.release()
        logging.debug("del_connection() finished")
    
    
    def packages(self):
        plist = []
        for i in range(self.impl.packageCount()):
            plist.append(self.impl.getPackageName(i))
        return plist
    
    
    def classes(self, package, kind=CLASS_OBJECT):
        clist = []
        for i in range(self.impl.classCount(package)):
            key = self.impl.getClass(package, i)
            class_kind = self.impl.getClassKind(key)
            if class_kind == kind:
                if kind == CLASS_OBJECT:
                    clist.append(SchemaObjectClass(None, None, {"impl":self.impl.getObjectClass(key)}))
                elif kind == CLASS_EVENT:
                    clist.append(SchemaEventClass(None, None, {"impl":self.impl.getEventClass(key)}))
        return clist
    
    
    def bind_package(self, package):
        return self.impl.bindPackage(package)
    
    
    def bind_class(self, kwargs = {}):
        if "key" in kwargs:
            self.impl.bindClass(kwargs["key"])
        elif "package" in kwargs:
            package = kwargs["package"]
            if "class" in kwargs:
                self.impl.bindClass(package, kwargs["class"])
            else:
                self.impl.bindClass(package)
        else:
            raise Exception("Argument error: invalid arguments, use 'key' or 'package'[,'class']")
    
    
    def agents(self, broker=None):
        blist = []
        if broker:
            blist.append(broker)
        else:
            self._cv.acquire()
            try:
                # copy while holding lock
                blist = self._broker_list[:]
            finally:
                self._cv.release()

        agents = []
        for b in blist:
            for idx in range(b.impl.agentCount()):
                agents.append(AgentProxy(b.impl.getAgent(idx), b))

        return agents
    
    
    def objects(self, query, kwargs = {}):
        timeout = 30
        agent = None
        temp_args = kwargs.copy()
        if type(query) == type({}):
            temp_args.update(query)

        if "_timeout" in temp_args:
            timeout = temp_args["_timeout"]
            temp_args.pop("_timeout")

        if "_agent" in temp_args:
            agent = temp_args["_agent"]
            temp_args.pop("_agent")

        if type(query) == type({}):
            query = Query(temp_args)

        self._select = {}
        for k in temp_args.iterkeys():
            if type(k) == str:
                self._select[k] = temp_args[k]

        self._cv.acquire()
        try:
            self._sync_count = 1
            self._sync_result = []
            broker = self._broker_list[0]
            broker.send_query(query.impl, None, agent)
            self._cv.wait(timeout)
            if self._sync_count == 1:
                raise Exception("Timed out: waiting for query response")
        finally:
            self._cv.release()

        return self._sync_result
    
    
    def object(self, query, kwargs = {}):
        '''
        Return one and only one object or None.
        '''
        objs = objects(query, kwargs)
        if len(objs) == 1:
            return objs[0]
        else:
            return None


    def first_object(self, query, kwargs = {}):
        '''
        Return the first of potentially many objects.
        '''
        objs = objects(query, kwargs)
        if objs:
            return objs[0]
        else:
            return None


    # Check the object against select to check for a match
    def _select_match(self, object):
        schema_props = object.properties()
        for key in self._select.iterkeys():
            for prop in schema_props:
                if key == p[0].name() and self._select[key] != p[1]:
                    return False
        return True


    def _get_result(self, list, context):
        '''
        Called by Broker proxy to return the result of a query.
        '''
        self._cv.acquire()
        try:
            for item in list:
                if self._select_match(item):
                    self._sync_result.append(item)
            self._sync_count -= 1
            self._cv.notify()
        finally:
            self._cv.release()


    def start_sync(self, query): pass
    
    
    def touch_sync(self, sync): pass
    
    
    def end_sync(self, sync): pass
    
    
    def run(self):
        while self._operational:
            self._cb_cond.acquire()
            try:
                self._cb_cond.wait(1)
                while self._do_console_events():
                    pass
            finally:
                self._cb_cond.release()
        logging.debug("Shutting down Console thread")


    def start_console_events(self):
        self._cb_cond.acquire()
        try:
            self._cb_cond.notify()
        finally:
            self._cb_cond.release()


    def _do_console_events(self):
        '''
        Called by the Console thread to poll for events.  Passes the events
        onto the ConsoleHandler associated with this Console.  Is called
        periodically, but can also be kicked by Console.start_console_events().
        '''
        count = 0
        valid = self.impl.getEvent(self._event)
        while valid:
            count += 1
            try:
                if self._event.kind == qmfengine.ConsoleEvent.AGENT_ADDED:
                    logging.debug("Console Event AGENT_ADDED received")
                    if self._handler:
                        self._handler.agent_added(AgentProxy(self._event.agent, None))
                elif self._event.kind == qmfengine.ConsoleEvent.AGENT_DELETED:
                    logging.debug("Console Event AGENT_DELETED received")
                    if self._handler:
                        self._handler.agent_deleted(AgentProxy(self._event.agent, None))
                elif self._event.kind == qmfengine.ConsoleEvent.NEW_PACKAGE:
                    logging.debug("Console Event NEW_PACKAGE received")
                    if self._handler:
                        self._handler.new_package(self._event.name)
                elif self._event.kind == qmfengine.ConsoleEvent.NEW_CLASS:
                    logging.debug("Console Event NEW_CLASS received")
                    if self._handler:
                        self._handler.new_class(SchemaClassKey(self._event.classKey))
                elif self._event.kind == qmfengine.ConsoleEvent.OBJECT_UPDATE:
                    logging.debug("Console Event OBJECT_UPDATE received")
                    if self._handler:
                        self._handler.object_update(ConsoleObject(None, {"impl":self._event.object}),
                                                    self._event.hasProps, self._event.hasStats)
                elif self._event.kind == qmfengine.ConsoleEvent.EVENT_RECEIVED:
                    logging.debug("Console Event EVENT_RECEIVED received")
                elif self._event.kind == qmfengine.ConsoleEvent.AGENT_HEARTBEAT:
                    logging.debug("Console Event AGENT_HEARTBEAT received")
                    if self._handler:
                        self._handler.agent_heartbeat(AgentProxy(self._event.agent, None), self._event.timestamp)
                elif self._event.kind == qmfengine.ConsoleEvent.METHOD_RESPONSE:
                    logging.debug("Console Event METHOD_RESPONSE received")
                else:
                    logging.debug("Console thread received unknown event: '%s'" % str(self._event.kind))
            except e:
                print "Exception caught in callback thread:", e
            self.impl.popEvent()
            valid = self.impl.getEvent(self._event)
        return count



class AgentProxy:
    # attr_reader :broker
    def __init__(self, impl, broker):
        self.impl = impl
        self.broker = broker
        self.key = "%d.%d" % (self.impl.getBrokerBank(), self.impl.getAgentBank())


    def label(self):
        return self.impl.getLabel()


    def key(self):
        return self.key


class Broker(ConnectionHandler):
    #   attr_reader :impl :conn, :console, :broker_bank
    def __init__(self, console, conn):
        self.broker_bank = 1
        self.console = console
        self.conn = conn
        self._session = None
        self._cv = Condition()
        self._stable = None
        self._event = qmfengine.BrokerEvent()
        self._xmtMessage = qmfengine.Message()
        self.impl = qmfengine.BrokerProxy(self.console.impl)
        self.console.impl.addConnection(self.impl, self)
        self.conn.add_conn_handler(self)
        self._operational = True
    
    
    def shutdown(self):
        logging.debug("broker.shutdown() called.")
        self.console.impl.delConnection(self.impl)
        self.conn.del_conn_handler(self)
        if self._session:
            self.impl.sessionClosed()
            logging.debug("broker.shutdown() sessionClosed done.")
            self._session.destroy()
            logging.debug("broker.shutdown() session destroy done.")
            self._session = None
        self._operational = False
        logging.debug("broker.shutdown() done.")


    def wait_for_stable(self, timeout = None):
        self._cv.acquire()
        try:
            if self._stable:
                return
            if timeout:
                self._cv.wait(timeout)
                if not self._stable:
                    raise Exception("Timed out: waiting for broker connection to become stable")
            else:
                while not self._stable:
                    self._cv.wait()
        finally:
            self._cv.release()


    def send_query(self, query, ctx, agent):
        agent_impl = None
        if agent:
            agent_impl = agent.impl
        self.impl.sendQuery(query, ctx, agent_impl)
        self.conn.kick()


    def _do_broker_events(self):
        count = 0
        valid = self.impl.getEvent(self._event)
        while valid:
            count += 1
            if self._event.kind == qmfengine.BrokerEvent.BROKER_INFO:
                logging.debug("Broker Event BROKER_INFO received");
            elif self._event.kind == qmfengine.BrokerEvent.DECLARE_QUEUE:
                logging.debug("Broker Event DECLARE_QUEUE received");
                self.conn.impl.declareQueue(self._session.handle, self._event.name)
            elif self._event.kind == qmfengine.BrokerEvent.DELETE_QUEUE:
                logging.debug("Broker Event DELETE_QUEUE received");
                self.conn.impl.deleteQueue(self._session.handle, self._event.name)
            elif self._event.kind == qmfengine.BrokerEvent.BIND:
                logging.debug("Broker Event BIND received");
                self.conn.impl.bind(self._session.handle, self._event.exchange, self._event.name, self._event.bindingKey)
            elif self._event.kind == qmfengine.BrokerEvent.UNBIND:
                logging.debug("Broker Event UNBIND received");
                self.conn.impl.unbind(self._session.handle, self._event.exchange, self._event.name, self._event.bindingKey)
            elif self._event.kind == qmfengine.BrokerEvent.SETUP_COMPLETE:
                logging.debug("Broker Event SETUP_COMPLETE received");
                self.impl.startProtocol()
            elif self._event.kind == qmfengine.BrokerEvent.STABLE:
                logging.debug("Broker Event STABLE received");
                self._cv.acquire()
                try:
                    self._stable = True
                    self._cv.notify()
                finally:
                    self._cv.release()
            elif self._event.kind == qmfengine.BrokerEvent.QUERY_COMPLETE:
                result = []
                for idx in range(self._event.queryResponse.getObjectCount()):
                    result.append(ConsoleObject(None, {"impl":self._event.queryResponse.getObject(idx), "broker":self}))
                self.console._get_result(result, self._event.context)
            elif self._event.kind == qmfengine.BrokerEvent.METHOD_RESPONSE:
                obj = self._event.context
                obj._method_result(MethodResponse(self._event.methodResponse()))
            
            self.impl.popEvent()
            valid = self.impl.getEvent(self._event)
        
        return count
    
    
    def _do_broker_messages(self):
        count = 0
        valid = self.impl.getXmtMessage(self._xmtMessage)
        while valid:
            count += 1
            logging.debug("Broker: sending msg on connection")
            self.conn.impl.sendMessage(self._session.handle, self._xmtMessage)
            self.impl.popXmt()
            valid = self.impl.getXmtMessage(self._xmtMessage)
        
        return count
    
    
    def _do_events(self):
        while True:
            self.console.start_console_events()
            bcnt = self._do_broker_events()
            mcnt = self._do_broker_messages()
            if bcnt == 0 and mcnt == 0:
                break;
    
    
    def conn_event_connected(self):
        logging.debug("Broker: Connection event CONNECTED")
        self._session = Session(self.conn, "qmfc-%s.%d" % (socket.gethostname(), os.getpid()), self)
        self.impl.sessionOpened(self._session.handle)
        self._do_events()
    
    
    def conn_event_disconnected(self, error):
        logging.debug("Broker: Connection event DISCONNECTED")
        pass
    
    
    def conn_event_visit(self):
        self._do_events()


    def sess_event_session_closed(self, context, error):
        logging.debug("Broker: Session event CLOSED")
        self.impl.sessionClosed()
    
    
    def sess_event_recv(self, context, message):
        logging.debug("Broker: Session event MSG_RECV")
        if not self._operational:
            logging.warning("Unexpected session event message received by Broker proxy: context='%s'" % str(context))
        self.impl.handleRcvMessage(message)
        self._do_events()



  ##==============================================================================
  ## AGENT
  ##==============================================================================



class AgentHandler:
    def get_query(self, context, query, userId): None
    def method_call(self, context, name, object_id, args, userId): None



class Agent(ConnectionHandler):
    def __init__(self, handler, label=""):
        if label == "":
            self._agentLabel = "rb-%s.%d" % (socket.gethostname(), os.getpid())
        else:
            self._agentLabel = label
        self._conn = None
        self._handler = handler
        self.impl = qmfengine.Agent(self._agentLabel)
        self._event = qmfengine.AgentEvent()
        self._xmtMessage = qmfengine.Message()
    
    
    def set_connection(self, conn):
        self._conn = conn
        self._conn.add_conn_handler(self)
    
    
    def register_class(self, cls):
        self.impl.registerClass(cls.impl)
    
    
    def alloc_object_id(self, low = 0, high = 0):
        return ObjectId(self.impl.allocObjectId(low, high))
    
    
    def query_response(self, context, obj):
        self.impl.queryResponse(context, obj.impl)
    
    
    def query_complete(self, context):
        self.impl.queryComplete(context)
    
    
    def method_response(self, context, status, text, arguments):
        self.impl.methodResponse(context, status, text, arguments.map)
    
    
    def do_agent_events(self):
        count = 0
        valid = self.impl.getEvent(self._event)
        while valid:
            count += 1
            if self._event.kind == qmfengine.AgentEvent.GET_QUERY:
                self._handler.get_query(self._event.sequence,
                                        Query({"impl":self._event.query}),
                                        self._event.authUserId)
                
            elif self._event.kind == qmfengine.AgentEvent.START_SYNC:
                pass
            elif self._event.kind == qmfengine.AgentEvent.END_SYNC:
                pass
            elif self._event.kind == qmfengine.AgentEvent.METHOD_CALL:
                args = Arguments(self._event.arguments)
                self._handler.method_call(self._event.sequence, self._event.name,
                                          ObjectId(self._event.objectId),
                                          args, self._event.authUserId)
                
            elif self._event.kind == qmfengine.AgentEvent.DECLARE_QUEUE:
                self._conn.impl.declareQueue(self._session.handle, self._event.name)
                
            elif self._event.kind == qmfengine.AgentEvent.DELETE_QUEUE:
                self._conn.impl.deleteQueue(self._session.handle, self._event.name)
                
            elif self._event.kind == qmfengine.AgentEvent.BIND:
                self._conn.impl.bind(self._session.handle, self._event.exchange,
                                     self._event.name, self._event.bindingKey)
                
            elif self._event.kind == qmfengine.AgentEvent.UNBIND:
                self._conn.impl.unbind(self._session.handle, self._event.exchange,
                                       self._event.name, self._event.bindingKey)
                
            elif self._event.kind == qmfengine.AgentEvent.SETUP_COMPLETE:
                self.impl.startProtocol()
                
            self.impl.popEvent()
            valid = self.impl.getEvent(self._event)
        return count
    
    
    def do_agent_messages(self):
        count = 0
        valid = self.impl.getXmtMessage(self._xmtMessage)
        while valid:
            count += 1
            self._conn.impl.sendMessage(self._session.handle, self._xmtMessage)
            self.impl.popXmt()
            valid = self.impl.getXmtMessage(self._xmtMessage)
        return count
    
    
    def do_events(self):
        while True:
            ecnt = self.do_agent_events()
            mcnt = self.do_agent_messages()
            if ecnt == 0 and mcnt == 0:
                break
    
    
    def conn_event_connected(self):
        logging.debug("Agent Connection Established...")
        self._session = Session(self._conn,
                                "qmfa-%s.%d" % (socket.gethostname(), os.getpid()),
                                self)
        self.impl.newSession()
        self.do_events()
    
    
    def conn_event_disconnected(self, error):
        logging.debug("Agent Connection Lost")
        pass
    
    
    def conn_event_visit(self):
        self.do_events()


    def sess_event_session_closed(self, context, error):
        logging.debug("Agent Session Lost")
        pass
    
    
    def sess_event_recv(self, context, message):
        self.impl.handleRcvMessage(message)
        self.do_events()


