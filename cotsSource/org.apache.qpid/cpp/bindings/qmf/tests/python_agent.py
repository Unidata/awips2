#!/usr/bin/env python
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


import qmf
import sys
import time


class Model:
    # attr_reader :parent_class, :child_class
    def __init__(self):
        self.parent_class = qmf.SchemaObjectClass("org.apache.qpid.qmf", "parent")
        self.parent_class.add_property(qmf.SchemaProperty("name", qmf.TYPE_SSTR, {"index":True}))
        self.parent_class.add_property(qmf.SchemaProperty("state", qmf.TYPE_SSTR))

        self.parent_class.add_property(qmf.SchemaProperty("uint64val", qmf.TYPE_UINT64))
        self.parent_class.add_property(qmf.SchemaProperty("uint32val", qmf.TYPE_UINT32))
        self.parent_class.add_property(qmf.SchemaProperty("uint16val", qmf.TYPE_UINT16))
        self.parent_class.add_property(qmf.SchemaProperty("uint8val", qmf.TYPE_UINT8))

        self.parent_class.add_property(qmf.SchemaProperty("int64val", qmf.TYPE_INT64))
        self.parent_class.add_property(qmf.SchemaProperty("int32val", qmf.TYPE_INT32))
        self.parent_class.add_property(qmf.SchemaProperty("int16val", qmf.TYPE_INT16))
        self.parent_class.add_property(qmf.SchemaProperty("int8val", qmf.TYPE_INT8))

        self.parent_class.add_statistic(qmf.SchemaStatistic("queryCount", qmf.TYPE_UINT32, {"unit":"query", "desc":"Query count"}))

        _method = qmf.SchemaMethod("echo", {"desc":"Check responsiveness of the agent object"})
        _method.add_argument(qmf.SchemaArgument("sequence", qmf.TYPE_UINT32, {"dir":qmf.DIR_IN_OUT}))
        self.parent_class.add_method(_method)

        _method = qmf.SchemaMethod("set_numerics", {"desc":"Set the numeric values in the object"})
        _method.add_argument(qmf.SchemaArgument("test", qmf.TYPE_SSTR, {"dir":qmf.DIR_IN}))
        self.parent_class.add_method(_method)

        _method = qmf.SchemaMethod("create_child", {"desc":"Create a new child object"})
        _method.add_argument(qmf.SchemaArgument("child_name", qmf.TYPE_LSTR, {"dir":qmf.DIR_IN}))
        _method.add_argument(qmf.SchemaArgument("child_ref", qmf.TYPE_REF, {"dir":qmf.DIR_OUT}))
        self.parent_class.add_method(_method)

        _method = qmf.SchemaMethod("probe_userid", {"desc":"Return the user-id for this method call"})
        _method.add_argument(qmf.SchemaArgument("userid", qmf.TYPE_SSTR, {"dir":qmf.DIR_OUT}))
        self.parent_class.add_method(_method)

        self.child_class = qmf.SchemaObjectClass("org.apache.qpid.qmf", "child")
        self.child_class.add_property(qmf.SchemaProperty("name", qmf.TYPE_SSTR, {"index":True}))


    def register(self, agent):
        agent.register_class(self.parent_class)
        agent.register_class(self.child_class)



class App(qmf.AgentHandler):
    '''
    Object that handles events received by the Agent.
    '''
    def get_query(self, context, query, userId):
        '''
        Respond to a Query request from a console.
        '''
        #print "Query: user=%s context=%d class=%s" % (userId, context, query.class_name())
        #if query.object_id():
        #    print query.object_id().object_num_low()
        self._parent.inc_attr("queryCount")
        if query.class_name() == 'parent':
            self._agent.query_response(context, self._parent)
        elif query.object_id() == self._parent_oid:
            self._agent.query_response(context, self._parent)
        self._agent.query_complete(context)
    
    
    def method_call(self, context, name, object_id, args, userId):
        '''
        Invoke a method call requested by the console.
        '''
        #print "Method: name=%s user=%s context=%d object_id=%s args=%s" % (name, userId, context, object_id, args)
        if name == "echo":
            self._agent.method_response(context, 0, "OK", args)

        elif name == "set_numerics":
            _retCode = 0
            _retText = "OK"

            if args['test'] == "big":
                #
                # note the alternate forms for setting object attributes:
                #
                self._parent.set_attr("uint64val", 0x9494949449494949)
                self._parent.uint32val = 0xa5a55a5a
                self._parent.set_attr("uint16val", 0xb66b)
                self._parent["uint8val"] = 0xc7

                self._parent.int64val = 1000000000000000000
                self._parent.set_attr("int32val", 1000000000)
                self._parent["int16val"] = 10000
                self._parent.set_attr("int8val",  100)

                ## Test the __getattr__ implementation:
                ## @todo: remove once python_client implements this
                ## form of property access
                assert self._parent["uint8val"] == 0xc7
                assert self._parent.uint64val == 0x9494949449494949

            # note the alternative argument access syntax:
            elif args.test == "small":
                self._parent.set_attr("uint64val", 4)
                self._parent.set_attr("uint32val", 5)
                self._parent.set_attr("uint16val", 6)
                self._parent.set_attr("uint8val",  7)

                self._parent.set_attr("int64val", 8)
                self._parent.set_attr("int32val", 9)
                self._parent.set_attr("int16val", 10)
                self._parent.set_attr("int8val",  11)

            elif args['test'] == "negative":
                self._parent.set_attr("uint64val", 0)
                self._parent.set_attr("uint32val", 0)
                self._parent.set_attr("uint16val", 0)
                self._parent.set_attr("uint8val",  0)

                self._parent.set_attr("int64val", -10000000000)
                self._parent.set_attr("int32val", -100000)
                self._parent.set_attr("int16val", -1000)
                self._parent.set_attr("int8val",  -100)

            else:
                _retCode = 1
                _retText = "Invalid argument value for test"

            self._agent.method_response(context, _retCode, _retText, args)

        elif name == "create_child":
            #
            # Instantiate an object based on the Child Schema Class
            #
            _oid = self._agent.alloc_object_id(2)
            args['child_ref'] = _oid
            self._child = qmf.AgentObject(self._model.child_class)
            self._child.set_attr("name", args["child_name"])
            self._child.set_object_id(_oid)
            self._agent.method_response(context, 0, "OK", args)

        elif name == "probe_userid":
            args['userid'] = userId
            self._agent.method_response(context, 0, "OK", args)

        else:
            self._agent.method_response(context, 1, "Unimplemented Method: %s" % name, args)


    def main(self):
        '''
        Agent application's main processing loop.
        '''
        # Connect to the broker
        self._settings = qmf.ConnectionSettings()
        self._settings.sendUserId = True
        if len(sys.argv) > 1:
            self._settings.host = str(sys.argv[1])
        if len(sys.argv) > 2:
            self._settings.port = int(sys.argv[2])
        self._connection = qmf.Connection(self._settings)

        # Instantiate an Agent to serve me queries and method calls
        self._agent = qmf.Agent(self)

        # Dynamically define the parent and child schemas, then
        # register them with the agent
        self._model = Model()
        self._model.register(self._agent)

        # Tell the agent about our connection to the broker
        self._agent.set_connection(self._connection)

        # Instantiate and populate an instance of the Parent
        # Schema Object
        self._parent = qmf.AgentObject(self._model.parent_class)

        ## @todo how do we force a test failure?
        # verify the properties() and statistics() object methods:
        assert len(self._parent.properties()) == 10
        assert len(self._parent.statistics()) == 1

        self._parent.set_attr("name", "Parent One")
        self._parent.set_attr("state", "OPERATIONAL")

        self._parent.set_attr("uint64val", 0)
        self._parent.set_attr("uint32val", 0)
        self._parent.set_attr("uint16val", 0)
        self._parent.set_attr("uint8val",  0)

        self._parent.set_attr("int64val", 0)
        self._parent.set_attr("int32val", 0)
        self._parent.set_attr("int16val", 0)
        self._parent.set_attr("int8val",  0)

        self._parent_oid = self._agent.alloc_object_id(1)
        self._parent.set_object_id(self._parent_oid)

        # Now wait for events arriving on the connection
        # to the broker...
        while True:
            time.sleep(1000)



app = App()
app.main()

