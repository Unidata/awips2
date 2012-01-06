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

from qpid.datatypes import Message, RangedSet
from qpid.testlib import TestBase010

class ExampleTest (TestBase010):
    """
    An example Qpid test, illustrating the unittest framework and the
    python Qpid client. The test class must inherit TestBase.  The
    test code uses the Qpid client to interact with a qpid broker and
    verify it behaves as expected.
    """ 

    def test_example(self):
        """
        An example test. Note that test functions must start with 'test_'
        to be recognized by the test framework.
        """

        # By inheriting TestBase, self.client is automatically connected
        # and self.session is automatically opened as session(1)
        # Other session methods mimic the protocol.
        session = self.session

        # Now we can send regular commands. If you want to see what the method
        # arguments mean or what other commands are available, you can use the
        # python builtin help() method. For example:
        #help(chan)
        #help(chan.exchange_declare)

        # If you want browse the available protocol methods without being
        # connected to a live server you can use the amqp-doc utility:
        #
        #   Usage amqp-doc [<options>] <spec> [<pattern_1> ... <pattern_n>]
        #
        #   Options:
        #       -e, --regexp    use regex instead of glob when matching

        # Now that we know what commands are available we can use them to
        # interact with the server.

        # Here we use ordinal arguments.
        session.exchange_declare("test", "direct")

        # Here we use keyword arguments.
        session.queue_declare(queue="test-queue", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="test-queue", exchange="test", binding_key="key")

        # Call Session.subscribe to register as a consumer.
        # All the protocol methods return a message object. The message object
        # has fields corresponding to the reply method fields, plus a content
        # field that is filled if the reply includes content. In this case the
        # interesting field is the consumer_tag.
        session.message_subscribe(queue="test-queue", destination="consumer_tag")
        session.message_flow(destination="consumer_tag", unit=session.credit_unit.message, value=0xFFFFFFFFL)
        session.message_flow(destination="consumer_tag", unit=session.credit_unit.byte, value=0xFFFFFFFFL)

        # We can use the session.incoming(...) method to access the messages
        # delivered for our consumer_tag.
        queue = session.incoming("consumer_tag")

        # Now lets publish a message and see if our consumer gets it. To do
        # this we need to import the Message class.
        delivery_properties = session.delivery_properties(routing_key="key")
        sent = Message(delivery_properties, "Hello World!")
        session.message_transfer(destination="test", message=sent)

        # Now we'll wait for the message to arrive. We can use the timeout
        # argument in case the server hangs. By default queue.get() will wait
        # until a message arrives or the connection to the server dies.
        msg = queue.get(timeout=10)

        # And check that we got the right response with assertEqual
        self.assertEqual(sent.body, msg.body)

        # Now acknowledge the message.
        session.message_accept(RangedSet(msg.id))

