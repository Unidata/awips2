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

from qpid.content import Content
from qpid.testlib import TestBase

class ExampleTest (TestBase):
    """
    An example Qpid test, illustrating the unittest frameowkr and the
    python Qpid client. The test class must inherit TestCase.  The
    test code uses the Qpid client to interact with a qpid broker and
    verify it behaves as expected.
    """ 

    def test_example(self):
        """
        An example test. Note that test functions must start with 'test_'
        to be recognized by the test framework.
        """

        # By inheriting TestBase, self.client is automatically connected
        # and self.channel is automatically opened as channel(1)
        # Other channel methods mimic the protocol.
        channel = self.channel

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
        self.exchange_declare(channel, 0, "test", "direct")

        # Here we use keyword arguments.
        self.queue_declare(channel, queue="test-queue")
        channel.queue_bind(queue="test-queue", exchange="test", routing_key="key")

        # Call Channel.basic_consume to register as a consumer.
        # All the protocol methods return a message object. The message object
        # has fields corresponding to the reply method fields, plus a content
        # field that is filled if the reply includes content. In this case the
        # interesting field is the consumer_tag.
        reply = channel.basic_consume(queue="test-queue")

        # We can use the Client.queue(...) method to access the queue
        # corresponding to our consumer_tag.
        queue = self.client.queue(reply.consumer_tag)

        # Now lets publish a message and see if our consumer gets it. To do
        # this we need to import the Content class.
        body = "Hello World!"
        channel.basic_publish(exchange="test",
                              routing_key="key",
                              content=Content(body))

        # Now we'll wait for the message to arrive. We can use the timeout
        # argument in case the server hangs. By default queue.get() will wait
        # until a message arrives or the connection to the server dies.
        msg = queue.get(timeout=10)

        # And check that we got the right response with assertEqual
        self.assertEqual(body, msg.content.body)

        # Now acknowledge the message.
        channel.basic_ack(msg.delivery_tag, True)

