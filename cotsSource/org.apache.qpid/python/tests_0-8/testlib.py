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

#
# Tests for the testlib itself.
# 

from qpid.content import Content
from qpid.testlib import TestBase
from Queue import Empty

import sys
from traceback import *

def mytrace(frame, event, arg):
    print_stack(frame);
    print "===="
    return mytrace
    
class TestBaseTest(TestBase):
    """Verify TestBase functions work as expected""" 

    def testAssertEmptyPass(self):
        """Test assert empty works"""
        self.queue_declare(queue="empty")
        q = self.consume("empty")
        self.assertEmpty(q)
        try:
            q.get(timeout=1)
            self.fail("Queue is not empty.")
        except Empty: None              # Ignore

    def testAssertEmptyFail(self):
        self.queue_declare(queue="full")
        q = self.consume("full")
        self.channel.basic_publish(routing_key="full")
        try:
            self.assertEmpty(q);
            self.fail("assertEmpty did not assert on non-empty queue")
        except AssertionError: None     # Ignore

    def testMessageProperties(self):
        """Verify properties are passed with message"""
        props={"headers":{"x":1, "y":2}}
        self.queue_declare(queue="q")
        q = self.consume("q")
        self.assertPublishGet(q, routing_key="q", properties=props)



