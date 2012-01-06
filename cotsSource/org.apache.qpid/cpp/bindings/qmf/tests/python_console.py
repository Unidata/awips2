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

import sys
from qpid.testlib import TestBase010
from qpid.datatypes import Message
from qpid.queue import Empty
from time import sleep

class QmfInteropTests(TestBase010):

    def test_A_agent_presence(self):
        self.startQmf();
        qmf = self.qmf

        agents = []
        count = 0
        while len(agents) == 0:
            agents = qmf.getObjects(_class="agent")
            sleep(1)
            count += 1
            if count > 10:
                self.fail("Timed out waiting for remote agent")

    def test_B_basic_method_invocation(self):
        self.startQmf();
        qmf = self.qmf

        parents = qmf.getObjects(_class="parent")
        self.assertEqual(len(parents), 1)
        parent = parents[0]
        for seq in range(10):
            result = parent.echo(seq)
            self.assertEqual(result.status, 0)
            self.assertEqual(result.text, "OK")
            self.assertEqual(result.sequence, seq)

        result = parent.set_numerics("bogus")
        self.assertEqual(result.status, 1)
        self.assertEqual(result.text, "Invalid argument value for test")

    def test_C_basic_types_numeric_big(self):
        self.startQmf();
        qmf = self.qmf

        parents = qmf.getObjects(_class="parent")
        self.assertEqual(len(parents), 1)
        parent = parents[0]

        result = parent.set_numerics("big")
        self.assertEqual(result.status, 0)
        self.assertEqual(result.text, "OK")

        parent.update()

        self.assertEqual(parent.uint64val, 0x9494949449494949)
        self.assertEqual(parent.uint32val, 0xA5A55A5A)
        self.assertEqual(parent.uint16val, 0xB66B)
        self.assertEqual(parent.uint8val,  0xC7)

        self.assertEqual(parent.int64val, 1000000000000000000)
        self.assertEqual(parent.int32val, 1000000000)
        self.assertEqual(parent.int16val, 10000)
        self.assertEqual(parent.int8val,  100)

    def test_C_basic_types_numeric_small(self):
        self.startQmf();
        qmf = self.qmf

        parents = qmf.getObjects(_class="parent")
        self.assertEqual(len(parents), 1)
        parent = parents[0]

        result = parent.set_numerics("small")
        self.assertEqual(result.status, 0)
        self.assertEqual(result.text, "OK")

        parent.update()

        self.assertEqual(parent.uint64val, 4)
        self.assertEqual(parent.uint32val, 5)
        self.assertEqual(parent.uint16val, 6)
        self.assertEqual(parent.uint8val,  7)

        self.assertEqual(parent.int64val, 8)
        self.assertEqual(parent.int32val, 9)
        self.assertEqual(parent.int16val, 10)
        self.assertEqual(parent.int8val,  11)

    def test_C_basic_types_numeric_negative(self):
        self.startQmf();
        qmf = self.qmf

        parents = qmf.getObjects(_class="parent")
        self.assertEqual(len(parents), 1)
        parent = parents[0]

        result = parent.set_numerics("negative")
        self.assertEqual(result.status, 0)
        self.assertEqual(result.text, "OK")

        parent.update()

        self.assertEqual(parent.uint64val, 0)
        self.assertEqual(parent.uint32val, 0)
        self.assertEqual(parent.uint16val, 0)
        self.assertEqual(parent.uint8val,  0)

        self.assertEqual(parent.int64val, -10000000000)
        self.assertEqual(parent.int32val, -100000)
        self.assertEqual(parent.int16val, -1000)
        self.assertEqual(parent.int8val,  -100)

    def disabled_test_D_userid_for_method(self):
        self.startQmf();
        qmf = self.qmf

        parents = qmf.getObjects(_class="parent")
        self.assertEqual(len(parents), 1)
        parent = parents[0]

        result = parent.probe_userid()
        self.assertEqual(result.status, 0)
        self.assertEqual(result.userid, "guest")

    def test_D_get_by_object_id(self):
        self.startQmf()
        qmf = self.qmf

        parents = qmf.getObjects(_class="parent")
        self.assertEqual(len(parents), 1)
        parent = parents[0]

        newList = qmf.getObjects(_objectId=parent.getObjectId())
        self.assertEqual(len(newList), 1)

    def test_E_filter_by_object_id(self):
        self.startQmf()
        qmf = self.qmf

        list = qmf.getObjects(_class="exchange", name="qpid.management")
        self.assertEqual(len(list), 1, "No Management Exchange")
        mgmt_exchange = list[0]

        bindings = qmf.getObjects(_class="binding", exchangeRef=mgmt_exchange.getObjectId())
        if len(bindings) == 0:
            self.fail("No bindings found on management exchange")

        for binding in bindings:
            self.assertEqual(binding.exchangeRef, mgmt_exchange.getObjectId())

    def getProperty(self, msg, name):
        for h in msg.headers:
            if hasattr(h, name): return getattr(h, name)
        return None            

    def getAppHeader(self, msg, name):
        headers = self.getProperty(msg, "application_headers")
        if headers:
            return headers[name]
        return None
