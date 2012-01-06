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

import os, tempfile, shutil, stat
from unittest import TestCase
from qpid.codec010 import Codec, StringCodec
from qpid.ops import *

class SpecTest(TestCase):

  def testSessionHeader(self):
    sc = StringCodec()
    sc.write_compound(Header(sync=True))
    assert sc.encoded == "\x01\x01"

    sc = StringCodec()
    sc.write_compound(Header(sync=False))
    assert sc.encoded == "\x01\x00"

  def encdec(self, value):
    sc = StringCodec()
    sc.write_compound(value)
    decoded = sc.read_compound(value.__class__)
    return decoded

  def testMessageProperties(self):
    props = MessageProperties(content_length=3735928559L,
                              reply_to=ReplyTo(exchange="the exchange name",
                                               routing_key="the routing key"))
    dec = self.encdec(props)
    assert props.content_length == dec.content_length
    assert props.reply_to.exchange == dec.reply_to.exchange
    assert props.reply_to.routing_key == dec.reply_to.routing_key

  def testMessageSubscribe(self):
    cmd = MessageSubscribe(exclusive=True, destination="this is a test")
    dec = self.encdec(cmd)
    assert cmd.exclusive == dec.exclusive
    assert cmd.destination == dec.destination

  def testXid(self):
    sc = StringCodec()
    xid = Xid(format=0, global_id="gid", branch_id="bid")
    sc.write_compound(xid)
    assert sc.encoded == '\x00\x00\x00\x10\x06\x04\x07\x00\x00\x00\x00\x00\x03gid\x03bid'
    dec = sc.read_compound(Xid)
    assert xid.__dict__ == dec.__dict__

#   def testLoadReadOnly(self):
#     spec = "amqp.0-10-qpid-errata.xml"
#     f = testrunner.get_spec_file(spec)
#     dest = tempfile.mkdtemp()
#     shutil.copy(f, dest)
#     shutil.copy(os.path.join(os.path.dirname(f), "amqp.0-10.dtd"), dest)
#     os.chmod(dest, stat.S_IRUSR | stat.S_IXUSR)
#     fname = os.path.join(dest, spec)
#     load(fname)
#     assert not os.path.exists("%s.pcl" % fname)
