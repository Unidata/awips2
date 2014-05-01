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

# setup, usage, teardown, errors(sync), errors(async), stress, soak,
# boundary-conditions, config

from qpid.tests import Test
from qpid.framing import *

class Base(Test):

  def cmp_frames(self, frm1, frm2):
    assert frm1.flags == frm2.flags, "expected: %r, got %r" % (frm1, frm2)
    assert frm1.type == frm2.type, "expected: %r, got %r" % (frm1, frm2)
    assert frm1.track == frm2.track, "expected: %r, got %r" % (frm1, frm2)
    assert frm1.channel == frm2.channel, "expected: %r, got %r" % (frm1, frm2)
    assert frm1.payload == frm2.payload, "expected: %r, got %r" % (frm1, frm2)

  def cmp_segments(self, seg1, seg2):
    assert seg1.first == seg2.first, "expected: %r, got %r" % (seg1, seg2)
    assert seg1.last == seg2.last, "expected: %r, got %r" % (seg1, seg2)
    assert seg1.type == seg2.type, "expected: %r, got %r" % (seg1, seg2)
    assert seg1.track == seg2.track, "expected: %r, got %r" % (seg1, seg2)
    assert seg1.channel == seg2.channel, "expected: %r, got %r" % (seg1, seg2)
    assert seg1.payload == seg2.payload, "expected: %r, got %r" % (seg1, seg2)

  def cmp_list(self, l1, l2):
    if l1 is None:
      assert l2 is None
      return

    assert len(l1) == len(l2)
    for v1, v2 in zip(l1, l2):
      if isinstance(v1, Compound):
        self.cmp_ops(v1, v2)
      else:
        assert v1 == v2

  def cmp_ops(self, op1, op2):
    if op1 is None:
      assert op2 is None
      return

    assert op1.__class__ == op2.__class__
    cls = op1.__class__
    assert op1.NAME == op2.NAME
    assert op1.CODE == op2.CODE
    assert op1.FIELDS == op2.FIELDS
    for f in cls.FIELDS:
      v1 = getattr(op1, f.name)
      v2 = getattr(op2, f.name)
      if COMPOUND.has_key(f.type) or f.type == "struct32":
        self.cmp_ops(v1, v2)
      elif f.type in ("list", "array"):
        self.cmp_list(v1, v2)
      else:
        assert v1 == v2, "expected: %r, got %r" % (v1, v2)

    if issubclass(cls, Command) or issubclass(cls, Control):
      assert op1.channel == op2.channel

    if issubclass(cls, Command):
      assert op1.sync == op2.sync, "expected: %r, got %r" % (op1.sync, op2.sync)
      assert (op1.headers is None and op2.headers is None) or \
          (op1.headers is not None and op2.headers is not None)
      if op1.headers is not None:
        assert len(op1.headers) == len(op2.headers)
        for h1, h2 in zip(op1.headers, op2.headers):
          self.cmp_ops(h1, h2)

class FrameTest(Base):

  def enc_dec(self, frames, encoded=None):
    enc = FrameEncoder()
    dec = FrameDecoder()

    enc.write(*frames)
    bytes = enc.read()
    if encoded is not None:
      assert bytes == encoded, "expected %r, got %r" % (encoded, bytes)
    dec.write(bytes)
    dframes = dec.read()

    assert len(frames) == len(dframes)
    for f, df, in zip(frames, dframes):
      self.cmp_frames(f, df)

  def testEmpty(self):
    self.enc_dec([Frame(0, 0, 0, 0, "")],
                 "\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00")

  def testSingle(self):
    self.enc_dec([Frame(0, 0, 0, 1, "payload")],
                 "\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x00payload")

  def testMaxChannel(self):
    self.enc_dec([Frame(0, 0, 0, 65535, "max-channel")],
                 "\x00\x00\x00\x17\x00\x00\xff\xff\x00\x00\x00\x00max-channel")

  def testMaxType(self):
    self.enc_dec([Frame(0, 255, 0, 0, "max-type")],
                 "\x00\xff\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00max-type")

  def testMaxTrack(self):
    self.enc_dec([Frame(0, 0, 15, 0, "max-track")],
                 "\x00\x00\x00\x15\x00\x0f\x00\x00\x00\x00\x00\x00max-track")

  def testSequence(self):
    self.enc_dec([Frame(0, 0, 0, 0, "zero"),
                  Frame(0, 0, 0, 1, "one"),
                  Frame(0, 0, 1, 0, "two"),
                  Frame(0, 0, 1, 1, "three"),
                  Frame(0, 1, 0, 0, "four"),
                  Frame(0, 1, 0, 1, "five"),
                  Frame(0, 1, 1, 0, "six"),
                  Frame(0, 1, 1, 1, "seven"),
                  Frame(1, 0, 0, 0, "eight"),
                  Frame(1, 0, 0, 1, "nine"),
                  Frame(1, 0, 1, 0, "ten"),
                  Frame(1, 0, 1, 1, "eleven"),
                  Frame(1, 1, 0, 0, "twelve"),
                  Frame(1, 1, 0, 1, "thirteen"),
                  Frame(1, 1, 1, 0, "fourteen"),
                  Frame(1, 1, 1, 1, "fifteen")])

class SegmentTest(Base):

  def enc_dec(self, segments, frames=None, interleave=None, max_payload=Frame.MAX_PAYLOAD):
    enc = SegmentEncoder(max_payload)
    dec = SegmentDecoder()

    enc.write(*segments)
    frms = enc.read()
    if frames is not None:
      assert len(frames) == len(frms), "expected %s, got %s" % (frames, frms)
      for f1, f2 in zip(frames, frms):
        self.cmp_frames(f1, f2)
    if interleave is not None:
      ilvd = []
      for f in frms:
        ilvd.append(f)
        if interleave:
          ilvd.append(interleave.pop(0))
      ilvd.extend(interleave)
      dec.write(*ilvd)
    else:
      dec.write(*frms)
    segs = dec.read()
    assert len(segments) == len(segs)
    for s1, s2 in zip(segments, segs):
      self.cmp_segments(s1, s2)

  def testEmpty(self):
    self.enc_dec([Segment(True, True, 0, 0, 0, "")],
                 [Frame(FIRST_FRM | LAST_FRM | FIRST_SEG | LAST_SEG, 0, 0, 0,
                        "")])

  def testSingle(self):
    self.enc_dec([Segment(True, True, 0, 0, 0, "payload")],
                 [Frame(FIRST_FRM | LAST_FRM | FIRST_SEG | LAST_SEG, 0, 0, 0,
                        "payload")])

  def testMaxChannel(self):
    self.enc_dec([Segment(False, False, 0, 0, 65535, "max-channel")],
                 [Frame(FIRST_FRM | LAST_FRM, 0, 0, 65535, "max-channel")])

  def testMaxType(self):
    self.enc_dec([Segment(False, False, 255, 0, 0, "max-type")],
                 [Frame(FIRST_FRM | LAST_FRM, 255, 0, 0, "max-type")])

  def testMaxTrack(self):
    self.enc_dec([Segment(False, False, 0, 15, 0, "max-track")],
                 [Frame(FIRST_FRM | LAST_FRM, 0, 15, 0, "max-track")])

  def testSequence(self):
    self.enc_dec([Segment(True, False, 0, 0, 0, "one"),
                  Segment(False, False, 0, 0, 0, "two"),
                  Segment(False, True, 0, 0, 0, "three")],
                 [Frame(FIRST_FRM | LAST_FRM | FIRST_SEG, 0, 0, 0, "one"),
                  Frame(FIRST_FRM | LAST_FRM, 0, 0, 0, "two"),
                  Frame(FIRST_FRM | LAST_FRM | LAST_SEG, 0, 0, 0, "three")])

  def testInterleaveChannel(self):
    frames = [Frame(0, 0, 0, 0, chr(ord("a") + i)) for i in range(7)]
    frames[0].flags |= FIRST_FRM
    frames[-1].flags |= LAST_FRM

    ilvd = [Frame(0, 0, 0, 1, chr(ord("a") + i)) for i in range(7)]

    self.enc_dec([Segment(False, False, 0, 0, 0, "abcdefg")], frames, ilvd, max_payload=1)

  def testInterleaveTrack(self):
    frames = [Frame(0, 0, 0, 0, "%c%c" % (ord("a") + i, ord("a") + i + 1))
              for i in range(0, 8, 2)]
    frames[0].flags |= FIRST_FRM
    frames[-1].flags |= LAST_FRM

    ilvd = [Frame(0, 0, 1, 0, "%c%c" % (ord("a") + i, ord("a") + i + 1))
            for i in range(0, 8, 2)]

    self.enc_dec([Segment(False, False, 0, 0, 0, "abcdefgh")], frames, ilvd, max_payload=2)

from qpid.ops import *

class OpTest(Base):

  def enc_dec(self, ops):
    enc = OpEncoder()
    dec = OpDecoder()
    enc.write(*ops)
    segs = enc.read()
    dec.write(*segs)
    dops = dec.read()
    assert len(ops) == len(dops)
    for op1, op2 in zip(ops, dops):
      self.cmp_ops(op1, op2)

  def testEmtpyMT(self):
    self.enc_dec([MessageTransfer()])

  def testEmptyMTSync(self):
    self.enc_dec([MessageTransfer(sync=True)])

  def testMT(self):
    self.enc_dec([MessageTransfer(destination="asdf")])

  def testSyncMT(self):
    self.enc_dec([MessageTransfer(destination="asdf", sync=True)])

  def testEmptyPayloadMT(self):
    self.enc_dec([MessageTransfer(payload="")])

  def testPayloadMT(self):
    self.enc_dec([MessageTransfer(payload="test payload")])

  def testHeadersEmptyPayloadMT(self):
    self.enc_dec([MessageTransfer(headers=[DeliveryProperties()])])

  def testHeadersPayloadMT(self):
    self.enc_dec([MessageTransfer(headers=[DeliveryProperties()], payload="test payload")])

  def testMultiHeadersEmptyPayloadMT(self):
    self.enc_dec([MessageTransfer(headers=[DeliveryProperties(), MessageProperties()])])

  def testMultiHeadersPayloadMT(self):
    self.enc_dec([MessageTransfer(headers=[MessageProperties(), DeliveryProperties()], payload="test payload")])

  def testContentTypeHeadersPayloadMT(self):
    self.enc_dec([MessageTransfer(headers=[MessageProperties(content_type="text/plain")], payload="test payload")])

  def testMulti(self):
    self.enc_dec([MessageTransfer(),
                  MessageTransfer(sync=True),
                  MessageTransfer(destination="one"),
                  MessageTransfer(destination="two", sync=True),
                  MessageTransfer(destination="three", payload="test payload")])

  def testControl(self):
    self.enc_dec([SessionAttach(name="asdf")])

  def testMixed(self):
    self.enc_dec([SessionAttach(name="fdsa"), MessageTransfer(destination="test")])

  def testChannel(self):
    self.enc_dec([SessionAttach(name="asdf", channel=3), MessageTransfer(destination="test", channel=1)])

  def testCompound(self):
    self.enc_dec([MessageTransfer(headers=[MessageProperties(reply_to=ReplyTo(exchange="exch", routing_key="rk"))])])

  def testListCompound(self):
    self.enc_dec([ExecutionResult(value=RecoverResult(in_doubt=[Xid(global_id="one"),
                                                                Xid(global_id="two"),
                                                                Xid(global_id="three")]))])
