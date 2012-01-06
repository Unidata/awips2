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

import struct

FIRST_SEG = 0x08
LAST_SEG = 0x04
FIRST_FRM = 0x02
LAST_FRM = 0x01

class Frame:

  HEADER = "!2BHxBH4x"
  HEADER_SIZE = struct.calcsize(HEADER)
  MAX_PAYLOAD = 65535 - struct.calcsize(HEADER)

  def __init__(self, flags, type, track, channel, payload):
    if len(payload) > Frame.MAX_PAYLOAD:
      raise ValueError("max payload size exceeded: %s" % len(payload))
    self.flags = flags
    self.type = type
    self.track = track
    self.channel = channel
    self.payload = payload

  def isFirstSegment(self):
    return bool(FIRST_SEG & self.flags)

  def isLastSegment(self):
    return bool(LAST_SEG & self.flags)

  def isFirstFrame(self):
    return bool(FIRST_FRM & self.flags)

  def isLastFrame(self):
    return bool(LAST_FRM & self.flags)

  def __repr__(self):
    return "%s%s%s%s %s %s %s %r" % (int(self.isFirstSegment()),
                                     int(self.isLastSegment()),
                                     int(self.isFirstFrame()),
                                     int(self.isLastFrame()),
                                     self.type,
                                     self.track,
                                     self.channel,
                                     self.payload)

class Segment:

  def __init__(self, first, last, type, track, channel, payload):
    self.id = None
    self.offset = None
    self.first = first
    self.last = last
    self.type = type
    self.track = track
    self.channel = channel
    self.payload = payload

  def __repr__(self):
    return "%s%s %s %s %s %r" % (int(self.first), int(self.last), self.type,
                                 self.track, self.channel, self.payload)

class FrameDecoder:

  def __init__(self):
    self.input = ""
    self.output = []
    self.parse = self.__frame_header

  def write(self, bytes):
    self.input += bytes
    while True:
      next = self.parse()
      if next is None:
        break
      else:
        self.parse = next

  def __consume(self, n):
    result = self.input[:n]
    self.input = self.input[n:]
    return result

  def __frame_header(self):
    if len(self.input) >= Frame.HEADER_SIZE:
      st = self.__consume(Frame.HEADER_SIZE)
      self.flags, self.type, self.size, self.track, self.channel = \
          struct.unpack(Frame.HEADER, st)
      return self.__frame_body

  def __frame_body(self):
    size = self.size - Frame.HEADER_SIZE
    if len(self.input) >= size:
      payload = self.__consume(size)
      frame = Frame(self.flags, self.type, self.track, self.channel, payload)
      self.output.append(frame)
      return self.__frame_header

  def read(self):
    result = self.output
    self.output = []
    return result

class FrameEncoder:

  def __init__(self):
    self.output = ""

  def write(self, *frames):
    for frame in frames:
      size = len(frame.payload) + Frame.HEADER_SIZE
      track = frame.track & 0x0F
      self.output += struct.pack(Frame.HEADER, frame.flags, frame.type, size,
                                 track, frame.channel)
      self.output += frame.payload

  def read(self):
    result = self.output
    self.output = ""
    return result

class SegmentDecoder:

  def __init__(self):
    self.fragments = {}
    self.segments = []

  def write(self, *frames):
    for frm in frames:
      key = (frm.channel, frm.track)
      seg = self.fragments.get(key)

      if seg == None:
        seg = Segment(frm.isFirstSegment(), frm.isLastSegment(),
                      frm.type, frm.track, frm.channel, "")
        self.fragments[key] = seg

      seg.payload += frm.payload

      if frm.isLastFrame():
        self.fragments.pop(key)
        self.segments.append(seg)

  def read(self):
    result = self.segments
    self.segments = []
    return result

class SegmentEncoder:

  def __init__(self, max_payload=Frame.MAX_PAYLOAD):
    self.max_payload = max_payload
    self.frames = []

  def write(self, *segments):
    for seg in segments:
      remaining = seg.payload

      first = True
      while first or remaining:
        payload = remaining[:self.max_payload]
        remaining = remaining[self.max_payload:]

        flags = 0
        if first:
          flags |= FIRST_FRM
          first = False
        if not remaining:
          flags |= LAST_FRM
        if seg.first:
          flags |= FIRST_SEG
        if seg.last:
          flags |= LAST_SEG

        frm = Frame(flags, seg.type, seg.track, seg.channel, payload)
        self.frames.append(frm)

  def read(self):
    result = self.frames
    self.frames = []
    return result

from ops import COMMANDS, CONTROLS, COMPOUND, Header, segment_type, track
from spec import SPEC

from codec010 import StringCodec

class OpEncoder:

  def __init__(self):
    self.segments = []

  def write(self, *ops):
    for op in ops:
      if COMMANDS.has_key(op.NAME):
        seg_type = segment_type.command
        seg_track = track.command
        enc = self.encode_command(op)
      elif CONTROLS.has_key(op.NAME):
        seg_type = segment_type.control
        seg_track = track.control
        enc = self.encode_compound(op)
      else:
        raise ValueError(op)
      seg = Segment(True, False, seg_type, seg_track, op.channel, enc)
      self.segments.append(seg)
      if hasattr(op, "headers") and op.headers is not None:
        hdrs = ""
        for h in op.headers:
          hdrs += self.encode_compound(h)
        seg = Segment(False, False, segment_type.header, seg_track, op.channel,
                      hdrs)
        self.segments.append(seg)
      if hasattr(op, "payload") and op.payload is not None:
        self.segments.append(Segment(False, False, segment_type.body, seg_track,
                                     op.channel, op.payload))
      self.segments[-1].last = True

  def encode_command(self, cmd):
    sc = StringCodec()
    sc.write_uint16(cmd.CODE)
    sc.write_compound(Header(sync=cmd.sync))
    sc.write_fields(cmd)
    return sc.encoded

  def encode_compound(self, op):
    sc = StringCodec()
    sc.write_compound(op)
    return sc.encoded

  def read(self):
    result = self.segments
    self.segments = []
    return result

class OpDecoder:

  def __init__(self):
    self.op = None
    self.ops = []

  def write(self, *segments):
    for seg in segments:
      if seg.first:
        if seg.type == segment_type.command:
          self.op = self.decode_command(seg.payload)
        elif seg.type == segment_type.control:
          self.op = self.decode_control(seg.payload)
        else:
          raise ValueError(seg)
        self.op.channel = seg.channel
      elif seg.type == segment_type.header:
        if self.op.headers is None:
          self.op.headers = []
        self.op.headers.extend(self.decode_headers(seg.payload))
      elif seg.type == segment_type.body:
        if self.op.payload is None:
          self.op.payload = seg.payload
        else:
          self.op.payload += seg.payload
      if seg.last:
        self.ops.append(self.op)
        self.op = None

  def decode_command(self, encoded):
    sc = StringCodec(encoded)
    code = sc.read_uint16()
    cls = COMMANDS[code]
    hdr = sc.read_compound(Header)
    cmd = cls()
    sc.read_fields(cmd)
    cmd.sync = hdr.sync
    return cmd

  def decode_control(self, encoded):
    sc = StringCodec(encoded)
    code = sc.read_uint16()
    cls = CONTROLS[code]
    ctl = cls()
    sc.read_fields(ctl)
    return ctl

  def decode_headers(self, encoded):
    sc = StringCodec(encoded)
    result = []
    while sc.encoded:
      result.append(sc.read_struct32())
    return result

  def read(self):
    result = self.ops
    self.ops = []
    return result
