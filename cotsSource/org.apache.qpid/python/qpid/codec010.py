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

import datetime
from packer import Packer
from datatypes import serial, timestamp, RangedSet, Struct, UUID
from ops import Compound, PRIMITIVE, COMPOUND

class CodecException(Exception): pass

def direct(t):
  return lambda x: t

def map_str(s):
  for c in s:
    if ord(c) >= 0x80:
      return "vbin16"
  return "str16"

class Codec(Packer):

  ENCODINGS = {
    unicode: direct("str16"),
    str: map_str,
    buffer: direct("vbin32"),
    int: direct("int64"),
    long: direct("int64"),
    float: direct("double"),
    None.__class__: direct("void"),
    list: direct("list"),
    tuple: direct("list"),
    dict: direct("map"),
    timestamp: direct("datetime"),
    datetime.datetime: direct("datetime"),
    UUID: direct("uuid"),
    Compound: direct("struct32")
    }

  def encoding(self, obj):
    enc = self._encoding(obj.__class__, obj)
    if enc is None:
      raise CodecException("no encoding for %r" % obj)
    return PRIMITIVE[enc]

  def _encoding(self, klass, obj):
    if self.ENCODINGS.has_key(klass):
      return self.ENCODINGS[klass](obj)
    for base in klass.__bases__:
      result = self._encoding(base, obj)
      if result != None:
        return result

  def read_primitive(self, type):
    return getattr(self, "read_%s" % type.NAME)()
  def write_primitive(self, type, v):
    getattr(self, "write_%s" % type.NAME)(v)

  def read_void(self):
    return None
  def write_void(self, v):
    assert v == None

  def read_bit(self):
    return True
  def write_bit(self, b):
    if not b: raise ValueError(b)

  def read_uint8(self):
    return self.unpack("!B")
  def write_uint8(self, n):
    return self.pack("!B", n)

  def read_int8(self):
    return self.unpack("!b")
  def write_int8(self, n):
    self.pack("!b", n)

  def read_char(self):
    return self.unpack("!c")
  def write_char(self, c):
    self.pack("!c", c)

  def read_boolean(self):
    return self.read_uint8() != 0
  def write_boolean(self, b):
    if b: n = 1
    else: n = 0
    self.write_uint8(n)


  def read_uint16(self):
    return self.unpack("!H")
  def write_uint16(self, n):
    self.pack("!H", n)

  def read_int16(self):
    return self.unpack("!h")
  def write_int16(self, n):
    self.pack("!h", n)


  def read_uint32(self):
    return self.unpack("!L")
  def write_uint32(self, n):
    self.pack("!L", n)

  def read_int32(self):
    return self.unpack("!l")
  def write_int32(self, n):
    self.pack("!l", n)

  def read_float(self):
    return self.unpack("!f")
  def write_float(self, f):
    self.pack("!f", f)

  def read_sequence_no(self):
    return serial(self.read_uint32())
  def write_sequence_no(self, n):
    self.write_uint32(n.value)


  def read_uint64(self):
    return self.unpack("!Q")
  def write_uint64(self, n):
    self.pack("!Q", n)

  def read_int64(self):
    return self.unpack("!q")
  def write_int64(self, n):
    self.pack("!q", n)

  def read_datetime(self):
    return timestamp(self.read_uint64())
  def write_datetime(self, t):
    if isinstance(t, datetime.datetime):
      t = timestamp(t)
    self.write_uint64(t)

  def read_double(self):
    return self.unpack("!d")
  def write_double(self, d):
    self.pack("!d", d)

  def read_vbin8(self):
    return self.read(self.read_uint8())
  def write_vbin8(self, b):
    if isinstance(b, buffer):
      b = str(b)
    self.write_uint8(len(b))
    self.write(b)

  def read_str8(self):
    return self.read_vbin8().decode("utf8")
  def write_str8(self, s):
    self.write_vbin8(s.encode("utf8"))

  def read_str16(self):
    return self.read_vbin16().decode("utf8")
  def write_str16(self, s):
    self.write_vbin16(s.encode("utf8"))

  def read_str16_latin(self):
    return self.read_vbin16().decode("iso-8859-15")
  def write_str16_latin(self, s):
    self.write_vbin16(s.encode("iso-8859-15"))


  def read_vbin16(self):
    return self.read(self.read_uint16())
  def write_vbin16(self, b):
    if isinstance(b, buffer):
      b = str(b)
    self.write_uint16(len(b))
    self.write(b)

  def read_sequence_set(self):
    result = RangedSet()
    size = self.read_uint16()
    nranges = size/8
    while nranges > 0:
      lower = self.read_sequence_no()
      upper = self.read_sequence_no()
      result.add(lower, upper)
      nranges -= 1
    return result
  def write_sequence_set(self, ss):
    size = 8*len(ss.ranges)
    self.write_uint16(size)
    for range in ss.ranges:
      self.write_sequence_no(range.lower)
      self.write_sequence_no(range.upper)

  def read_vbin32(self):
    return self.read(self.read_uint32())
  def write_vbin32(self, b):
    if isinstance(b, buffer):
      b = str(b)
    self.write_uint32(len(b))
    self.write(b)

  def read_map(self):
    sc = StringCodec(self.read_vbin32())
    if not sc.encoded:
      return None
    count = sc.read_uint32()
    result = {}
    while sc.encoded:
      k = sc.read_str8()
      code = sc.read_uint8()
      type = PRIMITIVE[code]
      v = sc.read_primitive(type)
      result[k] = v
    return result
  def write_map(self, m):
    sc = StringCodec()
    if m is not None:
      sc.write_uint32(len(m))
      for k, v in m.items():
        type = self.encoding(v)
        sc.write_str8(k)
        sc.write_uint8(type.CODE)
        sc.write_primitive(type, v)
    self.write_vbin32(sc.encoded)

  def read_array(self):
    sc = StringCodec(self.read_vbin32())
    if not sc.encoded:
      return None
    type = PRIMITIVE[sc.read_uint8()]
    count = sc.read_uint32()
    result = []
    while count > 0:
      result.append(sc.read_primitive(type))
      count -= 1
    return result
  def write_array(self, a):
    sc = StringCodec()
    if a is not None:
      if len(a) > 0:
        type = self.encoding(a[0])
      else:
        type = self.encoding(None)
      sc.write_uint8(type.CODE)
      sc.write_uint32(len(a))
      for o in a:
        sc.write_primitive(type, o)
    self.write_vbin32(sc.encoded)

  def read_list(self):
    sc = StringCodec(self.read_vbin32())
    if not sc.encoded:
      return None
    count = sc.read_uint32()
    result = []
    while count > 0:
      type = PRIMITIVE[sc.read_uint8()]
      result.append(sc.read_primitive(type))
      count -= 1
    return result
  def write_list(self, l):
    sc = StringCodec()
    if l is not None:
      sc.write_uint32(len(l))
      for o in l:
        type = self.encoding(o)
        sc.write_uint8(type.CODE)
        sc.write_primitive(type, o)
    self.write_vbin32(sc.encoded)

  def read_struct32(self):
    size = self.read_uint32()
    code = self.read_uint16()
    cls = COMPOUND[code]
    op = cls()
    self.read_fields(op)
    return op
  def write_struct32(self, value):
    self.write_compound(value)

  def read_compound(self, cls):
    size = self.read_size(cls.SIZE)
    if cls.CODE is not None:
      code = self.read_uint16()
      assert code == cls.CODE
    op = cls()
    self.read_fields(op)
    return op
  def write_compound(self, op):
    sc = StringCodec()
    if op.CODE is not None:
      sc.write_uint16(op.CODE)
    sc.write_fields(op)
    self.write_size(op.SIZE, len(sc.encoded))
    self.write(sc.encoded)

  def read_fields(self, op):
    flags = 0
    for i in range(op.PACK):
      flags |= (self.read_uint8() << 8*i)

    for i in range(len(op.FIELDS)):
      f = op.FIELDS[i]
      if flags & (0x1 << i):
        if COMPOUND.has_key(f.type):
          value = self.read_compound(COMPOUND[f.type])
        else:
          value = getattr(self, "read_%s" % f.type)()
        setattr(op, f.name, value)
  def write_fields(self, op):
    flags = 0
    for i in range(len(op.FIELDS)):
      f = op.FIELDS[i]
      value = getattr(op, f.name)
      if f.type == "bit":
        present = value
      else:
        present = value != None
      if present:
        flags |= (0x1 << i)
    for i in range(op.PACK):
      self.write_uint8((flags >> 8*i) & 0xFF)
    for i in range(len(op.FIELDS)):
      f = op.FIELDS[i]
      if flags & (0x1 << i):
        if COMPOUND.has_key(f.type):
          enc = self.write_compound
        else:
          enc = getattr(self, "write_%s" % f.type)
        value = getattr(op, f.name)
        enc(value)

  def read_size(self, width):
    if width > 0:
      attr = "read_uint%d" % (width*8)
      return getattr(self, attr)()
  def write_size(self, width, n):
    if width > 0:
      attr = "write_uint%d" % (width*8)
      getattr(self, attr)(n)

  def read_uuid(self):
    return UUID(self.unpack("16s"))
  def write_uuid(self, s):
    if isinstance(s, UUID):
      s = s.bytes
    self.pack("16s", s)

  def read_bin128(self):
    return self.unpack("16s")
  def write_bin128(self, b):
    self.pack("16s", b)



class StringCodec(Codec):

  def __init__(self, encoded = ""):
    self.encoded = encoded

  def read(self, n):
    result = self.encoded[:n]
    self.encoded = self.encoded[n:]
    return result

  def write(self, s):
    self.encoded += s
