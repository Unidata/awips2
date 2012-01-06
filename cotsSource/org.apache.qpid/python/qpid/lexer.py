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
import re

class Type:

  def __init__(self, name, pattern=None):
    self.name = name
    self.pattern = pattern

  def __repr__(self):
    return self.name

class Lexicon:

  def __init__(self):
    self.types = []
    self._eof = None

  def define(self, name, pattern):
    t = Type(name, pattern)
    self.types.append(t)
    return t

  def eof(self, name):
    t = Type(name)
    self._eof = t
    return t

  def compile(self):
    types = self.types[:]
    joined = "|".join(["(%s)" % t.pattern for t in types])
    rexp = re.compile(joined)
    return Lexer(types, self._eof, rexp)

class Token:

  def __init__(self, type, value, input, position):
    self.type = type
    self.value = value
    self.input = input
    self.position = position

  def line_info(self):
    return line_info(self.input, self.position)

  def __repr__(self):
    if self.value is None:
      return repr(self.type)
    else:
      return "%s(%r)" % (self.type, self.value)


class LexError(Exception):
  pass

def line_info(st, pos):
  idx = 0
  lineno = 1
  column = 0
  line_pos = 0
  while idx < pos:
    if st[idx] == "\n":
      lineno += 1
      column = 0
      line_pos = idx
    column += 1
    idx += 1

  end = st.find("\n", line_pos)
  if end < 0:
    end = len(st)
  line = st[line_pos:end]

  return line, lineno, column

class Lexer:

  def __init__(self, types, eof, rexp):
    self.types = types
    self.eof = eof
    self.rexp = rexp

  def lex(self, st):
    pos = 0
    while pos < len(st):
      m = self.rexp.match(st, pos)
      if m is None:
        line, ln, col = line_info(st, pos)
        raise LexError("unrecognized characters line:%s,%s: %s" % (ln, col, line))
      else:
        idx = m.lastindex
        t = Token(self.types[idx - 1], m.group(idx), st, pos)
        yield t
      pos = m.end()
    yield Token(self.eof, None, st, pos)
