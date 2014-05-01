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

class ParseError(Exception):

  def __init__(self, token, *expected):
    line, ln, col = token.line_info()
    exp = ", ".join(map(str, expected))
    if len(expected) > 1:
      exp = "(%s)" % exp
    if expected:
      msg = "expecting %s, got %s line:%s,%s:%s" % (exp, token, ln, col, line)
    else:
      msg = "unexpected token %s line:%s,%s:%s" % (token, ln, col, line)
    Exception.__init__(self, msg)
    self.token = token
    self.expected = expected

class Parser:

  def __init__(self, tokens):
    self.tokens = tokens
    self.idx = 0

  def next(self):
    return self.tokens[self.idx]

  def matches(self, *types):
    return self.next().type in types

  def eat(self, *types):
    if types and not self.matches(*types):
      raise ParseError(self.next(), *types)
    else:
      t = self.next()
      self.idx += 1
      return t

  def eat_until(self, *types):
    result = []
    while not self.matches(*types):
      result.append(self.eat())
    return result

  def remove(self, start, end):
    start_idx = self.tokens.index(start)
    end_idx = self.tokens.index(end) + 1
    del self.tokens[start_idx:end_idx]
    self.idx -= end_idx - start_idx

  def reset(self):
    self.idx = 0
