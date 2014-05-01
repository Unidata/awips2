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

class Parser:

  def __init__(self, **kwargs):
    self.output = ""
    self.environ = {"out": self.parse}
    for k, v in kwargs.items():
      self.environ[k] = v
    self.text = ""
    self.level = 0
    self.line = None

  def action(self, actor):
    text = self.text
    self.text = ""
    actor(text)

  def out(self, text):
    self.output += text

  def prefix_lines(self, text):
    return "%s%s" % ("\n"*(self.line - 1 - text.count("\n")), text)

  def evaluate(self, text):
    self.out(str(eval(self.prefix_lines(text), self.environ, self.environ)))

  def execute(self, text):
    exec self.prefix_lines(text) in self.environ, self.environ

  def parse(self, input):
    old_line = self.line
    try:
      state = self.start
      self.line = 1
      for ch in input:
        state = state(ch)
        if ch == "\n":
          self.line += 1
      if state == self.start:
        self.action(self.out)
      elif state == self.alnum:
        self.action(self.evaluate)
      else:
        raise ParseError()
    finally:
      self.line = old_line

  def start(self, ch):
    if ch == "$":
      return self.dollar
    else:
      self.text += ch
      return self.start

  def dollar(self, ch):
    if ch == "$":
      self.text += "$"
      return self.start
    elif ch == "(":
      self.action(self.out)
      return self.expression
    elif ch == "{":
      self.action(self.out)
      return self.block
    else:
      self.action(self.out)
      self.text += ch
      return self.alnum

  def alnum(self, ch):
    if ch.isalnum():
      self.text += ch
      return self.alnum
    else:
      self.action(self.evaluate)
      self.text += ch
      return self.start

  def match(self, ch, start, end):
    if ch == start:
      self.level += 1
    if ch == end:
      self.level -= 1

  def block(self, ch):
    if not self.level and ch == "}":
      self.action(self.execute)
      return self.start
    else:
      self.match(ch, "{", "}")
      self.text += ch
      return self.block

  def expression(self, ch):
    if not self.level and ch == ")":
      self.action(self.evaluate)
      return self.start
    else:
      self.match(ch, "(", ")")
      self.text += ch
      return self.expression
