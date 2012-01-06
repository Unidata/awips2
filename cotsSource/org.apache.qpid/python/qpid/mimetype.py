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
import re, rfc822
from lexer import Lexicon, LexError
from parser import Parser, ParseError

l = Lexicon()

LPAREN = l.define("LPAREN", r"\(")
RPAREN = l.define("LPAREN", r"\)")
SLASH = l.define("SLASH", r"/")
SEMI = l.define("SEMI", r";")
EQUAL = l.define("EQUAL", r"=")
TOKEN = l.define("TOKEN", r'[^()<>@,;:\\"/\[\]?= ]+')
STRING = l.define("STRING", r'"(?:[^\\"]|\\.)*"')
WSPACE = l.define("WSPACE", r"[ \n\r\t]+")
EOF = l.eof("EOF")

LEXER = l.compile()

def lex(st):
  return LEXER.lex(st)

class MimeTypeParser(Parser):

  def __init__(self, tokens):
    Parser.__init__(self, [t for t in tokens if t.type is not WSPACE])

  def parse(self):
    result = self.mimetype()
    self.eat(EOF)
    return result

  def mimetype(self):
    self.remove_comments()
    self.reset()

    type = self.eat(TOKEN).value.lower()
    self.eat(SLASH)
    subtype = self.eat(TOKEN).value.lower()

    params = []
    while True:
      if self.matches(SEMI):
        params.append(self.parameter())
      else:
        break

    return type, subtype, params

  def remove_comments(self):
    while True:
      self.eat_until(LPAREN, EOF)
      if self.matches(LPAREN):
        self.remove(*self.comment())
      else:
        break

  def comment(self):
    start = self.eat(LPAREN)

    while True:
      self.eat_until(LPAREN, RPAREN)
      if self.matches(LPAREN):
        self.comment()
      else:
        break

    end = self.eat(RPAREN)
    return start, end

  def parameter(self):
    self.eat(SEMI)
    name = self.eat(TOKEN).value
    self.eat(EQUAL)
    value = self.value()
    return name, value

  def value(self):
    if self.matches(TOKEN):
      return self.eat().value
    elif self.matches(STRING):
      return rfc822.unquote(self.eat().value)
    else:
      raise ParseError(self.next(), TOKEN, STRING)

def parse(addr):
  return MimeTypeParser(lex(addr)).parse()

__all__ = ["parse", "ParseError"]
