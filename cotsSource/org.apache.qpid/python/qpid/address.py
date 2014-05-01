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
from lexer import Lexicon, LexError
from parser import Parser, ParseError

l = Lexicon()

LBRACE = l.define("LBRACE", r"\{")
RBRACE = l.define("RBRACE", r"\}")
LBRACK = l.define("LBRACK", r"\[")
RBRACK = l.define("RBRACK", r"\]")
COLON = l.define("COLON", r":")
SEMI = l.define("SEMI", r";")
SLASH = l.define("SLASH", r"/")
COMMA = l.define("COMMA", r",")
NUMBER = l.define("NUMBER", r'[+-]?[0-9]*\.?[0-9]+')
ID = l.define("ID", r'[a-zA-Z_](?:[a-zA-Z0-9_-]*[a-zA-Z0-9_])?')
STRING = l.define("STRING", r""""(?:[^\\"]|\\.)*"|'(?:[^\\']|\\.)*'""")
ESC = l.define("ESC", r"\\[^ux]|\\x[0-9a-fA-F][0-9a-fA-F]|\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]")
SYM = l.define("SYM", r"[.#*%@$^!+-]")
WSPACE = l.define("WSPACE", r"[ \n\r\t]+")
EOF = l.eof("EOF")

LEXER = l.compile()

def lex(st):
  return LEXER.lex(st)

def tok2str(tok):
  if tok.type is STRING:
    return eval(tok.value)
  elif tok.type is ESC:
    if tok.value[1] == "x":
      return eval('"%s"' % tok.value)
    elif tok.value[1] == "u":
      return eval('u"%s"' % tok.value)
    else:
      return tok.value[1]
  else:
    return tok.value

def tok2obj(tok):
  if tok.type in (STRING, NUMBER):
    return eval(tok.value)
  else:
    return tok.value

def toks2str(toks):
  if toks:
    return "".join(map(tok2str, toks))
  else:
    return None

class AddressParser(Parser):

  def __init__(self, tokens):
    Parser.__init__(self, [t for t in tokens if t.type is not WSPACE])

  def parse(self):
    result = self.address()
    self.eat(EOF)
    return result

  def address(self):
    name = toks2str(self.eat_until(SLASH, SEMI, EOF))

    if name is None:
      raise ParseError(self.next())

    if self.matches(SLASH):
      self.eat(SLASH)
      subject = toks2str(self.eat_until(SEMI, EOF))
    else:
      subject = None

    if self.matches(SEMI):
      self.eat(SEMI)
      options = self.map()
    else:
      options = None
    return name, subject, options

  def map(self):
    self.eat(LBRACE)

    result = {}
    while True:
      if self.matches(ID):
        n, v = self.nameval()
        result[n] = v
        if self.matches(COMMA):
          self.eat(COMMA)
        elif self.matches(RBRACE):
          break
        else:
          raise ParseError(self.next(), COMMA, RBRACE)
      elif self.matches(RBRACE):
        break
      else:
        raise ParseError(self.next(), ID, RBRACE)

    self.eat(RBRACE)
    return result

  def nameval(self):
    name = self.eat(ID).value
    self.eat(COLON)
    val = self.value()
    return (name, val)

  def value(self):
    if self.matches(NUMBER, STRING, ID):
      return tok2obj(self.eat())
    elif self.matches(LBRACE):
      return self.map()
    elif self.matches(LBRACK):
      return self.list()
    else:
      raise ParseError(self.next(), NUMBER, STRING, ID, LBRACE, LBRACK)

  def list(self):
    self.eat(LBRACK)

    result = []

    while True:
      if self.matches(RBRACK):
        break
      else:
        result.append(self.value())
        if self.matches(COMMA):
          self.eat(COMMA)
        elif self.matches(RBRACK):
          break
        else:
          raise ParseError(self.next(), COMMA, RBRACK)

    self.eat(RBRACK)
    return result

def parse(addr):
  return AddressParser(lex(addr)).parse()

__all__ = ["parse", "ParseError"]
