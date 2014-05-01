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

from qpid.tests import Test
from qpid.mimetype import lex, parse, ParseError, EOF, WSPACE
from parser import ParserBase

class MimeTypeTests(ParserBase, Test):

  EXCLUDE = (WSPACE, EOF)

  def do_lex(self, st):
    return lex(st)

  def do_parse(self, st):
    return parse(st)

  def valid(self, addr, type=None, subtype=None, parameters=None):
    ParserBase.valid(self, addr, (type, subtype, parameters))

  def testTypeOnly(self):
    self.invalid("type", "expecting SLASH, got EOF line:1,4:type")

  def testTypeSubtype(self):
    self.valid("type/subtype", "type", "subtype", [])

  def testTypeSubtypeParam(self):
    self.valid("type/subtype ; name=value",
               "type", "subtype", [("name", "value")])

  def testTypeSubtypeParamComment(self):
    self.valid("type/subtype ; name(This is a comment.)=value",
               "type", "subtype", [("name", "value")])

  def testMultipleParams(self):
    self.valid("type/subtype ; name1=value1 ; name2=value2",
               "type", "subtype", [("name1", "value1"), ("name2", "value2")])

  def testCaseInsensitivity(self):
    self.valid("Type/Subtype", "type", "subtype", [])
