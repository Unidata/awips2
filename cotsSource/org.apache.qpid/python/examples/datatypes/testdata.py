#
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
#

# -*- encoding: utf-8 -*-

from qpid.datatypes import uuid4, timestamp

#----- Some variables to test boundary conditions on various data types

void = None
boolean_true = True
boolean_false = False
Uint8_0 = 0
Uint8_max = 255
Uint16_0 = 0
Uint16_max = 65535
Uint32_0 = 0
Uint32_max = 4294967295
Uint64_0 = 0
Uint64_max = 18446744073709551615
Int8_min = -128
Int8_0 = 0
Int8_max = 127
Int16_min = -32768
Int16_0 = 0
Int16_max = 32767
Int32_min = -2147483648
Int32_0 = 0
Int32_max = 2147483647
Int64_min = -9223372036854775808
Int64_0 = 0
Int64_max = 9223372036854775807

Float_pi = 3.14159265
Float_neg = -1E4
Float_big = 1267.43233E12
Float_small = 12.78e-12
Float_neg0 = -0
Float_pos0 = 0
Float_INF = float('inf')
Float_Negative_INF = float('-inf')

Double_pi = 3.1415926535897932384626433832795
Double_neg = -1E4
Double_big = 1267.43233E12
Double_small = 12.78e-2
Double_neg0 = -0
Double_pos0 = 0
Double_INF = float('inf')
Double_Negative_INF = float('-inf')

char_1byte = u'0024' # $
char_2byte = u'00A2' # ¢
char_3byte = u'20AC' # €
char_4byte = u'10ABCD'

timestamp = timestamp()

UUID = uuid4()

String_Greek = u"ἐξίσταντο δὲ πάντες καὶ διηπόρουν, ἄλλος πρὸς ἄλλον λέγοντες, Τί θέλει τοῦτο εἶναι;"

String_Empty = ""

#----- A few functions ----------------------------------------------------------

def near_enough(float1, float2, delta):
  return abs(float1-float2) < delta

def set_application_headers(message_properties):

  message_properties.application_headers = {}
  message_properties.application_headers["void"] = None
  message_properties.application_headers["boolean_true"] =  boolean_true
  message_properties.application_headers["boolean_false"] = boolean_false
  message_properties.application_headers["Uint8_0"] = Uint8_0
  message_properties.application_headers["Uint8_max"] = Uint8_max
  message_properties.application_headers["Uint16_0"] = Uint16_0
  message_properties.application_headers["Uint16_max"] = Uint16_max
  message_properties.application_headers["Uint32_0"] = Uint32_0
  message_properties.application_headers["Uint32_max"] = Uint32_max
  message_properties.application_headers["Uint64_0"] = Uint64_0
#  message_properties.application_headers["Uint64_max"] = Uint64_max
  message_properties.application_headers["Int8_min"] = Int8_min
  message_properties.application_headers["Int8_0"] = Int8_0
  message_properties.application_headers["Int8_max"] = Int8_max
  message_properties.application_headers["Int16_min"] = Int16_min
  message_properties.application_headers["Int16_0"] = Int16_0
  message_properties.application_headers["Int16_max"] = Int16_max
  message_properties.application_headers["Int32_min"] = Int32_min
  message_properties.application_headers["Int32_0"] = Int32_0
  message_properties.application_headers["Int32_max"] = Int32_max
  message_properties.application_headers["Int64_min"] = Int64_min
  message_properties.application_headers["Int64_0"] = Int64_0
  message_properties.application_headers["Int64_max"] = Int64_max
 
  message_properties.application_headers["Float_pi"] = Float_pi
  message_properties.application_headers["Float_neg"] = Float_neg
  message_properties.application_headers["Float_big"] = Float_big
  message_properties.application_headers["Float_small"] = Float_small
  message_properties.application_headers["Float_neg0"] = Float_neg0
  message_properties.application_headers["Float_pos0"] = Float_pos0
  message_properties.application_headers["Float_INF"] = Float_INF
  message_properties.application_headers["Float_Negative_INF"] = Float_Negative_INF

  message_properties.application_headers["Double_pi"] = Double_pi
  message_properties.application_headers["Double_neg"] = Double_neg
  message_properties.application_headers["Double_big"] = Double_big
  message_properties.application_headers["Double_small"] = Double_small
  message_properties.application_headers["Double_neg0"] = Double_neg0
  message_properties.application_headers["Double_pos0"] = Double_pos0
  message_properties.application_headers["Double_INF"] = Double_INF
  message_properties.application_headers["Double_Negative_INF"] = Double_Negative_INF

  message_properties.application_headers["char_1byte"] = char_1byte
  message_properties.application_headers["char_2byte"] = char_2byte
  message_properties.application_headers["char_3byte"] = char_3byte
  message_properties.application_headers["char_4byte"] = char_4byte

  message_properties.application_headers["timestamp"] = timestamp
  message_properties.application_headers["UUID"] = uuid4() 
  message_properties.application_headers["String_Greek"] = String_Greek 
  message_properties.application_headers["String_Empty"] = String_Empty

def check_message(message):

#  message_properties = message.message_properties()
  message_properties = message.get("message_properties")
  assert message_properties.application_headers["void"] == None
  assert message_properties.application_headers["boolean_true"] == boolean_true
  assert message_properties.application_headers["boolean_false"] == boolean_false
  assert message_properties.application_headers["Uint8_0"] == Uint8_0
  assert message_properties.application_headers["Uint8_max"] == Uint8_max
  assert message_properties.application_headers["Uint16_0"] == Uint16_0
  assert message_properties.application_headers["Uint16_max"] == Uint16_max
  assert message_properties.application_headers["Uint32_0"] == Uint32_0
  assert message_properties.application_headers["Uint32_max"] == Uint32_max
  assert message_properties.application_headers["Uint64_0"] == Uint64_0
#  assert message_properties.application_headers["Uint64_max"] == Uint64_max
  assert message_properties.application_headers["Int8_min"] == Int8_min
  assert message_properties.application_headers["Int8_0"] == Int8_0
  assert message_properties.application_headers["Int8_max"] == Int8_max
  assert message_properties.application_headers["Int16_min"] == Int16_min
  assert message_properties.application_headers["Int16_0"] == Int16_0
  assert message_properties.application_headers["Int16_max"] == Int16_max
  assert message_properties.application_headers["Int32_min"] == Int32_min
  assert message_properties.application_headers["Int32_0"] == Int32_0
  assert message_properties.application_headers["Int32_max"] == Int32_max
  assert message_properties.application_headers["Int64_min"] == Int64_min
  assert message_properties.application_headers["Int64_0"] == Int64_0
  assert message_properties.application_headers["Int64_max"] == Int64_max
  
# Change floating point comparisons to allow inexactness

  assert near_enough(message_properties.application_headers["Float_pi"], Float_pi, 0.00001)
  assert near_enough(message_properties.application_headers["Float_neg"], Float_neg, 0.00001)
  assert near_enough(message_properties.application_headers["Float_big"], Float_big, Float_big/1000000)
  assert near_enough(message_properties.application_headers["Float_small"], Float_small, 0.00001)
  assert message_properties.application_headers["Float_neg0"] == Float_neg0
  assert message_properties.application_headers["Float_pos0"] == Float_pos0
  assert message_properties.application_headers["Float_INF"] == Float_INF
  assert message_properties.application_headers["Float_Negative_INF"] == Float_Negative_INF

  assert near_enough(message_properties.application_headers["Double_pi"], Double_pi, 0.00001)
  assert near_enough(message_properties.application_headers["Double_neg"], Double_neg, 0.00001)
  assert near_enough(message_properties.application_headers["Double_big"], Double_big, Double_big/1000000)
  assert near_enough(message_properties.application_headers["Double_small"], Double_small, 0.00001)
  assert message_properties.application_headers["Double_neg0"] == Double_neg0
  assert message_properties.application_headers["Double_pos0"] == Double_pos0
  assert message_properties.application_headers["Double_INF"] == Double_INF
  assert message_properties.application_headers["Double_Negative_INF"] == Double_Negative_INF

  assert message_properties.application_headers["char_1byte"] == char_1byte
  assert message_properties.application_headers["char_2byte"] == char_2byte
  assert message_properties.application_headers["char_3byte"] == char_3byte
  assert message_properties.application_headers["char_4byte"] == char_4byte

#  assert message_properties.application_headers["timestamp"] == timestamp
#  assert message_properties.application_headers["UUID"] == UUID
  assert message_properties.application_headers["String_Greek"] == String_Greek
  assert message_properties.application_headers["String_Empty"] == String_Empty


