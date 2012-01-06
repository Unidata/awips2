#ifndef QPID_AMQP_0_10_TYPEFORCODE_H
#define QPID_AMQP_0_10_TYPEFORCODE_H
/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */

///
/// This file was automatically generated from the AMQP specification.
/// Do not edit.
///


#include "qpid/amqp_0_10/built_in_types.h"
#include "qpid/amqp_0_10/UnknownType.h"

namespace qpid {
namespace amqp_0_10 {


template <uint8_t Code> struct TypeForCode;

template <> struct TypeForCode<0x00> {  typedef Bin8 type; };
template <> struct TypeForCode<0x01> {  typedef Int8 type; };
template <> struct TypeForCode<0x02> {  typedef Uint8 type; };
template <> struct TypeForCode<0x04> {  typedef Char type; };
template <> struct TypeForCode<0x08> {  typedef Boolean type; };
template <> struct TypeForCode<0x10> {  typedef Bin16 type; };
template <> struct TypeForCode<0x11> {  typedef Int16 type; };
template <> struct TypeForCode<0x12> {  typedef Uint16 type; };
template <> struct TypeForCode<0x20> {  typedef Bin32 type; };
template <> struct TypeForCode<0x21> {  typedef Int32 type; };
template <> struct TypeForCode<0x22> {  typedef Uint32 type; };
template <> struct TypeForCode<0x23> {  typedef Float type; };
template <> struct TypeForCode<0x27> {  typedef CharUtf32 type; };
template <> struct TypeForCode<0x30> {  typedef Bin64 type; };
template <> struct TypeForCode<0x31> {  typedef Int64 type; };
template <> struct TypeForCode<0x32> {  typedef Uint64 type; };
template <> struct TypeForCode<0x33> {  typedef Double type; };
template <> struct TypeForCode<0x38> {  typedef Datetime type; };
template <> struct TypeForCode<0x40> {  typedef Bin128 type; };
template <> struct TypeForCode<0x48> {  typedef Uuid type; };
template <> struct TypeForCode<0x50> {  typedef Bin256 type; };
template <> struct TypeForCode<0x60> {  typedef Bin512 type; };
template <> struct TypeForCode<0x70> {  typedef Bin1024 type; };
template <> struct TypeForCode<0x80> {  typedef Vbin8 type; };
template <> struct TypeForCode<0x84> {  typedef Str8Latin type; };
template <> struct TypeForCode<0x85> {  typedef Str8 type; };
template <> struct TypeForCode<0x86> {  typedef Str8Utf16 type; };
template <> struct TypeForCode<0x90> {  typedef Vbin16 type; };
template <> struct TypeForCode<0x94> {  typedef Str16Latin type; };
template <> struct TypeForCode<0x95> {  typedef Str16 type; };
template <> struct TypeForCode<0x96> {  typedef Str16Utf16 type; };
template <> struct TypeForCode<0xa0> {  typedef Vbin32 type; };
template <> struct TypeForCode<0xa8> {  typedef Map type; };
template <> struct TypeForCode<0xa9> {  typedef List type; };
template <> struct TypeForCode<0xaa> {  typedef Array type; };
template <> struct TypeForCode<0xab> {  typedef Struct32 type; };
template <> struct TypeForCode<0xc0> {  typedef Bin40 type; };
template <> struct TypeForCode<0xc8> {  typedef Dec32 type; };
template <> struct TypeForCode<0xd0> {  typedef Bin72 type; };
template <> struct TypeForCode<0xd8> {  typedef Dec64 type; };
template <> struct TypeForCode<0xf0> {  typedef Void type; };
template <> struct TypeForCode<0xf1> {  typedef Bit type; };

template <class V> typename V::result_type
apply_visitor(V& visitor, uint8_t code) {
    switch (code) {
        case 0x00: return visitor((Bin8*)0);
        case 0x01: return visitor((Int8*)0);
        case 0x02: return visitor((Uint8*)0);
        case 0x04: return visitor((Char*)0);
        case 0x08: return visitor((Boolean*)0);
        case 0x10: return visitor((Bin16*)0);
        case 0x11: return visitor((Int16*)0);
        case 0x12: return visitor((Uint16*)0);
        case 0x20: return visitor((Bin32*)0);
        case 0x21: return visitor((Int32*)0);
        case 0x22: return visitor((Uint32*)0);
        case 0x23: return visitor((Float*)0);
        case 0x27: return visitor((CharUtf32*)0);
        case 0x30: return visitor((Bin64*)0);
        case 0x31: return visitor((Int64*)0);
        case 0x32: return visitor((Uint64*)0);
        case 0x33: return visitor((Double*)0);
        case 0x38: return visitor((Datetime*)0);
        case 0x40: return visitor((Bin128*)0);
        case 0x48: return visitor((Uuid*)0);
        case 0x50: return visitor((Bin256*)0);
        case 0x60: return visitor((Bin512*)0);
        case 0x70: return visitor((Bin1024*)0);
        case 0x80: return visitor((Vbin8*)0);
        case 0x84: return visitor((Str8Latin*)0);
        case 0x85: return visitor((Str8*)0);
        case 0x86: return visitor((Str8Utf16*)0);
        case 0x90: return visitor((Vbin16*)0);
        case 0x94: return visitor((Str16Latin*)0);
        case 0x95: return visitor((Str16*)0);
        case 0x96: return visitor((Str16Utf16*)0);
        case 0xa0: return visitor((Vbin32*)0);
        case 0xa8: return visitor((Map*)0);
        case 0xa9: return visitor((List*)0);
        case 0xaa: return visitor((Array*)0);
        case 0xab: return visitor((Struct32*)0);
        case 0xc0: return visitor((Bin40*)0);
        case 0xc8: return visitor((Dec32*)0);
        case 0xd0: return visitor((Bin72*)0);
        case 0xd8: return visitor((Dec64*)0);
        case 0xf0: return visitor((Void*)0);
        case 0xf1: return visitor((Bit*)0);
        default: return visitor((UnknownType*)0);
    }
}

std::string typeName(uint8_t code);

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_TYPEFORCODE_H*/
