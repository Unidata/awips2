#ifndef QPID_FRAMING_TYPECODE_H
#define QPID_FRAMING_TYPECODE_H
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


#include <iosfwd>
#include "qpid/sys/IntegerTypes.h"

namespace qpid {
namespace framing {

enum TypeCode {
    TYPE_CODE_BIN8 = 0x00,
    TYPE_CODE_INT8 = 0x01,
    TYPE_CODE_UINT8 = 0x02,
    TYPE_CODE_CHAR = 0x04,
    TYPE_CODE_BOOLEAN = 0x08,
    TYPE_CODE_BIN16 = 0x10,
    TYPE_CODE_INT16 = 0x11,
    TYPE_CODE_UINT16 = 0x12,
    TYPE_CODE_BIN32 = 0x20,
    TYPE_CODE_INT32 = 0x21,
    TYPE_CODE_UINT32 = 0x22,
    TYPE_CODE_FLOAT = 0x23,
    TYPE_CODE_CHAR_UTF32 = 0x27,
    TYPE_CODE_BIN64 = 0x30,
    TYPE_CODE_INT64 = 0x31,
    TYPE_CODE_UINT64 = 0x32,
    TYPE_CODE_DOUBLE = 0x33,
    TYPE_CODE_DATETIME = 0x38,
    TYPE_CODE_BIN128 = 0x40,
    TYPE_CODE_UUID = 0x48,
    TYPE_CODE_BIN256 = 0x50,
    TYPE_CODE_BIN512 = 0x60,
    TYPE_CODE_BIN1024 = 0x70,
    TYPE_CODE_VBIN8 = 0x80,
    TYPE_CODE_STR8_LATIN = 0x84,
    TYPE_CODE_STR8 = 0x85,
    TYPE_CODE_STR8_UTF16 = 0x86,
    TYPE_CODE_VBIN16 = 0x90,
    TYPE_CODE_STR16_LATIN = 0x94,
    TYPE_CODE_STR16 = 0x95,
    TYPE_CODE_STR16_UTF16 = 0x96,
    TYPE_CODE_VBIN32 = 0xa0,
    TYPE_CODE_MAP = 0xa8,
    TYPE_CODE_LIST = 0xa9,
    TYPE_CODE_ARRAY = 0xaa,
    TYPE_CODE_STRUCT32 = 0xab,
    TYPE_CODE_BIN40 = 0xc0,
    TYPE_CODE_DEC32 = 0xc8,
    TYPE_CODE_BIN72 = 0xd0,
    TYPE_CODE_DEC64 = 0xd8,
    TYPE_CODE_VOID = 0xf0,
    TYPE_CODE_BIT = 0xf1
};

/** True if t is a valid TypeCode value */
bool isTypeCode(uint8_t t);

/** Throw exception if not a valid TypeCode */
TypeCode typeCode(uint8_t);

/**@return 0 if t is not a valid enum TypeCode value. */
const char* typeName(TypeCode t);

std::ostream& operator<<(std::ostream&, TypeCode);


}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_TYPECODE_H*/
