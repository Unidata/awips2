#ifndef QPID_AMQP_0_10_CODEFORTYPE_H
#define QPID_AMQP_0_10_CODEFORTYPE_H
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

namespace qpid {
namespace amqp_0_10 {


template <class T> struct CodeForType;

template <> struct CodeForType<Bin8> { static const uint8_t value; };
template <> struct CodeForType<Int8> { static const uint8_t value; };
template <> struct CodeForType<Uint8> { static const uint8_t value; };
template <> struct CodeForType<Char> { static const uint8_t value; };
template <> struct CodeForType<Boolean> { static const uint8_t value; };
template <> struct CodeForType<Bin16> { static const uint8_t value; };
template <> struct CodeForType<Int16> { static const uint8_t value; };
template <> struct CodeForType<Uint16> { static const uint8_t value; };
template <> struct CodeForType<Bin32> { static const uint8_t value; };
template <> struct CodeForType<Int32> { static const uint8_t value; };
template <> struct CodeForType<Uint32> { static const uint8_t value; };
template <> struct CodeForType<Float> { static const uint8_t value; };
template <> struct CodeForType<CharUtf32> { static const uint8_t value; };
template <> struct CodeForType<Bin64> { static const uint8_t value; };
template <> struct CodeForType<Int64> { static const uint8_t value; };
template <> struct CodeForType<Uint64> { static const uint8_t value; };
template <> struct CodeForType<Double> { static const uint8_t value; };
template <> struct CodeForType<Datetime> { static const uint8_t value; };
template <> struct CodeForType<Bin128> { static const uint8_t value; };
template <> struct CodeForType<Uuid> { static const uint8_t value; };
template <> struct CodeForType<Bin256> { static const uint8_t value; };
template <> struct CodeForType<Bin512> { static const uint8_t value; };
template <> struct CodeForType<Bin1024> { static const uint8_t value; };
template <> struct CodeForType<Vbin8> { static const uint8_t value; };
template <> struct CodeForType<Str8Latin> { static const uint8_t value; };
template <> struct CodeForType<Str8> { static const uint8_t value; };
template <> struct CodeForType<Str8Utf16> { static const uint8_t value; };
template <> struct CodeForType<Vbin16> { static const uint8_t value; };
template <> struct CodeForType<Str16Latin> { static const uint8_t value; };
template <> struct CodeForType<Str16> { static const uint8_t value; };
template <> struct CodeForType<Str16Utf16> { static const uint8_t value; };
template <> struct CodeForType<Vbin32> { static const uint8_t value; };
template <> struct CodeForType<Map> { static const uint8_t value; };
template <> struct CodeForType<List> { static const uint8_t value; };
template <> struct CodeForType<Array> { static const uint8_t value; };
template <> struct CodeForType<Struct32> { static const uint8_t value; };
template <> struct CodeForType<Bin40> { static const uint8_t value; };
template <> struct CodeForType<Dec32> { static const uint8_t value; };
template <> struct CodeForType<Bin72> { static const uint8_t value; };
template <> struct CodeForType<Dec64> { static const uint8_t value; };
template <> struct CodeForType<Void> { static const uint8_t value; };
template <> struct CodeForType<Bit> { static const uint8_t value; };

template <class T> uint8_t codeFor(const T&) { return CodeForType<T>::value; }

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_CODEFORTYPE_H*/
