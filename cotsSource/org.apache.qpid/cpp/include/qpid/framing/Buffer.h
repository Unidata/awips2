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
#include "qpid/framing/amqp_types.h"
#include "qpid/Exception.h"
#include "qpid/CommonImportExport.h"
#include <boost/iterator/iterator_facade.hpp>

#ifndef _Buffer_
#define _Buffer_

namespace qpid {
namespace framing {

struct OutOfBounds : qpid::Exception {
    OutOfBounds() : qpid::Exception(std::string("Out of Bounds")) {}
};

class Content;
class FieldTable;

class Buffer
{
    uint32_t size;
    char* data;
    uint32_t position;
    uint32_t r_position;

    void checkAvailable(uint32_t count) { if (position + count > size) throw OutOfBounds(); }

  public:

    /** Buffer input/output iterator.
     * Supports using an amqp_0_10::Codec with a framing::Buffer.
     */
    class Iterator  : public boost::iterator_facade<
      Iterator, char, boost::random_access_traversal_tag>
    {
      public:
        Iterator(Buffer& b) : buffer(&b) {}

      private:
      friend class boost::iterator_core_access;
        char& dereference() const { return buffer->data[buffer->position]; }
        void increment() { ++buffer->position; }
        bool equal(const Iterator& x) const { return buffer == x.buffer; }

        Buffer* buffer;
    };

  friend class Iterator;

    QPID_COMMON_EXTERN Buffer(char* data=0, uint32_t size=0);

    QPID_COMMON_EXTERN void record();
    QPID_COMMON_EXTERN void restore(bool reRecord = false);
    QPID_COMMON_EXTERN void reset();

    QPID_COMMON_EXTERN uint32_t available() { return size - position; }
    QPID_COMMON_EXTERN uint32_t getSize() { return size; }
    QPID_COMMON_EXTERN uint32_t getPosition() { return position; }
    QPID_COMMON_EXTERN Iterator getIterator() { return Iterator(*this); }
    QPID_COMMON_EXTERN char* getPointer() { return data; }

    QPID_COMMON_EXTERN void putOctet(uint8_t i);
    QPID_COMMON_EXTERN void putShort(uint16_t i);
    QPID_COMMON_EXTERN void putLong(uint32_t i);
    QPID_COMMON_EXTERN void putLongLong(uint64_t i);
    QPID_COMMON_EXTERN void putInt8(int8_t i);
    QPID_COMMON_EXTERN void putInt16(int16_t i);
    QPID_COMMON_EXTERN void putInt32(int32_t i);
    QPID_COMMON_EXTERN void putInt64(int64_t i);
    QPID_COMMON_EXTERN void putFloat(float f);
    QPID_COMMON_EXTERN void putDouble(double f);
    QPID_COMMON_EXTERN void putBin128(const uint8_t* b);

    QPID_COMMON_EXTERN uint8_t  getOctet();
    QPID_COMMON_EXTERN uint16_t getShort();
    QPID_COMMON_EXTERN uint32_t getLong();
    QPID_COMMON_EXTERN uint64_t getLongLong();
    QPID_COMMON_EXTERN int8_t   getInt8();
    QPID_COMMON_EXTERN int16_t  getInt16();
    QPID_COMMON_EXTERN int32_t  getInt32();
    QPID_COMMON_EXTERN int64_t  getInt64();
    QPID_COMMON_EXTERN float    getFloat();
    QPID_COMMON_EXTERN double   getDouble();

    template <int n>
    QPID_COMMON_EXTERN uint64_t getUInt();

    template <int n>
    QPID_COMMON_EXTERN void putUInt(uint64_t);

    QPID_COMMON_EXTERN void putShortString(const string& s);
    QPID_COMMON_EXTERN void putMediumString(const string& s);
    QPID_COMMON_EXTERN void putLongString(const string& s);
    QPID_COMMON_EXTERN void getShortString(string& s);
    QPID_COMMON_EXTERN void getMediumString(string& s);
    QPID_COMMON_EXTERN void getLongString(string& s);
    QPID_COMMON_EXTERN void getBin128(uint8_t* b);

    QPID_COMMON_EXTERN void putRawData(const string& s);
    QPID_COMMON_EXTERN void getRawData(string& s, uint32_t size);

    QPID_COMMON_EXTERN void putRawData(const uint8_t* data, size_t size);
    QPID_COMMON_EXTERN void getRawData(uint8_t* data, size_t size);

    template <class T> void put(const T& data) { data.encode(*this); }
    template <class T> void get(T& data) { data.decode(*this); }

    QPID_COMMON_EXTERN void dump(std::ostream&) const;
};

std::ostream& operator<<(std::ostream&, const Buffer&);

}} // namespace qpid::framing


#endif
