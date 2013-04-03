#ifndef _Management_Buffer_
#define _Management_Buffer_
/*
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
#include "qpid/CommonImportExport.h"
#include "qpid/types/Exception.h"
#include "qpid/types/Variant.h"
#include <string>

namespace qpid {
namespace framing {
    class Buffer;
}

namespace management {

struct OutOfBounds : qpid::types::Exception {
    OutOfBounds() : qpid::types::Exception(std::string("Out of Bounds")) {}
};


/**
 * This class is a wrapper around qpid::framing::Buffer that does not include any dependencies
 * from boost or from qpid::framing.
 */
class Buffer
{
public:
    QPID_COMMON_EXTERN Buffer(char* data=0, uint32_t size=0);
    QPID_COMMON_EXTERN ~Buffer();

    QPID_COMMON_EXTERN void record();
    QPID_COMMON_EXTERN void restore(bool reRecord = false);
    QPID_COMMON_EXTERN void reset();

    QPID_COMMON_EXTERN uint32_t available();
    QPID_COMMON_EXTERN uint32_t getSize();
    QPID_COMMON_EXTERN uint32_t getPosition();
    QPID_COMMON_EXTERN char* getPointer();

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

    QPID_COMMON_EXTERN void putShortString(const std::string& s);
    QPID_COMMON_EXTERN void putMediumString(const std::string& s);
    QPID_COMMON_EXTERN void putLongString(const std::string& s);
    QPID_COMMON_EXTERN void getShortString(std::string& s);
    QPID_COMMON_EXTERN void getMediumString(std::string& s);
    QPID_COMMON_EXTERN void getLongString(std::string& s);
    QPID_COMMON_EXTERN void getBin128(uint8_t* b);

    QPID_COMMON_EXTERN void putMap(const types::Variant::Map& map);
    QPID_COMMON_EXTERN void putList(const types::Variant::List& list);
    QPID_COMMON_EXTERN void getMap(types::Variant::Map& map);
    QPID_COMMON_EXTERN void getList(types::Variant::List& list);

    QPID_COMMON_EXTERN void putRawData(const std::string& s);
    QPID_COMMON_EXTERN void getRawData(std::string& s, uint32_t size);

    QPID_COMMON_EXTERN void putRawData(const uint8_t* data, size_t size);
    QPID_COMMON_EXTERN void getRawData(uint8_t* data, size_t size);

private:
    framing::Buffer* impl;
};

}} // namespace qpid::management

#endif
