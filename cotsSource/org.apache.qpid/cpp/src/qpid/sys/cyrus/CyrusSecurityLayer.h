#ifndef QPID_SYS_CYRUS_CYRUSSECURITYLAYER_H
#define QPID_SYS_CYRUS_CYRUSSECURITYLAYER_H

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

#include "qpid/sys/IntegerTypes.h"
#include "qpid/sys/SecurityLayer.h"
#include <sasl/sasl.h>

namespace qpid {
namespace sys {
namespace cyrus {


/**
 * Implementation of SASL security layer using cyrus-sasl library
 */
class CyrusSecurityLayer : public qpid::sys::SecurityLayer
{
  public:
    CyrusSecurityLayer(sasl_conn_t*, uint16_t maxFrameSize);
    size_t decode(const char* buffer, size_t size);
    size_t encode(const char* buffer, size_t size);
    bool canEncode();
    void init(qpid::sys::Codec*);
  private:
    struct DataBuffer
    {
        char* data;
        size_t position;
        const size_t size;
        DataBuffer(size_t);
        ~DataBuffer();
    };

    sasl_conn_t* conn;
    const char* decrypted;
    unsigned decryptedSize;
    const char* encrypted;
    unsigned encryptedSize;
    qpid::sys::Codec* codec;
    size_t maxInputSize;
    DataBuffer decodeBuffer;
    DataBuffer encodeBuffer;
    size_t encoded;
};
}}} // namespace qpid::sys::cyrus

#endif  /*!QPID_SYS_CYRUS_CYRUSSECURITYLAYER_H*/
