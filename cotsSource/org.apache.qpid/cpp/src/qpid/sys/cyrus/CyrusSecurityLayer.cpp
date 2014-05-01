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
#include "qpid/sys/cyrus/CyrusSecurityLayer.h"
#include <algorithm>
#include "qpid/framing/reply_exceptions.h"
#include "qpid/log/Statement.h"
#include <string.h>

namespace qpid {
namespace sys {
namespace cyrus {

CyrusSecurityLayer::CyrusSecurityLayer(sasl_conn_t* c, uint16_t maxFrameSize) : 
    conn(c), decrypted(0), decryptedSize(0), encrypted(0), encryptedSize(0), codec(0), maxInputSize(0), 
    decodeBuffer(maxFrameSize), encodeBuffer(maxFrameSize), encoded(0)
{
    const void* value(0);
    int result = sasl_getprop(conn, SASL_MAXOUTBUF, &value);
    if (result != SASL_OK) {
        throw framing::InternalErrorException(QPID_MSG("SASL encode error: " << sasl_errdetail(conn)));
    }
    maxInputSize = *(reinterpret_cast<const unsigned*>(value));
}

size_t CyrusSecurityLayer::decode(const char* input, size_t size)
{
    size_t inStart = 0;
    do {
        size_t inSize = std::min(size - inStart, maxInputSize);
        int result = sasl_decode(conn, input + inStart, inSize, &decrypted, &decryptedSize);
        if (result != SASL_OK) {
            throw framing::InternalErrorException(QPID_MSG("SASL decode error: " << sasl_errdetail(conn)));
        }
        inStart += inSize;
        size_t copied = 0;
        do {
            size_t count = std::min(decryptedSize - copied, decodeBuffer.size - decodeBuffer.position);
            ::memcpy(decodeBuffer.data + decodeBuffer.position, decrypted + copied, count);
            copied += count;
            decodeBuffer.position += count;
            size_t decodedSize = codec->decode(decodeBuffer.data, decodeBuffer.position);
            if (decodedSize < decodeBuffer.position) {
                ::memmove(decodeBuffer.data, decodeBuffer.data + decodedSize, decodeBuffer.position - decodedSize);
            }
            decodeBuffer.position -= decodedSize;
        } while (copied < decryptedSize);
    } while (inStart < size);
    return size;
}

size_t CyrusSecurityLayer::encode(const char* buffer, size_t size)
{
    size_t processed = 0;//records how many bytes have been written to buffer
    do {
        if (!encrypted) {
            if (!encoded) {
                encodeBuffer.position = 0;
                encoded = codec->encode(encodeBuffer.data, encodeBuffer.size);
                if (!encoded) break;//nothing more to do
            }

            size_t encryptable = std::min(encoded, maxInputSize); 
            int result = sasl_encode(conn, encodeBuffer.data + encodeBuffer.position, encryptable, &encrypted, &encryptedSize);
            if (result != SASL_OK) {
                throw framing::InternalErrorException(QPID_MSG("SASL encode error: " << sasl_errdetail(conn)));
            }
            encodeBuffer.position += encryptable;
            encoded -= encryptable;
        }
        size_t remaining = size - processed;
        if (remaining < encryptedSize) {
            //can't fit all encrypted data in the buffer we've
            //been given, copy in what we can and hold on to the
            //rest until the next call
            ::memcpy(const_cast<char*>(buffer + processed), encrypted, remaining);
            processed += remaining;
            encrypted += remaining;
            encryptedSize -= remaining;
        } else {
            ::memcpy(const_cast<char*>(buffer + processed), encrypted, encryptedSize);
            processed += encryptedSize;
            encrypted = 0; 
            encryptedSize = 0;
        }
    } while (processed < size);
    return processed;
}

bool CyrusSecurityLayer::canEncode()
{
    return encrypted || codec->canEncode();
}

void CyrusSecurityLayer::init(qpid::sys::Codec* c)
{
    codec = c;
}

CyrusSecurityLayer::DataBuffer::DataBuffer(size_t s) : position(0), size(s)
{
    data = new char[size];
}

CyrusSecurityLayer::DataBuffer::~DataBuffer()
{
    delete[] data;
}

}}} // namespace qpid::sys::cyrus
