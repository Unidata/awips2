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

#include "BlobEncoder.h"
#include <qpid/Exception.h>
#include <qpid/broker/Persistable.h>
#include <qpid/broker/PersistableMessage.h>
#include <boost/intrusive_ptr.hpp>
#include <memory.h>

namespace qpid {
namespace store {
namespace ms_sql {

template <class ITEM> void
BlobEncoder::encode(const ITEM &item)
{
    SAFEARRAYBOUND bound[1] = {0, 0};
    bound[0].cElements = item.encodedSize();
    blob = SafeArrayCreate(VT_UI1, 1, bound);
    if (S_OK != SafeArrayLock(blob)) {
        SafeArrayDestroy(blob);
        blob = 0;
        throw qpid::Exception("Error locking blob area for persistable item");
    }
    try {
        qpid::framing::Buffer buff((char *)blob->pvData, bound[0].cElements);
        item.encode(buff);
    }
    catch(...) {
        SafeArrayUnlock(blob);
        SafeArrayDestroy(blob);
        blob = 0;
        throw;
    }
    this->vt = VT_ARRAY | VT_UI1;
    this->parray = blob;
    SafeArrayUnlock(blob);
}

template <> void
BlobEncoder::encode(const boost::intrusive_ptr<qpid::broker::PersistableMessage> &item)
{
    // NOTE! If this code changes, verify the recovery code in MessageRecordset
    SAFEARRAYBOUND bound[1] = {0, 0};
    bound[0].cElements = item->encodedSize() + sizeof(uint32_t);
    blob = SafeArrayCreate(VT_UI1, 1, bound);
    if (S_OK != SafeArrayLock(blob)) {
        SafeArrayDestroy(blob);
        blob = 0;
        throw qpid::Exception("Error locking blob area for message");
    }
    try {
        uint32_t headerSize = item->encodedHeaderSize();
        qpid::framing::Buffer buff((char *)blob->pvData, bound[0].cElements);
        buff.putLong(headerSize);
        item->encode(buff);
    }
    catch(...) {
        SafeArrayUnlock(blob);
        SafeArrayDestroy(blob);
        blob = 0;
        throw;
    }
    this->vt = VT_ARRAY | VT_UI1;
    this->parray = blob;
    SafeArrayUnlock(blob);
}

template <> void
BlobEncoder::encode(const std::string &item)
{
    SAFEARRAYBOUND bound[1] = {0, 0};
    bound[0].cElements = item.size();
    blob = SafeArrayCreate(VT_UI1, 1, bound);
    if (S_OK != SafeArrayLock(blob)) {
        SafeArrayDestroy(blob);
        blob = 0;
        throw qpid::Exception("Error locking blob area for string");
    }
    memcpy_s(blob->pvData, item.size(), item.data(), item.size());
    this->vt = VT_ARRAY | VT_UI1;
    this->parray = blob;
    SafeArrayUnlock(blob);
}

BlobEncoder::BlobEncoder(const qpid::broker::Persistable &item) : blob(0)
{
    encode(item);
}

BlobEncoder::BlobEncoder(const boost::intrusive_ptr<qpid::broker::PersistableMessage> &msg) : blob(0)
{
    encode(msg);
}

BlobEncoder::BlobEncoder(const qpid::framing::FieldTable &fields) : blob(0)
{
    encode(fields);
}

BlobEncoder::BlobEncoder(const std::string &data) : blob(0)
{
    encode(data);
}

BlobEncoder::~BlobEncoder()
{
    if (blob)
        SafeArrayDestroy(blob);
    blob = 0;
    this->parray = 0;
}

}}}  // namespace qpid::store::ms_sql
