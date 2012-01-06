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

#include "BlobAdapter.h"
#include <qpid/Exception.h>

namespace qpid {
namespace store {
namespace ms_sql {

void
BlobAdapter::extractBuff()
{
    // To give a valid Buffer back, lock the safearray, obtaining a pointer to
    // the actual data. Record the pointer in the Buffer so the destructor
    // knows to unlock the safearray.
    if (buff.getPointer() == 0) {
        char *blob;
        SafeArrayAccessData(this->parray, (void **)&blob);
        qpid::framing::Buffer lockedBuff(blob, buff.getSize());
        buff = lockedBuff;
    }
}


BlobAdapter::~BlobAdapter()
{
    // If buff's pointer is set, the safearray is locked, so unlock it
    if (buff.getPointer() != 0)
        SafeArrayUnaccessData(this->parray);
}

BlobAdapter::operator qpid::framing::Buffer& ()
{
    extractBuff();
    return buff;
}

BlobAdapter::operator qpid::framing::FieldTable& ()
{
    extractBuff();
    fields.decode(buff);
    return fields;
}

}}}  // namespace qpid::store::ms_sql
