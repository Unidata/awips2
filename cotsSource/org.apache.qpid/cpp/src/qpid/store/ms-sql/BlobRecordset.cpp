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

#include <qpid/Exception.h>
#include <qpid/log/Statement.h>

#include "BlobRecordset.h"
#include "BlobEncoder.h"
#include "VariantHelper.h"

namespace qpid {
namespace store {
namespace ms_sql {

void
BlobRecordset::add(const qpid::broker::Persistable& item)
{
    BlobEncoder blob (item);   // Marshall item info to a blob
    rs->AddNew();
    rs->Fields->GetItem("fieldTableBlob")->AppendChunk(blob);
    rs->Update();
    uint64_t id = rs->Fields->Item["persistenceId"]->Value;
    item.setPersistenceId(id);
}

void
BlobRecordset::remove(uint64_t id)
{
    // Look up the item by its persistenceId
    std::ostringstream filter;
    filter << "persistenceId = " << id << std::ends;
    rs->PutFilter (VariantHelper<std::string>(filter.str()));
    if (!rs->EndOfFile) {
        // Delete the record
        rs->Delete(adAffectCurrent);
        rs->Update();
    }
}

void
BlobRecordset::remove(const qpid::broker::Persistable& item)
{
    remove(item.getPersistenceId());
}

void
BlobRecordset::dump()
{
    Recordset::dump();
#if 1
    if (rs->EndOfFile && rs->BOF)    // No records
        return;

    rs->MoveFirst();
    while (!rs->EndOfFile) {
        uint64_t id = rs->Fields->Item["persistenceId"]->Value;
        QPID_LOG(notice, "  -> " << id);
        rs->MoveNext();
    }
#else
    for (Iterator iter = begin(); iter != end(); ++iter) {
        uint64_t id = *iter.first;
        QPID_LOG(notice, "  -> " << id);
    }
#endif
}

}}}  // namespace qpid::store::ms_sql
