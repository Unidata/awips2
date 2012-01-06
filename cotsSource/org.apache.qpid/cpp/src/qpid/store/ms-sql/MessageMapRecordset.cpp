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
#include <qpid/store/StorageProvider.h>

#include "MessageMapRecordset.h"
#include "VariantHelper.h"

namespace qpid {
namespace store {
namespace ms_sql {

void
MessageMapRecordset::add(uint64_t messageId, uint64_t queueId)
{
    rs->AddNew();
    rs->Fields->GetItem("messageId")->Value = messageId;
    rs->Fields->GetItem("queueId")->Value = queueId;
    rs->Update();
}

bool
MessageMapRecordset::remove(uint64_t messageId, uint64_t queueId)
{
    // Look up all mappings for the specified message. Then scan
    // for the specified queue and keep track of whether or not the
    // message exists on any queue we are not looking for a well.
    std::ostringstream filter;
    filter << "messageId = " << messageId << std::ends;
    rs->PutFilter (VariantHelper<std::string>(filter.str()));
    if (rs->RecordCount == 0)
        return false;
    MessageMap m;
    IADORecordBinding *piAdoRecordBinding;
    rs->QueryInterface(__uuidof(IADORecordBinding),
                       (LPVOID *)&piAdoRecordBinding);
    piAdoRecordBinding->BindToRecordset(&m);
    bool moreEntries = false, deleted = false;
    rs->MoveFirst();
    // If the desired mapping gets deleted, and we already know there are
    // other mappings for the message, don't bother finishing the scan.
    while (!rs->EndOfFile && !(deleted && moreEntries)) {
        if (m.queueId == queueId) {
            rs->Delete(adAffectCurrent);
            rs->Update();
            deleted = true;
        }
        else {
            moreEntries = true;
        }
        rs->MoveNext();
    }
    piAdoRecordBinding->Release();
    rs->Filter = "";
    return moreEntries;
}

void
MessageMapRecordset::removeForQueue(uint64_t queueId,
                                    std::vector<uint64_t>& orphaned)
{
    // Read all the messages queued on queueId and add them to the orphaned
    // list. Then remove each one and learn if there are references to it
    // from other queues. The ones without references are left in the
    // orphaned list, others are removed.
    std::ostringstream filter;
    filter << "queueId = " << queueId << std::ends;
    rs->PutFilter (VariantHelper<std::string>(filter.str()));
    MessageMap m;
    IADORecordBinding *piAdoRecordBinding;
    rs->QueryInterface(__uuidof(IADORecordBinding), 
                       (LPVOID *)&piAdoRecordBinding);
    piAdoRecordBinding->BindToRecordset(&m);
    while (!rs->EndOfFile) {
        orphaned.push_back(m.messageId);
        rs->MoveNext();
    }
    piAdoRecordBinding->Release();
    rs->Filter = "";     // Remove filter on queueId
    rs->Requery(adOptionUnspecified);  // Get the entire map again

    // Now delete all the messages on this queue; any message that still has
    // references from other queue(s) is removed from orphaned.
    for (std::vector<uint64_t>::iterator i = orphaned.begin();
         i != orphaned.end();
         ) {
        if (remove(*i, queueId))
            i = orphaned.erase(i);     // There are other refs to message *i
        else
            ++i;
    }
}

void
MessageMapRecordset::recover(MessageQueueMap& msgMap)
{
    if (rs->BOF && rs->EndOfFile)
        return;   // Nothing to do
    rs->MoveFirst();
    MessageMap b;
    IADORecordBinding *piAdoRecordBinding;
    rs->QueryInterface(__uuidof(IADORecordBinding), 
                       (LPVOID *)&piAdoRecordBinding);
    piAdoRecordBinding->BindToRecordset(&b);
    while (!rs->EndOfFile) {
        msgMap[b.messageId].push_back(b.queueId);
        rs->MoveNext();
    }

    piAdoRecordBinding->Release();
}

void
MessageMapRecordset::dump()
{
    Recordset::dump();
    if (rs->EndOfFile && rs->BOF)    // No records
        return;
    rs->MoveFirst();

    MessageMap m;
    IADORecordBinding *piAdoRecordBinding;
    rs->QueryInterface(__uuidof(IADORecordBinding), 
                       (LPVOID *)&piAdoRecordBinding);
    piAdoRecordBinding->BindToRecordset(&m);
   
    while (!rs->EndOfFile) {
        QPID_LOG(notice, "msg " << m.messageId << " on queue " << m.queueId);
        rs->MoveNext();
    }

    piAdoRecordBinding->Release();
}

}}}  // namespace qpid::store::ms_sql
