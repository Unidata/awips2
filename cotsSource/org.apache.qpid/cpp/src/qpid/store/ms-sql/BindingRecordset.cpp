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

#include "BindingRecordset.h"
#include "BlobAdapter.h"
#include "BlobEncoder.h"
#include "VariantHelper.h"

namespace qpid {
namespace store {
namespace ms_sql {

void
BindingRecordset::removeFilter(const std::string& filter)
{
    rs->PutFilter (VariantHelper<std::string>(filter));
    long recs = rs->GetRecordCount();
    if (recs == 0)
        return;   // Nothing to do
    while (recs > 0) {
        // Deleting adAffectAll doesn't work as documented; go one by one.
        rs->Delete(adAffectCurrent);
        if (--recs > 0)
            rs->MoveNext();
    }
    rs->Update();
}

void
BindingRecordset::add(uint64_t exchangeId,
                      uint64_t queueId,
                      const std::string& routingKey,
                      const qpid::framing::FieldTable& args)
{
    VariantHelper<std::string> routingKeyStr(routingKey);
    BlobEncoder blob (args);   // Marshall field table to a blob
    rs->AddNew();
    rs->Fields->GetItem("exchangeId")->Value = exchangeId;
    rs->Fields->GetItem("queueId")->Value = queueId;
    rs->Fields->GetItem("routingKey")->Value = routingKeyStr;
    rs->Fields->GetItem("fieldTableBlob")->AppendChunk(blob);
    rs->Update();
}

void
BindingRecordset::remove(uint64_t exchangeId,
                         uint64_t queueId,
                         const std::string& routingKey,
                         const qpid::framing::FieldTable& /*args*/)
{
    // Look up the affected binding.
    std::ostringstream filter;
    filter << "exchangeId = " << exchangeId
           << " AND queueId = " << queueId
           << " AND routingKey = '" << routingKey << "'" << std::ends;
    removeFilter(filter.str());
}

void
BindingRecordset::removeForExchange(uint64_t exchangeId)
{
    // Look up the affected bindings by the exchange ID
    std::ostringstream filter;
    filter << "exchangeId = " << exchangeId << std::ends;
    removeFilter(filter.str());
}

void
BindingRecordset::removeForQueue(uint64_t queueId)
{
    // Look up the affected bindings by the queue ID
    std::ostringstream filter;
    filter << "queueId = " << queueId << std::ends;
    removeFilter(filter.str());
}

void
BindingRecordset::recover(broker::RecoveryManager& recoverer,
                          const store::ExchangeMap& exchMap,
                          const store::QueueMap& queueMap)
{
    if (rs->BOF && rs->EndOfFile)
        return;   // Nothing to do
    rs->MoveFirst();
    Binding b;
    IADORecordBinding *piAdoRecordBinding;
    rs->QueryInterface(__uuidof(IADORecordBinding), 
                       (LPVOID *)&piAdoRecordBinding);
    piAdoRecordBinding->BindToRecordset(&b);
    while (!rs->EndOfFile) {
        long blobSize = rs->Fields->Item["fieldTableBlob"]->ActualSize;
        BlobAdapter blob(blobSize);
        blob = rs->Fields->Item["fieldTableBlob"]->GetChunk(blobSize);
        store::ExchangeMap::const_iterator exch = exchMap.find(b.exchangeId);
        if (exch == exchMap.end()) {
            std::ostringstream msg;
            msg << "Error recovering bindings; exchange ID " << b.exchangeId
                << " not found in exchange map";
            throw qpid::Exception(msg.str());
        }
        broker::RecoverableExchange::shared_ptr exchPtr = exch->second;
        store::QueueMap::const_iterator q = queueMap.find(b.queueId);
        if (q == queueMap.end()) {
            std::ostringstream msg;
            msg << "Error recovering bindings; queue ID " << b.queueId
                << " not found in queue map";
            throw qpid::Exception(msg.str());
        }
        broker::RecoverableQueue::shared_ptr qPtr = q->second;
        // The recovery manager wants the queue name, so get it from the
        // RecoverableQueue.
        std::string key(b.routingKey);
        exchPtr->bind(qPtr->getName(), key, blob);
        rs->MoveNext();
    }

    piAdoRecordBinding->Release();
}

void
BindingRecordset::dump()
{
    Recordset::dump();
    if (rs->EndOfFile && rs->BOF)    // No records
        return;
    rs->MoveFirst();

    Binding b;
    IADORecordBinding *piAdoRecordBinding;
    rs->QueryInterface(__uuidof(IADORecordBinding), 
                       (LPVOID *)&piAdoRecordBinding);
    piAdoRecordBinding->BindToRecordset(&b);
   
    while (VARIANT_FALSE == rs->EndOfFile) {
      QPID_LOG(notice, "exch Id " << b.exchangeId
                       << ", q Id " << b.queueId
                       << ", k: " << b.routingKey);
      rs->MoveNext();
    }

    piAdoRecordBinding->Release();
}

}}}  // namespace qpid::store::ms_sql
