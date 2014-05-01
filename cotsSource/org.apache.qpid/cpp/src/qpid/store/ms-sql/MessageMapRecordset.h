#ifndef QPID_STORE_MSSQL_MESSAGEMAPRECORDSET_H
#define QPID_STORE_MSSQL_MESSAGEMAPRECORDSET_H

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

#include <icrsint.h>
#include <vector>
#include "Recordset.h"
#include <qpid/broker/RecoveryManager.h>

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class MessageMapRecordset
 *
 * Class for the message map (message -> queue) records.
 */
class MessageMapRecordset : public Recordset {

    class MessageMap : public CADORecordBinding {
        BEGIN_ADO_BINDING(MessageMap)
          ADO_FIXED_LENGTH_ENTRY2(1, adBigInt, messageId, FALSE)
          ADO_FIXED_LENGTH_ENTRY2(2, adBigInt, queueId, FALSE)
        END_ADO_BINDING()

    public:
        uint64_t messageId;
        uint64_t queueId;
    };

public:
    // Add a new mapping
    void add(uint64_t messageId, uint64_t queueId);

    // Remove a specific mapping. Returns true if the message is still
    // enqueued on at least one other queue; false if the message no longer
    // exists on any other queues.
    bool remove(uint64_t messageId, uint64_t queueId);

    // Remove mappings for all messages on a specified queue. If there are
    // messages that were only on the specified queue and are, therefore,
    // orphaned now, return them in the orphaned vector. The orphaned
    // messages can be deleted permanently as they are not referenced on
    // any other queues.
    void removeForQueue(uint64_t queueId, std::vector<uint64_t>& orphaned);

    // Recover the mappings of message ID -> vector<queue ID>.
    void recover(MessageQueueMap& msgMap);

    // Dump table contents; useful for debugging.
    void dump();
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_MESSAGEMAPRECORDSET_H */
