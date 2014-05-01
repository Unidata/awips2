#ifndef QPID_STORE_MSSQL_BINDINGRECORDSET_H
#define QPID_STORE_MSSQL_BINDINGRECORDSET_H

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
#include "Recordset.h"
#include <qpid/store/StorageProvider.h>
#include <qpid/broker/RecoveryManager.h>

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class BindingRecordset
 *
 * Class for the binding records.
 */
class BindingRecordset : public Recordset {

    class Binding : public CADORecordBinding {
        BEGIN_ADO_BINDING(Binding)
          ADO_FIXED_LENGTH_ENTRY2(1, adBigInt, exchangeId, FALSE)
          ADO_FIXED_LENGTH_ENTRY2(2, adBigInt, queueId, FALSE)
          ADO_VARIABLE_LENGTH_ENTRY4(3, adVarChar, routingKey, 
                                     sizeof(routingKey), FALSE)
        END_ADO_BINDING()

    public:
        uint64_t exchangeId;
        uint64_t queueId;
        char routingKey[256];
    };

    // Remove all records matching the specified filter/query.
    void removeFilter(const std::string& filter);

public:
    // Add a new binding
    void add(uint64_t exchangeId,
             uint64_t queueId,
             const std::string& routingKey,
             const qpid::framing::FieldTable& args);

    // Remove a specific binding
    void remove(uint64_t exchangeId,
                uint64_t queueId,
                const std::string& routingKey,
                const qpid::framing::FieldTable& args);

    // Remove all bindings for the specified exchange
    void removeForExchange(uint64_t exchangeId);

    // Remove all bindings for the specified queue
    void removeForQueue(uint64_t queueId);

    // Recover bindings set using exchMap to get from Id to RecoverableExchange.
    void recover(qpid::broker::RecoveryManager& recoverer,
                 const qpid::store::ExchangeMap& exchMap,
                 const qpid::store::QueueMap& queueMap);

    // Dump table contents; useful for debugging.
    void dump();
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_BINDINGRECORDSET_H */
