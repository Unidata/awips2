#ifndef QPID_STORE_MSSQL_MESSAGERECORDSET_H
#define QPID_STORE_MSSQL_MESSAGERECORDSET_H

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
#include "BlobRecordset.h"
#include <qpid/broker/PersistableMessage.h>
#include <qpid/broker/RecoveryManager.h>
#include <boost/intrusive_ptr.hpp>

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class MessageRecordset
 *
 * Class for storing and recovering messages. Messages are primarily blobs
 * and handled similarly. However, messages larger than the staging threshold
 * are not contained completely in memory; they're left mostly in the store
 * and the header is held in memory. So when the message "blob" is saved,
 * an additional size-of-the-header field is prepended to the blob.
 * On recovery, the size-of-the-header is used to get only what's needed
 * until it's determined if the entire message is to be recovered to memory.
 */
class MessageRecordset : public BlobRecordset {
    class Binding : public CADORecordBinding {
        BEGIN_ADO_BINDING(Binding)
          ADO_FIXED_LENGTH_ENTRY2(1, adBigInt, messageId, FALSE)
        END_ADO_BINDING()

    public:
        uint64_t messageId;
    };

public:
    // Store a message. Store the header size (4 bytes) then the regular
    // blob comprising the message.
    void add(const boost::intrusive_ptr<qpid::broker::PersistableMessage>& msg);

    // Append additional content to an existing message.
    void append(const boost::intrusive_ptr<const qpid::broker::PersistableMessage>& msg,
                const std::string& data);

    // Remove an existing message
    void remove(const boost::intrusive_ptr<const qpid::broker::PersistableMessage>& msg);

    // Load all or part of a stored message. This skips the header parts and
    // loads content.
    void loadContent(const boost::intrusive_ptr<const qpid::broker::PersistableMessage>& msg,
                     std::string& data,
                     uint64_t offset,
                     uint32_t length);

    // Recover messages and save a map of those recovered.
    void recover(qpid::broker::RecoveryManager& recoverer,
                 std::map<uint64_t, broker::RecoverableMessage::shared_ptr>& messageMap);

    // Dump table contents; useful for debugging.
    void dump();
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_MESSAGERECORDSET_H */
