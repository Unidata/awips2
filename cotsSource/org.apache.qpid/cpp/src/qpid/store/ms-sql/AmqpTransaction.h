#ifndef QPID_STORE_MSSQL_AMQPTRANSACTION_H
#define QPID_STORE_MSSQL_AMQPTRANSACTION_H

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

#include <qpid/broker/TransactionalStore.h>
#include <string>
#include <memory>

namespace qpid {
namespace store {
namespace ms_sql {

class DatabaseConnection;

/**
 * @class AmqpTransaction
 *
 * Class representing an AMQP transaction. This is used around a set of
 * enqueue and dequeue operations that occur when the broker is acting
 * on a transaction commit/abort from the client.
 */
class AmqpTransaction : public qpid::broker::TransactionContext {

    std::auto_ptr<DatabaseConnection> db;

    // Since ADO w/ SQLOLEDB can't do nested transaction via its BeginTrans(),
    // et al, nested transactions are carried out with direct SQL commands.
    // To ensure the state of this is known, keep track of how deeply the
    // transactions are nested.
    unsigned int transDepth;

public:
    AmqpTransaction(std::auto_ptr<DatabaseConnection>& _db);
    virtual ~AmqpTransaction();

    DatabaseConnection *dbConn() { return db.get(); }

    void begin();
    void commit();
    void abort();
};

/**
 * @class AmqpTPCTransaction
 *
 * Class representing a Two-Phase-Commit (TPC) AMQP transaction. This is
 * used around a set of enqueue and dequeue operations that occur when the
 * broker is acting on a transaction prepare/commit/abort from the client.
 */
class AmqpTPCTransaction : public AmqpTransaction,
                           public qpid::broker::TPCTransactionContext {
    std::string  xid;

public:
    AmqpTPCTransaction(std::auto_ptr<DatabaseConnection>& _db,
                       const std::string& _xid);
    virtual ~AmqpTPCTransaction();

    void prepare();
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_AMQPTRANSACTION_H */
