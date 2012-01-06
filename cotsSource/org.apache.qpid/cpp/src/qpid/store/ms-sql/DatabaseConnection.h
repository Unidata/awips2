#ifndef QPID_STORE_MSSQL_DATABASECONNECTION_H
#define QPID_STORE_MSSQL_DATABASECONNECTION_H

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

// Bring in ADO 2.8 (yes, I know it says "15", but that's it...)
#import "C:\Program Files\Common Files\System\ado\msado15.dll" \
        no_namespace rename("EOF", "EndOfFile")

#include <string>

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class DatabaseConnection
 *
 * Represents a connection to the SQL database. This class wraps the
 * needed _ConnectionPtr for ADO as well as the needed COM initialization
 * and cleanup that each thread requires. It is expected that this class
 * will be maintained in thread-specific storage so it has no locks.
 */
class DatabaseConnection {
protected:
    _ConnectionPtr conn;

public:
    DatabaseConnection();
    ~DatabaseConnection();
    void open(const std::string& connectString,
              const std::string& dbName = "");
    void close();
    operator _ConnectionPtr () { return conn; }

    void beginTransaction() { conn->BeginTrans(); }
    void commitTransaction() {conn->CommitTrans(); }
    void rollbackTransaction() { conn->RollbackTrans(); }
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_DATABASECONNECTION_H */
