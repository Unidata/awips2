#ifndef QPID_STORE_MSSQL_STATE_H
#define QPID_STORE_MSSQL_STATE_H

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

namespace qpid {
namespace store {
namespace ms_sql {

class DatabaseConnection;

/**
 * @struct State
 *
 * Represents a thread's state for accessing ADO and the database.
 * Creating an instance of State initializes COM for this thread, and
 * destroying it uninitializes COM. There's also a DatabaseConnection
 * for this thread's default access to the database. More DatabaseConnections
 * can always be created, but State has one that can always be used by
 * the thread whose state is represented.
 *
 * This class is intended to be one-per-thread, so it should be accessed
 * via thread-specific storage.
 */
struct State {
    State();
    ~State();
    DatabaseConnection *dbConn;
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_STATE_H */
