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

#include "DatabaseConnection.h"
#include "Exception.h"
#include <comdef.h>
namespace {
inline void TESTHR(HRESULT x) {if FAILED(x) _com_issue_error(x);};
}

namespace qpid {
namespace store {
namespace ms_sql {

DatabaseConnection::DatabaseConnection() : conn(0)
{
}

DatabaseConnection::~DatabaseConnection()
{
    close();
}

void
DatabaseConnection::open(const std::string& connectString,
                         const std::string& dbName)
{
    if (conn && conn->State == adStateOpen)
        return;
    std::string adoConnect = "Provider=SQLOLEDB;" + connectString;
    try {
        TESTHR(conn.CreateInstance(__uuidof(Connection)));
        conn->ConnectionString = adoConnect.c_str();
        conn->Open("", "", "", adConnectUnspecified);
        if (dbName.length() > 0)
            conn->DefaultDatabase = dbName.c_str();
    }
    catch(_com_error &e) {
        close();
        throw ADOException("MSSQL can't open " + dbName + " at " + adoConnect, e);
    }
}

void
DatabaseConnection::close()
{
    if (conn && conn->State == adStateOpen)
        conn->Close();
    conn = 0;
}

}}}  // namespace qpid::store::ms_sql
