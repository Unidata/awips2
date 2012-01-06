#ifndef QPID_STORE_MSSQL_BLOBRECORDSET_H
#define QPID_STORE_MSSQL_BLOBRECORDSET_H

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

#include "Recordset.h"
#include <qpid/broker/Persistable.h>
#include <string>

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class BlobRecordset
 *
 * Class for the "blob" records that record an id, varbinary(max) pair.
 */
class BlobRecordset : public Recordset {
protected:

public:
    void add(const qpid::broker::Persistable& item);

    // Remove a record given its Id.
    void remove(uint64_t id);
    void remove(const qpid::broker::Persistable& item);

    // Dump table contents; useful for debugging.
    void dump();
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_BLOBRECORDSET_H */
