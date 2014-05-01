#ifndef QPID_STORE_MSSQL_BLOBADAPTER_H
#define QPID_STORE_MSSQL_BLOBADAPTER_H

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

#include <comutil.h>
#include <qpid/framing/Buffer.h>
#include <qpid/framing/FieldTable.h>

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class BlobAdapter
 *
 * Adapter for accessing a blob (varbinary SQL field) as a qpid::framing::Buffer
 * in an exception-safe way.
 */
class BlobAdapter : public _variant_t {
private:
    // This Buffer's pointer indicates whether or not a safearray has
    // been locked; if it's 0, no locking was done.
    qpid::framing::Buffer buff;
    qpid::framing::FieldTable fields;

    void extractBuff();

public:
    // Initialize with the known length of the data that will come.
    // Assigning a _variant_t to this object will set up the array to be
    // accessed with the operator Buffer&()
    BlobAdapter(long blobSize) : _variant_t(), buff(0, blobSize) {}
    ~BlobAdapter();
    BlobAdapter& operator=(_variant_t& var_t_Src)
      { _variant_t::operator=(var_t_Src); return *this; }
    operator qpid::framing::Buffer& ();
    operator qpid::framing::FieldTable& ();
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_BLOBADAPTER_H */
