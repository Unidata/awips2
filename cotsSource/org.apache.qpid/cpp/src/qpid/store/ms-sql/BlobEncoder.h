#ifndef QPID_STORE_MSSQL_BLOBENCODER_H
#define QPID_STORE_MSSQL_BLOBENCODER_H

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
#include <string>
#include <boost/intrusive_ptr.hpp>
#include <qpid/broker/Persistable.h>
#include <qpid/broker/PersistableMessage.h>
#include <qpid/framing/Buffer.h>
#include <qpid/framing/FieldTable.h>

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class BlobEncoder
 *
 * Encodes a blob (varbinary) field from a qpid::broker::Persistable or a
 * qpid::framing::FieldTable (both of which can be encoded to
 * qpid::framing::Buffer) so it can be passed to ADO methods for writing
 * to the database.
 */
class BlobEncoder : public _variant_t {
private:
    SAFEARRAY *blob;

    template <class ITEM> void encode(const ITEM &item);

public:
    BlobEncoder(const qpid::broker::Persistable &item);
    BlobEncoder(const boost::intrusive_ptr<qpid::broker::PersistableMessage> &msg);
    BlobEncoder(const qpid::framing::FieldTable &fields);
    BlobEncoder(const std::string& data);
    ~BlobEncoder();
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_BLOBENCODER_H */
