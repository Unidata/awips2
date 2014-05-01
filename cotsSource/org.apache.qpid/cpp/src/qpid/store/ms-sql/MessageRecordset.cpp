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

#include <qpid/Exception.h>
#include <qpid/log/Statement.h>

#include "MessageRecordset.h"
#include "BlobAdapter.h"
#include "BlobEncoder.h"
#include "VariantHelper.h"

#include <boost/intrusive_ptr.hpp>

class qpid::broker::PersistableMessage;

namespace qpid {
namespace store {
namespace ms_sql {

void
MessageRecordset::add(const boost::intrusive_ptr<qpid::broker::PersistableMessage>& msg)
{
    BlobEncoder blob (msg);   // Marshall headers and content to a blob
    rs->AddNew();
    rs->Fields->GetItem("fieldTableBlob")->AppendChunk(blob);
    rs->Update();
    uint64_t id = rs->Fields->Item["persistenceId"]->Value;
    msg->setPersistenceId(id);
}

void
MessageRecordset::append(const boost::intrusive_ptr<const qpid::broker::PersistableMessage>& msg,
                         const std::string& data)
{
    // Look up the message by its Id
    std::ostringstream filter;
    filter << "persistenceId = " << msg->getPersistenceId() << std::ends;
    rs->PutFilter (VariantHelper<std::string>(filter.str()));
    if (rs->RecordCount == 0) {
        throw Exception("Can't append to message not stored in database");
    }
    BlobEncoder blob (data);   // Marshall string data to a blob
    rs->Fields->GetItem("fieldTableBlob")->AppendChunk(blob);
    rs->Update();
}

void
MessageRecordset::remove(const boost::intrusive_ptr<const qpid::broker::PersistableMessage>& msg)
{
    BlobRecordset::remove(msg->getPersistenceId());
}

void
MessageRecordset::loadContent(const boost::intrusive_ptr<const qpid::broker::PersistableMessage>& msg,
                              std::string& data,
                              uint64_t offset,
                              uint32_t length)
{
    // Look up the message by its Id
    std::ostringstream filter;
    filter << "persistenceId = " << msg->getPersistenceId() << std::ends;
    rs->PutFilter (VariantHelper<std::string>(filter.str()));
    if (rs->RecordCount == 0) {
        throw Exception("Can't load message not stored in database");
    }

    // NOTE! If this code needs to change, please verify the encoding
    // code in BlobEncoder.
    long blobSize = rs->Fields->Item["fieldTableBlob"]->ActualSize;
    uint32_t headerSize;
    const size_t headerFieldLength = sizeof(headerSize);
    BlobAdapter blob(headerFieldLength);
    blob =
        rs->Fields->Item["fieldTableBlob"]->GetChunk((long)headerFieldLength);
    headerSize = ((qpid::framing::Buffer&)blob).getLong();

    // GetChunk always begins reading where the previous GetChunk left off,
    // so we can't just tell it to ignore the header and read the data.
    // So, read the header plus the offset, plus the desired data, then
    // copy the desired data to the supplied string. If this ends up asking
    // for more than is available in the field, reduce it to what's there.
    long getSize = headerSize + offset + length;
    if (getSize + (long)headerFieldLength > blobSize) {
        size_t reduce = (getSize + headerFieldLength) - blobSize;
        getSize -= reduce;
        length -= reduce;
    }
    BlobAdapter header_plus(getSize);
    header_plus = rs->Fields->Item["fieldTableBlob"]->GetChunk(getSize);
    uint8_t *throw_away = new uint8_t[headerSize + offset];
    ((qpid::framing::Buffer&)header_plus).getRawData(throw_away, headerSize + offset);
    delete throw_away;
    ((qpid::framing::Buffer&)header_plus).getRawData(data, length);
}

void
MessageRecordset::recover(qpid::broker::RecoveryManager& recoverer,
                          std::map<uint64_t, broker::RecoverableMessage::shared_ptr>& messageMap)
{
    if (rs->BOF && rs->EndOfFile)
        return;   // Nothing to do
    rs->MoveFirst();
    Binding b;
    IADORecordBinding *piAdoRecordBinding;
    rs->QueryInterface(__uuidof(IADORecordBinding), 
                       (LPVOID *)&piAdoRecordBinding);
    piAdoRecordBinding->BindToRecordset(&b);
    while (!rs->EndOfFile) {
        // The blob was written as normal, but with the header length
        // prepended in a uint32_t. Due to message staging threshold
        // limits, the header may be all that's read in; get it first,
        // recover that message header, then see if the rest is needed.
        //
        // NOTE! If this code needs to change, please verify the encoding
        // code in BlobEncoder.
        long blobSize = rs->Fields->Item["fieldTableBlob"]->ActualSize;
        uint32_t headerSize;
        const size_t headerFieldLength = sizeof(headerSize);
        BlobAdapter blob(headerFieldLength);
        blob =
          rs->Fields->Item["fieldTableBlob"]->GetChunk((long)headerFieldLength);
        headerSize = ((qpid::framing::Buffer&)blob).getLong();
        BlobAdapter header(headerSize);
        header = rs->Fields->Item["fieldTableBlob"]->GetChunk(headerSize);
        broker::RecoverableMessage::shared_ptr msg;
        msg = recoverer.recoverMessage(header);
        msg->setPersistenceId(b.messageId);
        messageMap[b.messageId] = msg;

        // Now, do we need the rest of the content?
        long contentLength = blobSize - headerFieldLength - headerSize;
        if (msg->loadContent(contentLength)) {
            BlobAdapter content(contentLength);
             content =
                rs->Fields->Item["fieldTableBlob"]->GetChunk(contentLength);
            msg->decodeContent(content);
        }
        rs->MoveNext();
    }

    piAdoRecordBinding->Release();
}

void
MessageRecordset::dump()
{
    Recordset::dump();
    if (rs->EndOfFile && rs->BOF)    // No records
        return;
    rs->MoveFirst();

    Binding b;
    IADORecordBinding *piAdoRecordBinding;
    rs->QueryInterface(__uuidof(IADORecordBinding), 
                       (LPVOID *)&piAdoRecordBinding);
    piAdoRecordBinding->BindToRecordset(&b);
   
    while (VARIANT_FALSE == rs->EndOfFile) {
        QPID_LOG(notice, "Msg " << b.messageId);
        rs->MoveNext();
    }

    piAdoRecordBinding->Release();
}

}}}  // namespace qpid::store::ms_sql
