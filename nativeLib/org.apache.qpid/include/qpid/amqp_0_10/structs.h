#ifndef QPID_AMQP_0_10_STRUCTS_H
#define QPID_AMQP_0_10_STRUCTS_H
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

///
/// This file was automatically generated from the AMQP specification.
/// Do not edit.
///


#include "qpid/amqp_0_10/specification_fwd.h"
#include "qpid/amqp_0_10/Map.h"
#include "qpid/amqp_0_10/Array.h"
#include "qpid/amqp_0_10/Struct.h"
#include "qpid/amqp_0_10/UnknownStruct.h"
#include "qpid/amqp_0_10/Packer.h"

namespace qpid {
namespace amqp_0_10 {


namespace connection {


} // namespace connection


namespace session {


struct Header
{
    Bit sync;
    
    static const char* NAME;
    static const uint8_t SIZE=1;
    static const uint8_t PACK=1;
    static const uint8_t CODE=0;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Header(Bit sync_=Bit());
    template <class S> void serialize(S& s) {
        s(sync);
    }
    
};
inline SizedPacker<Header> serializable(Header& x) { return SizedPacker<Header>(x); }
std::ostream& operator << (std::ostream&, const Header&);
bool operator==(const Header&, const Header&);

struct CommandFragment
{
    SequenceNo commandId;
    ByteRanges byteRanges;
    
    static const char* NAME;
    static const uint8_t SIZE=0;
    static const uint8_t PACK=0;
    static const uint8_t CODE=0;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit CommandFragment(
        const SequenceNo& commandId_=SequenceNo(),
        const ByteRanges& byteRanges_=ByteRanges()
    );
    template <class S> void serialize(S& s) {
        s(commandId)(byteRanges);
    }
    
};
std::ostream& operator << (std::ostream&, const CommandFragment&);
bool operator==(const CommandFragment&, const CommandFragment&);

} // namespace session


namespace execution {


} // namespace execution


namespace message {


struct DeliveryProperties:
    public Struct
{
    Bit discardUnroutable;
    Bit immediate;
    Bit redelivered;
    DeliveryPriority priority;
    DeliveryMode deliveryMode;
    Uint64 ttl;
    Datetime timestamp;
    Datetime expiration;
    exchange::Name exchange;
    Str8 routingKey;
    ResumeId resumeId;
    Uint64 resumeTtl;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit DeliveryProperties(
        Bit discardUnroutable_=Bit(),
        Bit immediate_=Bit(),
        Bit redelivered_=Bit(),
        const message::DeliveryPriority& priority_=message::DeliveryPriority(),
        const message::DeliveryMode& deliveryMode_=message::DeliveryMode(),
        Uint64 ttl_=Uint64(),
        const Datetime& timestamp_=Datetime(),
        const Datetime& expiration_=Datetime(),
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& routingKey_=Str8(),
        const message::ResumeId& resumeId_=message::ResumeId(),
        Uint64 resumeTtl_=Uint64()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(discardUnroutable)(immediate)(redelivered)(priority)(deliveryMode)(ttl)(timestamp)(expiration)(exchange)(routingKey)(resumeId)(resumeTtl);
    }
    
};
inline SizedPacker<DeliveryProperties> serializable(DeliveryProperties& x) { return SizedPacker<DeliveryProperties>(x); }
std::ostream& operator << (std::ostream&, const DeliveryProperties&);
bool operator==(const DeliveryProperties&, const DeliveryProperties&);

struct FragmentProperties:
    public Struct
{
    Bit first;
    Bit last;
    Uint64 fragmentSize;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit FragmentProperties(
        Bit first_=Bit(),
        Bit last_=Bit(),
        Uint64 fragmentSize_=Uint64()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(first)(last)(fragmentSize);
    }
    
};
inline SizedPacker<FragmentProperties> serializable(FragmentProperties& x) { return SizedPacker<FragmentProperties>(x); }
std::ostream& operator << (std::ostream&, const FragmentProperties&);
bool operator==(const FragmentProperties&, const FragmentProperties&);

struct ReplyTo
{
    exchange::Name exchange;
    Str8 routingKey;
    
    static const char* NAME;
    static const uint8_t SIZE=2;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit ReplyTo(
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& routingKey_=Str8()
    );
    template <class S> void serialize(S& s) {
        s(exchange)(routingKey);
    }
    
};
inline SizedPacker<ReplyTo> serializable(ReplyTo& x) { return SizedPacker<ReplyTo>(x); }
std::ostream& operator << (std::ostream&, const ReplyTo&);
bool operator==(const ReplyTo&, const ReplyTo&);

struct MessageProperties:
    public Struct
{
    Uint64 contentLength;
    Uuid messageId;
    Vbin16 correlationId;
    ReplyTo replyTo;
    Str8 contentType;
    Str8 contentEncoding;
    Vbin16 userId;
    Vbin16 appId;
    Map applicationHeaders;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit MessageProperties(
        Uint64 contentLength_=Uint64(),
        const Uuid& messageId_=Uuid(),
        const Vbin16& correlationId_=Vbin16(),
        const message::ReplyTo& replyTo_=message::ReplyTo(),
        const Str8& contentType_=Str8(),
        const Str8& contentEncoding_=Str8(),
        const Vbin16& userId_=Vbin16(),
        const Vbin16& appId_=Vbin16(),
        const Map& applicationHeaders_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(contentLength)(messageId)(correlationId)(replyTo)(contentType)(contentEncoding)(userId)(appId)(applicationHeaders);
    }
    
};
inline SizedPacker<MessageProperties> serializable(MessageProperties& x) { return SizedPacker<MessageProperties>(x); }
std::ostream& operator << (std::ostream&, const MessageProperties&);
bool operator==(const MessageProperties&, const MessageProperties&);

struct Acquired:
    public Struct
{
    session::Commands transfers;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Acquired(const session::Commands& transfers_=session::Commands());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(transfers);
    }
    
};
inline SizedPacker<Acquired> serializable(Acquired& x) { return SizedPacker<Acquired>(x); }
std::ostream& operator << (std::ostream&, const Acquired&);
bool operator==(const Acquired&, const Acquired&);

struct MessageResumeResult:
    public Struct
{
    Uint64 offset;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit MessageResumeResult(Uint64 offset_=Uint64());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(offset);
    }
    
};
inline SizedPacker<MessageResumeResult> serializable(MessageResumeResult& x) { return SizedPacker<MessageResumeResult>(x); }
std::ostream& operator << (std::ostream&, const MessageResumeResult&);
bool operator==(const MessageResumeResult&, const MessageResumeResult&);

} // namespace message


namespace tx {


} // namespace tx


namespace dtx {


struct XaResult:
    public Struct
{
    XaStatus status;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit XaResult(const dtx::XaStatus& status_=dtx::XaStatus());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(status);
    }
    
};
inline SizedPacker<XaResult> serializable(XaResult& x) { return SizedPacker<XaResult>(x); }
std::ostream& operator << (std::ostream&, const XaResult&);
bool operator==(const XaResult&, const XaResult&);

struct Xid:
    public Struct
{
    Uint32 format;
    Vbin8 globalId;
    Vbin8 branchId;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit Xid(
        Uint32 format_=Uint32(),
        const Vbin8& globalId_=Vbin8(),
        const Vbin8& branchId_=Vbin8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(format)(globalId)(branchId);
    }
    
};
inline SizedPacker<Xid> serializable(Xid& x) { return SizedPacker<Xid>(x); }
std::ostream& operator << (std::ostream&, const Xid&);
bool operator==(const Xid&, const Xid&);

struct GetTimeoutResult:
    public Struct
{
    Uint32 timeout;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit GetTimeoutResult(Uint32 timeout_=Uint32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(timeout);
    }
    
};
inline SizedPacker<GetTimeoutResult> serializable(GetTimeoutResult& x) { return SizedPacker<GetTimeoutResult>(x); }
std::ostream& operator << (std::ostream&, const GetTimeoutResult&);
bool operator==(const GetTimeoutResult&, const GetTimeoutResult&);

struct RecoverResult:
    public Struct
{
    ArrayDomain<dtx::Xid>  inDoubt;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit RecoverResult(const ArrayDomain<dtx::Xid> & inDoubt_=ArrayDomain<dtx::Xid> ());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(inDoubt);
    }
    
};
inline SizedPacker<RecoverResult> serializable(RecoverResult& x) { return SizedPacker<RecoverResult>(x); }
std::ostream& operator << (std::ostream&, const RecoverResult&);
bool operator==(const RecoverResult&, const RecoverResult&);

} // namespace dtx


namespace exchange {


struct ExchangeQueryResult:
    public Struct
{
    Str8 type;
    Bit durable;
    Bit notFound;
    Map arguments;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=exchange::CODE;
    static const char* CLASS_NAME;
    explicit ExchangeQueryResult(
        const Str8& type_=Str8(),
        Bit durable_=Bit(),
        Bit notFound_=Bit(),
        const Map& arguments_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(type)(durable)(notFound)(arguments);
    }
    
};
inline SizedPacker<ExchangeQueryResult> serializable(ExchangeQueryResult& x) { return SizedPacker<ExchangeQueryResult>(x); }
std::ostream& operator << (std::ostream&, const ExchangeQueryResult&);
bool operator==(const ExchangeQueryResult&, const ExchangeQueryResult&);

struct ExchangeBoundResult:
    public Struct
{
    Bit exchangeNotFound;
    Bit queueNotFound;
    Bit queueNotMatched;
    Bit keyNotMatched;
    Bit argsNotMatched;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=exchange::CODE;
    static const char* CLASS_NAME;
    explicit ExchangeBoundResult(
        Bit exchangeNotFound_=Bit(),
        Bit queueNotFound_=Bit(),
        Bit queueNotMatched_=Bit(),
        Bit keyNotMatched_=Bit(),
        Bit argsNotMatched_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(exchangeNotFound)(queueNotFound)(queueNotMatched)(keyNotMatched)(argsNotMatched);
    }
    
};
inline SizedPacker<ExchangeBoundResult> serializable(ExchangeBoundResult& x) { return SizedPacker<ExchangeBoundResult>(x); }
std::ostream& operator << (std::ostream&, const ExchangeBoundResult&);
bool operator==(const ExchangeBoundResult&, const ExchangeBoundResult&);

} // namespace exchange


namespace queue {


struct QueueQueryResult:
    public Struct
{
    Name queue;
    exchange::Name alternateExchange;
    Bit durable;
    Bit exclusive;
    Bit autoDelete;
    Map arguments;
    Uint32 messageCount;
    Uint32 subscriberCount;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=queue::CODE;
    static const char* CLASS_NAME;
    explicit QueueQueryResult(
        const queue::Name& queue_=queue::Name(),
        const exchange::Name& alternateExchange_=exchange::Name(),
        Bit durable_=Bit(),
        Bit exclusive_=Bit(),
        Bit autoDelete_=Bit(),
        const Map& arguments_=Map(),
        Uint32 messageCount_=Uint32(),
        Uint32 subscriberCount_=Uint32()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(alternateExchange)(durable)(exclusive)(autoDelete)(arguments)(messageCount)(subscriberCount);
    }
    
};
inline SizedPacker<QueueQueryResult> serializable(QueueQueryResult& x) { return SizedPacker<QueueQueryResult>(x); }
std::ostream& operator << (std::ostream&, const QueueQueryResult&);
bool operator==(const QueueQueryResult&, const QueueQueryResult&);

} // namespace queue


namespace file {


struct FileProperties:
    public Struct
{
    Str8 contentType;
    Str8 contentEncoding;
    Map headers;
    Uint8 priority;
    Str8 replyTo;
    Str8 messageId;
    Str8 filename;
    Datetime timestamp;
    Str8 clusterId;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit FileProperties(
        const Str8& contentType_=Str8(),
        const Str8& contentEncoding_=Str8(),
        const Map& headers_=Map(),
        Uint8 priority_=Uint8(),
        const Str8& replyTo_=Str8(),
        const Str8& messageId_=Str8(),
        const Str8& filename_=Str8(),
        const Datetime& timestamp_=Datetime(),
        const Str8& clusterId_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(contentType)(contentEncoding)(headers)(priority)(replyTo)(messageId)(filename)(timestamp)(clusterId);
    }
    
};
inline SizedPacker<FileProperties> serializable(FileProperties& x) { return SizedPacker<FileProperties>(x); }
std::ostream& operator << (std::ostream&, const FileProperties&);
bool operator==(const FileProperties&, const FileProperties&);

} // namespace file


namespace stream {


struct StreamProperties:
    public Struct
{
    Str8 contentType;
    Str8 contentEncoding;
    Map headers;
    Uint8 priority;
    Datetime timestamp;
    
    static const char* NAME;
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit StreamProperties(
        const Str8& contentType_=Str8(),
        const Str8& contentEncoding_=Str8(),
        const Map& headers_=Map(),
        Uint8 priority_=Uint8(),
        const Datetime& timestamp_=Datetime()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(contentType)(contentEncoding)(headers)(priority)(timestamp);
    }
    
};
inline SizedPacker<StreamProperties> serializable(StreamProperties& x) { return SizedPacker<StreamProperties>(x); }
std::ostream& operator << (std::ostream&, const StreamProperties&);
bool operator==(const StreamProperties&, const StreamProperties&);

} // namespace stream


namespace cluster {


} // namespace cluster


namespace cluster_connection {


} // namespace cluster_connection


}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_STRUCTS_H*/
