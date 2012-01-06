#ifndef QPID_AMQP_0_10_STRUCTHOLDER_H
#define QPID_AMQP_0_10_STRUCTHOLDER_H
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


#include "qpid/amqp_0_10/ApplyStruct.h"
#include "qpid/amqp_0_10/Holder.h"
#include "qpid/amqp_0_10/structs.h"

namespace qpid {
namespace amqp_0_10 {


namespace struct_max {

static const size_t MAX000=0;
static const size_t MAX001 = sizeof(message::DeliveryProperties) > MAX000 ? sizeof(message::DeliveryProperties) : MAX000;
static const size_t MAX002 = sizeof(message::FragmentProperties) > MAX001 ? sizeof(message::FragmentProperties) : MAX001;
static const size_t MAX003 = sizeof(message::MessageProperties) > MAX002 ? sizeof(message::MessageProperties) : MAX002;
static const size_t MAX004 = sizeof(message::Acquired) > MAX003 ? sizeof(message::Acquired) : MAX003;
static const size_t MAX005 = sizeof(message::MessageResumeResult) > MAX004 ? sizeof(message::MessageResumeResult) : MAX004;
static const size_t MAX006 = sizeof(dtx::XaResult) > MAX005 ? sizeof(dtx::XaResult) : MAX005;
static const size_t MAX007 = sizeof(dtx::Xid) > MAX006 ? sizeof(dtx::Xid) : MAX006;
static const size_t MAX008 = sizeof(dtx::GetTimeoutResult) > MAX007 ? sizeof(dtx::GetTimeoutResult) : MAX007;
static const size_t MAX009 = sizeof(dtx::RecoverResult) > MAX008 ? sizeof(dtx::RecoverResult) : MAX008;
static const size_t MAX010 = sizeof(exchange::ExchangeQueryResult) > MAX009 ? sizeof(exchange::ExchangeQueryResult) : MAX009;
static const size_t MAX011 = sizeof(exchange::ExchangeBoundResult) > MAX010 ? sizeof(exchange::ExchangeBoundResult) : MAX010;
static const size_t MAX012 = sizeof(queue::QueueQueryResult) > MAX011 ? sizeof(queue::QueueQueryResult) : MAX011;
static const size_t MAX013 = sizeof(file::FileProperties) > MAX012 ? sizeof(file::FileProperties) : MAX012;
static const size_t MAX014 = sizeof(stream::StreamProperties) > MAX013 ? sizeof(stream::StreamProperties) : MAX013;
static const size_t MAX015 = sizeof(UnknownStruct) > MAX014 ? sizeof(UnknownStruct) : MAX014;
static const int MAX=MAX015;

} // namespace struct_max

struct StructHolder:
    public amqp_0_10::Holder<StructHolder, Struct, struct_max::MAX>
{
    StructHolder() {}
    template <class T> explicit StructHolder(const T& t) : amqp_0_10::Holder<StructHolder, Struct, struct_max::MAX>(t) {}
    using amqp_0_10::Holder<StructHolder, Struct, struct_max::MAX>::operator=;
    void set(uint8_t classCode, uint8_t code);
};

std::ostream& operator<<(std::ostream& o, const StructHolder& h);

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_STRUCTHOLDER_H*/
