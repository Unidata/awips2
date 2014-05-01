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
#include "qpid/cluster/FailoverExchange.h"
#include "qpid/broker/Message.h"
#include "qpid/broker/DeliverableMessage.h"
#include "qpid/broker/Queue.h"
#include "qpid/framing/MessageProperties.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/AMQHeaderBody.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/Array.h"
#include <boost/bind.hpp>
#include <algorithm>

namespace qpid {
namespace cluster {
using namespace std;

using namespace broker;
using namespace framing;

const string FailoverExchange::TYPE_NAME("amq.failover");

FailoverExchange::FailoverExchange(management::Manageable* parent) : Exchange(TYPE_NAME, parent) {
    if (mgmtExchange != 0)
        mgmtExchange->set_type(TYPE_NAME);
}


void FailoverExchange::setUrls(const vector<Url>& u) {
    Lock l(lock);
    urls=u;
    if (urls.empty()) return;
    std::for_each(queues.begin(), queues.end(),
                  boost::bind(&FailoverExchange::sendUpdate, this, _1));
}

string FailoverExchange::getType() const { return TYPE_NAME; }

bool FailoverExchange::bind(Queue::shared_ptr queue, const string&, const framing::FieldTable*) {
    Lock l(lock);
    sendUpdate(queue);
    return queues.insert(queue).second;
}

bool FailoverExchange::unbind(Queue::shared_ptr queue, const string&, const framing::FieldTable*) {
    Lock l(lock);
    return queues.erase(queue);
}

bool FailoverExchange::isBound(Queue::shared_ptr queue, const string* const, const framing::FieldTable*) {
    Lock l(lock);
    return queues.find(queue) != queues.end();
}

void FailoverExchange::route(Deliverable&, const string& , const framing::FieldTable* ) {
    QPID_LOG(warning, "Message received by exchange " << TYPE_NAME << " ignoring");
}

void FailoverExchange::sendUpdate(const Queue::shared_ptr& queue) {
    // Called with lock held.
    if (urls.empty()) return;
    framing::Array array(0x95);
    for (Urls::const_iterator i = urls.begin(); i != urls.end(); ++i) 
        array.add(boost::shared_ptr<Str16Value>(new Str16Value(i->str())));
    const ProtocolVersion v;
    boost::intrusive_ptr<Message> msg(new Message);
    AMQFrame command(MessageTransferBody(v, TYPE_NAME, 1, 0));
    command.setLastSegment(false);
    msg->getFrames().append(command);
    AMQHeaderBody header;
    header.get<MessageProperties>(true)->setContentLength(0);
    header.get<MessageProperties>(true)->getApplicationHeaders().setArray(TYPE_NAME, array);
    AMQFrame headerFrame(header);
    headerFrame.setFirstSegment(false);
    msg->getFrames().append(headerFrame);    
    DeliverableMessage(msg).deliverTo(queue);
}

}} // namespace cluster
