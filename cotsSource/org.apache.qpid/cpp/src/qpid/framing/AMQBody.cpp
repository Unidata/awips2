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

#include "qpid/framing/AMQBody.h"
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/AMQHeaderBody.h"
#include "qpid/framing/AMQContentBody.h"
#include "qpid/framing/AMQHeartbeatBody.h"
#include <iostream>

namespace qpid {
namespace framing {

std::ostream& operator<<(std::ostream& out, const AMQBody& body) 
{
    body.print(out);
    return out;
}

AMQBody::~AMQBody() {}

namespace {
struct MatchBodies : public AMQBodyConstVisitor {
    const AMQBody& body;
    bool match;

    MatchBodies(const AMQBody& b) :  body(b), match(false) {}
    virtual ~MatchBodies() {}
    
    virtual void visit(const AMQHeaderBody&) { match=dynamic_cast<const AMQHeaderBody*>(&body); }
    virtual void visit(const AMQContentBody&) { match=dynamic_cast<const AMQContentBody*>(&body); }
    virtual void visit(const AMQHeartbeatBody&) { match=dynamic_cast<const AMQHeartbeatBody*>(&body); }
    virtual void visit(const AMQMethodBody& x)  {
        const AMQMethodBody* y=dynamic_cast<const AMQMethodBody*>(&body);
        match = (y && y->amqpMethodId() == x.amqpMethodId() && y->amqpClassId() == x.amqpClassId());
    }
};

}
bool AMQBody::match(const AMQBody& a, const AMQBody& b) {
    MatchBodies matcher(a);
    b.accept(matcher);
    return matcher.match;
}

}} // namespace 
