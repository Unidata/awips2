#ifndef QPID_FRAMING_FRAMEVISITOR_H
#define QPID_FRAMING_FRAMEVISITOR_H

/*
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

#include "qpid/framing/MethodBodyDefaultVisitor.h"
#include "qpid/framing/AMQBody.h"
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/AMQHeaderBody.h"
#include "qpid/framing/AMQContentBody.h"
#include "qpid/framing/AMQHeartbeatBody.h"

namespace qpid {
namespace framing {
/**
 * Visitor for all concrete frame body types, which combines
 * AMQBodyConstVisitor and MethodBodyDefaultVisitor.
 * 
 * Derived classes can override visit methods to specify actions.
 * Derived classes must override defaultVisit(), which is called
 * for any non-overridden visit functions.
 *
 */
struct FrameDefaultVisitor : public AMQBodyConstVisitor,
                             protected MethodBodyDefaultVisitor
{
    virtual void defaultVisit(const AMQBody&) = 0;
    void defaultVisit(const AMQMethodBody& method) { defaultVisit(static_cast<const AMQBody&>(method)); }

    void visit(const AMQHeaderBody& b) { defaultVisit(b); }
    void visit(const AMQContentBody& b) { defaultVisit(b); }
    void visit(const AMQHeartbeatBody& b) { defaultVisit(b); }
    void visit(const AMQMethodBody& b) { b.accept(static_cast<MethodBodyDefaultVisitor&>(*this)); }
    
    using AMQBodyConstVisitor::visit;
    using MethodBodyDefaultVisitor::visit;
};

}} // namespace qpid::framing


#endif  /*!QPID_FRAMING_FRAMEVISITOR_H*/
