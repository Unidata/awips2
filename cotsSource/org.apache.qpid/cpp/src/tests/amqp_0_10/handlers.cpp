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

#include "amqp_0_10/unit_test.h"
#include "qpid/Exception.h"
#include "qpid/amqp_0_10/Unit.h"
#include "qpid/amqp_0_10/ControlHolder.h"
#include "qpid/amqp_0_10/CommandHolder.h"
#include "qpid/amqp_0_10/handlers.h"
#include "qpid/amqp_0_10/specification.h"

QPID_AUTO_TEST_SUITE(handler_tests)

using namespace qpid::amqp_0_10;
using namespace std;

string called;                  // Set by called handler function

// Note on handlers:
// 
// Control and Command handlers are separate, both behave the same way,
// so substitute "control or command" for command in the following.
//
// Command handlers derive from CommandHandler and implement functions
// for all the commands they handle. Handling an unimplemented command
// will raise NotImplementedException.
//
// Using virtual inheritance from CommandHandler allows multiple
// handlers to be aggregated into one with multiple inheritance,
// See test code for example. 
//
// E.g. the existing broker model would have two control handlers:
//  - ConnectionHandler: ControlHandler for connection controls.
//  - SessionHandler: ControlHandler for session controls.
// It would have class-command handlers for each AMQP class:  
//    - QueueHandler, MessageHandler etc.. handle each class.
// And an aggregate handler in place of BrokerAdapter
//  - BrokerCommandHandler: public QueueHandler, MessageHandler ...
//  
// In other applications (e.g. cluster) any combination of commands
// can be handled by a given handler. It _might_ simplify the code
// to collaps ConnectionHandler and SessionHandler into a single
// ControlHandler (or it might not.)

struct TestExecutionHandler : public virtual CommandHandler {
    void executionSync() { called = "executionSync"; }
    // ... etc. for all execution commands
};

struct TestMessageHandler : public virtual CommandHandler {
    void messageCancel(const Str8&)  { called="messageCancel"; }
    // ... etc.
};

// Aggregate handler for all recognised commands.
struct TestCommandHandler :
    public TestExecutionHandler,
    public TestMessageHandler
    // ... etc. handlers for all command classes.
{};                            // Nothing to do.


// Sample unit handler, written as a static_visitor. 
// Note it could equally be written with if/else statements
// in handle.
// 
struct TestUnitHandler : public boost::static_visitor<void> {
    TestCommandHandler handler;
    void handle(const Unit& u) { u.applyVisitor(*this); }
    
    void operator()(const Body&)  { called="Body"; }
    void operator()(const Header&) { called="Header"; }
    void operator()(const ControlHolder&) { throw qpid::Exception("I don't do controls."); }
    void operator()(const CommandHolder& c) { c.invoke(handler); }
};

QPID_AUTO_TEST_CASE(testHandlers) {
    TestUnitHandler handler;
    Unit u;

    u = Body();
    handler.handle(u);
    BOOST_CHECK_EQUAL("Body", called);

    u = Header();
    handler.handle(u);
    BOOST_CHECK_EQUAL("Header", called);

    // in_place<Foo>(...) is equivalent to Foo(...) but
    // constructs Foo directly in the holder, avoiding
    // a copy.
    
    u = CommandHolder(in_place<execution::Sync>());
    handler.handle(u);
    BOOST_CHECK_EQUAL("executionSync", called);

    u = ControlHolder(in_place<connection::Start>(Map(), Str16Array(), Str16Array()));
    try {
        handler.handle(u);
    } catch (const qpid::Exception&) {}

    u = CommandHolder(in_place<message::Cancel>(Str8()));
    handler.handle(u);
    BOOST_CHECK_EQUAL("messageCancel", called);
}

QPID_AUTO_TEST_SUITE_END()
