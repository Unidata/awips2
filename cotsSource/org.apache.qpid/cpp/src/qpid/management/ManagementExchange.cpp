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

#include "qpid/management/ManagementExchange.h"
#include "qpid/log/Statement.h"

using namespace qpid::management;
using namespace qpid::broker;
using namespace qpid::framing;
using namespace qpid::sys;

ManagementExchange::ManagementExchange (const string& _name, Manageable* _parent, Broker* b) :
    Exchange (_name, _parent, b), TopicExchange(_name, _parent, b) {}
ManagementExchange::ManagementExchange (const std::string& _name,
                                        bool               _durable,
                                        const FieldTable&  _args,
                                        Manageable*        _parent, Broker* b) :
    Exchange (_name, _durable, _args, _parent, b), 
    TopicExchange(_name, _durable, _args, _parent, b) {}

void ManagementExchange::route (Deliverable&      msg,
                                const string&     routingKey,
                                const FieldTable* args)
{
    bool routeIt = true;

    // Intercept management agent commands
    if ((routingKey.length() > 6 &&
         routingKey.substr(0, 6).compare("agent.") == 0) ||
        (routingKey == "broker"))
        routeIt = managementAgent->dispatchCommand(msg, routingKey, args);

    if (routeIt)
        TopicExchange::route(msg, routingKey, args);
}

bool ManagementExchange::bind (Queue::shared_ptr queue,
                               const string& routingKey,
                               const qpid::framing::FieldTable* args)
{
    managementAgent->clientAdded(routingKey);
    return TopicExchange::bind(queue, routingKey, args);
}

void ManagementExchange::setManagmentAgent (ManagementAgent* agent)
{
    managementAgent = agent;
}


ManagementExchange::~ManagementExchange() {}

const std::string ManagementExchange::typeName("management");

