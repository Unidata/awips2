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
#include "qpid/broker/QueueBindings.h"
#include "qpid/broker/ExchangeRegistry.h"
#include "qpid/framing/reply_exceptions.h"

using qpid::framing::FieldTable;
using qpid::framing::NotFoundException;
using std::string;
using namespace qpid::broker;

void QueueBindings::add(const string& exchange, const string& key, const FieldTable& args)
{
    bindings.push_back(QueueBinding(exchange, key, args));
}

void QueueBindings::unbind(ExchangeRegistry& exchanges, Queue::shared_ptr queue)
{
    for (Bindings::iterator i = bindings.begin(); i != bindings.end(); i++) {
        try {
            exchanges.get(i->exchange)->unbind(queue, i->key, &(i->args));
        } catch (const NotFoundException&) {}
    }
}

QueueBinding::QueueBinding(const string& _exchange, const string& _key, const FieldTable& _args)
    : exchange(_exchange), key(_key), args(_args)
{}
