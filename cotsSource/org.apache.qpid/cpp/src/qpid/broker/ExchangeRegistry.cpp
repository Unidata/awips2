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

#include "qpid/broker/ExchangeRegistry.h"
#include "qpid/broker/DirectExchange.h"
#include "qpid/broker/FanOutExchange.h"
#include "qpid/broker/HeadersExchange.h"
#include "qpid/broker/TopicExchange.h"
#include "qpid/management/ManagementExchange.h"
#include "qpid/framing/reply_exceptions.h"

using namespace qpid::broker;
using namespace qpid::sys;
using std::pair;
using qpid::framing::FieldTable;

pair<Exchange::shared_ptr, bool> ExchangeRegistry::declare(const string& name, const string& type){

    return declare(name, type, false, FieldTable());
}

pair<Exchange::shared_ptr, bool> ExchangeRegistry::declare(const string& name, const string& type, 
                                                           bool durable, const FieldTable& args){
    RWlock::ScopedWlock locker(lock);
    ExchangeMap::iterator i =  exchanges.find(name);
    if (i == exchanges.end()) {
        Exchange::shared_ptr exchange;

        if(type == TopicExchange::typeName){
            exchange = Exchange::shared_ptr(new TopicExchange(name, durable, args, parent, broker));
        }else if(type == DirectExchange::typeName){
            exchange = Exchange::shared_ptr(new DirectExchange(name, durable, args, parent, broker));
        }else if(type == FanOutExchange::typeName){
            exchange = Exchange::shared_ptr(new FanOutExchange(name, durable, args, parent, broker));
        }else if (type == HeadersExchange::typeName) {
            exchange = Exchange::shared_ptr(new HeadersExchange(name, durable, args, parent, broker));
        }else if (type == ManagementExchange::typeName) {
            exchange = Exchange::shared_ptr(new ManagementExchange(name, durable, args, parent, broker));
        }
	else{
            FunctionMap::iterator i =  factory.find(type);
            if (i == factory.end()) {
                throw UnknownExchangeTypeException();    
            } else {
                exchange = i->second(name, durable, args, parent, broker);
            }
        }
        exchanges[name] = exchange;
        return std::pair<Exchange::shared_ptr, bool>(exchange, true);
    } else {
        return std::pair<Exchange::shared_ptr, bool>(i->second, false);
    }
}

void ExchangeRegistry::destroy(const string& name){
    if (name.empty() ||
        (name.find("amq.") == 0 &&
         (name == "amq.direct" || name == "amq.fanout" || name == "amq.topic" || name == "amq.match")) ||
        name == "qpid.management")
        throw framing::NotAllowedException(QPID_MSG("Cannot delete default exchange: '" << name << "'"));
    RWlock::ScopedWlock locker(lock);
    ExchangeMap::iterator i =  exchanges.find(name);
    if (i != exchanges.end()) {
        exchanges.erase(i);
    }
}

Exchange::shared_ptr ExchangeRegistry::get(const string& name){
    RWlock::ScopedRlock locker(lock);
    ExchangeMap::iterator i =  exchanges.find(name);
    if (i == exchanges.end())
        throw framing::NotFoundException(QPID_MSG("Exchange not found: " << name));
    return i->second;
}

bool ExchangeRegistry::registerExchange(const Exchange::shared_ptr& ex) {
    return exchanges.insert(ExchangeMap::value_type(ex->getName(), ex)).second;
}

void ExchangeRegistry::registerType(const std::string& type, FactoryFunction f)
{
    factory[type] = f;
}


namespace 
{
const std::string empty;
}

Exchange::shared_ptr ExchangeRegistry::getDefault()
{
    return get(empty);
}
