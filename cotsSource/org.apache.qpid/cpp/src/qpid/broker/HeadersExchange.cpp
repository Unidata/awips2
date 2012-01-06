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
#include "qpid/broker/HeadersExchange.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/log/Statement.h"
#include <algorithm>


using namespace qpid::broker;
using namespace qpid::framing;
using namespace qpid::sys;
namespace _qmf = qmf::org::apache::qpid::broker;

// TODO aconway 2006-09-20: More efficient matching algorithm.
// The current search algorithm really sucks.
// Fieldtables are heavy, maybe use shared_ptr to do handle-body.

using namespace qpid::broker;

namespace {
    const std::string all("all");
    const std::string any("any");
    const std::string x_match("x-match");
    const std::string empty;
}

HeadersExchange::HeadersExchange(const string& _name, Manageable* _parent, Broker* b) :
    Exchange(_name, _parent, b)
{
    if (mgmtExchange != 0)
        mgmtExchange->set_type (typeName);
}

HeadersExchange::HeadersExchange(const std::string& _name, bool _durable,
                                 const FieldTable& _args, Manageable* _parent, Broker* b) :
    Exchange(_name, _durable, _args, _parent, b)
{
    if (mgmtExchange != 0)
        mgmtExchange->set_type (typeName);
}

std::string HeadersExchange::getMatch(const FieldTable* args)
{
    if (!args) {
        throw InternalErrorException(QPID_MSG("No arguments given."));
    }
    FieldTable::ValuePtr what = args->get(x_match);
    if (!what) {
        return empty;
    }
    if (!what->convertsTo<std::string>()) {
        throw InternalErrorException(QPID_MSG("Invalid x-match value binding to headers exchange."));
    }
    return what->get<std::string>();
}

bool HeadersExchange::bind(Queue::shared_ptr queue, const string& bindingKey, const FieldTable* args){
    std::string what = getMatch(args);
    if (what != all && what != any)
        throw InternalErrorException(QPID_MSG("Invalid x-match value binding to headers exchange."));

    Binding::shared_ptr binding (new Binding (bindingKey, queue, this, *args));
    if (bindings.add_unless(binding, MatchArgs(queue, args))) {
        if (mgmtExchange != 0) {
            mgmtExchange->inc_bindingCount();
        }
        routeIVE();
        return true;
    } else {
        return false;
    }
}

bool HeadersExchange::unbind(Queue::shared_ptr queue, const string& bindingKey, const FieldTable*){
    if (bindings.remove_if(MatchKey(queue, bindingKey))) {
        if (mgmtExchange != 0) {
            mgmtExchange->dec_bindingCount();
        }
        return true;
    } else {
        return false;
    }
}


void HeadersExchange::route(Deliverable& msg, const string& /*routingKey*/, const FieldTable* args)
{
    if (!args) {
        //can't match if there were no headers passed in
        if (mgmtExchange != 0) {
            mgmtExchange->inc_msgReceives();
            mgmtExchange->inc_byteReceives(msg.contentSize());
            mgmtExchange->inc_msgDrops();
            mgmtExchange->inc_byteDrops(msg.contentSize());
        }
        return;
    }

    PreRoute pr(msg, this);

    ConstBindingList p = bindings.snapshot();
    BindingList b(new std::vector<boost::shared_ptr<qpid::broker::Exchange::Binding> >);
    if (p.get())
    {
        for (std::vector<Binding::shared_ptr>::const_iterator i = p->begin(); i != p->end(); ++i) {
            if (match((*i)->args, *args)) {
                b->push_back(*i);
            }
        }
    }
    doRoute(msg, b);
}


bool HeadersExchange::isBound(Queue::shared_ptr queue, const string* const, const FieldTable* const args)
{
    Bindings::ConstPtr p = bindings.snapshot();
    if (p.get()){
        for (std::vector<Binding::shared_ptr>::const_iterator i = p->begin(); i != p->end(); ++i) {
            if ( (!args || equal((*i)->args, *args)) && (!queue || (*i)->queue == queue)) {
                return true;
            }
        }
    }
    return false;
}

HeadersExchange::~HeadersExchange() {}

const std::string HeadersExchange::typeName("headers");

namespace 
{

    bool match_values(const FieldValue& bind, const FieldValue& msg) {
        return  bind.empty() || bind == msg;
    }

}


bool HeadersExchange::match(const FieldTable& bind, const FieldTable& msg) {
    typedef FieldTable::ValueMap Map;
    std::string what = getMatch(&bind);
    if (what == all) {
        for (Map::const_iterator i = bind.begin();
             i != bind.end();
             ++i)
        {
            if (i->first != x_match) 
            {
                Map::const_iterator j = msg.find(i->first);
                if (j == msg.end()) return false;
                if (!match_values(*(i->second), *(j->second))) return false;
            }
        }
        return true;
    } else if (what == any) {
        for (Map::const_iterator i = bind.begin();
             i != bind.end();
             ++i)
        {
            if (i->first != x_match) 
            {
                Map::const_iterator j = msg.find(i->first);
                if (j != msg.end()) {
                    if (match_values(*(i->second), *(j->second))) return true;
                }
            }
        }
        return false;
    } else {
        return false;
    }
}

bool HeadersExchange::equal(const FieldTable& a, const FieldTable& b) {
    typedef FieldTable::ValueMap Map;
    for (Map::const_iterator i = a.begin();
         i != a.end();
         ++i)
    {
        Map::const_iterator j = b.find(i->first);
        if (j == b.end()) return false;
        if (!match_values(*(i->second), *(j->second))) return false;
    }
    return true;
}

HeadersExchange::MatchArgs::MatchArgs(Queue::shared_ptr q, const qpid::framing::FieldTable* a) : queue(q), args(a) {}
bool HeadersExchange::MatchArgs::operator()(Exchange::Binding::shared_ptr b)
{
    return b->queue == queue && b->args == *args;
}

HeadersExchange::MatchKey::MatchKey(Queue::shared_ptr q, const std::string& k) : queue(q), key(k) {}

bool HeadersExchange::MatchKey::operator()(Exchange::Binding::shared_ptr b)
{
    return b->queue == queue && b->key == key;
}
