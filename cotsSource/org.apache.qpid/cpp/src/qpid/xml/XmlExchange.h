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
#ifndef _XmlExchange_
#define _XmlExchange_

#include "qpid/broker/Exchange.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/sys/CopyOnWriteArray.h"
#include "qpid/sys/Monitor.h"
#include "qpid/broker/Queue.h"

#include <xqilla/xqilla-simple.hpp>

#include <boost/scoped_ptr.hpp>

#include <map>
#include <vector>

namespace qpid {
namespace broker {

class Broker;
class XmlExchange : public virtual Exchange {

    typedef boost::shared_ptr<XQQuery> Query;

    struct XmlBinding : public Exchange::Binding {
        typedef boost::shared_ptr<XmlBinding> shared_ptr;
        typedef qpid::sys::CopyOnWriteArray<XmlBinding::shared_ptr> vector;

        boost::shared_ptr<XQQuery> xquery;
        bool parse_message_content;

        XmlBinding(const std::string& key, const Queue::shared_ptr queue, Exchange* parent, Query query):
            Binding(key, queue, parent), xquery(query), parse_message_content(true) {}
    };

        
    typedef std::map<string, XmlBinding::vector > XmlBindingsMap;

    XmlBindingsMap bindingsMap;
    XQilla xqilla;
    qpid::sys::RWlock lock;

    bool matches(Query& query, Deliverable& msg, const qpid::framing::FieldTable* args, bool parse_message_content);

  public:
    static const std::string typeName;
        
    XmlExchange(const std::string& name, management::Manageable* parent = 0, Broker* broker = 0);
    XmlExchange(const string& _name, bool _durable,
		const qpid::framing::FieldTable& _args, management::Manageable* parent = 0, Broker* broker = 0);

    virtual std::string getType() const { return typeName; }
        
    virtual bool bind(Queue::shared_ptr queue, const std::string& routingKey, const qpid::framing::FieldTable* args);

    virtual bool unbind(Queue::shared_ptr queue, const std::string& routingKey, const qpid::framing::FieldTable* args);

    virtual void route(Deliverable& msg, const std::string& routingKey, const qpid::framing::FieldTable* args);

    virtual bool isBound(Queue::shared_ptr queue, const string* const routingKey, const qpid::framing::FieldTable* const args);

    virtual ~XmlExchange();
};


}
}


#endif
