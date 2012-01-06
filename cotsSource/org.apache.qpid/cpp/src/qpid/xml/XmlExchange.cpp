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

#include "config.h"

#include "qpid/xml/XmlExchange.h"

#include "qpid/broker/DeliverableMessage.h"

#include "qpid/log/Statement.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/framing/reply_exceptions.h"

#include "qpid/Plugin.h"

#include <xercesc/framework/MemBufInputSource.hpp>

#include <xqilla/ast/XQGlobalVariable.hpp>

#include <xqilla/context/ItemFactory.hpp>
#include <xqilla/xqilla-simple.hpp>

#include <iostream>
#include <sstream>

using namespace qpid::framing;
using namespace qpid::sys;
using qpid::management::Manageable;
namespace _qmf = qmf::org::apache::qpid::broker;

namespace qpid {
namespace broker {


    XmlExchange::XmlExchange(const string& _name, Manageable* _parent, Broker* b) : Exchange(_name, _parent, b)
{
    if (mgmtExchange != 0)
        mgmtExchange->set_type (typeName);
}

XmlExchange::XmlExchange(const std::string& _name, bool _durable,
                         const FieldTable& _args, Manageable* _parent, Broker* b) :
    Exchange(_name, _durable, _args, _parent, b)
{
    if (mgmtExchange != 0)
        mgmtExchange->set_type (typeName);
}


      // #### TODO: The Binding should take the query text
      // #### only. Consider encapsulating the entire block, including
      // #### the if condition.
      

bool XmlExchange::bind(Queue::shared_ptr queue, const string& routingKey, const FieldTable* bindingArguments)
{
    string queryText = bindingArguments->getAsString("xquery");

    try {
        RWlock::ScopedWlock l(lock);

	    XmlBinding::vector& bindings(bindingsMap[routingKey]);
	    XmlBinding::vector::ConstPtr p = bindings.snapshot();
	    if (!p || std::find_if(p->begin(), p->end(), MatchQueue(queue)) == p->end()) {
	        Query query(xqilla.parse(X(queryText.c_str())));
	        XmlBinding::shared_ptr binding(new XmlBinding (routingKey, queue, this, query));
	        bindings.add(binding);
	        QPID_LOG(trace, "Bound successfully with query: " << queryText );

                binding->parse_message_content = false;

                if (query->getQueryBody()->getStaticAnalysis().areContextFlagsUsed()) {
                    binding->parse_message_content = true;
                }
                else {
                    GlobalVariables &vars = const_cast<GlobalVariables&>(query->getVariables());
                    for(GlobalVariables::iterator it = vars.begin(); it != vars.end(); ++it) {
                        if ((*it)->getStaticAnalysis().areContextFlagsUsed()) {
                            binding->parse_message_content = true;
                            break;
                        } 
                    }
                }

	        if (mgmtExchange != 0) {
	            mgmtExchange->inc_bindingCount();
                ((_qmf::Queue*) queue->GetManagementObject())->inc_bindingCount();
	        }
	    } else {
	        return false;
	    }
    }
    catch (XQException& e) {
        throw InternalErrorException(QPID_MSG("Could not parse xquery:"+ queryText));
    }
    catch (...) {
        throw InternalErrorException(QPID_MSG("Unexpected error - Could not parse xquery:"+ queryText));
    }
    routeIVE();
	return true;
}

bool XmlExchange::unbind(Queue::shared_ptr queue, const string& routingKey, const FieldTable* /*args*/)
{
    RWlock::ScopedWlock l(lock);
    if (bindingsMap[routingKey].remove_if(MatchQueue(queue))) {
        if (mgmtExchange != 0) {
            mgmtExchange->dec_bindingCount();
            ((_qmf::Queue*) queue->GetManagementObject())->dec_bindingCount();
        }
        return true;
    } else {
        return false;      
    }
}

bool XmlExchange::matches(Query& query, Deliverable& msg, const qpid::framing::FieldTable* args, bool parse_message_content) 
{
  string msgContent;

  try {
      QPID_LOG(trace, "matches: query is [" << UTF8(query->getQueryText()) << "]");

      boost::scoped_ptr<DynamicContext> context(query->createDynamicContext());
      if (!context.get()) {
          throw InternalErrorException(QPID_MSG("Query context looks munged ..."));
      }

      if (parse_message_content) {

          msg.getMessage().getFrames().getContent(msgContent);

          QPID_LOG(trace, "matches: message content is [" << msgContent << "]");

          XERCES_CPP_NAMESPACE::MemBufInputSource xml((const XMLByte*) msgContent.c_str(), 
                                                      msgContent.length(), "input" );

	// This will parse the document using either Xerces or FastXDM, depending
	// on your XQilla configuration. FastXDM can be as much as 10x faster.

          Sequence seq(context->parseDocument(xml));

          if(!seq.isEmpty() && seq.first()->isNode()) {
              context->setContextItem(seq.first());
              context->setContextPosition(1);
              context->setContextSize(1);
          }
      }

      if (args) {
          FieldTable::ValueMap::const_iterator v = args->begin();
          for(; v != args->end(); ++v) {
              // ### TODO: Do types properly
              if (v->second->convertsTo<std::string>()) {
                  QPID_LOG(trace, "XmlExchange, external variable: " << v->first << " = " << v->second->getData().getString().c_str());
                  Item::Ptr value = context->getItemFactory()->createString(X(v->second->getData().getString().c_str()), context.get());
                  context->setExternalVariable(X(v->first.c_str()), value);
              }
          }
      }

      Result result = query->execute(context.get());
      return result->getEffectiveBooleanValue(context.get(), 0);
  }
  catch (XQException& e) {
      QPID_LOG(warning, "Could not parse XML content (or message headers):" << msgContent);
  }
  catch (...) {
      QPID_LOG(warning, "Unexpected error routing message: " << msgContent);
  }
  return 0;
}

// Future optimization: If any query in a binding for a given routing key requires
// message content, parse the message once, and use that parsed form for all bindings.
//
// Future optimization: XQilla does not currently do document projection for data
// accessed via the context item. If there is a single query for a given routing key,
// and it accesses document data, this could be a big win.
//
// Document projection often is not a win if you have multiple queries on the same data.
// But for very large messages, if all these queries are on the first part of the data,
// it could still be a big win.

void XmlExchange::route(Deliverable& msg, const string& routingKey, const FieldTable* args)
{
    PreRoute pr(msg, this);
    try {
        XmlBinding::vector::ConstPtr p;
        BindingList b(new std::vector<boost::shared_ptr<qpid::broker::Exchange::Binding> >);
        {
            RWlock::ScopedRlock l(lock);
            p = bindingsMap[routingKey].snapshot();
            if (!p.get()) return;
        }

        for (std::vector<XmlBinding::shared_ptr>::const_iterator i = p->begin(); i != p->end(); i++) {
            if (matches((*i)->xquery, msg, args, (*i)->parse_message_content)) { 
                b->push_back(*i);
            }
        }
        doRoute(msg, b);
    } catch (...) {
        QPID_LOG(warning, "XMLExchange " << getName() << ": exception routing message with query " << routingKey);
    }
}


bool XmlExchange::isBound(Queue::shared_ptr queue, const string* const routingKey, const FieldTable* const) 
{
    RWlock::ScopedRlock l(lock);
    if (routingKey) {
        XmlBindingsMap::iterator i = bindingsMap.find(*routingKey);

        if (i == bindingsMap.end())
	    return false;
        if (!queue)
	    return true;
        XmlBinding::vector::ConstPtr p = i->second.snapshot();
        return p && std::find_if(p->begin(), p->end(), MatchQueue(queue)) != p->end();
    } else if (!queue) {
        //if no queue or routing key is specified, just report whether any bindings exist
        return bindingsMap.size() > 0;
    } else {
        for (XmlBindingsMap::iterator i = bindingsMap.begin(); i != bindingsMap.end(); i++) {
	    XmlBinding::vector::ConstPtr p = i->second.snapshot();
            if (p && std::find_if(p->begin(), p->end(), MatchQueue(queue)) != p->end()) return true;
	}
	return false;
    }

}


XmlExchange::~XmlExchange() 
{
    bindingsMap.clear();
}

const std::string XmlExchange::typeName("xml");
 
}
}
