#ifndef _broker_ExchangeRegistry_h
#define _broker_ExchangeRegistry_h

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

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/broker/Exchange.h"
#include "qpid/broker/MessageStore.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/sys/Monitor.h"
#include "qpid/management/Manageable.h"

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <algorithm>
#include <map>

namespace qpid {
namespace broker {

struct UnknownExchangeTypeException{};

class ExchangeRegistry{
  public:
    typedef boost::function5<Exchange::shared_ptr, const std::string&, 
                             bool, const qpid::framing::FieldTable&, qpid::management::Manageable*, qpid::broker::Broker*> FactoryFunction;

    ExchangeRegistry (Broker* b = 0) : parent(0), broker(b) {}
    QPID_BROKER_EXTERN std::pair<Exchange::shared_ptr, bool> declare
      (const std::string& name, const std::string& type);
    QPID_BROKER_EXTERN std::pair<Exchange::shared_ptr, bool> declare
      (const std::string& name,
       const std::string& type, 
       bool durable,
       const qpid::framing::FieldTable& args = framing::FieldTable());
    QPID_BROKER_EXTERN void destroy(const std::string& name);
    QPID_BROKER_EXTERN Exchange::shared_ptr get(const std::string& name);
    Exchange::shared_ptr getDefault();

    /**
     * Register the manageable parent for declared exchanges
     */
    void setParent (management::Manageable* _parent) { parent = _parent; }

    /** Register an exchange instance.
     *@return true if registered, false if exchange with same name is already  registered.
     */
    bool registerExchange(const Exchange::shared_ptr&);

    QPID_BROKER_EXTERN void registerType(const std::string& type, FactoryFunction);

    /** Call f for each exchange in the registry. */
    template <class F> void eachExchange(F f) const {
        qpid::sys::RWlock::ScopedRlock l(lock);
        for (ExchangeMap::const_iterator i = exchanges.begin(); i != exchanges.end(); ++i)
            f(i->second);
    }
        
  private:
    typedef std::map<std::string, Exchange::shared_ptr> ExchangeMap;
    typedef std::map<std::string, FactoryFunction > FunctionMap;

    ExchangeMap exchanges;
    FunctionMap factory;
    mutable qpid::sys::RWlock lock;
    management::Manageable* parent;
    Broker* broker;
};

}} // namespace qpid::broker


#endif  /*!_broker_ExchangeRegistry_h*/
