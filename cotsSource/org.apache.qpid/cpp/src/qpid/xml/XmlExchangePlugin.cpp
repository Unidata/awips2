/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <sstream>
#include "qpid/acl/Acl.h"
#include "qpid/broker/Broker.h"
#include "qpid/Plugin.h"
#include "qpid/log/Statement.h"

#include <boost/shared_ptr.hpp>
#include <boost/utility/in_place_factory.hpp>

#include "qpid/xml/XmlExchange.h"

namespace qpid {
namespace broker {  // ACL uses the acl namespace here - should I?

using namespace std;
class Broker;

Exchange::shared_ptr create(const std::string& name, bool durable,
                            const framing::FieldTable& args, 
                            management::Manageable* parent,
                            Broker* broker)
{
    Exchange::shared_ptr e(new XmlExchange(name, durable, args, parent, broker));
    return e;
}


class XmlExchangePlugin : public Plugin
{
public:
    void earlyInitialize(Plugin::Target& target);
    void initialize(Plugin::Target& target);
};
  

void XmlExchangePlugin::earlyInitialize(Plugin::Target& target)
{
      Broker* broker = dynamic_cast<broker::Broker*>(&target);
      if (broker) {
          broker->getExchanges().registerType(XmlExchange::typeName, &create);
          QPID_LOG(info, "Registered xml exchange");
      }
}

void XmlExchangePlugin::initialize(Target&) {}


static XmlExchangePlugin matchingPlugin;


}} // namespace qpid::acl
