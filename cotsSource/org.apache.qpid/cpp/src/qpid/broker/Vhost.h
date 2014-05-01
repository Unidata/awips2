#ifndef _Vhost_
#define _Vhost_

//
// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
// 
//   http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//

#include "qpid/management/Manageable.h"
#include "qmf/org/apache/qpid/broker/Vhost.h"
#include <boost/shared_ptr.hpp>

namespace qpid { 
namespace broker {

class Broker;
class Vhost : public management::Manageable
{
  private:

    qmf::org::apache::qpid::broker::Vhost* mgmtObject;

  public:

    typedef boost::shared_ptr<Vhost> shared_ptr;

    Vhost (management::Manageable* parentBroker, Broker* broker = 0);

    management::ManagementObject* GetManagementObject (void) const
    { return mgmtObject; }
    void setFederationTag(const std::string& tag);
};

}}

#endif  /*!_Vhost_*/
