#ifndef QPID_ACL_ACL_H
#define QPID_ACL_ACL_H


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



#include "qpid/acl/AclReader.h"
#include "qpid/RefCounted.h"
#include "qpid/broker/AclModule.h"
#include "qpid/management/Manageable.h"
#include "qpid/management/ManagementAgent.h"
#include "qmf/org/apache/qpid/acl/Acl.h"

#include <map>
#include <string>


namespace qpid {
namespace broker {
class Broker;
}

namespace acl {

struct AclValues {
	std::string aclFile;
};


class Acl : public broker::AclModule, public RefCounted, public management::Manageable
{

private:
   acl::AclValues aclValues;
   broker::Broker* broker;
   bool transferAcl;
   boost::shared_ptr<AclData> data;
   qmf::org::apache::qpid::acl::Acl* mgmtObject; // mgnt owns lifecycle
   qpid::management::ManagementAgent* agent;

public:
   Acl (AclValues& av, broker::Broker& b);

   void initialize();
   
   inline virtual bool doTransferAcl() {return transferAcl;};
   
   // create specilied authorise methods for cases that need faster matching as needed.
   virtual bool authorise(const std::string& id, const Action& action, const ObjectType& objType, const std::string& name, std::map<Property, std::string>* params=0);
   virtual bool authorise(const std::string& id, const Action& action, const ObjectType& objType, const std::string& ExchangeName,const std::string& RoutingKey);

   virtual ~Acl();
private:
   bool result(const AclResult& aclreslt, const std::string& id, const Action& action, const ObjectType& objType, const std::string& name);
   bool readAclFile(std::string& errorText);
   bool readAclFile(std::string& aclFile, std::string& errorText);
   virtual qpid::management::ManagementObject* GetManagementObject(void) const;
   virtual management::Manageable::status_t ManagementMethod (uint32_t methodId, management::Args& args, std::string& text);

};



}} // namespace qpid::acl

#endif // QPID_ACL_ACL_H
