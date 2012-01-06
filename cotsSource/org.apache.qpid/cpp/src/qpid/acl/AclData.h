#ifndef QPID_ACL_ACLDATA_H
#define QPID_ACL_ACLDATA_H


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

#include "qpid/broker/AclModule.h"
#include <vector>
#include <sstream>

namespace qpid {
namespace acl {

class AclData {


public:

   typedef std::map<qpid::acl::Property, std::string> propertyMap;
   typedef propertyMap::const_iterator propertyMapItr;
   struct rule {
	  
	   bool log;
	   bool logOnly;  // this is a rule is to log only
	   
	   // key value map
      //??
      propertyMap props;
	  
	  
	  rule (propertyMap& p):log(false),logOnly(false),props(p) {};

	  std::string toString () const {
	  	std::ostringstream ruleStr;
	  	ruleStr << "[log=" << log << ", logOnly=" << logOnly << " props{";
	  	for (propertyMapItr pMItr = props.begin(); pMItr != props.end(); pMItr++) {
	  		ruleStr << " " << AclHelper::getPropertyStr((Property) pMItr-> first) << "=" << pMItr->second;
	  	}
	  	ruleStr << " }]";
	  	return ruleStr.str();
	  }
   };
   typedef  std::vector<rule> ruleSet;
   typedef  ruleSet::const_iterator ruleSetItr;
   typedef  std::map<std::string, ruleSet > actionObject; // user 
   typedef  actionObject::iterator actObjItr;
   typedef  actionObject* aclAction;
  
   // Action*[] -> Object*[] -> map<user -> set<Rule> >
   aclAction* actionList[qpid::acl::ACTIONSIZE];
   qpid::acl::AclResult decisionMode;  // determines if the rule set is an deny or accept basis. 
   bool transferAcl;
  
   AclResult lookup(const std::string& id, const Action& action, const ObjectType& objType, const std::string& name, std::map<Property, std::string>* params=0);
   AclResult lookup(const std::string& id, const Action& action, const ObjectType& objType, const std::string& ExchangeName, const std::string& RoutingKey);
   AclResult getACLResult(bool logOnly, bool log);
  
   bool matchProp(const std::string & src, const std::string& src1);
   void clear ();
  
   AclData();
   virtual ~AclData();
};
    
}} // namespace qpid::acl

#endif // QPID_ACL_ACLDATA_H
