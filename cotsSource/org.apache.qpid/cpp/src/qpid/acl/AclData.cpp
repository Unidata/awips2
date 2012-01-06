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

#include "qpid/acl/AclData.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/IntegerTypes.h"
#include <boost/lexical_cast.hpp>

namespace qpid {
namespace acl {

AclData::AclData():decisionMode(qpid::acl::DENY),transferAcl(false)
{
	for (unsigned int cnt=0; cnt< qpid::acl::ACTIONSIZE; cnt++){
	    actionList[cnt]=0;
	}

}

void AclData::clear ()
{
    for (unsigned int cnt=0; cnt< qpid::acl::ACTIONSIZE; cnt++){
	    if (actionList[cnt]){
		    for (unsigned int cnt1=0; cnt1< qpid::acl::OBJECTSIZE; cnt1++)
			    delete actionList[cnt][cnt1]; 
		}
		delete[] actionList[cnt];
	}
	
}

bool AclData::matchProp(const std::string & src, const std::string& src1)
{
    // allow wildcard on the end of strings...
	if (src.data()[src.size()-1]=='*') {
	    return (src.compare(0, src.size()-1, src1, 0,src.size()-1  ) == 0);
	} else {
		return (src.compare(src1)==0) ;
	}
}
 
AclResult AclData::lookup(const std::string& id, const Action& action, const ObjectType& objType,
		const std::string& name, std::map<Property, std::string>* params) {

	QPID_LOG(debug, "ACL: Lookup for id:" << id << " action:" << AclHelper::getActionStr((Action) action)
		 << " objectType:" << AclHelper::getObjectTypeStr((ObjectType) objType) << " name:" << name
		 << " with params " << AclHelper::propertyMapToString(params));

	AclResult aclresult = decisionMode;
	if (actionList[action] && actionList[action][objType]) {
		AclData::actObjItr itrRule = actionList[action][objType]->find(id);
		if (itrRule == actionList[action][objType]->end())
			itrRule = actionList[action][objType]->find("*");

		if (itrRule != actionList[action][objType]->end()) {

			QPID_LOG(debug, "ACL: checking the following rules for : " << itrRule->first );

			//loop the vector
			for (ruleSetItr i = itrRule->second.begin(); i < itrRule->second.end(); i++) {
				QPID_LOG(debug, "ACL: checking rule " <<  i->toString());
				// loop the names looking for match
				bool match = true;
				for (propertyMapItr pMItr = i->props.begin(); (pMItr != i->props.end()) && match; pMItr++) {
					//match name is exists first
					if (pMItr->first == acl::PROP_NAME) {
						if (matchProp(pMItr->second, name)){
							QPID_LOG(debug, "ACL: name '" << name << "' matched with name '"
								 << pMItr->second << "' given in the rule");
					        }else{
							match = false;
							QPID_LOG(debug, "ACL: name '" << name << "' didn't match with name '"
								 << pMItr->second << "' given in the rule");
						}
					} else if (params) { //match pMItr against params
						propertyMapItr paramItr = params->find(pMItr->first);
						if (paramItr == params->end()) {
							match = false;
							QPID_LOG(debug, "ACL: the given parameter map in lookup doesn't contain the property '"
								 << AclHelper::getPropertyStr(pMItr->first) << "'");
						}else if ( pMItr->first == acl::PROP_MAXQUEUECOUNT || pMItr->first == acl::PROP_MAXQUEUESIZE ) {
                                                      if ( pMItr->first == paramItr->first ) {
                                                          uint64_t aclMax = boost::lexical_cast<uint64_t>(pMItr->second);
                                                          uint64_t paramMax = boost::lexical_cast<uint64_t>(paramItr->second);
                                                          QPID_LOG(debug, "ACL: Numeric comparison for property " <<
                                                                   AclHelper::getPropertyStr(paramItr->first)  <<
                                                                   " (value given in lookup = " << 
                                                                   boost::lexical_cast<std::string>(paramItr->second) << 
                                                                   ", value give in rule = " <<
                                                                   boost::lexical_cast<std::string>(pMItr->second) << " )");  
                                                          if (( aclMax ) && ( paramMax == 0 || paramMax > aclMax)){
                                                              match = decisionMode == qpid::acl::ALLOW ;
                                                              QPID_LOG(debug, "ACL: Limit exceeded and match=" << 
                                                              (match ? "true": "false") <<
                                                              " as decision mode is " << AclHelper::getAclResultStr(decisionMode));
                                                          }
                                                      }
                                                }else if (matchProp(pMItr->second, paramItr->second)) {
                                                       QPID_LOG(debug, "ACL: the pair("
								 << AclHelper::getPropertyStr(paramItr->first) << "," << paramItr->second
								 << ") given in lookup matched the pair("
								 << AclHelper::getPropertyStr(pMItr->first) << "," << pMItr->second << ") given in the rule"); 
						} else {
							QPID_LOG(debug, "ACL: the pair("
								 << AclHelper::getPropertyStr(paramItr->first) << "," << paramItr->second
								 << ") given in lookup doesn't match the pair("
								 << AclHelper::getPropertyStr(pMItr->first) << "," << pMItr->second << ") given in the rule");
							match = false;
                  
                                                } 
					}
				}
				if (match)
				{
					aclresult = getACLResult(i->logOnly, i->log);
					QPID_LOG(debug,"Successful match, the decision is:" << AclHelper::getAclResultStr(aclresult));
					return aclresult;
				}
			}
		}
	}

	QPID_LOG(debug,"No successful match, defaulting to the decision mode " << AclHelper::getAclResultStr(aclresult));
	return aclresult;
}

AclResult AclData::lookup(const std::string& id, const Action& action, const ObjectType& objType, const std::string& /*Exchange*/ name, const std::string& RoutingKey)
{

	QPID_LOG(debug, "ACL: Lookup for id:" << id << " action:" << AclHelper::getActionStr((Action) action)
		 << " objectType:" << AclHelper::getObjectTypeStr((ObjectType) objType) << " exchange name:" << name
		 << " with routing key " << RoutingKey);

        AclResult aclresult = decisionMode;
	
	if (actionList[action] && actionList[action][objType]){
	          AclData::actObjItr itrRule = actionList[action][objType]->find(id);
		  
                  if (itrRule == actionList[action][objType]->end())
		       itrRule = actionList[action][objType]->find("*");

		  if (itrRule != actionList[action][objType]->end() ) {
			   
                        QPID_LOG(debug, "ACL: checking the following rules for : " << itrRule->first );
    
			   //loop the vector
    		        for (ruleSetItr i=itrRule->second.begin(); i<itrRule->second.end(); i++) {
				QPID_LOG(debug, "ACL: checking rule " <<  i->toString());
	                   
					// loop the names looking for match
					bool match =true;
					for (propertyMapItr pMItr = i->props.begin(); (pMItr != i->props.end()) && match; pMItr++)
					{
                                                //match name is exists first
						if (pMItr->first == acl::PROP_NAME){
						     if (matchProp(pMItr->second, name)){  							     
                                                         QPID_LOG(debug, "ACL: name '" << name << "' matched with name '"
								  << pMItr->second << "' given in the rule");
                                                             
					             }else{
                                                         match= false;
                                                         QPID_LOG(debug, "ACL: name '" << name << "' didn't match with name '"
							          << pMItr->second << "' given in the rule");
                                                     }    
						}else if (pMItr->first == acl::PROP_ROUTINGKEY){
						     if (matchProp(pMItr->second, RoutingKey)){  
							 QPID_LOG(debug, "ACL: name '" << name << "' matched with routing_key '"
							          << pMItr->second << "' given in the rule");    
					             }else{
                                                         match= false;
                                                         QPID_LOG(debug, "ACL: name '" << name << "' didn't match with routing_key '"
							          << pMItr->second << "' given in the rule");
                                                     } 
						}
					}
					if (match){
                                             aclresult = getACLResult(i->logOnly, i->log);
					     QPID_LOG(debug,"Successful match, the decision is:" << AclHelper::getAclResultStr(aclresult));
					     return aclresult;
                                        }
    		        }
		  }
	}
	QPID_LOG(debug,"No successful match, defaulting to the decision mode " << AclHelper::getAclResultStr(aclresult));
        return aclresult;

}


AclResult AclData::getACLResult(bool logOnly, bool log)
{
	switch (decisionMode)
	{
	case qpid::acl::ALLOWLOG:
	case qpid::acl::ALLOW:
    	 if (logOnly) return qpid::acl::ALLOWLOG;
		 if (log)
	         return qpid::acl::DENYLOG;
		 else
	         return qpid::acl::DENY;
	
	
	case qpid::acl::DENYLOG:
	case qpid::acl::DENY:
	     if (logOnly) return qpid::acl::DENYLOG;
		 if (log)
    	     return qpid::acl::ALLOWLOG;
		 else
    	     return qpid::acl::ALLOW;
	}
	
    QPID_LOG(error, "ACL Decision Failed, setting DENY");
	return qpid::acl::DENY;
}

AclData::~AclData()
{
    clear();
}

}} 
