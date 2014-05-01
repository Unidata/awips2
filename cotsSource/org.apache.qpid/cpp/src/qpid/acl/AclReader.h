#ifndef QPID_ACL_ACLREADER_H
#define QPID_ACL_ACLREADER_H


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

#include <boost/shared_ptr.hpp>
#include <map>
#include <set>
#include <string>
#include <vector>
#include <sstream>
#include "qpid/acl/AclData.h"
#include "qpid/broker/AclModule.h"

namespace qpid {
namespace acl {

class AclReader {
    typedef std::set<std::string> nameSet;
    typedef nameSet::const_iterator nsCitr;
    typedef boost::shared_ptr<nameSet> nameSetPtr;

    typedef std::pair<std::string, nameSetPtr> groupPair;
    typedef std::map<std::string, nameSetPtr> groupMap;
    typedef groupMap::const_iterator gmCitr;
    typedef std::pair<gmCitr, bool> gmRes;

    typedef std::pair<Property, std::string> propNvPair;
    typedef std::map<Property, std::string> propMap;
    typedef propMap::const_iterator pmCitr;

    class aclRule {
      public:
        enum objectStatus {NONE, VALUE, ALL};
        AclResult res;
        nameSet names;
        bool actionAll; // True if action is set to keyword "all"
        Action action; // Ignored if action is set to keyword "all"
        objectStatus objStatus;
        ObjectType object; // Ignored for all status values except VALUE
        propMap props;
      public:
        aclRule(const AclResult r, const std::string n, const groupMap& groups); // action = "all"
        aclRule(const AclResult r, const std::string n, const groupMap& groups, const Action a);
        void setObjectType(const ObjectType o);
        void setObjectTypeAll();
        bool addProperty(const Property p, const std::string v);
        bool validate(const AclHelper::objectMapPtr& validationMap);
        std::string toString(); // debug aid
      private:
        void processName(const std::string& name, const groupMap& groups);
    };
    typedef boost::shared_ptr<aclRule> aclRulePtr;
    typedef std::vector<aclRulePtr> ruleList;
    typedef ruleList::const_iterator rlCitr;

    typedef std::vector<std::string> tokList;
    typedef tokList::const_iterator tlCitr;

    typedef std::set<std::string> keywordSet;
    typedef keywordSet::const_iterator ksCitr;
    typedef std::pair<std::string, std::string> nvPair; // Name-Value pair

    std::string fileName;
    int lineNumber;
    bool contFlag;
    std::string groupName;
    nameSet names;
    groupMap groups;
    ruleList rules;
    AclHelper::objectMapPtr validationMap;
    std::ostringstream errorStream;

  public:
    AclReader();
    virtual ~AclReader();
    int read(const std::string& fn, boost::shared_ptr<AclData> d);
    std::string getError();

  private:
    bool processLine(char* line);
    void loadDecisionData( boost::shared_ptr<AclData> d);
    int tokenize(char* line, tokList& toks);

    bool processGroupLine(tokList& toks, const bool cont);
    gmCitr addGroup(const std::string& groupName);
    void addName(const std::string& name, nameSetPtr groupNameSet);
    void addName(const std::string& name);
    void printNames() const; // debug aid

    bool processAclLine(tokList& toks);
    void printRules() const; // debug aid
    bool isValidUserName(const std::string& name);

    static bool checkName(const std::string& name);
    static nvPair splitNameValuePair(const std::string& nvpString);
};

}} // namespace qpid::acl

#endif // QPID_ACL_ACLREADER_H
