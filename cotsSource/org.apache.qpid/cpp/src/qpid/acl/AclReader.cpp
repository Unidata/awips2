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

#include <cctype>
#include <cstring>
#include <fstream>
#include <sstream>
#include "qpid/log/Statement.h"
#include "qpid/Exception.h"

#include <iomanip> // degug
#include <iostream> // debug

#define ACL_FORMAT_ERR_LOG_PREFIX "ACL format error: " << fileName << ":" << lineNumber << ": "

namespace qpid {
namespace acl {

AclReader::aclRule::aclRule(const AclResult r, const std::string n, const groupMap& groups) : res(r), actionAll(true), objStatus(NONE) {
    processName(n, groups);
}
AclReader::aclRule::aclRule(const AclResult r, const std::string n, const groupMap& groups, const Action a) : res(r), actionAll(false), action(a), objStatus(NONE) {
    processName(n, groups);
}

void AclReader::aclRule::setObjectType(const ObjectType o) {
    objStatus = VALUE;
    object = o;
}

void AclReader::aclRule::setObjectTypeAll() {
    objStatus = ALL;
}

bool AclReader::aclRule::addProperty(const Property p, const std::string v) {
    return props.insert(propNvPair(p, v)).second;
}

bool AclReader::aclRule::validate(const AclHelper::objectMapPtr& /*validationMap*/) {
    // TODO - invalid rules won't ever be called in real life...
    return true;
}

// Debug aid
std::string AclReader::aclRule::toString() {
    std::ostringstream oss;
    oss << AclHelper::getAclResultStr(res) << " [";
    for (nsCitr itr = names.begin(); itr != names.end(); itr++) {
        if (itr != names.begin()) oss << ", ";
        oss << *itr;
    }
    oss << "]";
    if (actionAll) {
        oss << " *";
    } else {
        oss << " " << AclHelper::getActionStr(action);
    }
    if (objStatus == ALL) {
        oss << " *";
    } else if (objStatus == VALUE) {
        oss << " " << AclHelper::getObjectTypeStr(object);
    }
    for (pmCitr i=props.begin(); i!=props.end(); i++) {
        oss << " " << AclHelper::getPropertyStr(i->first) << "=" << i->second;
    }
    return oss.str();
}

void AclReader::loadDecisionData(boost::shared_ptr<AclData> d) {
	d->clear();
	QPID_LOG(debug, "ACL Load Rules");
	int cnt = rules.size();
	bool foundmode = false;

	for (rlCitr i = rules.end() - 1; cnt; i--, cnt--) {
		QPID_LOG(debug, "ACL Processing " << std::setfill(' ') << std::setw(2)
				<< cnt << " " << (*i)->toString());

		if (!foundmode && (*i)->actionAll && (*i)->names.size() == 1
				&& (*((*i)->names.begin())).compare("*") == 0) {
			d->decisionMode = (*i)->res;
			QPID_LOG(debug, "ACL FoundMode " << AclHelper::getAclResultStr(
					d->decisionMode));
			foundmode = true;
		} else {
			AclData::rule rule((*i)->props);
			bool addrule = true;

			switch ((*i)->res) {
			case qpid::acl::ALLOWLOG:
				rule.log = true;
				if (d->decisionMode == qpid::acl::ALLOW || d->decisionMode
						== qpid::acl::ALLOWLOG)
					rule.logOnly = true;
				break;
			case qpid::acl::ALLOW:
				if (d->decisionMode == qpid::acl::ALLOW || d->decisionMode
						== qpid::acl::ALLOWLOG)
					addrule = false;
				break;
			case qpid::acl::DENYLOG:
				rule.log = true;
				if (d->decisionMode == qpid::acl::DENY || d->decisionMode
						== qpid::acl::DENYLOG)
					rule.logOnly = true;
				break;
			case qpid::acl::DENY:
				if (d->decisionMode == qpid::acl::DENY || d->decisionMode
						== qpid::acl::DENYLOG)
					addrule = false;
				break;
			default:
				throw Exception("Invalid ACL Result loading rules.");
			}

			// Action -> Object -> map<user -> set<Rule> >
			if (addrule) {
				std::ostringstream actionstr;
				for (int acnt = ((*i)->actionAll ? 0 : (*i)->action); acnt
						< acl::ACTIONSIZE; (*i)->actionAll ? acnt++ : acnt
						= acl::ACTIONSIZE) {

					if (acnt == acl::ACT_PUBLISH)
						d->transferAcl = true; // we have transfer ACL

					actionstr << AclHelper::getActionStr((Action) acnt) << ",";

					//find the Action, create if not exist
					if (d->actionList[acnt] == NULL) {
						d->actionList[acnt]
								= new AclData::aclAction[qpid::acl::OBJECTSIZE];
						for (int j = 0; j < qpid::acl::OBJECTSIZE; j++)
							d->actionList[acnt][j] = NULL;
					}

					// optimize this loop to limit to valid options only!!
					for (int ocnt = ((*i)->objStatus != aclRule::VALUE ? 0
							: (*i)->object); ocnt < acl::OBJECTSIZE; (*i)->objStatus
							!= aclRule::VALUE ? ocnt++ : ocnt = acl::OBJECTSIZE) {

						//find the Object, create if not exist
						if (d->actionList[acnt][ocnt] == NULL)
							d->actionList[acnt][ocnt]
									= new AclData::actionObject;

						// add users and Rule to object set
						bool allNames = false;
						// check to see if names.begin is '*'
						if ((*(*i)->names.begin()).compare("*") == 0)
							allNames = true;

						for (nsCitr itr = (allNames ? names.begin()
								: (*i)->names.begin()); itr
								!= (allNames ? names.end() : (*i)->names.end()); itr++) {

							AclData::actObjItr itrRule =
									d->actionList[acnt][ocnt]->find(*itr);

							if (itrRule == d->actionList[acnt][ocnt]->end()) {
								AclData::ruleSet rSet;
								rSet.push_back(rule);
								d->actionList[acnt][ocnt]->insert(make_pair(
										std::string(*itr), rSet));
							} else {

								// TODO add code to check for dead rules
								// allow peter create queue name=tmp <-- dead rule!!
								// allow peter create queue

								itrRule->second.push_back(rule);
							}
						}

					}
				}

				std::ostringstream objstr;
				for (int ocnt = ((*i)->objStatus != aclRule::VALUE ? 0 : (*i)->object); ocnt < acl::OBJECTSIZE;
					 (*i)->objStatus != aclRule::VALUE ? ocnt++ : ocnt = acl::OBJECTSIZE) {
										objstr << AclHelper::getObjectTypeStr((ObjectType) ocnt) << ",";
				}

				bool allNames = ((*(*i)->names.begin()).compare("*") == 0);
				std::ostringstream userstr;
				for (nsCitr itr = (allNames ? names.begin() : (*i)->names.begin());
				     itr != (allNames ? names.end() : (*i)->names.end()); itr++) {
											userstr << *itr << ",";
				}

				QPID_LOG(debug,"ACL: Adding actions {" << actionstr.str().substr(0,actionstr.str().length()-1)
						       << "} to objects {" << objstr.str().substr(0,objstr.str().length()-1)
						       << "} with props " << AclHelper::propertyMapToString(&rule.props)
						       << " for users {" << userstr.str().substr(0,userstr.str().length()-1) << "}" );
			} else {
				QPID_LOG(debug, "ACL Skipping based on Mode:"
						<< AclHelper::getAclResultStr(d->decisionMode));
			}
		}

	}

}


void AclReader::aclRule::processName(const std::string& name, const groupMap& groups) {
    if (name.compare("all") == 0) {
        names.insert("*");
    } else {
        gmCitr itr = groups.find(name);
        if (itr == groups.end()) {
            names.insert(name);
        } else {
            names.insert(itr->second->begin(), itr->second->end());
        }
    }
}

AclReader::AclReader() : lineNumber(0), contFlag(false), validationMap(new AclHelper::objectMap) {
    AclHelper::loadValidationMap(validationMap);
	names.insert("*");
}

AclReader::~AclReader() {}

std::string AclReader::getError() {
	return errorStream.str();
}

int AclReader::read(const std::string& fn, boost::shared_ptr<AclData> d) {
    fileName = fn;
    lineNumber = 0;
    char buff[1024];
    std::ifstream ifs(fn.c_str(), std::ios_base::in);
    if (!ifs.good()) {
        errorStream << "Unable to open ACL file \"" << fn << "\": eof=" << (ifs.eof()?"T":"F") << "; fail=" << (ifs.fail()?"T":"F") << "; bad=" << (ifs.bad()?"T":"F");
        return -1;
    }
    try {
        bool err = false;
        while (ifs.good()) {
            ifs.getline(buff, 1024);
            lineNumber++;
            if (std::strlen(buff) > 0 && buff[0] != '#') // Ignore blank lines and comments
                err |= !processLine(buff);
        }
        if (!ifs.eof())
        {
            errorStream << "Unable to read ACL file \"" << fn << "\": eof=" << (ifs.eof()?"T":"F") << "; fail=" << (ifs.fail()?"T":"F") << "; bad=" << (ifs.bad()?"T":"F");
            ifs.close();
            return -2;
        }
        ifs.close();
        if (err) return -3;
        QPID_LOG(notice, "Read ACL file \"" <<  fn << "\"");
    } catch (const std::exception& e) {
        errorStream << "Unable to read ACL file \"" << fn << "\": " << e.what();
        ifs.close();
        return -4;
    } catch (...) {
        errorStream << "Unable to read ACL file \"" << fn << "\": Unknown exception";
        ifs.close();
        return -5;
    }
    printNames();
    printRules();
	loadDecisionData(d);

    return 0;
}

bool AclReader::processLine(char* line) {
    bool ret = false;
    std::vector<std::string> toks;

    // Check for continuation
    char* contCharPtr = std::strrchr(line, '\\');
    bool cont = contCharPtr != 0;
    if (cont) *contCharPtr = 0;

    int numToks = tokenize(line, toks);
    if (numToks && (toks[0].compare("group") == 0 || contFlag)) {
        ret = processGroupLine(toks, cont);
    } else if (numToks && toks[0].compare("acl") == 0) {
        ret = processAclLine(toks);
    } else {
        // Check for whitespace only line, ignore these
        bool ws = true;
        for (unsigned i=0; i<std::strlen(line) && ws; i++) {
            if (!std::isspace(line[i])) ws = false;
        }
        if (ws) {
            ret = true;
        } else {
            errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Non-continuation line must start with \"group\" or \"acl\".";
            ret = false;
        }
    }
    contFlag = cont;
    return ret;
}

int  AclReader::tokenize(char* line, std::vector<std::string>& toks) {
    const char* tokChars = " \t\n\f\v\r";
    int cnt = 0;
    char* cp = std::strtok(line, tokChars);
    while (cp != 0) {
        toks.push_back(std::string(cp));
        cnt++;
        cp = std::strtok(0, tokChars);
    }
    return cnt;
}

// Return true if the line is successfully processed without errors
// If cont is true, then groupName must be set to the continuation group name
bool AclReader::processGroupLine(tokList& toks, const bool cont) {
    const unsigned toksSize = toks.size();
    if (contFlag) {
        gmCitr citr = groups.find(groupName);
        for (unsigned i = 0; i < toksSize; i++) {
            if (!checkName(toks[i])) {
                errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Name \"" << toks[i] << "\" contains illegal characters.";
                return false;
            }
            if (!isValidUserName(toks[i])) return false;
            addName(toks[i], citr->second);
        }
    } else {
        if (toksSize < (cont ? 2 : 3)) {
            errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Insufficient tokens for group definition.";
            return false;
        }
        if (!checkName(toks[1])) {
            errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Group name \"" << toks[1] << "\" contains illegal characters.";
            return false;
        }
        gmCitr citr = addGroup(toks[1]);
        if (citr == groups.end()) return false;
        for (unsigned i = 2; i < toksSize; i++) {
            if (!checkName(toks[i])) {
                errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Name \"" << toks[i] << "\" contains illegal characters.";
                return false;
            }
            if (!isValidUserName(toks[i])) return false;
            addName(toks[i], citr->second);
        }
    }
    return true;
}

// Return true if sucessfully added group
AclReader::gmCitr AclReader::addGroup(const std::string& newGroupName) {
    gmCitr citr = groups.find(newGroupName);
    if (citr != groups.end()) {
        errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Duplicate group name \"" << newGroupName << "\".";
        return groups.end();
    }
    groupPair p(newGroupName, nameSetPtr(new nameSet));
    gmRes res = groups.insert(p);
    assert(res.second);
    groupName = newGroupName;
    return res.first;
}

void AclReader::addName(const std::string& name, nameSetPtr groupNameSet) {
    gmCitr citr = groups.find(name);
    if (citr != groups.end() && citr->first != name){
        // This is a previously defined group: add all the names in that group to this group
        groupNameSet->insert(citr->second->begin(), citr->second->end());
    } else {
        // Not a known group name
        groupNameSet->insert(name);
        addName(name);
    }
}

void AclReader::addName(const std::string& name) {
    names.insert(name);
}

// Debug aid
void AclReader::printNames() const {
    QPID_LOG(debug, "Group list: " << groups.size() << " groups found:" );
    std::string tmp;
	for (gmCitr i=groups.begin(); i!= groups.end(); i++) {
        tmp += "  \"";
		tmp += i->first;
		tmp +=  "\":";
        for (nsCitr j=i->second->begin(); j!=i->second->end(); j++) {
            tmp += " ";
			tmp += *j;
        }
        QPID_LOG(debug, tmp);
		tmp.clear();
    }
	QPID_LOG(debug, "Name list: " << names.size() << " names found:" );
	tmp.clear();
    for (nsCitr k=names.begin(); k!=names.end(); k++) {
        tmp += " ";
		tmp += *k;
    }
    QPID_LOG(debug, tmp);
}

bool AclReader::processAclLine(tokList& toks) {
    const unsigned toksSize = toks.size();
    if (toksSize < 4) {
        errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Insufficient tokens for acl definition.";
        return false;
    }

    AclResult res;
    try {
        res = AclHelper::getAclResult(toks[1]);
    } catch (...) {
        errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Unknown ACL permission \"" << toks[1] << "\".";
        return false;
    }

    bool actionAllFlag = toks[3].compare("all") == 0;
    bool userAllFlag   = toks[2].compare("all") == 0;
    Action action;
    if (actionAllFlag) {

        if (userAllFlag && toksSize > 4) {
            errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Tokens found after action \"all\".";
            return false;
        }
        action = ACT_CONSUME; // dummy; compiler must initialize action for this code path
    } else {
        try {
            action = AclHelper::getAction(toks[3]);
        } catch (...) {
            errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Unknown action \"" << toks[3] << "\".";
            return false;
        }
    }

    // Create rule obj; then add object (if any) and properties (if any)
    aclRulePtr rule;
    if (actionAllFlag) {
        rule.reset(new aclRule(res, toks[2], groups));
    } else {
        rule.reset(new aclRule(res, toks[2], groups, action));
    }
    
    if (toksSize >= 5) { // object name-value pair
        if (toks[4].compare("all") == 0) {
            rule->setObjectTypeAll();
        } else {
            try {
                rule->setObjectType(AclHelper::getObjectType(toks[4]));
            } catch (...) {
                errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Unknown object \"" << toks[4] << "\".";
                return false;
            }
        }
    }

    if (toksSize >= 6) { // property name-value pair(s)
        for (unsigned i=5; i<toksSize; i++) {
            nvPair propNvp = splitNameValuePair(toks[i]);
            if (propNvp.second.size() == 0) {
                errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Badly formed property name-value pair \"" << propNvp.first << "\". (Must be name=value)";
                return false;
            }
            Property prop;
            try {
                prop = AclHelper::getProperty(propNvp.first);
            } catch (...) {
                errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Unknown property \"" << propNvp.first << "\".";
                return false;
            }
            rule->addProperty(prop, propNvp.second);
        }
    }
    // Check if name (toks[2]) is group; if not, add as name of individual
    if (toks[2].compare("all") != 0) {
        if (groups.find(toks[2]) == groups.end()) {
            addName(toks[2]);
        }
    }

    // If rule validates, add to rule list
    if (!rule->validate(validationMap)) {
        errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Invalid object/action/property combination.";
        return false;
    }
    rules.push_back(rule);

    return true;
}

// Debug aid
void AclReader::printRules() const {
    QPID_LOG(debug, "Rule list: " << rules.size() << " ACL rules found:");
    int cnt = 0;
    for (rlCitr i=rules.begin(); i<rules.end(); i++,cnt++) {
        QPID_LOG(debug, "  " << std::setfill(' ') << std::setw(2) << cnt << " " << (*i)->toString());
    }
}

// Static function
// Return true if the name is well-formed (ie contains legal characters)
bool AclReader::checkName(const std::string& name) {
    for (unsigned i=0; i<name.size(); i++) {
        const char ch = name.at(i);
        if (!std::isalnum(ch) && ch != '-' && ch != '_' && ch != '@') return false;
    }
    return true;
}

// Static function
// Split name-value pair around '=' char of the form "name=value"
AclReader::nvPair AclReader::splitNameValuePair(const std::string& nvpString) {
    std::size_t pos = nvpString.find("=");
    if (pos == std::string::npos || pos == nvpString.size() - 1) {
        return nvPair(nvpString, "");
    }
    return nvPair(nvpString.substr(0, pos), nvpString.substr(pos+1));
}

// Returns true if a username has the name@realm format
bool AclReader::isValidUserName(const std::string& name){
	size_t pos = name.find('@');
	if ( pos == std::string::npos || pos == name.length() -1){
		errorStream << ACL_FORMAT_ERR_LOG_PREFIX << "Username '" << name << "' must contain a realm";
		return false;
	}
	return true;
}

}} // namespace qpid::acl
