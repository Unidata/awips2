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
#include "qpid/Options.h"
#include "qpid/log/Statement.h"

#include <boost/shared_ptr.hpp>
#include <boost/utility/in_place_factory.hpp>

namespace qpid {
namespace acl {

using namespace std;

/** Note separating options from values to work around boost version differences.
 *  Old boost takes a reference to options objects, but new boost makes a copy.
 *  New boost allows a shared_ptr but that's not compatible with old boost.
 */
struct AclOptions : public Options {
    AclValues& values;

    AclOptions(AclValues& v) : Options("ACL Options"), values(v) {
        addOptions()
            ("acl-file", optValue(values.aclFile, "FILE"), "The policy file to load from, loaded from data dir");
    }
};

struct AclPlugin : public Plugin {

    AclValues values;
    AclOptions options;
    boost::intrusive_ptr<Acl> acl;

    AclPlugin() : options(values) {}

    Options* getOptions() { return &options; }

    void init(broker::Broker& b) {
        if (values.aclFile.empty()){
            QPID_LOG(info, "Policy file not specified. ACL Disabled, no ACL checking being done!");
        	return;
        }

    	if (acl) throw Exception("ACL plugin cannot be initialized twice in one process.");

    	if (values.aclFile.at(0) != '/' && !b.getDataDir().getPath().empty()) {
            std::ostringstream oss;
            oss << b.getDataDir().getPath() << "/" << values.aclFile;
            values.aclFile = oss.str();
    	}

        acl = new Acl(values, b);
		b.setAcl(acl.get());
        b.addFinalizer(boost::bind(&AclPlugin::shutdown, this));
    }

    template <class T> bool init(Plugin::Target& target) {
        T* t = dynamic_cast<T*>(&target);
        if (t) init(*t);
        return t;
    }

    void earlyInitialize(Plugin::Target&) {}

    void initialize(Plugin::Target& target) {
        init<broker::Broker>(target);
    }

    void shutdown() { acl = 0; }
};

static AclPlugin instance; // Static initialization.

// For test purposes.
boost::intrusive_ptr<Acl> getGlobalAcl() { return instance.acl; }

}} // namespace qpid::acl
