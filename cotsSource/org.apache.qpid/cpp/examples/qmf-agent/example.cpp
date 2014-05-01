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

#include <qpid/management/Manageable.h>
#include <qpid/management/ManagementObject.h>
#include <qpid/agent/ManagementAgent.h>
#include <qpid/sys/Mutex.h>
#include <qpid/sys/Time.h>
#include "qmf/org/apache/qpid/agent/example/Parent.h"
#include "qmf/org/apache/qpid/agent/example/Child.h"
#include "qmf/org/apache/qpid/agent/example/ArgsParentCreate_child.h"
#include "qmf/org/apache/qpid/agent/example/EventChildCreated.h"
#include "qmf/org/apache/qpid/agent/example/Package.h"

#include <signal.h>
#include <cstdlib>
#include <iostream>

#include <sstream>

static bool running = true;

using namespace std;
using qpid::management::ManagementAgent;
using qpid::management::ManagementObject;
using qpid::management::Manageable;
using qpid::management::Args;
using qpid::sys::Mutex;
namespace _qmf = qmf::org::apache::qpid::agent::example;

class ChildClass;

//==============================================================
// CoreClass is the operational class that corresponds to the
// "Parent" class in the management schema.
//==============================================================
class CoreClass : public Manageable
{
    string           name;
    ManagementAgent* agent;
    _qmf::Parent* mgmtObject;
    std::vector<ChildClass*> children;
    Mutex vectorLock;

public:

    CoreClass(ManagementAgent* agent, string _name);
    ~CoreClass() { mgmtObject->resourceDestroy(); }

    ManagementObject* GetManagementObject(void) const
    { return mgmtObject; }

    void doLoop();
    status_t ManagementMethod (uint32_t methodId, Args& args, string& text);
};

class ChildClass : public Manageable
{
    string name;
    _qmf::Child* mgmtObject;

public:

    ChildClass(ManagementAgent* agent, CoreClass* parent, string name);
    ~ChildClass() { mgmtObject->resourceDestroy(); }

    ManagementObject* GetManagementObject(void) const
    { return mgmtObject; }

    void doWork()
    {
        mgmtObject->inc_count(2);
    }
};

CoreClass::CoreClass(ManagementAgent* _agent, string _name) : name(_name), agent(_agent)
{
    static uint64_t persistId = 0x111222333444555LL;
    mgmtObject = new _qmf::Parent(agent, this, name);

    agent->addObject(mgmtObject, persistId++);
    mgmtObject->set_state("IDLE");
}

void CoreClass::doLoop()
{
    // Periodically bump a counter to provide a changing statistical value
    while (running) {
        qpid::sys::sleep(1);
        mgmtObject->inc_count();
        mgmtObject->set_state("IN_LOOP");

        {
            Mutex::ScopedLock _lock(vectorLock);

            for (std::vector<ChildClass*>::iterator iter = children.begin();
                 iter != children.end();
                 iter++) {
                (*iter)->doWork();
            }
        }
    }
}

Manageable::status_t CoreClass::ManagementMethod(uint32_t methodId, Args& args, string& /*text*/)
{
    Mutex::ScopedLock _lock(vectorLock);

    switch (methodId) {
    case _qmf::Parent::METHOD_CREATE_CHILD:
        _qmf::ArgsParentCreate_child& ioArgs = (_qmf::ArgsParentCreate_child&) args;

        ChildClass *child = new ChildClass(agent, this, ioArgs.i_name);
        ioArgs.o_childRef = child->GetManagementObject()->getObjectId();

        children.push_back(child);

        agent->raiseEvent(_qmf::EventChildCreated(ioArgs.i_name));

        return STATUS_OK;
    }

    return STATUS_NOT_IMPLEMENTED;
}

ChildClass::ChildClass(ManagementAgent* agent, CoreClass* parent, string name)
{
    mgmtObject = new _qmf::Child(agent, this, parent, name);

    agent->addObject(mgmtObject);
}


//==============================================================
// Main program
//==============================================================

ManagementAgent::Singleton* singleton;

void shutdown(int)
{
    running = false;
}

int main_int(int argc, char** argv)
{
    singleton = new ManagementAgent::Singleton();
    const char* host = argc>1 ? argv[1] : "127.0.0.1";
    int port = argc>2 ? atoi(argv[2]) : 5672;
    qpid::client::ConnectionSettings settings;

    settings.host = host;
    settings.port = port;

    signal(SIGINT, shutdown);

    // Create the qmf management agent
    ManagementAgent* agent = singleton->getInstance();

    // Register the Qmf_example schema with the agent
    _qmf::Package packageInit(agent);

    // Start the agent.  It will attempt to make a connection to the
    // management broker
    agent->init(settings, 5, false, ".magentdata");

    // Allocate some core objects
    CoreClass core1(agent, "Example Core Object #1");
    CoreClass core2(agent, "Example Core Object #2");
    CoreClass core3(agent, "Example Core Object #3");

    core1.doLoop();

    // done, cleanup and exit
    delete singleton;

    return 0;
}

int main(int argc, char** argv)
{
    try {
        return main_int(argc, argv);
    } catch(std::exception& e) {
        cout << "Top Level Exception: " << e.what() << endl;
    }
}

