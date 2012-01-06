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

#include "qpid/console/ConsoleListener.h"
#include "qpid/console/SessionManager.h"

using namespace std;
using namespace qpid::console;

class Listener : public ConsoleListener {
public:
    ~Listener() {}

    void brokerConnected(const Broker& broker) {
        cout << "brokerConnected: " << broker << endl;
    }

    void brokerDisconnected(const Broker& broker) {
        cout << "brokerDisconnected: " << broker << endl;
    }

    void newPackage(const std::string& name) {
        cout << "newPackage: " << name << endl;
    }

    void newClass(const ClassKey& classKey) {
        cout << "newClass: key=" << classKey << endl;
    }

    void newAgent(const Agent& agent) {
        cout << "newAgent: " << agent << endl;
    }

    void delAgent(const Agent& agent) {
        cout << "delAgent: " << agent << endl;
    }

    void objectProps(Broker& broker, Object& object) {
        cout << "objectProps: broker=" << broker << " object=" << object << endl;
    }

    void objectStats(Broker& broker, Object& object) {
        cout << "objectStats: broker=" << broker << " object=" << object << endl;
    }

    void event(Event& event) {
        cout << "event: " << event << endl;
    }
};


//==============================================================
// Main program
//==============================================================
int main_int(int /*argc*/, char** /*argv*/)
{
    //Listener listener;
    qpid::client::ConnectionSettings settings;

    cout << "Creating SessionManager" << endl;
    SessionManager sm;
    cout << "Adding broker" << endl;
    Broker* broker;

    broker = sm.addBroker(settings);

    cout << "Package List:" << endl;
    vector<string> packages;
    sm.getPackages(packages);
    for (vector<string>::iterator iter = packages.begin(); iter != packages.end(); iter++) {
        cout << "    " << *iter << endl;
        SessionManager::KeyVector classKeys;
        sm.getClasses(classKeys, *iter);
        for (SessionManager::KeyVector::iterator cIter = classKeys.begin();
             cIter != classKeys.end(); cIter++)
            cout << "        " << *cIter << endl;
    }

    Object::Vector list;
    cout << "getting exchanges..." << endl;
    sm.getObjects(list, "exchange");
    cout << "   returned " << list.size() << " elements" << endl;

    for (Object::Vector::iterator i = list.begin(); i != list.end(); i++) {
        cout << "exchange: " << *i << endl;
    }

    list.clear();
    cout << "getting queues..." << endl;
    sm.getObjects(list, "queue");
    cout << "   returned " << list.size() << " elements" << endl;

    for (Object::Vector::iterator i = list.begin(); i != list.end(); i++) {
        cout << "queue: " << *i << endl;
        cout << "  bindingCount=" << i->attrUint("bindingCount") << endl;
        cout << "  arguments=" << i->attrMap("arguments") << endl;
    }

    list.clear();
    sm.getObjects(list, "broker");
    if (list.size() == 1) {
        Object& broker = *list.begin();

        cout << "Broker: " << broker << endl;

        Object::AttributeMap args;
        MethodResponse result;
        args.addUint("sequence", 1);
        args.addString("body", "Testing...");

        cout << "Call echo method..." << endl;
        broker.invokeMethod("echo", args, result);
        cout << "Result: code=" << result.code << " text=" << result.text << endl;
        for (Object::AttributeMap::iterator aIter = result.arguments.begin();
             aIter != result.arguments.end(); aIter++) {
            cout << "   Output Arg: " << aIter->first << " => " << aIter->second->str() << endl;
        }
    }

    sm.delBroker(broker);
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

