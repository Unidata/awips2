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
#include "qpid/sys/Time.h"

using namespace std;
using namespace qpid::console;

//
// Declare a subclass of ConsoleListener to receive asynchronous data.
//
class Listener : public ConsoleListener {

    //
    // Declare a map from ObjectId to string to store queue names by their object IDs.
    //
    typedef map<ObjectId, string> QueueMap;
    QueueMap queueMap;
public:
    ~Listener() {}

    //
    // Receive property updates from the agent.
    //
    void objectProps(Broker& /*broker*/, Object& object) {
        string name = object.attrString("name");
        ObjectId oid = object.getObjectId();
        QueueMap::iterator iter = queueMap.find(oid);

        if (iter == queueMap.end()) {
            //
            // Object is not in the map.  Learn it.
            //
            cout << "New Queue: " << name << endl;
            queueMap[oid] = name;
        }
    }

    //
    // Receive statistic updates from the agent.
    //
    void objectStats(Broker& /*broker*/, Object& object) {
        ObjectId oid = object.getObjectId();
        QueueMap::iterator iter = queueMap.find(oid);
        if (iter == queueMap.end())
            //
            // Object id is not in the map.  We are not interested in this update.
            //
            return;

        cout << "Stats for: " << iter->second << endl;
        cout << "    msgTotalEnqueues = " << object.attrUint64("msgTotalEnqueues") << endl;
        cout << "    msgTotalDequeues = " << object.attrUint64("msgTotalDequeues") << endl;
        cout << "    msgDepth         = " << object.attrUint("msgDepth") << endl;

        if (object.isDeleted()) {
            //
            // Object was deleted and is in the map.  Remove it.
            //
            cout << "Queue Deleted: " << iter->second << endl;
            queueMap.erase(oid);
        }

        //
        // Note that the object-delete logic is done after processing statistics.
        // This allows us to get the "final" statistics for a deleted object.  It also
        // assures that very short-lived objects are accounted for (i.e. created, used,
        // and destroyed all within a single reporting interval).
        //
    }
};

//==============================================================
// Main program
//==============================================================
int main_int(int /*argc*/, char** /*argv*/)
{
    Listener listener;

    //
    // Tune the settings for this application:  We wish to receive objects but not events.
    // By using "userBindings", we can restrict which objects we receive updates for.
    //
    SessionManager::Settings sessionSettings;
    sessionSettings.rcvObjects = true;
    sessionSettings.rcvEvents = false;
    sessionSettings.rcvHeartbeats = false;
    sessionSettings.userBindings = true;

    SessionManager sm(&listener, sessionSettings);

    //
    // We want to receive updates only for the broker queue object.
    //
    sm.bindClass("org.apache.qpid.broker", "queue");

    //
    // Connect to the broker.
    //
    qpid::client::ConnectionSettings connSettings;
    Broker* broker = sm.addBroker(connSettings);

    //
    // Sleep while the listener does all the work asynchronously.
    //
    for (;;) {
      qpid::sys::sleep(1);
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

