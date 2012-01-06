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
#include "qpid/sys/Mutex.h"
#include <signal.h>
#include <map>

using namespace std;
using namespace qpid::console;
using qpid::sys::Mutex;

//
//  This example maintains connections to a number of brokers (assumed
//  to be running on localhost and at ports listed in the command line
//  arguments).
//
//  The program then periodically polls queue information from a
//  single operational broker.  This is a useful illustration of how
//  one might monitor statistics on a cluster of brokers.
//

//==============================================================
// Main program
//==============================================================

//
//  The Main class extends ConsoleListener so it can receive broker connected/disconnected
//  notifications.
//
class Main : public ConsoleListener {
    bool  stopping;  // Used to tell the program to exit
    Mutex lock;      // Mutex to protect the broker-map
    map<Broker*, bool> brokerMap; // Map of broker-pointers to boolean "operational" status

public:
    Main() : stopping(false) {}

    /** Invoked when a connection is established to a broker
     */
    void brokerConnected(const Broker& broker)
    {
        Mutex::ScopedLock l(lock);
        brokerMap[const_cast<Broker*>(&broker)] = true;
    }

    /** Invoked when the connection to a broker is lost
     */
    void brokerDisconnected(const Broker& broker)
    {
        Mutex::ScopedLock l(lock);
        brokerMap[const_cast<Broker*>(&broker)] = false;
    }

    int run(int argc, char** argv)
    {
        //
        // Tune the settings for this application:  We will operate synchronously only, we don't
        // wish to use the bandwidth needed to aysnchronously receive objects or events.
        //
        SessionManager::Settings sessionSettings;
        sessionSettings.rcvObjects = false;
        sessionSettings.rcvEvents = false;
        sessionSettings.rcvHeartbeats = false;

        SessionManager sm(this, sessionSettings);

        //
        // Connect to the brokers.
        //
        for (int idx = 1; idx < argc; idx++) {
            qpid::client::ConnectionSettings connSettings;
            connSettings.host = "localhost";
            connSettings.port = atoi(argv[idx]);
            Broker* broker = sm.addBroker(connSettings);

            Mutex::ScopedLock l(lock);
            brokerMap[broker] = false; // initially assume broker is disconnected
        }

        //
        // Periodically poll the first connected broker.
        //
        while (!stopping) {
            //
            // Find an operational broker
            //
            Broker* operationalBroker = 0;
            {
                Mutex::ScopedLock l(lock);
                for (map<Broker*, bool>::iterator iter = brokerMap.begin();
                     iter != brokerMap.end(); iter++) {
                    if (iter->second) {
                        operationalBroker = iter->first;
                        break;
                    }
                }
            }

            if (operationalBroker != 0) {
                Object::Vector list;
                sm.getObjects(list, "queue", operationalBroker);
                for (Object::Vector::iterator i = list.begin(); i != list.end(); i++) {
                    cout << "queue: " << i->attrString("name");
                    cout << "  bindingCount=" << i->attrUint64("bindingCount") << endl;
                }
            } else {
                cout << "No operational brokers" << endl;
            }

            qpid::sys::sleep(10);
            if (stopping)
                break;
        }

        {
            //
            //  The following code structure uses the mutex to protect the broker map while
            //  ensuring that sm.delBroker is called without the mutex held (which leads to
            //  a deadlock).
            //
            Mutex::ScopedLock l(lock);
            map<Broker*, bool>::iterator iter = brokerMap.begin();
            while (iter != brokerMap.end()) {
                Broker* broker = iter->first;
                brokerMap.erase(iter);
                {
                    Mutex::ScopedUnlock ul(lock);
                    sm.delBroker(broker);
                }
                iter = brokerMap.begin();
            }
        }

        return 0;
    }

    void stop() {
        stopping = true;
    }
};

Main main_program;

void signal_handler(int)
{
    main_program.stop();
}

int main(int argc, char** argv)
{
    signal(SIGINT, signal_handler);
    try {
        return main_program.run(argc, argv);
    } catch(std::exception& e) {
        cout << "Top Level Exception: " << e.what() << endl;
    }
}

