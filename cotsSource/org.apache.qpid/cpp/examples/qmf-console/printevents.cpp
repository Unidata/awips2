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
#include <signal.h>

using namespace std;
using namespace qpid::console;

//
// Define a listener class to receive asynchronous events.
//
class Listener : public ConsoleListener {
public:
    void brokerConnected(const Broker& broker) {
        cout << qpid::sys::now() << " NOTIC qpid-printevents:brokerConnected broker=" <<
            broker.getUrl() << endl;
    }

    void brokerDisconnected(const Broker& broker) {
        cout << qpid::sys::now() << " NOTIC qpid-printevents:brokerDisconnected broker=" <<
            broker.getUrl() << endl;
    }

    void event(Event& event) {
        cout << event << endl;
    }
};


//==============================================================
// Main program
//==============================================================
struct Main {
    bool stopping;

    Main() : stopping(false) {}

    int run(int /*argc*/, char** /*argv*/)
    {
        //
        // Declare an instance of our listener.
        //
        Listener listener;

        //
        // Declare connection settings for the messaging broker.  The settings default to
        // localhost:5672 with user guest (password guest).  Refer to the header file
        // <qpid/client/ConnectionSettings.h> for full details.
        //
        qpid::client::ConnectionSettings connSettings;

        //
        // Declare the (optional) session manager settings.  Disable the reception of
        // object updates and heartbeats.  Note that by disabling the reception of things
        // we don't need, we don't unnecessarily use network bandwidth.
        //
        SessionManager::Settings smSettings;
        smSettings.rcvObjects = false;
        smSettings.rcvHeartbeats = false;

        //
        // Declare the console session manager.
        //
        SessionManager sm(&listener, smSettings);

        //
        // Add a broker connection to the session manager.  If desired, multiple brokers may
        // be connected.
        //
        Broker* broker = sm.addBroker(connSettings);

        //
        //  Sleep indefinitely while asynchronous events are handled by the listener.
        //
        while (!stopping)
            qpid::sys::sleep(1);

        sm.delBroker(broker);
        return 0;
    }

    void stop()
    {
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

