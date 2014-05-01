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

#include "qpid/console/SessionManager.h"
#include "qpid/sys/Time.h"

using namespace std;
using namespace qpid::console;

//==============================================================
// Main program
//==============================================================
int main_int(int /*argc*/, char** /*argv*/)
{
    //
    // Declare connection settings for the messaging broker.  The settings default to
    // localhost:5672 with user guest (password guest).  Refer to the header file
    // <qpid/client/ConnectionSettings.h> for full details.
    //
    qpid::client::ConnectionSettings connSettings;

    //
    // Declare the (optional) session manager settings.  Override the default timeout
    // for methods calls to be 2 seconds.
    //
    SessionManager::Settings smSettings;
    smSettings.methodTimeout = 2;
    smSettings.getTimeout = 2;

    //
    // Declare the console session manager.  With a null listener argument, it defaults to
    // synchronous-only access mode.
    //
    SessionManager sm(0, smSettings);

    //
    // Add a broker connection to the session manager.
    //
    Broker* broker = sm.addBroker(connSettings);

    uint32_t count = 5;  // The number of echo requests we will send to the broker.
    Object::Vector list; // A container for holding objects retrieved from the broker.

    for (uint32_t iter = 0; iter < count; iter++) {
        cout << "Ping Broker: " << broker->getUrl() << "... ";
        cout.flush();

        //
        // Query for a list of 'broker' objects from the Management Database
        //
        sm.getObjects(list, "broker");

        //
        // We expect one object (since we are connected to only one broker)
        //
        if (list.size() == 1) {
            Object& brokerObject = *(list.begin());

            //
            // Declare a container for arguments to be sent with the "echo" method
            // that we will invoke on the remote "broker" object.
            //
            Object::AttributeMap args;

            //
            // Declare a container for the result of the method invocation.
            //
            MethodResponse result;

            //
            // Set the values of the input arguments.
            //
            args.addUint("sequence", iter);
            args.addString("body", "ABCDEFGHIJKLMNOPQRSTUVWXYZ");

            //
            // Invoke the method.  This is a synchronous operation that will block until
            // the method completes and returns a result.
            //
            brokerObject.invokeMethod("echo", args, result);

            //
            // result.code is the return code (0 => Success)
            // result.text is the return text
            // result.arguments is a container (of type Object::AttributeMap) that holds
            //     the output arguments (if any) from the method.
            //
            cout << "Result: code=" << result.code << " text=" << result.text;
            if (result.code == 0)
                cout << " seq=" << result.arguments["sequence"]->asUint();
            cout << endl;

            if (result.code == 0 && iter < count - 1)
              qpid::sys::sleep(1);
        } else {
            cout << "Disconnected..." << endl;
            qpid::sys::sleep(1);
        }
    }

    //
    // Disconnect the broker from the session manager.
    //
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

