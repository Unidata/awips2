//
// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
// 
//   http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//

#include "qpid/broker/System.h"
#include "qpid/broker/Broker.h"
#include "qpid/management/ManagementAgent.h"
#include "qpid/framing/Uuid.h"
#include "qpid/sys/SystemInfo.h"
#include <iostream>
#include <fstream>

using qpid::management::ManagementAgent;
using namespace qpid::broker;
using namespace std;
namespace _qmf = qmf::org::apache::qpid::broker;

System::System (string _dataDir, Broker* broker) : mgmtObject(0)
{
    ManagementAgent* agent = broker ? broker->getManagementAgent() : 0;

    if (agent != 0)
    {
        framing::Uuid systemId;

        if (_dataDir.empty ())
        {
            systemId.generate ();
        }
        else
        {
            string   filename (_dataDir + "/systemId");
            ifstream inFile (filename.c_str ());

            if (inFile.good ())
            {
                inFile >> systemId;
                inFile.close ();
            }
            else
            {
                systemId.generate ();
                ofstream outFile (filename.c_str ());
                if (outFile.good ())
                {
                    outFile << systemId << endl;
                    outFile.close ();
                }
            }
        }

        mgmtObject = new _qmf::System (agent, this, systemId);
        std::string sysname, nodename, release, version, machine;
        qpid::sys::SystemInfo::getSystemId (sysname,
                                            nodename,
                                            release,
                                            version,
                                            machine);
        mgmtObject->set_osName   (sysname);
        mgmtObject->set_nodeName (nodename);
        mgmtObject->set_release  (release);
        mgmtObject->set_version  (version);
        mgmtObject->set_machine  (machine);

        agent->addObject (mgmtObject, 0x1000000000000001LL);
    }
}

