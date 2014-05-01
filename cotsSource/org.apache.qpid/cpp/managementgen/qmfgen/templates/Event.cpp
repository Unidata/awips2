/*MGEN:commentPrefix=//*/
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

/*MGEN:Root.Disclaimer*/

#include "qpid/log/Statement.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/management/Manageable.h" 
#include "qpid//*MGEN:Event.AgentHeaderLocation*//ManagementAgent.h"
#include "Event/*MGEN:Event.NameCap*/.h"

using namespace qmf::/*MGEN:Event.Namespace*/;
using namespace qpid::framing;
using           qpid::management::ManagementAgent;
using           qpid::management::Manageable;
using           qpid::management::ManagementObject;
using           qpid::management::Args;
using           std::string;

string  Event/*MGEN:Event.NameCap*/::packageName  = string ("/*MGEN:Event.NamePackageLower*/");
string  Event/*MGEN:Event.NameCap*/::eventName    = string ("/*MGEN:Event.Name*/");
uint8_t Event/*MGEN:Event.NameCap*/::md5Sum[16]   =
    {/*MGEN:Event.SchemaMD5*/};

Event/*MGEN:Event.NameCap*/::Event/*MGEN:Event.NameCap*/ (/*MGEN:Event.ConstructorArgs*/) :
    /*MGEN:Event.ConstructorInits*/
{}

namespace {
    const string NAME("name");
    const string TYPE("type");
    const string DESC("desc");
    const string ARGCOUNT("argCount");
    const string ARGS("args");
}

void Event/*MGEN:Event.NameCap*/::registerSelf(ManagementAgent* agent)
{
    agent->registerEvent(packageName, eventName, md5Sum, writeSchema);
}

void Event/*MGEN:Event.NameCap*/::writeSchema (Buffer& buf)
{
    FieldTable ft;

    // Schema class header:
    buf.putOctet       (CLASS_KIND_EVENT);
    buf.putShortString (packageName); // Package Name
    buf.putShortString (eventName);   // Event Name
    buf.putBin128      (md5Sum);      // Schema Hash
    buf.putOctet       (0);           // No Superclass    
    buf.putShort       (/*MGEN:Event.ArgCount*/); // Argument Count

    // Arguments
/*MGEN:Event.ArgSchema*/
}

void Event/*MGEN:Event.NameCap*/::encode(::qpid::framing::Buffer& buf) const
{
/*MGEN:Event.ArgEncodes*/
}
