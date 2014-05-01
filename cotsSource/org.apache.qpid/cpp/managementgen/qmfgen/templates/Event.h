/*MGEN:commentPrefix=//*/
#ifndef _MANAGEMENT_/*MGEN:Event.NameUpper*/_
#define _MANAGEMENT_/*MGEN:Event.NameUpper*/_

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

#include "qpid/management/ManagementEvent.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/Uuid.h"

namespace qmf {
/*MGEN:Event.OpenNamespaces*/

class Event/*MGEN:Event.NameCap*/ : public ::qpid::management::ManagementEvent
{
  private:
    static void writeSchema (::qpid::framing::Buffer& buf);
    static std::string packageName;
    static std::string eventName;
    static uint8_t md5Sum[16];

/*MGEN:Event.ArgDeclarations*/

  public:
    writeSchemaCall_t getWriteSchemaCall(void) { return writeSchema; }

    Event/*MGEN:Event.NameCap*/(/*MGEN:Event.ConstructorArgs*/);
    ~Event/*MGEN:Event.NameCap*/() {};

    static void registerSelf(::qpid::management::ManagementAgent* agent);
    std::string& getPackageName() const { return packageName; }
    std::string& getEventName() const { return eventName; }
    uint8_t* getMd5Sum() const { return md5Sum; }
    uint8_t getSeverity() const { return /*MGEN:Event.Severity*/; }
    void encode(::qpid::framing::Buffer& buffer) const;
};

}/*MGEN:Event.CloseNamespaces*/

#endif  /*!_MANAGEMENT_/*MGEN:Event.NameUpper*/_*/
