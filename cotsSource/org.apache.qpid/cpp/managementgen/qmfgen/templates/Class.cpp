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
#include "qpid//*MGEN:Class.AgentHeaderLocation*//ManagementAgent.h"
#include "/*MGEN:Class.NameCap*/.h"
/*MGEN:Class.MethodArgIncludes*/

using namespace qmf::/*MGEN:Class.Namespace*/;
using namespace qpid::framing;
using           qpid::management::ManagementAgent;
using           qpid::management::Manageable;
using           qpid::management::ManagementObject;
using           qpid::management::Args;
using           std::string;

string  /*MGEN:Class.NameCap*/::packageName  = string ("/*MGEN:Class.NamePackageLower*/");
string  /*MGEN:Class.NameCap*/::className    = string ("/*MGEN:Class.NameLower*/");
uint8_t /*MGEN:Class.NameCap*/::md5Sum[16]   =
    {/*MGEN:Class.SchemaMD5*/};

/*MGEN:Class.NameCap*/::/*MGEN:Class.NameCap*/ (ManagementAgent*, Manageable* _core/*MGEN:Class.ParentArg*//*MGEN:Class.ConstructorArgs*/) :
    ManagementObject(_core)/*MGEN:Class.ConstructorInits*/
{
    /*MGEN:Class.ParentRefAssignment*/
/*MGEN:Class.InitializeElements*/
/*MGEN:IF(Class.ExistOptionals)*/
    // Optional properties start out not-present
    for (uint8_t idx = 0; idx < /*MGEN:Class.PresenceMaskBytes*/; idx++)
        presenceMask[idx] = 0;
/*MGEN:ENDIF*/
/*MGEN:IF(Class.ExistPerThreadStats)*/
    perThreadStatsArray = new struct PerThreadStats*[maxThreads];
    for (int idx = 0; idx < maxThreads; idx++)
        perThreadStatsArray[idx] = 0;
/*MGEN:ENDIF*/
}

/*MGEN:Class.NameCap*/::~/*MGEN:Class.NameCap*/ ()
{
/*MGEN:IF(Class.ExistPerThreadStats)*/
    for (int idx = 0; idx < maxThreads; idx++)
        if (perThreadStatsArray[idx] != 0)
            delete perThreadStatsArray[idx];
    delete[] perThreadStatsArray;
/*MGEN:ENDIF*/
}

namespace {
    const string NAME("name");
    const string TYPE("type");
    const string ACCESS("access");
    const string IS_INDEX("index");
    const string IS_OPTIONAL("optional");
    const string UNIT("unit");
    const string MIN("min");
    const string MAX("max");
    const string MAXLEN("maxlen");
    const string DESC("desc");
    const string ARGCOUNT("argCount");
    const string ARGS("args");
    const string DIR("dir");
    const string DEFAULT("default");
}

void /*MGEN:Class.NameCap*/::registerSelf(ManagementAgent* agent)
{
    agent->registerClass(packageName, className, md5Sum, writeSchema);
}

void /*MGEN:Class.NameCap*/::writeSchema (Buffer& buf)
{
    FieldTable ft;

    // Schema class header:
    buf.putOctet       (CLASS_KIND_TABLE);
    buf.putShortString (packageName); // Package Name
    buf.putShortString (className);   // Class Name
    buf.putBin128      (md5Sum);      // Schema Hash
    buf.putShort       (/*MGEN:Class.ConfigCount*/); // Config Element Count
    buf.putShort       (/*MGEN:Class.InstCount*/); // Inst Element Count
    buf.putShort       (/*MGEN:Class.MethodCount*/); // Method Count

    // Properties
/*MGEN:Class.PropertySchema*/
    // Statistics
/*MGEN:Class.StatisticSchema*/
    // Methods
/*MGEN:Class.MethodSchema*/
}

/*MGEN:IF(Class.ExistPerThreadStats)*/
void /*MGEN:Class.NameCap*/::aggregatePerThreadStats(struct PerThreadStats* totals)
{
/*MGEN:Class.InitializeTotalPerThreadStats*/
    for (int idx = 0; idx < maxThreads; idx++) {
        struct PerThreadStats* threadStats = perThreadStatsArray[idx];
        if (threadStats != 0) {
/*MGEN:Class.AggregatePerThreadStats*/
        }
    }
}
/*MGEN:ENDIF*/

void /*MGEN:Class.NameCap*/::writeProperties (Buffer& buf)
{
    ::qpid::sys::Mutex::ScopedLock mutex(accessLock);
    configChanged = false;

    writeTimestamps (buf);
/*MGEN:IF(Class.ExistOptionals)*/
    for (uint8_t idx = 0; idx < /*MGEN:Class.PresenceMaskBytes*/; idx++)
        buf.putOctet(presenceMask[idx]);
/*MGEN:ENDIF*/
/*MGEN:Class.WriteProperties*/
}

void /*MGEN:Class.NameCap*/::writeStatistics (Buffer& buf, bool skipHeaders)
{
    ::qpid::sys::Mutex::ScopedLock mutex(accessLock);
    instChanged = false;
/*MGEN:IF(Class.ExistPerThreadAssign)*/
    for (int idx = 0; idx < maxThreads; idx++) {
        struct PerThreadStats* threadStats = perThreadStatsArray[idx];
        if (threadStats != 0) {
/*MGEN:Class.PerThreadAssign*/
        }
    }
/*MGEN:ENDIF*/
/*MGEN:IF(Class.ExistPerThreadStats)*/
    struct PerThreadStats totals;
    aggregatePerThreadStats(&totals);
/*MGEN:ENDIF*/
/*MGEN:Class.Assign*/
    if (!skipHeaders)
        writeTimestamps (buf);
/*MGEN:Class.WriteStatistics*/

    // Maintenance of hi-lo statistics
/*MGEN:Class.HiLoStatResets*/
/*MGEN:IF(Class.ExistPerThreadResets)*/
    for (int idx = 0; idx < maxThreads; idx++) {
        struct PerThreadStats* threadStats = perThreadStatsArray[idx];
        if (threadStats != 0) {
/*MGEN:Class.PerThreadHiLoStatResets*/
        }
    }
/*MGEN:ENDIF*/
}

void /*MGEN:Class.NameCap*/::doMethod (/*MGEN:Class.DoMethodArgs*/)
{
    Manageable::status_t status = Manageable::STATUS_UNKNOWN_METHOD;
    std::string          text;

/*MGEN:Class.MethodHandlers*/
    outBuf.putLong(status);
    outBuf.putShortString(Manageable::StatusText(status, text));
}
