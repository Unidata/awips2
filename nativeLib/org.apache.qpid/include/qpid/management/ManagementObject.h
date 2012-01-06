#ifndef _ManagementObject_
#define _ManagementObject_

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

#include "qpid/sys/Time.h"
#include "qpid/sys/Mutex.h"
#include <qpid/framing/Buffer.h>
#include "qpid/CommonImportExport.h"
#include <map>

namespace qpid {
namespace management {

class Manageable;
class ObjectId;


class AgentAttachment {
    friend class ObjectId;
private:
    uint64_t first;
public:
    AgentAttachment() : first(0) {}
    QPID_COMMON_EXTERN void setBanks(uint32_t broker, uint32_t bank);
    uint64_t getFirst() const { return first; }
};


class ObjectId {
protected:
    const AgentAttachment* agent;
    uint64_t first;
    uint64_t second;
    void fromString(const std::string&);
public:
    QPID_COMMON_EXTERN ObjectId() : agent(0), first(0), second(0) {}
    QPID_COMMON_EXTERN ObjectId(framing::Buffer& buf) : agent(0) { decode(buf); }
    QPID_COMMON_EXTERN ObjectId(uint8_t flags, uint16_t seq, uint32_t broker, uint32_t bank, uint64_t object);
    QPID_COMMON_EXTERN ObjectId(AgentAttachment* _agent, uint8_t flags, uint16_t seq, uint64_t object);
    QPID_COMMON_EXTERN ObjectId(std::istream&);
    QPID_COMMON_EXTERN ObjectId(const std::string&);
    QPID_COMMON_EXTERN bool operator==(const ObjectId &other) const;
    QPID_COMMON_EXTERN bool operator<(const ObjectId &other) const;
    QPID_COMMON_EXTERN void encode(framing::Buffer& buffer);
    QPID_COMMON_EXTERN void decode(framing::Buffer& buffer);
    friend QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream&, const ObjectId&);
};

class ManagementItem {
public:
    static const uint8_t TYPE_U8        = 1;
    static const uint8_t TYPE_U16       = 2;
    static const uint8_t TYPE_U32       = 3;
    static const uint8_t TYPE_U64       = 4;
    static const uint8_t TYPE_SSTR      = 6;
    static const uint8_t TYPE_LSTR      = 7;
    static const uint8_t TYPE_ABSTIME   = 8;
    static const uint8_t TYPE_DELTATIME = 9;
    static const uint8_t TYPE_REF       = 10;
    static const uint8_t TYPE_BOOL      = 11;
    static const uint8_t TYPE_FLOAT     = 12;
    static const uint8_t TYPE_DOUBLE    = 13;
    static const uint8_t TYPE_UUID      = 14;
    static const uint8_t TYPE_FTABLE    = 15;
    static const uint8_t TYPE_S8        = 16;
    static const uint8_t TYPE_S16       = 17;
    static const uint8_t TYPE_S32       = 18;
    static const uint8_t TYPE_S64       = 19;

    static const uint8_t ACCESS_RC = 1;
    static const uint8_t ACCESS_RW = 2;
    static const uint8_t ACCESS_RO = 3;

    static const uint8_t DIR_I     = 1;
    static const uint8_t DIR_O     = 2;
    static const uint8_t DIR_IO    = 3;

    static const uint8_t FLAG_CONFIG = 0x01;
    static const uint8_t FLAG_INDEX  = 0x02;
    static const uint8_t FLAG_END    = 0x80;

    const static uint8_t CLASS_KIND_TABLE = 1;
    const static uint8_t CLASS_KIND_EVENT = 2;



public:
    virtual ~ManagementItem() {}
};

class ManagementObject : public ManagementItem
{
protected:

    uint64_t         createTime;
    uint64_t         destroyTime;
    uint64_t         updateTime;
    ObjectId         objectId;
    bool             configChanged;
    bool             instChanged;
    bool             deleted;
    Manageable*      coreObject;
    sys::Mutex       accessLock;
    uint32_t         flags;

    static int nextThreadIndex;
    bool             forcePublish;

    QPID_COMMON_EXTERN int  getThreadIndex();
    QPID_COMMON_EXTERN void writeTimestamps(qpid::framing::Buffer& buf);

  public:
    QPID_COMMON_EXTERN static int maxThreads;
    typedef void (*writeSchemaCall_t) (qpid::framing::Buffer&);

    ManagementObject(Manageable* _core) :
        createTime(uint64_t(qpid::sys::Duration(qpid::sys::now()))),
        destroyTime(0), updateTime(createTime), configChanged(true),
        instChanged(true), deleted(false),
        coreObject(_core), forcePublish(false) {}
    virtual ~ManagementObject() {}

    virtual writeSchemaCall_t getWriteSchemaCall() = 0;
    virtual void writeProperties(qpid::framing::Buffer& buf) = 0;
    virtual void writeStatistics(qpid::framing::Buffer& buf,
                                 bool skipHeaders = false) = 0;
    virtual void doMethod(std::string&           methodName,
                          qpid::framing::Buffer& inBuf,
                          qpid::framing::Buffer& outBuf) = 0;
    QPID_COMMON_EXTERN virtual void setReference(ObjectId objectId);

    virtual std::string& getClassName() const = 0;
    virtual std::string& getPackageName() const = 0;
    virtual uint8_t*     getMd5Sum() const = 0;

    void         setObjectId(ObjectId oid) { objectId = oid; }
    ObjectId     getObjectId() { return objectId; }
    inline  bool getConfigChanged() { return configChanged; }
    virtual bool getInstChanged() { return instChanged; }
    virtual bool hasInst() { return true; }
    inline  void setForcePublish(bool f) { forcePublish = f; }
    inline  bool getForcePublish() { return forcePublish; }
    inline  void setUpdateTime() { updateTime = (uint64_t(sys::Duration(sys::now()))); }

    inline void resourceDestroy() {
        destroyTime = uint64_t (qpid::sys::Duration(qpid::sys::now()));
        deleted     = true;
    }
    inline bool isDeleted() { return deleted; }
    inline void setFlags(uint32_t f) { flags = f; }
    inline uint32_t getFlags() { return flags; }
    bool isSameClass(ManagementObject& other) {
        for (int idx = 0; idx < 16; idx++)
            if (other.getMd5Sum()[idx] != getMd5Sum()[idx])
                return false;
        return other.getClassName() == getClassName() &&
            other.getPackageName() == getPackageName();
    }
};

typedef std::map<ObjectId, ManagementObject*> ManagementObjectMap;

}}



#endif  /*!_ManagementObject_*/
