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
#include "qpid/CommonImportExport.h"

#include "qpid/management/Mutex.h"
#include "qpid/types/Variant.h"

#include <map>
#include <vector>

namespace qpid {
namespace management {

class Manageable;
class ObjectId;
class ManagementObject;


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
    uint64_t agentEpoch;
    std::string v2Key;
    std::string agentName;
    void fromString(const std::string&);
public:
    QPID_COMMON_EXTERN ObjectId() : agent(0), first(0), second(0), agentEpoch(0) {}
    QPID_COMMON_EXTERN ObjectId(const types::Variant& map) :
    agent(0), first(0), second(0), agentEpoch(0) { mapDecode(map.asMap()); }
    QPID_COMMON_EXTERN ObjectId(uint8_t flags, uint16_t seq, uint32_t broker);
    QPID_COMMON_EXTERN ObjectId(AgentAttachment* _agent, uint8_t flags, uint16_t seq);
    QPID_COMMON_EXTERN ObjectId(std::istream&);
    QPID_COMMON_EXTERN ObjectId(const std::string&);
    QPID_COMMON_EXTERN ObjectId(const std::string& agentAddress, const std::string& key,
                                uint64_t epoch=0) : agent(0), first(0), second(0),
      agentEpoch(epoch), v2Key(key), agentName(agentAddress) {}

    // Deprecated:
    QPID_COMMON_EXTERN ObjectId(uint8_t flags, uint16_t seq, uint32_t broker, uint64_t object);
    QPID_COMMON_EXTERN bool operator==(const ObjectId &other) const;
    QPID_COMMON_EXTERN bool operator<(const ObjectId &other) const;
    QPID_COMMON_EXTERN void mapEncode(types::Variant::Map& map) const;
    QPID_COMMON_EXTERN void mapDecode(const types::Variant::Map& map);
    QPID_COMMON_EXTERN operator types::Variant::Map() const;
    QPID_COMMON_EXTERN uint32_t encodedSize() const { return 16; };
    QPID_COMMON_EXTERN void encode(std::string& buffer) const;
    QPID_COMMON_EXTERN void decode(const std::string& buffer);
    QPID_COMMON_EXTERN bool equalV1(const ObjectId &other) const;
    QPID_COMMON_EXTERN void setV2Key(const std::string& _key) { v2Key = _key; }
    QPID_COMMON_EXTERN void setV2Key(const ManagementObject& object);
    QPID_COMMON_EXTERN void setAgentName(const std::string& _name) { agentName = _name; }
    QPID_COMMON_EXTERN const std::string& getAgentName() const { return agentName; }
    QPID_COMMON_EXTERN const std::string& getV2Key() const { return v2Key; }
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
    static const uint8_t TYPE_LIST      = 21;

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
    mutable bool     configChanged;
    mutable bool     instChanged;
    bool             deleted;
    Manageable*      coreObject;
    mutable Mutex    accessLock;
    uint32_t         flags;

    static int nextThreadIndex;
    bool             forcePublish;

    QPID_COMMON_EXTERN int  getThreadIndex();
    QPID_COMMON_EXTERN void writeTimestamps(std::string& buf) const;
    QPID_COMMON_EXTERN void readTimestamps(const std::string& buf);
    QPID_COMMON_EXTERN uint32_t writeTimestampsSize() const;

  public:
    QPID_COMMON_EXTERN static const uint8_t MD5_LEN = 16;
    QPID_COMMON_EXTERN static int maxThreads;
    //typedef void (*writeSchemaCall_t) (qpid::framing::Buffer&);
    typedef void (*writeSchemaCall_t) (std::string&);

    QPID_COMMON_EXTERN ManagementObject(Manageable* _core);
    virtual ~ManagementObject() {}

    virtual writeSchemaCall_t getWriteSchemaCall() = 0;
    virtual std::string getKey() const = 0;

    // Encode & Decode the property and statistics values
    // for this object.
    virtual void mapEncodeValues(types::Variant::Map& map,
                                 bool includeProperties,
                                 bool includeStatistics) = 0;
    virtual void mapDecodeValues(const types::Variant::Map& map) = 0;
    virtual void doMethod(std::string&           methodName,
                          const types::Variant::Map& inMap,
                          types::Variant::Map& outMap,
                          const std::string& userId) = 0;
    QPID_COMMON_EXTERN void writeTimestamps(types::Variant::Map& map) const;
    QPID_COMMON_EXTERN void readTimestamps(const types::Variant::Map& buf);

    /**
     * The following five methods are not pure-virtual because they will only
     * be overridden in cases where QMFv1 is to be supported.
     */
    virtual uint32_t writePropertiesSize() const { return 0; }
    virtual void readProperties(const std::string&) {}
    virtual void writeProperties(std::string&) const {}
    virtual void writeStatistics(std::string&, bool = false) {}
    virtual void doMethod(std::string&, const std::string&, std::string&, const std::string&) {}

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
    QPID_COMMON_EXTERN void setUpdateTime();
    QPID_COMMON_EXTERN void resourceDestroy();
    inline bool isDeleted() { return deleted; }
    inline void setFlags(uint32_t f) { flags = f; }
    inline uint32_t getFlags() { return flags; }
    bool isSameClass(ManagementObject& other) {
        for (int idx = 0; idx < MD5_LEN; idx++)
            if (other.getMd5Sum()[idx] != getMd5Sum()[idx])
                return false;
        return other.getClassName() == getClassName() &&
            other.getPackageName() == getPackageName();
    }

    // QPID_COMMON_EXTERN void encode(qpid::framing::Buffer& buf) const { writeProperties(buf); }
    // QPID_COMMON_EXTERN void decode(qpid::framing::Buffer& buf) { readProperties(buf); }
    //QPID_COMMON_EXTERN uint32_t encodedSize() const { return writePropertiesSize(); }

    // Encode/Decode the entire object as a map
    //QPID_COMMON_EXTERN void mapEncode(types::Variant::Map& map,
    //bool includeProperties=true,
    //bool includeStatistics=true);

    //QPID_COMMON_EXTERN void mapDecode(const types::Variant::Map& map);
};

typedef std::map<ObjectId, ManagementObject*> ManagementObjectMap;
typedef std::vector<ManagementObject*> ManagementObjectVector;

}}



#endif  /*!_ManagementObject_*/
