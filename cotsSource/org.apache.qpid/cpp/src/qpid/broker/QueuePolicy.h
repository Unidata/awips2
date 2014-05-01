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
#ifndef _QueuePolicy_
#define _QueuePolicy_

#include <deque>
#include <iostream>
#include <memory>
#include "qpid/broker/BrokerImportExport.h"
#include "qpid/broker/QueuedMessage.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/sys/AtomicValue.h"
#include "qpid/sys/Mutex.h"

namespace qpid {
namespace broker {

class QueuePolicy
{
    static uint64_t defaultMaxSize;

    uint32_t maxCount;
    uint64_t maxSize;
    const std::string type;
    uint32_t count;
    uint64_t size;
    bool policyExceeded;
            
    static int getInt(const qpid::framing::FieldTable& settings, const std::string& key, int defaultValue);

  public:
    typedef std::deque<QueuedMessage> Messages;
    static QPID_BROKER_EXTERN const std::string maxCountKey;
    static QPID_BROKER_EXTERN const std::string maxSizeKey;
    static QPID_BROKER_EXTERN const std::string typeKey;
    static QPID_BROKER_EXTERN const std::string REJECT;
    static QPID_BROKER_EXTERN const std::string FLOW_TO_DISK;
    static QPID_BROKER_EXTERN const std::string RING;
    static QPID_BROKER_EXTERN const std::string RING_STRICT;            

    virtual ~QueuePolicy() {}
    QPID_BROKER_EXTERN void tryEnqueue(boost::intrusive_ptr<Message> msg);
    QPID_BROKER_EXTERN void recoverEnqueued(boost::intrusive_ptr<Message> msg);
    QPID_BROKER_EXTERN void enqueueAborted(boost::intrusive_ptr<Message> msg);
    virtual void enqueued(const QueuedMessage&);
    virtual void dequeued(const QueuedMessage&);
    virtual bool isEnqueued(const QueuedMessage&);
    QPID_BROKER_EXTERN void update(qpid::framing::FieldTable& settings);
    uint32_t getMaxCount() const { return maxCount; }
    uint64_t getMaxSize() const { return maxSize; }           
    void encode(framing::Buffer& buffer) const;
    void decode ( framing::Buffer& buffer );
    uint32_t encodedSize() const;
    virtual void getPendingDequeues(Messages& result);

    static QPID_BROKER_EXTERN std::auto_ptr<QueuePolicy> createQueuePolicy(const std::string& name, const qpid::framing::FieldTable& settings);
    static QPID_BROKER_EXTERN std::auto_ptr<QueuePolicy> createQueuePolicy(const std::string& name, uint32_t maxCount, uint64_t maxSize, const std::string& type = REJECT);
    static QPID_BROKER_EXTERN std::auto_ptr<QueuePolicy> createQueuePolicy(const qpid::framing::FieldTable& settings);
    static QPID_BROKER_EXTERN std::auto_ptr<QueuePolicy> createQueuePolicy(uint32_t maxCount, uint64_t maxSize, const std::string& type = REJECT);
    static std::string getType(const qpid::framing::FieldTable& settings);
    static void setDefaultMaxSize(uint64_t);
    friend QPID_BROKER_EXTERN std::ostream& operator<<(std::ostream&,
                                                       const QueuePolicy&);
  protected:
    const std::string name;

    QueuePolicy(const std::string& name, uint32_t maxCount, uint64_t maxSize, const std::string& type = REJECT);

    virtual bool checkLimit(boost::intrusive_ptr<Message> msg);
    void enqueued(uint64_t size);
    void dequeued(uint64_t size);
};


class FlowToDiskPolicy : public QueuePolicy
{
  public:
    FlowToDiskPolicy(const std::string& name, uint32_t maxCount, uint64_t maxSize);
    bool checkLimit(boost::intrusive_ptr<Message> msg);
};

class RingQueuePolicy : public QueuePolicy
{
  public:
    RingQueuePolicy(const std::string& name, uint32_t maxCount, uint64_t maxSize, const std::string& type = RING);
    void enqueued(const QueuedMessage&);
    void dequeued(const QueuedMessage&);
    bool isEnqueued(const QueuedMessage&);
    bool checkLimit(boost::intrusive_ptr<Message> msg);
    void getPendingDequeues(Messages& result);
  private:
    Messages pendingDequeues;
    Messages queue;
    const bool strict;

    bool find(const QueuedMessage&, Messages&, bool remove);
};

}}


#endif
