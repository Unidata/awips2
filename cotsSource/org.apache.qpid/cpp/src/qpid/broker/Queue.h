#ifndef _broker_Queue_h
#define _broker_Queue_h

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

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/broker/OwnershipToken.h"
#include "qpid/broker/Consumer.h"
#include "qpid/broker/Message.h"
#include "qpid/broker/PersistableQueue.h"
#include "qpid/broker/QueuePolicy.h"
#include "qpid/broker/QueueBindings.h"
#include "qpid/broker/QueueListeners.h"
#include "qpid/broker/RateTracker.h"

#include "qpid/framing/FieldTable.h"
#include "qpid/sys/Monitor.h"
#include "qpid/management/Manageable.h"
#include "qmf/org/apache/qpid/broker/Queue.h"
#include "qpid/framing/amqp_types.h"

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/intrusive_ptr.hpp>

#include <list>
#include <vector>
#include <memory>
#include <deque>
#include <algorithm>

namespace qpid {
    namespace broker {
        class Broker;
        class MessageStore;
        class QueueEvents;
        class QueueRegistry;
        class TransactionContext;
        class Exchange;

        using std::string;

        /**
         * The brokers representation of an amqp queue. Messages are
         * delivered to a queue from where they can be dispatched to
         * registered consumers or be stored until dequeued or until one
         * or more consumers registers.
         */
        class Queue : public boost::enable_shared_from_this<Queue>,
            public PersistableQueue, public management::Manageable {

            typedef std::deque<QueuedMessage> Messages;
            typedef std::map<string,boost::intrusive_ptr<Message> > LVQ;
            enum ConsumeCode {NO_MESSAGES=0, CANT_CONSUME=1, CONSUMED=2};

            const string name;
            const bool autodelete;
            MessageStore* store;
            const OwnershipToken* owner;
            uint32_t consumerCount;
            OwnershipToken* exclusive;
            bool noLocal;
            bool lastValueQueue;
            bool lastValueQueueNoBrowse;
            bool persistLastNode;
            bool inLastNodeFailure;
            std::string traceId;
            std::vector<std::string> traceExclude;
            QueueListeners listeners;
            Messages messages;
            Messages pendingDequeues;//used to avoid dequeuing during recovery
            LVQ lvq;
            mutable qpid::sys::Mutex consumerLock;
            mutable qpid::sys::Mutex messageLock;
            mutable qpid::sys::Mutex ownershipLock;
            mutable uint64_t persistenceId;
            framing::FieldTable settings;
            std::auto_ptr<QueuePolicy> policy;
            bool policyExceeded;
            QueueBindings bindings;
            std::string alternateExchangeName;
            boost::shared_ptr<Exchange> alternateExchange;
            framing::SequenceNumber sequence;
            qmf::org::apache::qpid::broker::Queue* mgmtObject;
            RateTracker dequeueTracker;
            int eventMode;
            QueueEvents* eventMgr;
            bool insertSeqNo;
            std::string seqNoKey;
            Broker* broker;

            void push(boost::intrusive_ptr<Message>& msg, bool isRecovery=false);
            void setPolicy(std::auto_ptr<QueuePolicy> policy);
            bool seek(QueuedMessage& msg, Consumer::shared_ptr position);
            bool getNextMessage(QueuedMessage& msg, Consumer::shared_ptr c);
            ConsumeCode consumeNextMessage(QueuedMessage& msg, Consumer::shared_ptr c);
            bool browseNextMessage(QueuedMessage& msg, Consumer::shared_ptr c);
            void notifyListener();

            void removeListener(Consumer::shared_ptr);

            bool isExcluded(boost::intrusive_ptr<Message>& msg);

            void dequeued(const QueuedMessage& msg);
            void popAndDequeue();
            QueuedMessage getFront();
            QueuedMessage& checkLvqReplace(QueuedMessage& msg);
            void clearLVQIndex(const QueuedMessage& msg);

            inline void mgntEnqStats(const boost::intrusive_ptr<Message>& msg)
            {
                if (mgmtObject != 0) {
                    mgmtObject->inc_msgTotalEnqueues ();
                    mgmtObject->inc_byteTotalEnqueues (msg->contentSize ());
                    if (msg->isPersistent ()) {
                        mgmtObject->inc_msgPersistEnqueues ();
                        mgmtObject->inc_bytePersistEnqueues (msg->contentSize ());
                    }
                }
            }
            inline void mgntDeqStats(const boost::intrusive_ptr<Message>& msg)
            {
                if (mgmtObject != 0){
                    mgmtObject->inc_msgTotalDequeues  ();
                    mgmtObject->inc_byteTotalDequeues (msg->contentSize());
                    if (msg->isPersistent ()){
                        mgmtObject->inc_msgPersistDequeues ();
                        mgmtObject->inc_bytePersistDequeues (msg->contentSize());
                    }
                }
            }
            
            Messages::iterator findAt(framing::SequenceNumber pos);

        public:

            virtual void notifyDurableIOComplete();
            typedef boost::shared_ptr<Queue> shared_ptr;

            typedef std::vector<shared_ptr> vector;

            QPID_BROKER_EXTERN Queue(const string& name,
                                     bool autodelete = false, 
                                     MessageStore* const store = 0, 
                                     const OwnershipToken* const owner = 0,
                                     management::Manageable* parent = 0,
                                     Broker* broker = 0);
            QPID_BROKER_EXTERN ~Queue();

            QPID_BROKER_EXTERN bool dispatch(Consumer::shared_ptr);
            /**
             * Check whether there would be a message available for
             * dispatch to this consumer. If not, the consumer will be
             * notified of events that may have changed this
             * situation.
             */
            bool checkForMessages(Consumer::shared_ptr);

            void create(const qpid::framing::FieldTable& settings);

            // "recovering" means we are doing a MessageStore recovery.
            QPID_BROKER_EXTERN void configure(const qpid::framing::FieldTable& settings,
                                              bool recovering = false);
            void destroy();
            QPID_BROKER_EXTERN void bound(const string& exchange,
                                          const string& key,
                                          const qpid::framing::FieldTable& args);
            QPID_BROKER_EXTERN void unbind(ExchangeRegistry& exchanges,
                                           Queue::shared_ptr shared_ref);

            QPID_BROKER_EXTERN bool acquire(const QueuedMessage& msg);
            QPID_BROKER_EXTERN bool acquireMessageAt(const qpid::framing::SequenceNumber& position, QueuedMessage& message);

            /**
             * Delivers a message to the queue. Will record it as
             * enqueued if persistent then process it.
             */
            QPID_BROKER_EXTERN void deliver(boost::intrusive_ptr<Message>& msg);
            /**
             * Dispatches the messages immediately to a consumer if
             * one is available or stores it for later if not.
             */
            QPID_BROKER_EXTERN void process(boost::intrusive_ptr<Message>& msg);
            /**
             * Returns a message to the in-memory queue (due to lack
             * of acknowledegement from a receiver). If a consumer is
             * available it will be dispatched immediately, else it
             * will be returned to the front of the queue.
             */
            QPID_BROKER_EXTERN void requeue(const QueuedMessage& msg);
            /**
             * Used during recovery to add stored messages back to the queue
             */
            QPID_BROKER_EXTERN void recover(boost::intrusive_ptr<Message>& msg);

            QPID_BROKER_EXTERN void consume(Consumer::shared_ptr c,
                                            bool exclusive = false);
            QPID_BROKER_EXTERN void cancel(Consumer::shared_ptr c);

            uint32_t purge(const uint32_t purge_request = 0); //defaults to all messages 
            QPID_BROKER_EXTERN void purgeExpired();

            //move qty # of messages to destination Queue destq
            uint32_t move(const Queue::shared_ptr destq, uint32_t qty); 

            QPID_BROKER_EXTERN uint32_t getMessageCount() const;
            QPID_BROKER_EXTERN uint32_t getEnqueueCompleteMessageCount() const;
            QPID_BROKER_EXTERN uint32_t getConsumerCount() const;
            inline const string& getName() const { return name; }
            bool isExclusiveOwner(const OwnershipToken* const o) const;
            void releaseExclusiveOwnership();
            bool setExclusiveOwner(const OwnershipToken* const o);
            bool hasExclusiveConsumer() const;
            bool hasExclusiveOwner() const;
            inline bool isDurable() const { return store != 0; }
            inline const framing::FieldTable& getSettings() const { return settings; }
            inline bool isAutoDelete() const { return autodelete; }
            bool canAutoDelete() const;
            const QueueBindings& getBindings() const { return bindings; }

            /**
             * used to take messages from in memory and flush down to disk.
             */
            QPID_BROKER_EXTERN void setLastNodeFailure();
            QPID_BROKER_EXTERN void clearLastNodeFailure();

            bool enqueue(TransactionContext* ctxt, boost::intrusive_ptr<Message> msg, bool suppressPolicyCheck = false);
            void enqueueAborted(boost::intrusive_ptr<Message> msg);
            /**
             * dequeue from store (only done once messages is acknowledged)
             */
            QPID_BROKER_EXTERN bool dequeue(TransactionContext* ctxt, const QueuedMessage &msg);
            /**
             * Inform the queue that a previous transactional dequeue
             * committed.
             */
            void dequeueCommitted(const QueuedMessage& msg);

            /**
             * Inform queue of messages that were enqueued, have since
             * been acquired but not yet accepted or released (and
             * thus are still logically on the queue) - used in
             * clustered broker.  
             */ 
            void enqueued(const QueuedMessage& msg);

            /**
             * Test whether the specified message (identified by its
             * sequence/position), is still enqueued (note this
             * doesn't mean it is available for delivery as it may
             * have been delievered to a subscriber who has not yet
             * accepted it).
             */
            bool isEnqueued(const QueuedMessage& msg);
            
            /**
             * Gets the next available message 
             */
            QPID_BROKER_EXTERN QueuedMessage get();

            /** Get the message at position pos */
            QPID_BROKER_EXTERN QueuedMessage find(framing::SequenceNumber pos) const;

            const QueuePolicy* getPolicy();

            void setAlternateExchange(boost::shared_ptr<Exchange> exchange);
            boost::shared_ptr<Exchange> getAlternateExchange();
            bool isLocal(boost::intrusive_ptr<Message>& msg);

            //PersistableQueue support:
            uint64_t getPersistenceId() const;
            void setPersistenceId(uint64_t persistenceId) const;
            void encode(framing::Buffer& buffer) const;
            uint32_t encodedSize() const;

            // "recovering" means we are doing a MessageStore recovery.
            static Queue::shared_ptr decode(QueueRegistry& queues, framing::Buffer& buffer, bool recovering = false );
            static void tryAutoDelete(Broker& broker, Queue::shared_ptr);

            virtual void setExternalQueueStore(ExternalQueueStore* inst);

            // Manageable entry points
            management::ManagementObject* GetManagementObject (void) const;
            management::Manageable::status_t
                ManagementMethod (uint32_t methodId, management::Args& args, std::string& text);

            /** Apply f to each Message on the queue. */
            template <class F> void eachMessage(F f) {
                sys::Mutex::ScopedLock l(messageLock);
                if (lastValueQueue) {
                    for (Messages::iterator i = messages.begin(); i != messages.end(); ++i) {
                        f(checkLvqReplace(*i));
                    }
                } else {
                    std::for_each(messages.begin(), messages.end(), f);
                }
            }

            /** Apply f to each QueueBinding on the queue */
            template <class F> void eachBinding(F f) {
                bindings.eachBinding(f);
            }

            void popMsg(QueuedMessage& qmsg);

            /** Set the position sequence number  for the next message on the queue.
             * Must be >= the current sequence number.
             * Used by cluster to replicate queues.
             */
            QPID_BROKER_EXTERN void setPosition(framing::SequenceNumber pos);
            /** return current position sequence number for the next message on the queue.
            */
            QPID_BROKER_EXTERN framing::SequenceNumber getPosition();
            int getEventMode();
            void setQueueEventManager(QueueEvents&);
            QPID_BROKER_EXTERN void insertSequenceNumbers(const std::string& key);
            /**
             * Notify queue that recovery has completed.
             */
            void recoveryComplete(ExchangeRegistry& exchanges);

            // For cluster update
            QueueListeners& getListeners();

            /**
             * Reserve space in policy for an enqueued message that
             * has been recovered in the prepared state (dtx only)
             */
            void recoverPrepared(boost::intrusive_ptr<Message>& msg);
        };
    }
}


#endif  /*!_broker_Queue_h*/
