#ifndef _broker_SessionAdapter_h
#define _broker_SessionAdapter_h

/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/broker/HandlerImpl.h"

#include "qpid/broker/ConnectionToken.h"
#include "qpid/broker/OwnershipToken.h"
#include "qpid/Exception.h"
#include "qpid/framing/AMQP_ServerOperations.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/StructHelper.h"

#include <algorithm>
#include <vector>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace broker {

class Channel;
class Connection;
class Broker;
class Queue;

/**
 * Per-channel protocol adapter.
 *
 * A container for a collection of AMQP-class adapters that translate
 * AMQP method bodies into calls on the core Broker objects. Each
 * adapter class also provides a client proxy to send methods to the
 * peer.
 * 
 */
 class SessionAdapter : public HandlerImpl, public framing::AMQP_ServerOperations
{
  public:
    SessionAdapter(SemanticState& session);

    framing::ProtocolVersion getVersion() const { return session.getConnection().getVersion();}

    MessageHandler* getMessageHandler(){ return &messageImpl;  }
    ExchangeHandler* getExchangeHandler(){ return &exchangeImpl; }
    QueueHandler* getQueueHandler(){ return &queueImpl;  }
    ExecutionHandler* getExecutionHandler(){ return &executionImpl; }
    TxHandler* getTxHandler(){ return &txImpl; }
    DtxHandler* getDtxHandler(){ return &dtxImpl; }

    ConnectionHandler* getConnectionHandler() { throw framing::NotImplementedException("Class not implemented"); }
    SessionHandler* getSessionHandler() { throw framing::NotImplementedException("Class not implemented"); }
    FileHandler* getFileHandler() { throw framing::NotImplementedException("Class not implemented"); }
    StreamHandler* getStreamHandler() { throw framing::NotImplementedException("Class not implemented"); }

    template <class F> void eachExclusiveQueue(F f) 
    { 
        queueImpl.eachExclusiveQueue(f);
    }


  private:
    //common base for utility methods etc that are specific to this adapter
    struct HandlerHelper : public HandlerImpl 
    {
        HandlerHelper(SemanticState& s) : HandlerImpl(s) {}

        Queue::shared_ptr getQueue(const string& name) const;
    };


    class ExchangeHandlerImpl :
        public ExchangeHandler,
        public HandlerHelper
    {
      public:
        ExchangeHandlerImpl(SemanticState& session) : HandlerHelper(session) {}
        
        void declare(const std::string& exchange, const std::string& type,
                     const std::string& alternateExchange, 
                     bool passive, bool durable, bool autoDelete, 
                     const qpid::framing::FieldTable& arguments); 
        void delete_(const std::string& exchange, bool ifUnused); 
        framing::ExchangeQueryResult query(const std::string& name);
        void bind(const std::string& queue, 
                  const std::string& exchange, const std::string& routingKey,
                  const qpid::framing::FieldTable& arguments); 
        void unbind(const std::string& queue,
                    const std::string& exchange,
                    const std::string& routingKey);
        framing::ExchangeBoundResult bound(const std::string& exchange,
                                           const std::string& queue,
                                           const std::string& routingKey,
                                           const framing::FieldTable& arguments);
      private:
        void checkType(boost::shared_ptr<Exchange> exchange, const std::string& type);

        void checkAlternate(boost::shared_ptr<Exchange> exchange,
                            boost::shared_ptr<Exchange> alternate);
    };

    class QueueHandlerImpl : public QueueHandler,
            public HandlerHelper
    {
        Broker& broker;
        std::vector< boost::shared_ptr<Queue> > exclusiveQueues;

      public:
        QueueHandlerImpl(SemanticState& session);
        ~QueueHandlerImpl();
        
        void declare(const std::string& queue,
                     const std::string& alternateExchange, 
                     bool passive, bool durable, bool exclusive, 
                     bool autoDelete,
                     const qpid::framing::FieldTable& arguments); 
        void delete_(const std::string& queue,
                     bool ifUnused, bool ifEmpty);
        void purge(const std::string& queue); 
        framing::QueueQueryResult query(const std::string& queue);
        bool isLocal(const ConnectionToken* t) const; 

        void destroyExclusiveQueues();
        template <class F> void eachExclusiveQueue(F f) 
        { 
            std::for_each(exclusiveQueues.begin(), exclusiveQueues.end(), f);
        }
    };

    class MessageHandlerImpl :
        public MessageHandler,
        public HandlerHelper
    {
        typedef boost::function<void(DeliveryId, DeliveryId)> RangedOperation;    
        RangedOperation releaseRedeliveredOp;
        RangedOperation releaseOp;
        RangedOperation rejectOp;
        RangedOperation acceptOp;

      public:
        MessageHandlerImpl(SemanticState& session);
        void transfer(const string& destination,
                      uint8_t acceptMode,
                      uint8_t acquireMode);
        
        void accept(const framing::SequenceSet& commands);
        
        void reject(const framing::SequenceSet& commands,
                    uint16_t code,
                    const string& text);
        
        void release(const framing::SequenceSet& commands,
                     bool setRedelivered);
        
        framing::MessageAcquireResult acquire(const framing::SequenceSet&);

        void subscribe(const string& queue,
                       const string& destination,
                       uint8_t acceptMode,
                       uint8_t acquireMode,
                       bool exclusive,
                       const string& resumeId,
                       uint64_t resumeTtl,
                       const framing::FieldTable& arguments);
        
        void cancel(const string& destination);
        
        void setFlowMode(const string& destination,
                         uint8_t flowMode);
        
        void flow(const string& destination,
                  uint8_t unit,
                  uint32_t value);
        
        void flush(const string& destination);
        
        void stop(const string& destination);

        framing::MessageResumeResult resume(const std::string& destination,
                                            const std::string& resumeId);
    
    };

    class ExecutionHandlerImpl : public ExecutionHandler, public HandlerHelper
    {
    public:
        ExecutionHandlerImpl(SemanticState& session) : HandlerHelper(session) {}

        void sync();            
        void result(const framing::SequenceNumber& commandId, const string& value);        
        void exception(uint16_t errorCode,
                       const framing::SequenceNumber& commandId,
                       uint8_t classCode,
                       uint8_t commandCode,
                       uint8_t fieldIndex,
                       const std::string& description,
                       const framing::FieldTable& errorInfo);

    };

    class TxHandlerImpl : public TxHandler, public HandlerHelper
    {
      public:
        TxHandlerImpl(SemanticState& session) : HandlerHelper(session) {}
        
        void select();
        void commit();
        void rollback();
    };

    class DtxHandlerImpl : public DtxHandler, public HandlerHelper, private framing::StructHelper
    {
        std::string convert(const framing::Xid& xid);

      public:
        DtxHandlerImpl(SemanticState& session) : HandlerHelper(session) {}

        void select();
            
        framing::XaResult start(const framing::Xid& xid,
                                         bool join,
                                         bool resume);
        
        framing::XaResult end(const framing::Xid& xid,
                                     bool fail,
                                     bool suspend);
        
        framing::XaResult commit(const framing::Xid& xid,
                                           bool onePhase);
        
        void forget(const framing::Xid& xid);
        
        framing::DtxGetTimeoutResult getTimeout(const framing::Xid& xid);
        
        framing::XaResult prepare(const framing::Xid& xid);
        
        framing::DtxRecoverResult recover();
        
        framing::XaResult rollback(const framing::Xid& xid);
        
        void setTimeout(const framing::Xid& xid, uint32_t timeout);        
    };

    ExchangeHandlerImpl exchangeImpl;
    QueueHandlerImpl queueImpl;
    MessageHandlerImpl messageImpl;
    ExecutionHandlerImpl executionImpl;
    TxHandlerImpl txImpl;
    DtxHandlerImpl dtxImpl;
};
}} // namespace qpid::broker



#endif  /*!_broker_SessionAdapter_h*/
