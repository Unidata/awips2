#ifndef QPID_FRAMING_AMQP_SERVEROPERATIONS_H
#define QPID_FRAMING_AMQP_SERVEROPERATIONS_H
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

///
/// This file was automatically generated from the AMQP specification.
/// Do not edit.
///


#include <sstream> 
#include "qpid/framing/ProtocolVersion.h"
#include "qpid/framing/amqp_structs.h"

namespace qpid {
namespace framing {

class AMQMethodBody;

class AMQP_ServerOperations {
  public:
    class Invoker;              // Declared in ServerInvoker

    virtual ~AMQP_ServerOperations() {}

    virtual ProtocolVersion getVersion() const = 0;

    // Inner classes
    // ==================== class ConnectionHandler ====================
    class ConnectionHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        ConnectionHandler(){};
        virtual ~ConnectionHandler() {}
        // Protocol methods
    
    virtual void startOk(const FieldTable& clientProperties,
    const string& mechanism,
    const string& response,
    const string& locale) = 0;
    
    virtual void secureOk(const string& response) = 0;
    
    virtual void tuneOk(uint16_t channelMax,
    uint16_t maxFrameSize,
    uint16_t heartbeat) = 0;
    
    virtual void open(const string& virtualHost,
    const Array& capabilities,
    bool insist) = 0;
    
    virtual void heartbeat(    ) = 0;
    
    virtual void close(uint16_t replyCode,
    const string& replyText) = 0;
    
    virtual void closeOk(    ) = 0;
    }; // class ConnectionHandler
    
    
    // ==================== class SessionHandler ====================
    class SessionHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        SessionHandler(){};
        virtual ~SessionHandler() {}
        // Protocol methods
    
    virtual void attach(const string& name,
    bool force) = 0;
    
    virtual void attached(const string& name) = 0;
    
    virtual void detach(const string& name) = 0;
    
    virtual void detached(const string& name,
    uint8_t code) = 0;
    
    virtual void requestTimeout(uint32_t timeout) = 0;
    
    virtual void timeout(uint32_t timeout) = 0;
    
    virtual void commandPoint(const SequenceNumber& commandId,
    uint64_t commandOffset) = 0;
    
    virtual void expected(const SequenceSet& commands,
    const Array& fragments) = 0;
    
    virtual void confirmed(const SequenceSet& commands,
    const Array& fragments) = 0;
    
    virtual void completed(const SequenceSet& commands,
    bool timelyReply) = 0;
    
    virtual void knownCompleted(const SequenceSet& commands) = 0;
    
    virtual void flush(bool expected,
    bool confirmed,
    bool completed) = 0;
    
    virtual void gap(const SequenceSet& commands) = 0;
    }; // class SessionHandler
    
    
    // ==================== class ExecutionHandler ====================
    class ExecutionHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        ExecutionHandler(){};
        virtual ~ExecutionHandler() {}
        // Protocol methods
    
    virtual void sync(    ) = 0;
    
    virtual void result(const SequenceNumber& commandId,
    const string& value) = 0;
    
    virtual void exception(uint16_t errorCode,
    const SequenceNumber& commandId,
    uint8_t classCode,
    uint8_t commandCode,
    uint8_t fieldIndex,
    const string& description,
    const FieldTable& errorInfo) = 0;
    }; // class ExecutionHandler
    
    
    // ==================== class MessageHandler ====================
    class MessageHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        MessageHandler(){};
        virtual ~MessageHandler() {}
        // Protocol methods
    
    virtual void accept(const SequenceSet& transfers) = 0;
    
    virtual void reject(const SequenceSet& transfers,
    uint16_t code,
    const string& text) = 0;
    
    virtual void release(const SequenceSet& transfers,
    bool setRedelivered) = 0;
    
    virtual MessageAcquireResult acquire(const SequenceSet& transfers) = 0;
    
    virtual MessageResumeResult resume(const string& destination,
    const string& resumeId) = 0;
    
    virtual void subscribe(const string& queue,
    const string& destination,
    uint8_t acceptMode,
    uint8_t acquireMode,
    bool exclusive,
    const string& resumeId,
    uint64_t resumeTtl,
    const FieldTable& arguments) = 0;
    
    virtual void cancel(const string& destination) = 0;
    
    virtual void setFlowMode(const string& destination,
    uint8_t flowMode) = 0;
    
    virtual void flow(const string& destination,
    uint8_t unit,
    uint32_t value) = 0;
    
    virtual void flush(const string& destination) = 0;
    
    virtual void stop(const string& destination) = 0;
    }; // class MessageHandler
    
    
    // ==================== class TxHandler ====================
    class TxHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        TxHandler(){};
        virtual ~TxHandler() {}
        // Protocol methods
    
    virtual void select(    ) = 0;
    
    virtual void commit(    ) = 0;
    
    virtual void rollback(    ) = 0;
    }; // class TxHandler
    
    
    // ==================== class DtxHandler ====================
    class DtxHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        DtxHandler(){};
        virtual ~DtxHandler() {}
        // Protocol methods
    
    virtual void select(    ) = 0;
    
    virtual XaResult start(const Xid& xid,
    bool join,
    bool resume) = 0;
    
    virtual XaResult end(const Xid& xid,
    bool fail,
    bool suspend) = 0;
    
    virtual XaResult commit(const Xid& xid,
    bool onePhase) = 0;
    
    virtual void forget(const Xid& xid) = 0;
    
    virtual DtxGetTimeoutResult getTimeout(const Xid& xid) = 0;
    
    virtual XaResult prepare(const Xid& xid) = 0;
    
    virtual DtxRecoverResult recover(    ) = 0;
    
    virtual XaResult rollback(const Xid& xid) = 0;
    
    virtual void setTimeout(const Xid& xid,
    uint32_t timeout) = 0;
    }; // class DtxHandler
    
    
    // ==================== class ExchangeHandler ====================
    class ExchangeHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        ExchangeHandler(){};
        virtual ~ExchangeHandler() {}
        // Protocol methods
    
    virtual void declare(const string& exchange,
    const string& type,
    const string& alternateExchange,
    bool passive,
    bool durable,
    bool autoDelete,
    const FieldTable& arguments) = 0;
    
    virtual void delete_(const string& exchange,
    bool ifUnused) = 0;
    
    virtual ExchangeQueryResult query(const string& name) = 0;
    
    virtual void bind(const string& queue,
    const string& exchange,
    const string& bindingKey,
    const FieldTable& arguments) = 0;
    
    virtual void unbind(const string& queue,
    const string& exchange,
    const string& bindingKey) = 0;
    
    virtual ExchangeBoundResult bound(const string& exchange,
    const string& queue,
    const string& bindingKey,
    const FieldTable& arguments) = 0;
    }; // class ExchangeHandler
    
    
    // ==================== class QueueHandler ====================
    class QueueHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        QueueHandler(){};
        virtual ~QueueHandler() {}
        // Protocol methods
    
    virtual void declare(const string& queue,
    const string& alternateExchange,
    bool passive,
    bool durable,
    bool exclusive,
    bool autoDelete,
    const FieldTable& arguments) = 0;
    
    virtual void delete_(const string& queue,
    bool ifUnused,
    bool ifEmpty) = 0;
    
    virtual void purge(const string& queue) = 0;
    
    virtual QueueQueryResult query(const string& queue) = 0;
    }; // class QueueHandler
    
    
    // ==================== class FileHandler ====================
    class FileHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        FileHandler(){};
        virtual ~FileHandler() {}
        // Protocol methods
    
    virtual void qos(uint32_t prefetchSize,
    uint16_t prefetchCount,
    bool global) = 0;
    
    virtual void consume(const string& queue,
    const string& consumerTag,
    bool noLocal,
    bool noAck,
    bool exclusive,
    bool nowait,
    const FieldTable& arguments) = 0;
    
    virtual void cancel(const string& consumerTag) = 0;
    
    virtual void open(const string& identifier,
    uint64_t contentSize) = 0;
    
    virtual void openOk(uint64_t stagedSize) = 0;
    
    virtual void publish(const string& exchange,
    const string& routingKey,
    bool mandatory,
    bool immediate,
    const string& identifier) = 0;
    
    virtual void ack(uint64_t deliveryTag,
    bool multiple) = 0;
    
    virtual void reject(uint64_t deliveryTag,
    bool requeue) = 0;
    }; // class FileHandler
    
    
    // ==================== class StreamHandler ====================
    class StreamHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ServerInvoker
          
        StreamHandler(){};
        virtual ~StreamHandler() {}
        // Protocol methods
    
    virtual void qos(uint32_t prefetchSize,
    uint16_t prefetchCount,
    uint32_t consumeRate,
    bool global) = 0;
    
    virtual void consume(const string& queue,
    const string& consumerTag,
    bool noLocal,
    bool exclusive,
    bool nowait,
    const FieldTable& arguments) = 0;
    
    virtual void cancel(const string& consumerTag) = 0;
    }; // class StreamHandler
    
    

    // Method handler get methods

    virtual ConnectionHandler* getConnectionHandler() = 0;
    virtual SessionHandler* getSessionHandler() = 0;
    virtual ExecutionHandler* getExecutionHandler() = 0;
    virtual MessageHandler* getMessageHandler() = 0;
    virtual TxHandler* getTxHandler() = 0;
    virtual DtxHandler* getDtxHandler() = 0;
    virtual ExchangeHandler* getExchangeHandler() = 0;
    virtual QueueHandler* getQueueHandler() = 0;
    virtual FileHandler* getFileHandler() = 0;
    virtual StreamHandler* getStreamHandler() = 0;
}; /* class AMQP_ServerOperations */
}}
#endif  /*!QPID_FRAMING_AMQP_SERVEROPERATIONS_H*/
