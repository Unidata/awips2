#ifndef QPID_FRAMING_AMQP_CLIENTOPERATIONS_H
#define QPID_FRAMING_AMQP_CLIENTOPERATIONS_H
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

class AMQP_ClientOperations {
  public:
    class Invoker;              // Declared in ClientInvoker

    virtual ~AMQP_ClientOperations() {}

    virtual ProtocolVersion getVersion() const = 0;

    // Inner classes
    // ==================== class ConnectionHandler ====================
    class ConnectionHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ClientInvoker
          
        ConnectionHandler(){};
        virtual ~ConnectionHandler() {}
        // Protocol methods
    
    virtual void start(const FieldTable& serverProperties,
    const Array& mechanisms,
    const Array& locales) = 0;
    
    virtual void secure(const string& challenge) = 0;
    
    virtual void tune(uint16_t channelMax,
    uint16_t maxFrameSize,
    uint16_t heartbeatMin,
    uint16_t heartbeatMax) = 0;
    
    virtual void openOk(const Array& knownHosts) = 0;
    
    virtual void redirect(const string& host,
    const Array& knownHosts) = 0;
    
    virtual void heartbeat(    ) = 0;
    
    virtual void close(uint16_t replyCode,
    const string& replyText) = 0;
    
    virtual void closeOk(    ) = 0;
    }; // class ConnectionHandler
    
    
    // ==================== class SessionHandler ====================
    class SessionHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ClientInvoker
          
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
        class Invoker;              // Declared in ClientInvoker
          
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
        class Invoker;              // Declared in ClientInvoker
          
        MessageHandler(){};
        virtual ~MessageHandler() {}
        // Protocol methods
    
    virtual void accept(const SequenceSet& transfers) = 0;
    
    virtual void reject(const SequenceSet& transfers,
    uint16_t code,
    const string& text) = 0;
    
    virtual void release(const SequenceSet& transfers,
    bool setRedelivered) = 0;
    
    virtual MessageResumeResult resume(const string& destination,
    const string& resumeId) = 0;
    
    virtual void setFlowMode(const string& destination,
    uint8_t flowMode) = 0;
    
    virtual void flow(const string& destination,
    uint8_t unit,
    uint32_t value) = 0;
    
    virtual void stop(const string& destination) = 0;
    }; // class MessageHandler
    
    
    // ==================== class FileHandler ====================
    class FileHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ClientInvoker
          
        FileHandler(){};
        virtual ~FileHandler() {}
        // Protocol methods
    
    virtual void qosOk(    ) = 0;
    
    virtual void consumeOk(const string& consumerTag) = 0;
    
    virtual void open(const string& identifier,
    uint64_t contentSize) = 0;
    
    virtual void openOk(uint64_t stagedSize) = 0;
    
    virtual void deliver(const string& consumerTag,
    uint64_t deliveryTag,
    bool redelivered,
    const string& exchange,
    const string& routingKey,
    const string& identifier) = 0;
    }; // class FileHandler
    
    
    // ==================== class StreamHandler ====================
    class StreamHandler {
        // Constructors and destructors
      public:
        class Invoker;              // Declared in ClientInvoker
          
        StreamHandler(){};
        virtual ~StreamHandler() {}
        // Protocol methods
    
    virtual void qosOk(    ) = 0;
    
    virtual void consumeOk(const string& consumerTag) = 0;
    }; // class StreamHandler
    
    

    // Method handler get methods

    virtual ConnectionHandler* getConnectionHandler() = 0;
    virtual SessionHandler* getSessionHandler() = 0;
    virtual ExecutionHandler* getExecutionHandler() = 0;
    virtual MessageHandler* getMessageHandler() = 0;
    virtual FileHandler* getFileHandler() = 0;
    virtual StreamHandler* getStreamHandler() = 0;
}; /* class AMQP_ClientOperations */
}}
#endif  /*!QPID_FRAMING_AMQP_CLIENTOPERATIONS_H*/
