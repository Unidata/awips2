#ifndef _QmfEngineAgent_
#define _QmfEngineAgent_

/*
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
 */

#include <qmf/engine/Schema.h>
#include <qmf/engine/ObjectId.h>
#include <qmf/engine/Object.h>
#include <qmf/engine/Event.h>
#include <qmf/engine/Query.h>
#include <qmf/engine/Value.h>
#include <qmf/engine/Message.h>

namespace qmf {
namespace engine {

    /**
     * AgentEvent
     *
     *  This structure represents a QMF event coming from the agent to
     *  the application.
     */
    struct AgentEvent {
        enum EventKind {
            GET_QUERY      = 1,
            START_SYNC     = 2,
            END_SYNC       = 3,
            METHOD_CALL    = 4,
            DECLARE_QUEUE  = 5,
            DELETE_QUEUE   = 6,
            BIND           = 7,
            UNBIND         = 8,
            SETUP_COMPLETE = 9
        };

        EventKind    kind;
        uint32_t     sequence;    // Protocol sequence (for all kinds)
        char*        authUserId;  // Authenticated user ID (for all kinds)
        char*        authToken;   // Authentication token if issued (for all kinds)
        char*        name;        // Name of the method/sync query
                                  //    (METHOD_CALL, START_SYNC, END_SYNC, DECLARE_QUEUE, BIND, UNBIND)
        Object*      object;      // Object involved in method call (METHOD_CALL)
        ObjectId*    objectId;    // ObjectId for method call (METHOD_CALL)
        Query*       query;       // Query parameters (GET_QUERY, START_SYNC)
        Value*       arguments;   // Method parameters (METHOD_CALL)
        char*        exchange;    // Exchange for bind (BIND, UNBIND)
        char*        bindingKey;  // Key for bind (BIND, UNBIND)
        const SchemaObjectClass* objectClass; // (METHOD_CALL)
    };

    class AgentImpl;

    /**
     * Agent - Protocol engine for the QMF agent
     */
    class Agent {
    public:
        Agent(char* label, bool internalStore=true);
        ~Agent();

        /**
         * Configure the directory path for storing persistent data.
         *@param path Null-terminated string containing a directory path where files can be
         *            created, written, and read.  If NULL, no persistent storage will be
         *            attempted.
         */
        void setStoreDir(const char* path);

        /**
         * Configure the directory path for files transferred over QMF.
         *@param path Null-terminated string containing a directory path where files can be
         *            created, deleted, written, and read.  If NULL, file transfers shall not
         *            be permitted.
         */
        void setTransferDir(const char* path);

        /**
         * Pass messages received from the AMQP session to the Agent engine.
         *@param message AMQP messages received on the agent session.
         */
        void handleRcvMessage(Message& message);

        /**
         * Get the next message to be sent to the AMQP network.
         *@param item The Message structure describing the message to be produced.
         *@return true if the Message is valid, false if there are no messages to send.
         */
        bool getXmtMessage(Message& item) const;

        /**
         * Remove and discard one message from the head of the transmit queue.
         */
        void popXmt();

        /**
         * Get the next application event from the agent engine.
         *@param event The event iff the return value is true
         *@return true if event is valid, false if there are no events to process
         */
        bool getEvent(AgentEvent& event) const;

        /**
         * Remove and discard one event from the head of the event queue.
         */
        void popEvent();

        /**
         * A new AMQP session has been established for Agent communication.
         */
        void newSession();

        /**
         * Start the QMF Agent protocol.  This should be invoked after a SETUP_COMPLETE event
         * is received from the Agent engine.
         */
        void startProtocol();

        /**
         * This method is called periodically so the agent can supply a heartbeat.
         */
        void heartbeat();

        /**
         * Respond to a method request.
         *@param sequence  The sequence number from the method request event.
         *@param status    The method's completion status.
         *@param text      Status text ("OK" or an error message)
         *@param arguments The list of output arguments from the method call.
         */
        void methodResponse(uint32_t sequence, uint32_t status, char* text, const Value& arguments);

        /**
         * Send a content indication to the QMF bus.  This is only needed for objects that are
         * managed by the application.  This is *NOT* needed for objects managed by the Agent
         * (inserted using addObject).
         *@param sequence The sequence number of the GET request or the SYNC_START request.
         *@param object   The object (annotated with "changed" flags) for publication.
         *@param prop     If true, changed object properties are transmitted.
         *@param stat     If true, changed object statistics are transmitted.
         */
        void queryResponse(uint32_t sequence, Object& object, bool prop = true, bool stat = true);

        /**
         * Indicate the completion of a query.  This is not used for SYNC_START requests.
         *@param sequence The sequence number of the GET request.
         */
        void queryComplete(uint32_t sequence);

        /**
         * Register a schema class with the Agent.
         *@param cls A SchemaObejctClass object that defines data managed by the agent.
         */
        void registerClass(SchemaObjectClass* cls);

        /**
         * Register a schema class with the Agent.
         *@param cls A SchemaEventClass object that defines events sent by the agent.
         */
        void registerClass(SchemaEventClass* cls);

        /**
         * Give an object to the Agent for storage and management.  Once added, the agent takes
         * responsibility for the life cycle of the object.
         *@param obj       The object to be managed by the Agent.
         *@param persistId A unique non-zero value if the object-id is to be persistent.
         *@return The objectId of the managed object.
         */
        const ObjectId* addObject(Object& obj, uint64_t persistId);
        //        const ObjectId* addObject(Object& obj, uint32_t persistIdLo, uint32_t persistIdHi);

        /**
         * Allocate an object-id for an object that will be managed by the application.
         *@param persistId A unique non-zero value if the object-id is to be persistent.
         *@return The objectId structure for the allocated ID.
         */
        const ObjectId* allocObjectId(uint64_t persistId);
        const ObjectId* allocObjectId(uint32_t persistIdLo, uint32_t persistIdHi);

        /**
         * Raise an event into the QMF network..
         *@param event The event object for the event to be raised.
         */
        void raiseEvent(Event& event);

    private:
        AgentImpl* impl;
    };
}
}

#endif

