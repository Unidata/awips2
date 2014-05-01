#ifndef _QmfAgentObject_
#define _QmfAgentObject_

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

#include "qmf/QmfImportExport.h"

namespace qmf {

    class AgentObjectImpl;
    class SchemaObjectClass;
    class ObjectId;
    class Value;
    class Agent;

    /**
     * AgentObject is an extension of Object with agent-specific methods added.
     *
     * \ingroup qmfapi
     */
    class AgentObject : public Object {
    public:
        /**
         * Create a new Object of a specific type.
         *
         * @param type Pointer to the schema class to use as a type for this object.
         */
        QMF_EXTERN AgentObject(const SchemaObjectClass* type);

        /**
         * Schedule this object for deletion.  Agent objects should never be directly
         * destroyed, rather this method should be called and all pointers to this
         * object dropped.  The agent will clean up and properly delete the object at
         * the appropraite time.
         */
        QMF_EXTERN void destroy();

        /**
         * Set the object ID for this object if it is to be managed by the agent.
         *
         * @param oid The new object ID for the managed object.
         */
        QMF_EXTERN void setObjectId(ObjectId& oid);

        /**
         * Handler for invoked method calls.  This will only be called for objects that
         * are being managed and stored by an agent (see internalStore argument in Agent::Agent).
         * If this function is not overridden in a child class, the default implementation will
         * cause AgentListener::methodCall to be invoked in the application program.
         *
         * If this function is overridden in a sub-class, the implementation must perform
         * the actions associated with the method call (i.e. implement the method).  Once the
         * method execution is complete, it must call Agent::methodResponse with the result
         * of the method execution.  Agent::methodResponse does not need to be called
         * synchronously in the context of this function call.  It may be called at a later
         * time from a different thread.
         *
         * @param context Context supplied by the agent and required to be passed in the
         *                call to Agent::methodResponse
         *
         * @param name The name of the method.
         *
         * @param args A Value (of type map) that contains the input and output arguments.
         *
         * @param userId The authenticated identity of the user who invoked the method.
         */
        QMF_EXTERN virtual void methodInvoked(uint32_t context, const char* name, Value& args,
                                              const char* userId);
    private:
        friend class Agent;
        virtual ~AgentObject();
        void setAgent(Agent* agent);
        AgentObjectImpl* impl;
    };

}

#endif 
