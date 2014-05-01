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
package org.apache.qpid.management.configuration;

import org.apache.qpid.management.domain.handler.base.IMessageHandler;

/**
 * Message Handler mapping used for associating an opcode with a message handler.
 */
class MessageHandlerMapping
{
    private final Character _opcode;
    private final IMessageHandler _handler;
        
    /**
     * Builds a new mapping with the given opcode and handler class.
     * 
     * @param opcode the opcode.
     * @param handlerClass the handler class.
     */
    MessageHandlerMapping(Character opcode, IMessageHandler handler) 
    {
        this._opcode = opcode;
        this._handler = handler;
    }
    
    /**
     * Returns the opcode of this mapping.
     * 
     * @return the code of this mapping.
     */
    Character getOpcode ()
    {
        return _opcode;
    }

    /**
     * Returns the message handler for this mapping.
     * 
     * @return the message handler for this mapping.
     */
    IMessageHandler getMessageHandler()
    {
        return _handler;
    }
}