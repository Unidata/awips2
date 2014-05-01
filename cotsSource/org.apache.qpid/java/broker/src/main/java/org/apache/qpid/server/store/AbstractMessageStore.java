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
package org.apache.qpid.server.store;

import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.configuration.VirtualHostConfiguration;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.messages.MessageStoreMessages;
import org.apache.qpid.server.logging.subjects.MessageStoreLogSubject;
import org.apache.qpid.server.logging.LogSubject;

public abstract class AbstractMessageStore implements MessageStore
{
    protected LogSubject _logSubject;

    public void configure(VirtualHost virtualHost) throws Exception
    {
        _logSubject = new MessageStoreLogSubject(virtualHost, this);
        CurrentActor.get().message(_logSubject, MessageStoreMessages.MST_CREATED(this.getClass().getName()));
    }

    public void close() throws Exception
    {
        CurrentActor.get().message(_logSubject,MessageStoreMessages.MST_CLOSED());
    }
}
