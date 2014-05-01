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

import org.apache.qpid.server.logging.LogSubject;
import org.apache.commons.configuration.Configuration;

/**
 * MessageStore defines the interface to a storage area, which can be used to preserve the state of messages.
 *
 */
public interface MessageStore extends DurableConfigurationStore, TransactionLog
{
    StoreFuture IMMEDIATE_FUTURE = new StoreFuture()
        {
            public boolean isComplete()
            {
                return true;
            }

            public void waitForCompletion()
            {

            }
        };


    /**
     * Called after instantiation in order to configure the message store. A particular implementation can define
     * whatever parameters it wants.
     *
     * @param name             The name to be used by this storem
     * @param recoveryHandler  Handler to be called as the store recovers on start up
     * @param config           The apache commons configuration object.
     *
     * @throws Exception If any error occurs that means the store is unable to configure itself.
     */
    void configureMessageStore(String name,
                               MessageStoreRecoveryHandler recoveryHandler,
                               Configuration config,
                               LogSubject logSubject) throws Exception;

    /**
     * Called to close and cleanup any resources used by the message store.
     *
     * @throws Exception If the close fails.
     */
    void close() throws Exception;


    public <T extends StorableMessageMetaData> StoredMessage<T> addMessage(T metaData);


    /**
     * Is this store capable of persisting the data
     *
     * @return true if this store is capable of persisting data
     */
    boolean isPersistent();


}
