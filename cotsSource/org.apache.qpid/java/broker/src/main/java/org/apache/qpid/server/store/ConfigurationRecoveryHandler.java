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

import java.nio.ByteBuffer;
import org.apache.qpid.framing.FieldTable;

public interface ConfigurationRecoveryHandler
{
    QueueRecoveryHandler begin(MessageStore store);

    public static interface QueueRecoveryHandler
    {
        void queue(String queueName, String owner, FieldTable arguments);
        ExchangeRecoveryHandler completeQueueRecovery();
    }

    public static interface ExchangeRecoveryHandler
    {
        void exchange(String exchangeName, String type, boolean autoDelete);
        BindingRecoveryHandler completeExchangeRecovery();
    }

    public static interface BindingRecoveryHandler
    {
        void binding(String exchangeName, String queueName, String bindingKey, ByteBuffer buf);
        void completeBindingRecovery();
    }

    public static interface QueueEntryRecoveryHandler
    {
        void complete();

        void queueEntry(String queueName, long messageId);
    }



}
