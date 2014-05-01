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
package org.apache.qpid.server.subscription;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.flow.FlowCreditManager;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.AMQChannel;

/**
 * Allows the customisation of the creation of a subscription. This is typically done within an AMQQueue. This factory
 * primarily assists testing although in future more sophisticated subscribers may need a different subscription
 * implementation.
 *
 * @see org.apache.qpid.server.queue.AMQQueue
 */
public interface SubscriptionFactory
{
    Subscription createSubscription(int channel,
                                    AMQProtocolSession protocolSession,
                                    AMQShortString consumerTag,
                                    boolean acks,
                                    FieldTable filters,
                                    boolean noLocal, FlowCreditManager creditManager) throws AMQException;


    Subscription createSubscription(AMQChannel channel,
                                            AMQProtocolSession protocolSession,
                                            AMQShortString consumerTag,
                                            boolean acks,
                                            FieldTable filters,
                                            boolean noLocal,
                                            FlowCreditManager creditManager,
                                            ClientDeliveryMethod clientMethod,
                                            RecordDeliveryMethod recordMethod
    )
            throws AMQException;
}
