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
package org.apache.qpid.server.logging.subjects;

import org.apache.qpid.server.subscription.Subscription;

import java.text.MessageFormat;

public class SubscriptionLogSubject extends AbstractLogSubject
{

    /**
     * LOG FORMAT for the SubscriptionLogSubject,
     * Uses a MessageFormat call to insert the requried values according to
     * these indicies:
     *
     * 0 - Subscription ID
     */
    public static String SUBSCRIPTION_FORMAT = "sub:{0}";

    /**
     * Create an QueueLogSubject that Logs in the following format.
     *
     * @param subscription
     */
    public SubscriptionLogSubject(Subscription subscription)
    {
        // Delegate the formating of the Queue to the QueueLogSubject. So final
        // log string format is:
        // [ sub:<id>(vh(<vhost>)/qu(<queue>)) ]

        String queueString = new QueueLogSubject(subscription.getQueue()).toString();

        _logString = "[" + MessageFormat.format(SubscriptionLogSubject.SUBSCRIPTION_FORMAT,
                                                subscription.getSubscriptionID())
                     + "("
                     // queueString is [vh(/{0})/qu({1}) ] so need to trim
                     //                ^                ^^
                     + queueString.substring(1,queueString.length() - 3)
                     + ")"
                     + "] ";

    }
}
