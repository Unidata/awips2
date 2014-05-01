#ifndef QPID_CLIENT_SUBSCRIPTIONSETTINGS_H
#define QPID_CLIENT_SUBSCRIPTIONSETTINGS_H

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
#include "qpid/client/FlowControl.h"
#include "qpid/framing/enum.h"

namespace qpid {
namespace client {

/** Bring AMQP enum definitions for message class into this namespace. */
using namespace qpid::framing::message;

enum CompletionMode {
    MANUAL_COMPLETION = 0,
    COMPLETE_ON_DELIVERY = 1,
    COMPLETE_ON_ACCEPT = 2
};
/**
 * Settings for a subscription.
 */
struct SubscriptionSettings
{
    SubscriptionSettings(
        FlowControl flow=FlowControl::unlimited(),
        AcceptMode accept=ACCEPT_MODE_EXPLICIT,
        AcquireMode acquire=ACQUIRE_MODE_PRE_ACQUIRED,
        unsigned int autoAck_=1,
        CompletionMode completion=COMPLETE_ON_DELIVERY
    ) : flowControl(flow), acceptMode(accept), acquireMode(acquire), autoAck(autoAck_), completionMode(completion), exclusive(false) {}

    FlowControl flowControl;    ///@< Flow control settings. @see FlowControl
    /**
     * The acceptMode determines whether the broker should expect
     * delivery of messages to be acknowledged by the client
     * indicating that it accepts them. A value of
     * ACCEPT_MODE_EXPLICIT means that messages must be accepted
     * (note: this may be done automatically by the library - see
     * autoAck - or through an explicit call be the application - see
     * Subscription::accept()) before they can be dequeued. A value of
     * ACCEPT_MODE_NONE means that the broker can dequeue a message as
     * soon as it is acquired.
     */
    AcceptMode acceptMode;      ///@< ACCEPT_MODE_EXPLICIT or ACCEPT_MODE_NONE
    /**
     * The acquireMode determines whether messages are locked for the
     * subscriber when delivered, and thus are not delivered to any
     * other subscriber unless this subscriber releases them. 
     * 
     * The default is ACQUIRE_MODE_PRE_ACQUIRED meaning that the
     * subscriber expects to have been given that message exclusively
     * (i.e. the message will not be given to any other subscriber
     * unless released explicitly or by this subscribers session
     * failing without having accepted the message).
     * 
     * Delivery of message in ACQUIRE_MODE_NOT_ACQUIRED mode means the
     * message will still be available for other subscribers to
     * receive. The application can if desired acquire a (set of)
     * messages through an explicit acquire call - see
     * Subscription::acquire().
     */
    AcquireMode acquireMode;    ///@< ACQUIRE_MODE_PRE_ACQUIRED or ACQUIRE_MODE_NOT_ACQUIRED

    /**
     * Configures the frequency at which messages are automatically
     * accepted (e.g. a value of 5 means that messages are accepted in
     * batches of 5). A value of 0 means no automatic acknowledgement
     * will occur and the application will itself be responsible for
     * accepting messages.
     */
    unsigned int autoAck;
    /**
     * In windowing mode, completion of a message will cause the
     * credit used up by that message to be reallocated. The
     * subscriptions completion mode controls how completion is
     * managed.
     *
     * If set to COMPLETE_ON_DELIVERY (which is the default), messages
     * will be marked as completed once they have been received. The
     * server will be explicitly notified of all completed messages
     * for the session when the next accept is sent through the
     * subscription (either explictly or through autAck). However the
     * server may also periodically request information on the
     * completed messages.
     *
     * If set to COMPLETE_ON_ACCEPT, messages will be marked as
     * completed once they are accepted (via the Subscription class)
     * and the server will also be notified of all completed messages
     * for the session.
     *
     * If set to MANUAL_COMPLETION the application is responsible for
     * completing messages (@see Session::markCompleted()).
     */
    CompletionMode completionMode;
    /**
     * If set, requests that no other subscriber be allowed to access
     * the queue while this subscription is active.
     */
    bool exclusive;
};

}} // namespace qpid::client

#endif  /*!QPID_CLIENT_SUBSCRIPTIONSETTINGS_H*/
