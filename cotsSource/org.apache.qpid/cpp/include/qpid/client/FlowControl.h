#ifndef QPID_CLIENT_FLOWCONTROL_H
#define QPID_CLIENT_FLOWCONTROL_H

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

#include <qpid/sys/IntegerTypes.h>

namespace qpid {
namespace client {

/**
 * Flow control works by associating a finite amount of "credit"
 * with a subscription.
 *
 * Credit includes a message count and a byte count. Each message
 * received decreases the message count by one, and the byte count by
 * the size of the message. Either count can have the special value
 * UNLIMITED which is never decreased.
 *
 * A subscription's credit is exhausted when the message count is 0 or
 * the byte count is too small for the next available message. The
 * subscription will not receive any further messages until is credit
 * is renewed.
 *
 * In "window mode" credit is automatically renewed when a message is
 * completed (which by default happens when it is accepted). In
 * non-window mode credit is not automatically renewed, it must be
 * explicitly re-set (@see Subscription)
 */
struct FlowControl {
    static const uint32_t UNLIMITED=0xFFFFFFFF;
    FlowControl(uint32_t messages_=0, uint32_t bytes_=0, bool window_=false)
        : messages(messages_), bytes(bytes_), window(window_) {}

    static FlowControl messageCredit(uint32_t messages_) { return FlowControl(messages_,UNLIMITED,false); }
    static FlowControl messageWindow(uint32_t messages_) { return FlowControl(messages_,UNLIMITED,true); }
    static FlowControl byteCredit(uint32_t bytes_) { return FlowControl(UNLIMITED,bytes_,false); }
    static FlowControl byteWindow(uint32_t bytes_) { return FlowControl(UNLIMITED,bytes_,true); }
    static FlowControl unlimited() { return FlowControl(UNLIMITED, UNLIMITED, false); }
    static FlowControl zero() { return FlowControl(0, 0, false); }

    /** Message credit: subscription can accept up to this many messages. */
    uint32_t messages;
    /** Byte credit: subscription can accept up to this many bytes of message content. */
    uint32_t bytes;
    /** Window mode. If true credit is automatically renewed as messages are acknowledged. */
    bool window;

    bool operator==(const FlowControl& x) {
        return messages == x.messages && bytes == x.bytes && window == x.window;
    };
};

}} // namespace qpid::client

#endif  /*!QPID_CLIENT_FLOWCONTROL_H*/
