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
#include "qpid/broker/SignalHandler.h"
#include "qpid/broker/Broker.h"
#include <signal.h>

namespace qpid {
namespace broker {

boost::intrusive_ptr<Broker> SignalHandler::broker;

void SignalHandler::setBroker(const boost::intrusive_ptr<Broker>& b) {
    broker = b;

    signal(SIGINT,shutdownHandler); 
    signal(SIGTERM, shutdownHandler);

    signal(SIGHUP,SIG_IGN); // TODO aconway 2007-07-18: reload config.

    signal(SIGCHLD,SIG_IGN); 
}

void SignalHandler::shutdown() { shutdownHandler(0); }

void SignalHandler::shutdownHandler(int) {
    if (broker.get()) {
        broker->shutdown();
        broker = 0;             // Release the broker reference.
    }
}

}} // namespace qpid::broker
