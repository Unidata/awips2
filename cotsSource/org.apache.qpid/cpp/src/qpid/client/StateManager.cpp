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

#include "qpid/client/StateManager.h"
#include "qpid/framing/amqp_framing.h"

using namespace qpid::client;
using namespace qpid::framing;
using namespace qpid::sys;

StateManager::StateManager(int s) : state(s) {}

void StateManager::waitForStateChange(int current)
{
    Monitor::ScopedLock l(stateLock);
    while (state == current) {
        stateLock.wait();
    }
}

void StateManager::waitFor(int desired)
{
    Monitor::ScopedLock l(stateLock);
    while (state != desired) {
        stateLock.wait();
    }
}

void StateManager::waitFor(std::set<int> desired)
{
    Monitor::ScopedLock l(stateLock);
    while (desired.find(state) == desired.end()) {
        stateLock.wait();
    }
}


void StateManager::setState(int s)
{
    Monitor::ScopedLock l(stateLock);
    state = s;
    stateLock.notifyAll();
}

bool StateManager::setState(int s, int expected)
{
    Monitor::ScopedLock l(stateLock);
    if (state == expected) {
        state = s;
        stateLock.notifyAll();
        return true;
    } else {
        return false;
    }
}

int StateManager::getState() const
{
    Monitor::ScopedLock l(stateLock);
    return state;
}

