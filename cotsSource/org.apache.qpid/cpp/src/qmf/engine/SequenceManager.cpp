/*
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
 */

#include "qmf/engine/SequenceManager.h"

using namespace std;
using namespace qmf::engine;
using namespace qpid::sys;

SequenceManager::SequenceManager() : nextSequence(1) {}

void SequenceManager::setUnsolicitedContext(SequenceContext::Ptr ctx)
{
    unsolicitedContext = ctx;
}

uint32_t SequenceManager::reserve(SequenceContext::Ptr ctx)
{
    Mutex::ScopedLock _lock(lock);
    if (ctx.get() == 0)
        ctx = unsolicitedContext;
    uint32_t seq = nextSequence;
    while (contextMap.find(seq) != contextMap.end())
        seq = seq < 0xFFFFFFFF ? seq + 1 : 1;
    nextSequence = seq < 0xFFFFFFFF ? seq + 1 : 1;
    contextMap[seq] = ctx;
    ctx->reserve();
    return seq;
}

void SequenceManager::release(uint32_t sequence)
{
    Mutex::ScopedLock _lock(lock);

    if (sequence == 0) {
        if (unsolicitedContext.get() != 0)
            unsolicitedContext->release();
        return;
    }

    map<uint32_t, SequenceContext::Ptr>::iterator iter = contextMap.find(sequence);
    if (iter != contextMap.end()) {
        if (iter->second != 0)
            iter->second->release();
        contextMap.erase(iter);
    }
}

void SequenceManager::releaseAll()
{
    Mutex::ScopedLock _lock(lock);
    contextMap.clear();
}

void SequenceManager::dispatch(uint8_t opcode, uint32_t sequence, const string& routingKey, qpid::framing::Buffer& buffer)
{
    Mutex::ScopedLock _lock(lock);
    bool done;

    if (sequence == 0) {
        if (unsolicitedContext.get() != 0) {
            done = unsolicitedContext->handleMessage(opcode, sequence, routingKey, buffer);
            if (done)
                unsolicitedContext->release();
        }
        return;
    }

    map<uint32_t, SequenceContext::Ptr>::iterator iter = contextMap.find(sequence);
    if (iter != contextMap.end()) {
        if (iter->second != 0) {
            done = iter->second->handleMessage(opcode, sequence, routingKey, buffer);
            if (done) {
                iter->second->release();
                contextMap.erase(iter);
            }
        }
    }
}

