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

#include "qpid/client/Demux.h"
#include "qpid/Exception.h"
#include "qpid/framing/MessageTransferBody.h"

#include <iostream>

namespace qpid {
namespace client {

ByTransferDest::ByTransferDest(const std::string& d) : dest(d) {}
bool ByTransferDest::operator()(const framing::FrameSet& frameset) const
{
    return frameset.isA<framing::MessageTransferBody>() &&
        frameset.as<framing::MessageTransferBody>()->getDestination() == dest;
}

ScopedDivert::ScopedDivert(const std::string& _dest, Demux& _demuxer) : dest(_dest), demuxer(_demuxer) 
{
    queue = demuxer.add(dest, ByTransferDest(dest));
}

ScopedDivert::~ScopedDivert() 
{ 
    demuxer.remove(dest); 
}

Demux::Demux() : defaultQueue(new Queue()) {}

Demux::~Demux() { close(sys::ExceptionHolder(new ClosedException())); }

Demux::QueuePtr ScopedDivert::getQueue()
{
    return queue;
}

void Demux::handle(framing::FrameSet::shared_ptr frameset)
{
    sys::Mutex::ScopedLock l(lock);
    bool matched = false;
    for (iterator i = records.begin(); i != records.end() && !matched; i++) {
        if (i->condition && i->condition(*frameset)) {
            matched = true;
            i->queue->push(frameset);
        }
    }
    if (!matched) {
        defaultQueue->push(frameset);
    }
}

void Demux::close(const sys::ExceptionHolder& ex)
{
    sys::Mutex::ScopedLock l(lock);
    for (iterator i = records.begin(); i != records.end(); i++) {
        i->queue->close(ex);
    }
    defaultQueue->close(ex);
}

void Demux::open()
{
    sys::Mutex::ScopedLock l(lock);
    for (iterator i = records.begin(); i != records.end(); i++) {
        i->queue->open();
    }
    defaultQueue->open();
}

Demux::QueuePtr Demux::add(const std::string& name, Condition condition)
{
    sys::Mutex::ScopedLock l(lock);
    iterator i = std::find_if(records.begin(), records.end(), Find(name));
    if (i == records.end()) {        
        Record r(name, condition);
        records.push_back(r);
        return r.queue;
    } else {
        throw Exception("Queue already exists for " + name);
    }
}

void Demux::remove(const std::string& name)
{
    sys::Mutex::ScopedLock l(lock);
    records.remove_if(Find(name));
}

Demux::QueuePtr Demux::get(const std::string& name)
{
    sys::Mutex::ScopedLock l(lock);
    iterator i = std::find_if(records.begin(), records.end(), Find(name));
    if (i == records.end()) {
        throw Exception("No queue for " + name);
    }
    return i->queue;
}

Demux::QueuePtr Demux::getDefault()
{
    return defaultQueue;
}

Demux::Find::Find(const std::string& n) : name(n) {}

bool Demux::Find::operator()(const Record& record) const
{
    return record.name == name;
}

}}

