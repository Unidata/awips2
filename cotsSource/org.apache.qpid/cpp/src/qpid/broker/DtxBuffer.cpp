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
#include "qpid/broker/DtxBuffer.h"

using namespace qpid::broker;
using qpid::sys::Mutex;

DtxBuffer::DtxBuffer(const std::string& _xid) 
    : xid(_xid), ended(false), suspended(false), failed(false), expired(false) {}

DtxBuffer::~DtxBuffer() {}

void DtxBuffer::markEnded() 
{ 
    Mutex::ScopedLock locker(lock); 
    ended = true; 
}

bool DtxBuffer::isEnded() 
{ 
    Mutex::ScopedLock locker(lock); 
    return ended; 
}

void DtxBuffer::setSuspended(bool isSuspended) 
{ 
    suspended = isSuspended; 
}

bool DtxBuffer::isSuspended() 
{ 
    return suspended; 
}

void DtxBuffer::fail()
{
    Mutex::ScopedLock locker(lock); 
    rollback();
    failed = true;
    ended = true;
}

bool DtxBuffer::isRollbackOnly()
{
    Mutex::ScopedLock locker(lock); 
    return failed;
}

const std::string& DtxBuffer::getXid()
{
    return xid;
}

void DtxBuffer::timedout()
{
    Mutex::ScopedLock locker(lock); 
    expired = true;
    fail();
}

bool DtxBuffer::isExpired()
{
    Mutex::ScopedLock locker(lock); 
    return expired;
}
