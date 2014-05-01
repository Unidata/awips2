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

#include "qpid/console/SequenceManager.h"

using namespace qpid::console;
using namespace qpid::sys;
using std::string;
using std::cout;
using std::endl;

uint32_t SequenceManager::reserve(const std::string& context)
{
    Mutex::ScopedLock l(lock);
    uint32_t result = sequence++;
    pending[result] = context;
    return result;
}

std::string SequenceManager::release(uint32_t seq)
{
    Mutex::ScopedLock l(lock);
    std::map<uint32_t, string>::iterator iter = pending.find(seq);
    if (iter == pending.end())
        return string();
    string result(iter->second);
    pending.erase(iter);
    return result;
}

