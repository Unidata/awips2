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

#include "qpid/client/Results.h"
#include "qpid/client/FutureResult.h"
#include "qpid/framing/SequenceSet.h"

using namespace qpid::framing;

namespace qpid {
namespace client {

Results::Results() {}

Results::~Results() {
  try { close(); } catch (const std::exception& /*e*/) { assert(0); }
}

void Results::close()
{
    for (Listeners::iterator i = listeners.begin(); i != listeners.end(); i++) {
        i->second->completed();
    }
    listeners.clear();
}

void Results::completed(const SequenceSet& set)
{   
    //call complete on those listeners whose ids fall within the set
    Listeners::iterator i = listeners.begin();
    while (i != listeners.end()) {
        if (set.contains(i->first)) {
            i->second->completed();
            listeners.erase(i++);
        } else {
            i++;
        }
    }
}

void Results::received(const SequenceNumber& id, const std::string& result)
{
    Listeners::iterator i = listeners.find(id);
    if (i != listeners.end()) {
        i->second->received(result);
        listeners.erase(i);
    }
}

Results::FutureResultPtr Results::listenForResult(const SequenceNumber& id)
{
    FutureResultPtr l(new FutureResult());
    listeners[id] = l;
    return l;
}

}}
