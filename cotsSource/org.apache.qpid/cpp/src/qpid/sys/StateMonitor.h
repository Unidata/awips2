#ifndef QPID_SYS_STATEMONITOR_H
#define QPID_SYS_STATEMONITOR_H

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
#include "qpid/sys/Waitable.h"

#include <bitset>

namespace qpid {
namespace sys {

/**
 * A monitor with an enum state value.
 *
 *@param Enum: enum type to use for states.
 *@param EnumMax: Highest enum value.
 */
template <class Enum, size_t MaxEnum>
class StateMonitor : public Waitable
{
  public:
    struct Set : public std::bitset<MaxEnum + 1> {
        Set() {}
        Set(Enum s) { set(s); }
        Set(Enum s, Enum t) { set(s).set(t); }
        Set(Enum s, Enum t, Enum u) { set(s).set(t).set(u); }
        Set(Enum s, Enum t, Enum u, Enum v) { set(s).set(t).set(u).set(v); }
    };


    StateMonitor(Enum initial) { state=initial; }

    /** @pre Caller holds a ScopedLock. */
    void set(Enum s) { state=s; notifyAll(); }
    /** @pre Caller holds a ScopedLock. */
    StateMonitor& operator=(Enum s) { set(s); return *this; }

    /** @pre Caller holds a ScopedLock. */
    Enum get() const { return state; }
    /** @pre Caller holds a ScopedLock. */
    operator Enum() const { return state; }

    /** @pre Caller holds a ScopedLock */
    void waitFor(Enum s) { ScopedWait(*this); while (s != state) wait(); }
    /** @pre Caller holds a ScopedLock */
    void waitFor(Set s) { ScopedWait(*this); while (!s.test(state)) wait(); }
    /** @pre Caller holds a ScopedLock */
    void waitNot(Enum s) { ScopedWait(*this); while (s == state) wait(); }
    /** @pre Caller holds a ScopedLock */
    void waitNot(Set s) { ScopedWait(*this); while (s.test(state)) wait(); }
    
  private:
    Enum state;
};

}}


#endif  /*!QPID_SYS_STATEMONITOR_H*/
