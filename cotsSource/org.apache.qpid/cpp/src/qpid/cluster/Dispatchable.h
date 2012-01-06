#ifndef QPID_CLUSTER_DISPATCHABLE_H
#define QPID_CLUSTER_DISPATCHABLE_H

/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

namespace qpid {
namespace cluster {

/**
 * Interface for classes that have some "events" that need dispatching
 * in a thread.
 */
class Dispatchable 
{
  public:
    virtual ~Dispatchable() {}

    /** Dispatch one event in current thread. */
    virtual void dispatchOne() = 0;
    /** Dispatch all available events, don't block. */
    virtual void dispatchAll() = 0;
    /** Blocking loop to dispatch cluster events */
    virtual void dispatchBlocking() = 0;

    /** Wait for at least one event, then dispatch all available events.
     * Don't block.  Useful for tests.
     */
    virtual void dispatchSome() { dispatchOne(); dispatchAll(); }    

};

}} // namespace qpid::cluster



#endif  /*!QPID_CLUSTER_DISPATCHABLE_H*/
