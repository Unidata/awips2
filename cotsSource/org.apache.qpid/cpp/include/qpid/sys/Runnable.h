#ifndef _Runnable_
#define _Runnable_
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

#include <boost/function.hpp>
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace sys {

/**
 * Interface for objects that can be run, e.g. in a thread.
 */
class Runnable
{
  public:
    /** Type to represent a runnable as a Functor */
    typedef boost::function0<void> Functor;

    QPID_COMMON_EXTERN virtual ~Runnable();

    /** Derived classes override run(). */
    virtual void run() = 0;

    /** Create a functor object that will call this->run(). */
    Functor functor();
};

}}


#endif
