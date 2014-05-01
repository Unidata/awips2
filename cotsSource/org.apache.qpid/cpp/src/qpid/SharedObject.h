#ifndef _SharedObject_
#define _SharedObject_

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

#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>

namespace qpid {
    /**
     * Template to enforce shared object conventions.
     * Shared object classes should inherit : public qpid::SharedObject
     * That ensures Foo:
     * - has typedef boost::shared_ptr<T> shared_ptr
     * - has virtual destructor
     * - is boost::noncopyable (no default copy or assign)
     * - has a protected default constructor.
     *
     * Shared objects should not have public constructors.
     * Make constructors protected and provide public statc create()
     * functions that return a shared_ptr.
     */
    template <class T>
    class SharedObject : private boost::noncopyable
    {
      public:
        typedef boost::shared_ptr<T> shared_ptr;

        virtual ~SharedObject() {};

      protected:
        SharedObject() {} 
    };
}

#endif  /*!_SharedObject_*/
