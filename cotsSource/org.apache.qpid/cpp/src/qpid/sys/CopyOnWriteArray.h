#ifndef QPID_SYS_COPYONWRITEARRAY_H
#define QPID_SYS_COPYONWRITEARRAY_H

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

#include "qpid/sys/Mutex.h"
#include <algorithm>
#include <vector>
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace sys {

/**
 * An array that copies on adding/removing element allowing lock-free
 * iteration.
 */
template <class T>
class CopyOnWriteArray
{
public:
    typedef boost::shared_ptr<const std::vector<T> > ConstPtr;

    CopyOnWriteArray() {}
    CopyOnWriteArray(const CopyOnWriteArray& c) : array(c.array) {}

    void add(T& t)
    {
        Mutex::ScopedLock l(lock);
        ArrayPtr copy(array ? new std::vector<T>(*array) : new std::vector<T>());
        copy->push_back(t);
        array = copy;
    }

    bool remove(T& t)
    {
        Mutex::ScopedLock l(lock);
        if (array && std::find(array->begin(), array->end(), t) != array->end()) {
            ArrayPtr copy(new std::vector<T>(*array));
            copy->erase(std::find(copy->begin(), copy->end(), t));
            array = copy;
            return true;
        } else {
            return false;
        }
    }

    bool clear()
    {
        Mutex::ScopedLock l(lock);
        if (array && !array->empty()) {
            ArrayPtr copy;
            array = copy;
            return true;
        } else {
            return false;
        }
    }

    template <class F>
    bool add_unless(T& t, F f)
    {
        Mutex::ScopedLock l(lock);
        if (array && std::find_if(array->begin(), array->end(), f) != array->end()) {
            return false;
        } else {
            ArrayPtr copy(array ? new std::vector<T>(*array) : new std::vector<T>());
            copy->push_back(t);
            array = copy;
            return true;
        }
    }

    template <class F>
    bool remove_if(F f)
    {
        Mutex::ScopedLock l(lock);
        if (array && std::find_if(array->begin(), array->end(), f) != array->end()) {
            ArrayPtr copy(new std::vector<T>(*array));
            copy->erase(std::remove_if(copy->begin(), copy->end(), f), copy->end());
            array = copy;
            return true;
        }
        return false;
    }

    template <class F>
    F for_each(F f)
    {
        ArrayPtr a;
        {
            Mutex::ScopedLock l(lock);
            a = array;
        }
        if (!a) return f;
        return std::for_each(a->begin(), a->end(), f);
    }

    ConstPtr snapshot()
    {
        ConstPtr a;
        {
            Mutex::ScopedLock l(lock);
            a = array;
        }
        return a;
    }

private:
    typedef boost::shared_ptr< std::vector<T> > ArrayPtr;
    Mutex lock;
    ArrayPtr array;
};

}}



#endif  /*!QPID_SYS_COPYONWRITEARRAY_H*/
