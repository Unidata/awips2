#ifndef _posix_ScopedIncrement_h
#define _posix_ScopedIncrement_h

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

#include <boost/noncopyable.hpp>
#include <boost/function.hpp>

namespace qpid {
namespace sys {

/**
 * Increment counter in constructor and decrement in destructor.
 * Optionally call a function if the decremented counter value is 0.
 * Note the function must not throw, it is called in the destructor.
 */
template <class T, class F=boost::function<void()> >
class ScopedIncrement : boost::noncopyable
{
  public:
    ScopedIncrement(T& c, F f=0)
        : count(c), callback(f) { ++count; }
    ~ScopedIncrement() { if (--count == 0 && callback) callback(); }

  private:
    T& count;
    F callback;
};


/** Decrement counter in constructor and increment in destructor. */
template <class T>
class ScopedDecrement : boost::noncopyable
{
  public:
    ScopedDecrement(T& c) : count(c) { value = --count; }
    ~ScopedDecrement() { ++count; }

    /** Return the value after the decrement. */
    operator long() { return value; }

  private:
    T& count;
    long value;
};


}}


#endif // _posix_ScopedIncrement_h
