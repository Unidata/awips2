#ifndef QPID_EXCEPTIONHOLDER_H
#define QPID_EXCEPTIONHOLDER_H

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


namespace qpid {
namespace sys {

struct Raisable {
    virtual ~Raisable() {};
    virtual void raise() const=0;
    virtual std::string what() const=0;
};

/**
 * Holder for exceptions. Allows the thread that notices an error condition to
 * create an exception and store it to be thrown by another thread.
 */
class ExceptionHolder : public Raisable {
  public:
    ExceptionHolder() {}
    // Use default copy & assign.
    
    /** Take ownership of ex */
    template <class Ex> ExceptionHolder(Ex* ex) { wrap(ex); }
    template <class Ex> ExceptionHolder(const boost::shared_ptr<Ex>& ex) { wrap(ex.release()); }

    template <class Ex> ExceptionHolder& operator=(Ex* ex) { wrap(ex); return *this; }
    template <class Ex> ExceptionHolder& operator=(boost::shared_ptr<Ex> ex) { wrap(ex.release()); return *this; }
        
    void raise() const { if (wrapper.get()) wrapper->raise() ; }
    std::string what() const { return wrapper.get() ? wrapper->what() : std::string(); }
    bool empty() const { return !wrapper.get(); }
    operator bool() const { return !empty(); }
    void reset() { wrapper.reset(); }

  private:
    template <class Ex> struct Wrapper : public Raisable {
        Wrapper(Ex* ptr) : exception(ptr) {}
        void raise() const { throw *exception; }
        std::string what() const { return exception->what(); }
        boost::shared_ptr<Ex> exception;
    };
    template <class Ex> void wrap(Ex* ex) { wrapper.reset(new Wrapper<Ex>(ex)); }
    boost::shared_ptr<Raisable> wrapper;
};
    

}} // namespace qpid::sys


#endif  /*!QPID_EXCEPTIONHOLDER_H*/
