#ifndef QPID_FRAMING_HANDLER_H
#define QPID_FRAMING_HANDLER_H

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
#include <boost/type_traits/remove_reference.hpp>
#include <assert.h>

namespace qpid {
namespace framing {

template <class T>
struct Handler {
    typedef T HandledType;
    typedef void handleFptr(T);
    typedef void result_type;   // Compatible with std/boost functors.

    Handler(Handler<T>* next_=0) : next(next_) {}
    virtual ~Handler() {}
    virtual void handle(T) = 0;
    
    /** Allow functor syntax for calling handle */ 
    void operator()(T t)  { handle(t); }


    /** Pointer to next handler in a linked list. */
    Handler<T>* next;

    /** Adapt any void(T) functor as a Handler.
     * Functor<F>(f) will copy f.
     * Functor<F&>(f) will only take a reference to x.
     */
    template <class F> class Functor : public Handler<T> {
      public:
        Functor(F f, Handler<T>* next=0) : Handler<T>(next), functor(f) {}
        void handle(T t) { functor(t); }
      private:
        F functor;
    };

    /** Adapt a member function of X as a Handler.
     * Only holds a reference to its target, not a copy.
     */
    template <class X, void (X::*F)(T)>
    class MemFunRef : public Handler<T> {
      public:
        MemFunRef(X& x, Handler<T>* next=0) : Handler(next), target(&x) {}
        void handle(T t) { (target->*F)(t); }

        /** Allow calling with -> syntax */
        MemFunRef* operator->() { return this; }

      private:
        X* target;
    };

    /** Interface for a handler that implements a
     * pair of in/out handle operations.
     * @see InOutHandler
     */
    class InOutHandlerInterface {
      public:
        virtual ~InOutHandlerInterface() {}
        virtual void handleIn(T) = 0;
        virtual void handleOut(T) = 0;
    };
        
    /** Support for implementing an in-out handler pair as a single class.
     * Overrides handleIn, handleOut functions in a single class.
     */
    struct InOutHandler : protected InOutHandlerInterface {
        InOutHandler(Handler<T>* nextIn=0, Handler<T>* nextOut=0) : in(*this, nextIn), out(*this, nextOut) {}
        MemFunRef<InOutHandlerInterface, &InOutHandlerInterface::handleIn> in;
        MemFunRef<InOutHandlerInterface, &InOutHandlerInterface::handleOut> out;
    };
};



}}
#endif  /*!QPID_FRAMING_HANDLER_H*/
//
