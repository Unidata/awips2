#ifndef QPID_MESSAGING_HANDLE_H
#define QPID_MESSAGING_HANDLE_H

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

#include "qpid/messaging/ImportExport.h"

namespace qpid {
namespace messaging {

template <class> class PrivateImplRef;

/** \ingroup messaging 
 * A handle is like a pointer: refers to an underlying implementation object.
 * Copying the handle does not copy the object.
 *
 * Handles can be null,  like a 0 pointer. Use isValid(), isNull() or the
 * conversion to bool to test for a null handle.
 */
template <class T> class Handle {
  public:

    /**@return true if handle is valid,  i.e. not null. */
    QPID_MESSAGING_EXTERN bool isValid() const { return impl; }

    /**@return true if handle is null. It is an error to call any function on a null handle. */
    QPID_MESSAGING_EXTERN bool isNull() const { return !impl; }

    /** Conversion to bool supports idiom if (handle) { handle->... } */
    QPID_MESSAGING_EXTERN operator bool() const { return impl; }

    /** Operator ! supports idiom if (!handle) { do_if_handle_is_null(); } */
    QPID_MESSAGING_EXTERN bool operator !() const { return !impl; }

    void swap(Handle<T>& h) { T* t = h.impl; h.impl = impl; impl = t; }

  protected:
    typedef T Impl;
    QPID_MESSAGING_EXTERN Handle() :impl() {}

    // Not implemented,subclasses must implement.
    QPID_MESSAGING_EXTERN Handle(const Handle&);
    QPID_MESSAGING_EXTERN Handle& operator=(const Handle&);

    Impl* impl;

  friend class PrivateImplRef<T>;
};

}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_HANDLE_H*/
