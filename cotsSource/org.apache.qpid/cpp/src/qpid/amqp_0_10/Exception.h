#ifndef QPID_AMQP_0_10_EXCEPTION_H
#define QPID_AMQP_0_10_EXCEPTION_H

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

#include "qpid/Exception.h"
#include "qpid/amqp_0_10/specification_fwd.h"

namespace qpid {
namespace amqp_0_10 {

/** 
 * Raised when the connection is unexpectedly closed. Sessions with
 * non-0 timeout may be available for re-attachment on another connection.
 */
struct ConnectionException : public qpid::Exception {
    // FIXME aconway 2008-04-04: Merge qpid::ConnectionException
    // into this when the old code is removed.
    typedef connection::CloseCode Code;
    ConnectionException(Code c, const std::string m)
        : qpid::Exception(m), code(c) {}
    Code code;
};

/**
 * Raised when a session is unexpectedly detached for any reason, or
 * if an attempt is made to use a session that is not attached.
 */
struct SessionException : public qpid::Exception  {
    // FIXME aconway 2008-04-04: should not have a code at this level.
    // Leave in place till old preview code is gone.
    SessionException(int /*code*/, const std::string& msg) : qpid::Exception(msg) {} 
};

/** Raised when the state of a session has been destroyed */
struct SessionDestroyedException : public SessionException {
    // FIXME aconway 2008-04-04: should not have a code at this level.
    // Leave in place till old preview code is gone.
    SessionDestroyedException(int code, const std::string& msg) : SessionException(code, msg){} 
};

/** Raised when a session is destroyed due to an execution.exception */
struct SessionAbortedException : public SessionDestroyedException {
    typedef execution::ErrorCode Code;
    SessionAbortedException(Code c, const std::string m)
        : SessionDestroyedException(c, m), code(c) {}
    Code code;
};

/**
 * Raised when a session with 0 timeout is unexpectedly detached
 * and therefore expires and is destroyed.
 */
struct SessionExpiredException : public SessionDestroyedException {
    typedef session::DetachCode Code;
    SessionExpiredException(Code c, const std::string m)
        : SessionDestroyedException(c, m), code(c) {}
    Code code;
};

/**
 * Raised when a session with non-0 timeout is unexpectedly detached
 * or if an attempt is made to use a session that is not attached.
 * 
 * The session is not necessarily destroyed, it may be possible to
 * re-attach.
 */
struct SessionDetachedException : public SessionException {
    typedef session::DetachCode Code;
    SessionDetachedException(Code c, const std::string m)
        : SessionException(c, m), code(c) {}
    Code code;
};
    
}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_EXCEPTION_H*/
