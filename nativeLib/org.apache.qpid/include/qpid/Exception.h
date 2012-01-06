#ifndef _Exception_
#define _Exception_

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

#include "qpid/framing/amqp_types.h"
#include "qpid/framing/constants.h"
#include "qpid/framing/enum.h"
#include "qpid/sys/StrError.h"
#include "qpid/Msg.h"
#include "qpid/CommonImportExport.h"
#include <memory>
#include <string>
#include <errno.h>

namespace qpid
{

/**
 * Base class for Qpid runtime exceptions.
 */
class Exception : public std::exception
{
  public:
    QPID_COMMON_EXTERN explicit Exception(const std::string& message=std::string()) throw();
    QPID_COMMON_EXTERN virtual ~Exception() throw();
    QPID_COMMON_EXTERN virtual const char* what() const throw(); // prefix: message
    QPID_COMMON_EXTERN virtual std::string getMessage() const; // Unprefixed message
    QPID_COMMON_EXTERN virtual std::string getPrefix() const;  // Prefix

  private:
    std::string message;
    mutable std::string whatStr;
};

/** Exception that includes an errno message. */
struct ErrnoException : public Exception {
    ErrnoException(const std::string& msg, int err) : Exception(msg+": "+qpid::sys::strError(err)) {}
    ErrnoException(const std::string& msg) : Exception(msg+": "+qpid::sys::strError(errno)) {}
};

struct SessionException : public Exception {
    const framing::execution::ErrorCode code;
    SessionException(framing::execution::ErrorCode code_, const std::string& message)
        : Exception(message), code(code_) {}
};

struct ChannelException : public Exception {
    const framing::session::DetachCode code;
    ChannelException(framing::session::DetachCode _code, const std::string& message)
        : Exception(message), code(_code) {}
};

struct ConnectionException : public Exception {
    const framing::connection::CloseCode code;
    ConnectionException(framing::connection::CloseCode _code, const std::string& message)
        : Exception(message), code(_code) {}
};

struct ClosedException : public Exception {
    QPID_COMMON_EXTERN ClosedException(const std::string& msg=std::string());
    QPID_COMMON_EXTERN std::string getPrefix() const;
};

/**
 * Exception representing transport failure
 */
struct TransportFailure : public Exception {
    TransportFailure(const std::string& msg=std::string()) : Exception(msg) {}
};

} // namespace qpid

#endif  /*!_Exception_*/
