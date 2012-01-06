#ifndef QPID_FRAMING_REPLY_EXCEPTIONS_H
#define QPID_FRAMING_REPLY_EXCEPTIONS_H
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

///
/// This file was automatically generated from the AMQP specification.
/// Do not edit.
///


#include "qpid/Exception.h"
#include "qpid/sys/ExceptionHolder.h"
#include "qpid/framing/enum.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {


/**
 * 
 *             The client attempted to work with a server entity to which it has no access due to
 *             security settings.
 *           
 */
struct UnauthorizedAccessException:
    SessionException
{
    std::string getPrefix() const { return "unauthorized-access"; }
    UnauthorizedAccessException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_UNAUTHORIZED_ACCESS, ""+msg) {}
};

/**
 * 
 *             The client attempted to work with a server entity that does not exist.
 *           
 */
struct NotFoundException:
    SessionException
{
    std::string getPrefix() const { return "not-found"; }
    NotFoundException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_NOT_FOUND, ""+msg) {}
};

/**
 * 
 *             The client attempted to work with a server entity to which it has no access because
 *             another client is working with it.
 *           
 */
struct ResourceLockedException:
    SessionException
{
    std::string getPrefix() const { return "resource-locked"; }
    ResourceLockedException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_RESOURCE_LOCKED, ""+msg) {}
};

/**
 * 
 *             The client requested a command that was not allowed because some precondition failed.
 *           
 */
struct PreconditionFailedException:
    SessionException
{
    std::string getPrefix() const { return "precondition-failed"; }
    PreconditionFailedException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_PRECONDITION_FAILED, ""+msg) {}
};

/**
 * 
 *             A server entity the client is working with has been deleted.
 *           
 */
struct ResourceDeletedException:
    SessionException
{
    std::string getPrefix() const { return "resource-deleted"; }
    ResourceDeletedException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_RESOURCE_DELETED, ""+msg) {}
};

/**
 * 
 *             The peer sent a command that is not permitted in the current state of the session.
 *           
 */
struct IllegalStateException:
    SessionException
{
    std::string getPrefix() const { return "illegal-state"; }
    IllegalStateException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_ILLEGAL_STATE, ""+msg) {}
};

/**
 * 
 *             The command segments could not be decoded.
 *           
 */
struct CommandInvalidException:
    SessionException
{
    std::string getPrefix() const { return "command-invalid"; }
    CommandInvalidException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_COMMAND_INVALID, ""+msg) {}
};

/**
 * 
 *             The client exceeded its resource allocation.
 *           
 */
struct ResourceLimitExceededException:
    SessionException
{
    std::string getPrefix() const { return "resource-limit-exceeded"; }
    ResourceLimitExceededException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_RESOURCE_LIMIT_EXCEEDED, ""+msg) {}
};

/**
 * 
 *             The peer tried to use a command a manner that is inconsistent with the rules described
 *             in the specification.
 *           
 */
struct NotAllowedException:
    SessionException
{
    std::string getPrefix() const { return "not-allowed"; }
    NotAllowedException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_NOT_ALLOWED, ""+msg) {}
};

/**
 * 
 *             The command argument is malformed, i.e. it does not fall within the specified domain.
 *             The illegal-argument exception can be raised on execution of any command which has
 *             domain valued fields.
 *           
 */
struct IllegalArgumentException:
    SessionException
{
    std::string getPrefix() const { return "illegal-argument"; }
    IllegalArgumentException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_ILLEGAL_ARGUMENT, ""+msg) {}
};

/**
 * 
 *             The peer tried to use functionality that is not implemented in its partner.
 *           
 */
struct NotImplementedException:
    SessionException
{
    std::string getPrefix() const { return "not-implemented"; }
    NotImplementedException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_NOT_IMPLEMENTED, ""+msg) {}
};

/**
 * 
 *             The peer could not complete the command because of an internal error. The peer may
 *             require intervention by an operator in order to resume normal operations.
 *           
 */
struct InternalErrorException:
    SessionException
{
    std::string getPrefix() const { return "internal-error"; }
    InternalErrorException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_INTERNAL_ERROR, ""+msg) {}
};

/**
 * 
 *             An invalid argument was passed to a command, and the operation could not
 *             proceed.  An invalid argument is not illegal (see illegal-argument), i.e. it matches
 *             the domain definition; however the particular value is invalid in this context.
 *           
 */
struct InvalidArgumentException:
    SessionException
{
    std::string getPrefix() const { return "invalid-argument"; }
    InvalidArgumentException(const std::string& msg=std::string()) : SessionException(execution::ERROR_CODE_INVALID_ARGUMENT, ""+msg) {}
};

QPID_COMMON_EXTERN sys::ExceptionHolder createSessionException(int code, const std::string& text);

/**
 * 
 *             An operator intervened to close the connection for some reason. The client may retry at
 *             some later date.
 *           
 */
struct ConnectionForcedException:
    ConnectionException
{
    std::string getPrefix() const { return "connection-forced"; }
    ConnectionForcedException(const std::string& msg=std::string()) : ConnectionException(connection::CLOSE_CODE_CONNECTION_FORCED, ""+msg) {}
};

/**
 * 
 *             The client tried to work with an unknown virtual host.
 *           
 */
struct InvalidPathException:
    ConnectionException
{
    std::string getPrefix() const { return "invalid-path"; }
    InvalidPathException(const std::string& msg=std::string()) : ConnectionException(connection::CLOSE_CODE_INVALID_PATH, ""+msg) {}
};

/**
 * 
 *             A valid frame header cannot be formed from the incoming byte stream.
 *           
 */
struct FramingErrorException:
    ConnectionException
{
    std::string getPrefix() const { return "framing-error"; }
    FramingErrorException(const std::string& msg=std::string()) : ConnectionException(connection::CLOSE_CODE_FRAMING_ERROR, ""+msg) {}
};

QPID_COMMON_EXTERN sys::ExceptionHolder createConnectionException(int code, const std::string& text);

/**
 * 
 *             The session is currently attached to another transport.
 *           
 */
struct SessionBusyException:
    ChannelException
{
    std::string getPrefix() const { return "session-busy"; }
    SessionBusyException(const std::string& msg=std::string()) : ChannelException(session::DETACH_CODE_SESSION_BUSY, ""+msg) {}
};

/**
 * 
 *             The transport is currently attached to another session.
 *           
 */
struct TransportBusyException:
    ChannelException
{
    std::string getPrefix() const { return "transport-busy"; }
    TransportBusyException(const std::string& msg=std::string()) : ChannelException(session::DETACH_CODE_TRANSPORT_BUSY, ""+msg) {}
};

/**
 * 
 *             The transport is not currently attached to any session.
 *           
 */
struct NotAttachedException:
    ChannelException
{
    std::string getPrefix() const { return "not-attached"; }
    NotAttachedException(const std::string& msg=std::string()) : ChannelException(session::DETACH_CODE_NOT_ATTACHED, ""+msg) {}
};

/**
 * 
 *             Command data was received prior to any use of the command-point control.
 *           
 */
struct UnknownIdsException:
    ChannelException
{
    std::string getPrefix() const { return "unknown-ids"; }
    UnknownIdsException(const std::string& msg=std::string()) : ChannelException(session::DETACH_CODE_UNKNOWN_IDS, ""+msg) {}
};

QPID_COMMON_EXTERN sys::ExceptionHolder createChannelException(int code, const std::string& text);

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_REPLY_EXCEPTIONS_H*/
