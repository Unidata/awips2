#ifndef QPID_AMQP_0_10_EXCEPTIONS_H
#define QPID_AMQP_0_10_EXCEPTIONS_H
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


#include "qpid/amqp_0_10/Exception.h"

namespace qpid {
namespace amqp_0_10 {


/**
 * 
 *             The client attempted to work with a server entity to which it has no access due to
 *             security settings.
 *           
 */
struct UnauthorizedAccessException:
    public SessionAbortedException
{
    UnauthorizedAccessException(const std::string& msg=std::string())
        : SessionAbortedException(execution::UNAUTHORIZED_ACCESS, msg) {}
  protected:
    std::string getPrefix() const { return "UnauthorizedAccessException"; }
};

/**
 * 
 *             The client attempted to work with a server entity that does not exist.
 *           
 */
struct NotFoundException:
    public SessionAbortedException
{
    NotFoundException(const std::string& msg=std::string())
        : SessionAbortedException(execution::NOT_FOUND, msg) {}
  protected:
    std::string getPrefix() const { return "NotFoundException"; }
};

/**
 * 
 *             The client attempted to work with a server entity to which it has no access because
 *             another client is working with it.
 *           
 */
struct ResourceLockedException:
    public SessionAbortedException
{
    ResourceLockedException(const std::string& msg=std::string())
        : SessionAbortedException(execution::RESOURCE_LOCKED, msg) {}
  protected:
    std::string getPrefix() const { return "ResourceLockedException"; }
};

/**
 * 
 *             The client requested a command that was not allowed because some precondition failed.
 *           
 */
struct PreconditionFailedException:
    public SessionAbortedException
{
    PreconditionFailedException(const std::string& msg=std::string())
        : SessionAbortedException(execution::PRECONDITION_FAILED, msg) {}
  protected:
    std::string getPrefix() const { return "PreconditionFailedException"; }
};

/**
 * 
 *             A server entity the client is working with has been deleted.
 *           
 */
struct ResourceDeletedException:
    public SessionAbortedException
{
    ResourceDeletedException(const std::string& msg=std::string())
        : SessionAbortedException(execution::RESOURCE_DELETED, msg) {}
  protected:
    std::string getPrefix() const { return "ResourceDeletedException"; }
};

/**
 * 
 *             The peer sent a command that is not permitted in the current state of the session.
 *           
 */
struct IllegalStateException:
    public SessionAbortedException
{
    IllegalStateException(const std::string& msg=std::string())
        : SessionAbortedException(execution::ILLEGAL_STATE, msg) {}
  protected:
    std::string getPrefix() const { return "IllegalStateException"; }
};

/**
 * 
 *             The command segments could not be decoded.
 *           
 */
struct CommandInvalidException:
    public SessionAbortedException
{
    CommandInvalidException(const std::string& msg=std::string())
        : SessionAbortedException(execution::COMMAND_INVALID, msg) {}
  protected:
    std::string getPrefix() const { return "CommandInvalidException"; }
};

/**
 * 
 *             The client exceeded its resource allocation.
 *           
 */
struct ResourceLimitExceededException:
    public SessionAbortedException
{
    ResourceLimitExceededException(const std::string& msg=std::string())
        : SessionAbortedException(execution::RESOURCE_LIMIT_EXCEEDED, msg) {}
  protected:
    std::string getPrefix() const { return "ResourceLimitExceededException"; }
};

/**
 * 
 *             The peer tried to use a command a manner that is inconsistent with the rules described
 *             in the specification.
 *           
 */
struct NotAllowedException:
    public SessionAbortedException
{
    NotAllowedException(const std::string& msg=std::string())
        : SessionAbortedException(execution::NOT_ALLOWED, msg) {}
  protected:
    std::string getPrefix() const { return "NotAllowedException"; }
};

/**
 * 
 *             The command argument is malformed, i.e. it does not fall within the specified domain.
 *             The illegal-argument exception can be raised on execution of any command which has
 *             domain valued fields.
 *           
 */
struct IllegalArgumentException:
    public SessionAbortedException
{
    IllegalArgumentException(const std::string& msg=std::string())
        : SessionAbortedException(execution::ILLEGAL_ARGUMENT, msg) {}
  protected:
    std::string getPrefix() const { return "IllegalArgumentException"; }
};

/**
 * 
 *             The peer tried to use functionality that is not implemented in its partner.
 *           
 */
struct NotImplementedException:
    public SessionAbortedException
{
    NotImplementedException(const std::string& msg=std::string())
        : SessionAbortedException(execution::NOT_IMPLEMENTED, msg) {}
  protected:
    std::string getPrefix() const { return "NotImplementedException"; }
};

/**
 * 
 *             The peer could not complete the command because of an internal error. The peer may
 *             require intervention by an operator in order to resume normal operations.
 *           
 */
struct InternalErrorException:
    public SessionAbortedException
{
    InternalErrorException(const std::string& msg=std::string())
        : SessionAbortedException(execution::INTERNAL_ERROR, msg) {}
  protected:
    std::string getPrefix() const { return "InternalErrorException"; }
};

/**
 * 
 *             An invalid argument was passed to a command, and the operation could not
 *             proceed.  An invalid argument is not illegal (see illegal-argument), i.e. it matches
 *             the domain definition; however the particular value is invalid in this context.
 *           
 */
struct InvalidArgumentException:
    public SessionAbortedException
{
    InvalidArgumentException(const std::string& msg=std::string())
        : SessionAbortedException(execution::INVALID_ARGUMENT, msg) {}
  protected:
    std::string getPrefix() const { return "InvalidArgumentException"; }
};


/**
 * 
 *             The session was detached by request.
 *           
 */
struct NormalDetachedException:
    public SessionDetachedException
{
    NormalDetachedException(const std::string& msg=std::string())
        : SessionDetachedException(session::NORMAL, msg) {}
  protected:
    std::string getPrefix() const { return "NormalDetachedException"; }
};

/**
 * 
 *             The session is currently attached to another transport.
 *           
 */
struct SessionBusyDetachedException:
    public SessionDetachedException
{
    SessionBusyDetachedException(const std::string& msg=std::string())
        : SessionDetachedException(session::SESSION_BUSY, msg) {}
  protected:
    std::string getPrefix() const { return "SessionBusyDetachedException"; }
};

/**
 * 
 *             The transport is currently attached to another session.
 *           
 */
struct TransportBusyDetachedException:
    public SessionDetachedException
{
    TransportBusyDetachedException(const std::string& msg=std::string())
        : SessionDetachedException(session::TRANSPORT_BUSY, msg) {}
  protected:
    std::string getPrefix() const { return "TransportBusyDetachedException"; }
};

/**
 * 
 *             The transport is not currently attached to any session.
 *           
 */
struct NotAttachedDetachedException:
    public SessionDetachedException
{
    NotAttachedDetachedException(const std::string& msg=std::string())
        : SessionDetachedException(session::NOT_ATTACHED, msg) {}
  protected:
    std::string getPrefix() const { return "NotAttachedDetachedException"; }
};

/**
 * 
 *             Command data was received prior to any use of the command-point control.
 *           
 */
struct UnknownIdsDetachedException:
    public SessionDetachedException
{
    UnknownIdsDetachedException(const std::string& msg=std::string())
        : SessionDetachedException(session::UNKNOWN_IDS, msg) {}
  protected:
    std::string getPrefix() const { return "UnknownIdsDetachedException"; }
};


/**
 * 
 *             The session was detached by request.
 *           
 */
struct NormalExpiredException:
    public SessionExpiredException
{
    NormalExpiredException(const std::string& msg=std::string())
        : SessionExpiredException(session::NORMAL, msg) {}
  protected:
    std::string getPrefix() const { return "NormalExpiredException"; }
};

/**
 * 
 *             The session is currently attached to another transport.
 *           
 */
struct SessionBusyExpiredException:
    public SessionExpiredException
{
    SessionBusyExpiredException(const std::string& msg=std::string())
        : SessionExpiredException(session::SESSION_BUSY, msg) {}
  protected:
    std::string getPrefix() const { return "SessionBusyExpiredException"; }
};

/**
 * 
 *             The transport is currently attached to another session.
 *           
 */
struct TransportBusyExpiredException:
    public SessionExpiredException
{
    TransportBusyExpiredException(const std::string& msg=std::string())
        : SessionExpiredException(session::TRANSPORT_BUSY, msg) {}
  protected:
    std::string getPrefix() const { return "TransportBusyExpiredException"; }
};

/**
 * 
 *             The transport is not currently attached to any session.
 *           
 */
struct NotAttachedExpiredException:
    public SessionExpiredException
{
    NotAttachedExpiredException(const std::string& msg=std::string())
        : SessionExpiredException(session::NOT_ATTACHED, msg) {}
  protected:
    std::string getPrefix() const { return "NotAttachedExpiredException"; }
};

/**
 * 
 *             Command data was received prior to any use of the command-point control.
 *           
 */
struct UnknownIdsExpiredException:
    public SessionExpiredException
{
    UnknownIdsExpiredException(const std::string& msg=std::string())
        : SessionExpiredException(session::UNKNOWN_IDS, msg) {}
  protected:
    std::string getPrefix() const { return "UnknownIdsExpiredException"; }
};


/**
 * 
 *             The connection closed normally.
 *           
 */
struct NormalException:
    public ConnectionException
{
    NormalException(const std::string& msg=std::string())
        : ConnectionException(connection::NORMAL, msg) {}
  protected:
    std::string getPrefix() const { return "NormalException"; }
};

/**
 * 
 *             An operator intervened to close the connection for some reason. The client may retry at
 *             some later date.
 *           
 */
struct ConnectionForcedException:
    public ConnectionException
{
    ConnectionForcedException(const std::string& msg=std::string())
        : ConnectionException(connection::CONNECTION_FORCED, msg) {}
  protected:
    std::string getPrefix() const { return "ConnectionForcedException"; }
};

/**
 * 
 *             The client tried to work with an unknown virtual host.
 *           
 */
struct InvalidPathException:
    public ConnectionException
{
    InvalidPathException(const std::string& msg=std::string())
        : ConnectionException(connection::INVALID_PATH, msg) {}
  protected:
    std::string getPrefix() const { return "InvalidPathException"; }
};

/**
 * 
 *             A valid frame header cannot be formed from the incoming byte stream.
 *           
 */
struct FramingErrorException:
    public ConnectionException
{
    FramingErrorException(const std::string& msg=std::string())
        : ConnectionException(connection::FRAMING_ERROR, msg) {}
  protected:
    std::string getPrefix() const { return "FramingErrorException"; }
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_EXCEPTIONS_H*/
