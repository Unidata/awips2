#ifndef QPID_MESSAGING_EXCEPTIONS_H
#define QPID_MESSAGING_EXCEPTIONS_H

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
#include "qpid/types/Exception.h"
#include "qpid/types/Variant.h"

namespace qpid {
namespace messaging {

/** \ingroup messaging 
 */

struct MessagingException : public qpid::types::Exception 
{
    QPID_MESSAGING_EXTERN MessagingException(const std::string& msg);
    QPID_MESSAGING_EXTERN virtual ~MessagingException() throw();
    
    qpid::types::Variant::Map detail;
    //TODO: override what() to include detail if present
};

struct InvalidOptionString : public MessagingException 
{
    QPID_MESSAGING_EXTERN InvalidOptionString(const std::string& msg);
};

struct KeyError : public MessagingException
{
    QPID_MESSAGING_EXTERN KeyError(const std::string&);
};

struct LinkError : public MessagingException
{
    QPID_MESSAGING_EXTERN LinkError(const std::string&);
};

struct AddressError : public LinkError
{
    QPID_MESSAGING_EXTERN AddressError(const std::string&);
};

/**
 * Thrown when a syntactically correct address cannot be resolved or
 * used.
 */
struct ResolutionError : public AddressError 
{
    QPID_MESSAGING_EXTERN ResolutionError(const std::string& msg);
};

struct AssertionFailed : public ResolutionError 
{
    QPID_MESSAGING_EXTERN AssertionFailed(const std::string& msg);
};

struct NotFound : public ResolutionError 
{
    QPID_MESSAGING_EXTERN NotFound(const std::string& msg);
};

/**
 * Thrown when an address string with inalid sytanx is used.
 */
struct MalformedAddress : public AddressError 
{
    QPID_MESSAGING_EXTERN MalformedAddress(const std::string& msg);
};

struct ReceiverError : public LinkError
{
    QPID_MESSAGING_EXTERN ReceiverError(const std::string&);
};

struct FetchError : public ReceiverError
{
    QPID_MESSAGING_EXTERN FetchError(const std::string&);
};

struct NoMessageAvailable : public FetchError
{
    QPID_MESSAGING_EXTERN NoMessageAvailable();
};

struct SenderError : public LinkError
{
    QPID_MESSAGING_EXTERN SenderError(const std::string&);
};

struct SendError : public SenderError
{
    QPID_MESSAGING_EXTERN SendError(const std::string&);
};

struct TargetCapacityExceeded : public SendError
{
    QPID_MESSAGING_EXTERN TargetCapacityExceeded(const std::string&);
};

struct SessionError : public MessagingException
{
    QPID_MESSAGING_EXTERN SessionError(const std::string&);
};

struct TransactionError : public SessionError
{
    QPID_MESSAGING_EXTERN TransactionError(const std::string&);
};

struct TransactionAborted : public TransactionError
{
    QPID_MESSAGING_EXTERN TransactionAborted(const std::string&);
};

struct UnauthorizedAccess : public SessionError
{
    QPID_MESSAGING_EXTERN UnauthorizedAccess(const std::string&);
};

struct ConnectionError : public MessagingException
{
    QPID_MESSAGING_EXTERN ConnectionError(const std::string&);
};

struct TransportFailure : public MessagingException
{
    QPID_MESSAGING_EXTERN TransportFailure(const std::string&);
};

}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_EXCEPTIONS_H*/
