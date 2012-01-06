#ifndef QPID_CLIENT_ARG_H
#define QPID_CLIENT_ARG_H
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


#define BOOST_PARAMETER_MAX_ARITY 9
#include <boost/parameter.hpp>

namespace qpid {
namespace client {
namespace arg {

BOOST_PARAMETER_KEYWORD(keyword_tags, sync)
BOOST_PARAMETER_KEYWORD(keyword_tags, commandId)
BOOST_PARAMETER_KEYWORD(keyword_tags, value)
BOOST_PARAMETER_KEYWORD(keyword_tags, errorCode)
BOOST_PARAMETER_KEYWORD(keyword_tags, classCode)
BOOST_PARAMETER_KEYWORD(keyword_tags, commandCode)
BOOST_PARAMETER_KEYWORD(keyword_tags, fieldIndex)
BOOST_PARAMETER_KEYWORD(keyword_tags, description)
BOOST_PARAMETER_KEYWORD(keyword_tags, errorInfo)
BOOST_PARAMETER_KEYWORD(keyword_tags, destination)
BOOST_PARAMETER_KEYWORD(keyword_tags, acceptMode)
BOOST_PARAMETER_KEYWORD(keyword_tags, acquireMode)
BOOST_PARAMETER_KEYWORD(keyword_tags, content)
BOOST_PARAMETER_KEYWORD(keyword_tags, transfers)
BOOST_PARAMETER_KEYWORD(keyword_tags, code)
BOOST_PARAMETER_KEYWORD(keyword_tags, text)
BOOST_PARAMETER_KEYWORD(keyword_tags, setRedelivered)
BOOST_PARAMETER_KEYWORD(keyword_tags, resumeId)
BOOST_PARAMETER_KEYWORD(keyword_tags, queue)
BOOST_PARAMETER_KEYWORD(keyword_tags, exclusive)
BOOST_PARAMETER_KEYWORD(keyword_tags, resumeTtl)
BOOST_PARAMETER_KEYWORD(keyword_tags, arguments)
BOOST_PARAMETER_KEYWORD(keyword_tags, flowMode)
BOOST_PARAMETER_KEYWORD(keyword_tags, unit)
BOOST_PARAMETER_KEYWORD(keyword_tags, xid)
BOOST_PARAMETER_KEYWORD(keyword_tags, join)
BOOST_PARAMETER_KEYWORD(keyword_tags, resume)
BOOST_PARAMETER_KEYWORD(keyword_tags, fail)
BOOST_PARAMETER_KEYWORD(keyword_tags, suspend)
BOOST_PARAMETER_KEYWORD(keyword_tags, onePhase)
BOOST_PARAMETER_KEYWORD(keyword_tags, timeout)
BOOST_PARAMETER_KEYWORD(keyword_tags, exchange)
BOOST_PARAMETER_KEYWORD(keyword_tags, type)
BOOST_PARAMETER_KEYWORD(keyword_tags, alternateExchange)
BOOST_PARAMETER_KEYWORD(keyword_tags, passive)
BOOST_PARAMETER_KEYWORD(keyword_tags, durable)
BOOST_PARAMETER_KEYWORD(keyword_tags, autoDelete)
BOOST_PARAMETER_KEYWORD(keyword_tags, ifUnused)
BOOST_PARAMETER_KEYWORD(keyword_tags, name)
BOOST_PARAMETER_KEYWORD(keyword_tags, bindingKey)
BOOST_PARAMETER_KEYWORD(keyword_tags, ifEmpty)

}}} // namespace qpid::client::arg

#endif  /*!QPID_CLIENT_ARG_H*/
