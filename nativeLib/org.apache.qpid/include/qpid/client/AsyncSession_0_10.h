#ifndef QPID_CLIENT_ASYNCSESSION_0_10_H
#define QPID_CLIENT_ASYNCSESSION_0_10_H
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


#include "qpid/client/no_keyword/AsyncSession_0_10.h"
#include "qpid/client/arg.h"
#include "qpid/client/ClientImportExport.h"

namespace qpid {
namespace client {

/**
 * AMQP 0-10 session API with keyword arguments.
 * This class provides the same set of functions as no_keyword::AsyncSession_0_10, but also
 * allows parameters be passed using keywords. The keyword is the
 * parameter name in the namespace "arg".
 * 
 * For example given the normal function "foo(int x=0, int y=0, int z=0)"
 * you could call it in either of the following ways:
 * 
 * @code
 * session.foo(1,2,3);             // Normal no keywords
 * session.foo(arg::z=3, arg::x=1); // Keywords and a default
 * @endcode
 * 
 * The keyword functions are easy to use but their declarations are hard
 * to read. You may find it easier to read the documentation for no_keyword::AsyncSession_0_10
 * which provides the same set of functions using normal non-keyword
 * declarations.
 * 
 * \ingroup clientapi
 * 
 * 
 * \details 
 * 
 * <h2>Publishing Messages</h2>
 * <ul>
 * <li><p>messageTransfer()</p>
 * <pre>session.messageTransfer(arg::content=message, arg::destination="amq.topic");</pre></li>
 * <li><p>messageTransfer() &mdash; asynchronous</p>
 * <pre>#include &lt;qpid/client/AsyncSession.h>
 * 
 * for (int i=0; i&lt;10; i++) {
 *     message.setData(message_data.str());
 *     async(session).messageTransfer(arg::content=message,  arg::destination="amq.direct");        
 * }
 * 
 * session.sync();
 * </pre>
 * </li>
 * </ul>
 * 
 * <h2>Exchanges</h2>
 * <ul>
 * <li><p>exchangeBind()</p>
 * <pre>session.exchangeBind(arg::exchange="amq.topic", arg::queue=queue, arg::bindingKey=routing_key);</pre>
 * </li>
 * <li><p>exchangeUnbind()</p>
 * <pre>session.exchangeUnBind(arg::exchange="amq.topic", arg::queue=queue, arg::bindingKey=routing_key);</pre></li>
 * <li><p>exchangeBound()</p>
 * <pre>if (session.exchangeBound(arg::exchange="amq.topic", arg::queue=queue, arg::bindingKey=rk)){...}</pre>
 * <pre>if (session.exchangeBound(arg::exchange="amq.topic", arg::queue=queue)){...}</pre>
 * </li>
 * <li><p>exchangeDeclare()</p>
 * <pre>session.exchangeDeclare(arg::exchange="my.topic", arg::type="topic");</pre>
 * <pre>session.exchangeDeclare(arg::exchange="xml", arg::type="xml");</pre>
 * </li>
 * <li><p>exchangeDelete()</p>
 * <pre>session.exchangeDeclare(arg::exchange="my.topic");</pre>
 * <pre>session.exchangeDeclare(arg::exchange="xml", arg::ifUnused=true);</pre>
 * </li>
 * <li><p>exchangeQuery()</p>
 * <pre>ExchangeQueryResult eqr = session.exchangeQuery(arg::exchange="my.topic");</pre></li>
 * </ul>
 * 
 * 
 * <h2>Configuring exchanges in session.exchangeDeclare</h2>
 * 
 * <pre>arg::durable=true</pre>
 * <p>Default: false.</p>
 * <p>If durable=true, an exchange remains active even if the server is restarted. If durable=false, an exchange is purged when a server restarts.</p>
 * 
 * <pre>arg::autoDelete=true</pre>
 * <p>Default: false.</p>
 * <p>If autoDelete=true, deleting the last binding for an exchange also deletes the exchange.</p>
 * 
 * <pre>arg::alternatExchange="my.exchange"</pre>
 * <p>Default: none.</p>
 * <p>If an alternate exchange is specified, messages that can not be delivered to any queue are sent to the alternate exchange.</p>
 * 
 * <h2>Queues</h2>
 * <ul>
 * <li><p>queueDeclare()</p>
 * <pre>session.queueDeclare(arg::queue="message_queue");</pre>
 * </li>
 * <li><p>queueDelete()</p>
 * <pre>session.queueDelete(arg::queue="message_queue");</pre></li>
 * <li><p>queuePurge()</p>
 * <pre>session.queuePurge(arg::queue="message_queue");</pre></li>
 * <li><p>queueQuery()</p>
 * <pre>QueueQueryResult qqr = session.queueQuery(arg::queue="message_queue");</pre></li>
 * </ul>
 * 
 * 
 * <h2>Configuring queues with session.queueDeclare</h2>
 * <pre>arg::durable=true</pre>
 * <p>Default: false.</p>
 * <p>If durable=true, a queue remains active if the server is restarted. If durable=false, a queue and its contents are lost when a server restarts.</p>
 * <br/>
 * 
 * <pre>arg::autoDelete=true</pre>
 * <p>Default: false.</p>
 * <p>If autoDelete=true, the queue is deleted when the last active Subscription to the Queue is canceled.</p>
 * <br/>
 * 
 * <pre>arg::exclusive=true</pre>
 * <p>Default: false.</p>
 * <p>If exclusive=true, only the Session that created a queue can access it.</p>
 * <br/>
 * 
 * <pre>arg::alternateExchange="my.exchange"</pre>
 * <p>Default: none. </p>
 * <p>If an alternate exchange is specified, messages are routed to it if (1) they are rejected by a client, or (2) they remain on the queue when it is deleted.</p>
 * <br/>
 * 
 * 
 * <h2>Accepting, Acquiring, Rejecting, or Releasing Messages</h2>
 * <ul>
 * <li><p>messageAccept()  &mdash; acknowledges messages</p>
 * <pre>SequenceSet tobeAccepted; 
 * toAccepted.add(msg.getId()); 
 * session.messageAccept(toBeAccepted);</pre>
 * </li>
 * <li><p>messageAcquire()</p>
 * <pre>SequenceSet tobeAcquired;
 * toBeAcquired.add(msg.getId()); 
 * session.messageAcquire(toBeAcquired);</pre>
 * </li>
 * <li><p>messageReject()</p>
 * <pre>SequenceSet tobeRejected; 
 * toRejected.add(msg.getId()); 
 * session.messageReject(toBeRejected);</pre>
 * </li>
 * <li><p>messageRelease()</p>
 * <pre>SequenceSet tobeReleased; 
 * toReleased.add(msg.getId()); 
 * session.messageRelease(toBeReleased);</pre></li>
 * </ul>
 * 
 * <h2>Transactions</h2>
 * <ul>
 * <li><p>txSelect()</p>
 * <pre>session.txSelect();</pre>
 * </li>
 * <li><p>txCommit()</p>
 * <pre>session.txSelect();</pre></li>
 * <li><p>txRollback()</p>
 * <pre>session.txRollback();</pre></li>
 * </ul>
 * 
 * 
 * 
 */
class AsyncSession_0_10:
    public no_keyword::AsyncSession_0_10
{
  public:
    
    QPID_CLIENT_EXTERN AsyncSession_0_10();
    QPID_CLIENT_EXTERN AsyncSession_0_10(const SessionBase_0_10& other);
    QPID_CLIENT_EXTERN AsyncSession_0_10& operator=(const SessionBase_0_10& other);
  private:
    typedef boost::parameter::parameters<arg::keyword_tags::sync> ExecutionSyncParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::commandId,arg::keyword_tags::value,arg::keyword_tags::sync> ExecutionResultParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::errorCode,arg::keyword_tags::commandId,arg::keyword_tags::classCode,arg::keyword_tags::commandCode,arg::keyword_tags::fieldIndex,arg::keyword_tags::description,arg::keyword_tags::errorInfo,arg::keyword_tags::sync> ExecutionExceptionParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::destination,arg::keyword_tags::acceptMode,arg::keyword_tags::acquireMode,arg::keyword_tags::content,arg::keyword_tags::sync> MessageTransferParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::transfers,arg::keyword_tags::sync> MessageAcceptParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::transfers,arg::keyword_tags::code,arg::keyword_tags::text,arg::keyword_tags::sync> MessageRejectParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::transfers,arg::keyword_tags::setRedelivered,arg::keyword_tags::sync> MessageReleaseParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::transfers,arg::keyword_tags::sync> MessageAcquireParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::destination,arg::keyword_tags::resumeId,arg::keyword_tags::sync> MessageResumeParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::queue,arg::keyword_tags::destination,arg::keyword_tags::acceptMode,arg::keyword_tags::acquireMode,arg::keyword_tags::exclusive,arg::keyword_tags::resumeId,arg::keyword_tags::resumeTtl,arg::keyword_tags::arguments,arg::keyword_tags::sync> MessageSubscribeParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::destination,arg::keyword_tags::sync> MessageCancelParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::destination,arg::keyword_tags::flowMode,arg::keyword_tags::sync> MessageSetFlowModeParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::destination,arg::keyword_tags::unit,arg::keyword_tags::value,arg::keyword_tags::sync> MessageFlowParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::destination,arg::keyword_tags::sync> MessageFlushParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::destination,arg::keyword_tags::sync> MessageStopParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::sync> TxSelectParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::sync> TxCommitParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::sync> TxRollbackParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::sync> DtxSelectParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::xid,arg::keyword_tags::join,arg::keyword_tags::resume,arg::keyword_tags::sync> DtxStartParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::xid,arg::keyword_tags::fail,arg::keyword_tags::suspend,arg::keyword_tags::sync> DtxEndParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::xid,arg::keyword_tags::onePhase,arg::keyword_tags::sync> DtxCommitParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::xid,arg::keyword_tags::sync> DtxForgetParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::xid,arg::keyword_tags::sync> DtxGetTimeoutParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::xid,arg::keyword_tags::sync> DtxPrepareParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::sync> DtxRecoverParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::xid,arg::keyword_tags::sync> DtxRollbackParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::xid,arg::keyword_tags::timeout,arg::keyword_tags::sync> DtxSetTimeoutParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::exchange,arg::keyword_tags::type,arg::keyword_tags::alternateExchange,arg::keyword_tags::passive,arg::keyword_tags::durable,arg::keyword_tags::autoDelete,arg::keyword_tags::arguments,arg::keyword_tags::sync> ExchangeDeclareParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::exchange,arg::keyword_tags::ifUnused,arg::keyword_tags::sync> ExchangeDeleteParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::name,arg::keyword_tags::sync> ExchangeQueryParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::queue,arg::keyword_tags::exchange,arg::keyword_tags::bindingKey,arg::keyword_tags::arguments,arg::keyword_tags::sync> ExchangeBindParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::queue,arg::keyword_tags::exchange,arg::keyword_tags::bindingKey,arg::keyword_tags::sync> ExchangeUnbindParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::exchange,arg::keyword_tags::queue,arg::keyword_tags::bindingKey,arg::keyword_tags::arguments,arg::keyword_tags::sync> ExchangeBoundParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::queue,arg::keyword_tags::alternateExchange,arg::keyword_tags::passive,arg::keyword_tags::durable,arg::keyword_tags::exclusive,arg::keyword_tags::autoDelete,arg::keyword_tags::arguments,arg::keyword_tags::sync> QueueDeclareParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::queue,arg::keyword_tags::ifUnused,arg::keyword_tags::ifEmpty,arg::keyword_tags::sync> QueueDeleteParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::queue,arg::keyword_tags::sync> QueuePurgeParameters;
    
    typedef boost::parameter::parameters<arg::keyword_tags::queue,arg::keyword_tags::sync> QueueQueryParameters;
    
    friend class Connection;
  public:
    BOOST_PARAMETER_MEMFUN(Completion, executionSync, 0, 1, ExecutionSyncParameters) {
        return no_keyword::AsyncSession_0_10::executionSync(
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, executionResult, 0, 3, ExecutionResultParameters) {
        return no_keyword::AsyncSession_0_10::executionResult(
            p[arg::commandId|SequenceNumber()],
            p[arg::value|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, executionException, 0, 8, ExecutionExceptionParameters) {
        return no_keyword::AsyncSession_0_10::executionException(
            p[arg::errorCode|0],
            p[arg::commandId|SequenceNumber()],
            p[arg::classCode|0],
            p[arg::commandCode|0],
            p[arg::fieldIndex|0],
            p[arg::description|string()],
            p[arg::errorInfo|FieldTable()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageTransfer, 0, 5, MessageTransferParameters) {
        return no_keyword::AsyncSession_0_10::messageTransfer(
            p[arg::destination|string()],
            p[arg::acceptMode|1],
            p[arg::acquireMode|0],
            p[arg::content|Message(std::string())],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageAccept, 0, 2, MessageAcceptParameters) {
        return no_keyword::AsyncSession_0_10::messageAccept(
            p[arg::transfers|SequenceSet()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageReject, 0, 4, MessageRejectParameters) {
        return no_keyword::AsyncSession_0_10::messageReject(
            p[arg::transfers|SequenceSet()],
            p[arg::code|0],
            p[arg::text|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageRelease, 0, 3, MessageReleaseParameters) {
        return no_keyword::AsyncSession_0_10::messageRelease(
            p[arg::transfers|SequenceSet()],
            p[arg::setRedelivered|false],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::MessageAcquireResult>, messageAcquire, 0, 2, MessageAcquireParameters) {
        return no_keyword::AsyncSession_0_10::messageAcquire(
            p[arg::transfers|SequenceSet()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::MessageResumeResult>, messageResume, 0, 3, MessageResumeParameters) {
        return no_keyword::AsyncSession_0_10::messageResume(
            p[arg::destination|string()],
            p[arg::resumeId|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageSubscribe, 0, 9, MessageSubscribeParameters) {
        return no_keyword::AsyncSession_0_10::messageSubscribe(
            p[arg::queue|string()],
            p[arg::destination|string()],
            p[arg::acceptMode|0],
            p[arg::acquireMode|0],
            p[arg::exclusive|false],
            p[arg::resumeId|string()],
            p[arg::resumeTtl|0],
            p[arg::arguments|FieldTable()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageCancel, 0, 2, MessageCancelParameters) {
        return no_keyword::AsyncSession_0_10::messageCancel(
            p[arg::destination|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageSetFlowMode, 0, 3, MessageSetFlowModeParameters) {
        return no_keyword::AsyncSession_0_10::messageSetFlowMode(
            p[arg::destination|string()],
            p[arg::flowMode|0],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageFlow, 0, 4, MessageFlowParameters) {
        return no_keyword::AsyncSession_0_10::messageFlow(
            p[arg::destination|string()],
            p[arg::unit|0],
            p[arg::value|0],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageFlush, 0, 2, MessageFlushParameters) {
        return no_keyword::AsyncSession_0_10::messageFlush(
            p[arg::destination|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, messageStop, 0, 2, MessageStopParameters) {
        return no_keyword::AsyncSession_0_10::messageStop(
            p[arg::destination|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, txSelect, 0, 1, TxSelectParameters) {
        return no_keyword::AsyncSession_0_10::txSelect(
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, txCommit, 0, 1, TxCommitParameters) {
        return no_keyword::AsyncSession_0_10::txCommit(
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, txRollback, 0, 1, TxRollbackParameters) {
        return no_keyword::AsyncSession_0_10::txRollback(
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, dtxSelect, 0, 1, DtxSelectParameters) {
        return no_keyword::AsyncSession_0_10::dtxSelect(
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::XaResult>, dtxStart, 0, 4, DtxStartParameters) {
        return no_keyword::AsyncSession_0_10::dtxStart(
            p[arg::xid|Xid()],
            p[arg::join|false],
            p[arg::resume|false],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::XaResult>, dtxEnd, 0, 4, DtxEndParameters) {
        return no_keyword::AsyncSession_0_10::dtxEnd(
            p[arg::xid|Xid()],
            p[arg::fail|false],
            p[arg::suspend|false],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::XaResult>, dtxCommit, 0, 3, DtxCommitParameters) {
        return no_keyword::AsyncSession_0_10::dtxCommit(
            p[arg::xid|Xid()],
            p[arg::onePhase|false],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, dtxForget, 0, 2, DtxForgetParameters) {
        return no_keyword::AsyncSession_0_10::dtxForget(
            p[arg::xid|Xid()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::DtxGetTimeoutResult>, dtxGetTimeout, 0, 2, DtxGetTimeoutParameters) {
        return no_keyword::AsyncSession_0_10::dtxGetTimeout(
            p[arg::xid|Xid()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::XaResult>, dtxPrepare, 0, 2, DtxPrepareParameters) {
        return no_keyword::AsyncSession_0_10::dtxPrepare(
            p[arg::xid|Xid()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::DtxRecoverResult>, dtxRecover, 0, 1, DtxRecoverParameters) {
        return no_keyword::AsyncSession_0_10::dtxRecover(
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::XaResult>, dtxRollback, 0, 2, DtxRollbackParameters) {
        return no_keyword::AsyncSession_0_10::dtxRollback(
            p[arg::xid|Xid()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, dtxSetTimeout, 0, 3, DtxSetTimeoutParameters) {
        return no_keyword::AsyncSession_0_10::dtxSetTimeout(
            p[arg::xid|Xid()],
            p[arg::timeout|0],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, exchangeDeclare, 0, 8, ExchangeDeclareParameters) {
        return no_keyword::AsyncSession_0_10::exchangeDeclare(
            p[arg::exchange|string()],
            p[arg::type|string()],
            p[arg::alternateExchange|string()],
            p[arg::passive|false],
            p[arg::durable|false],
            p[arg::autoDelete|false],
            p[arg::arguments|FieldTable()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, exchangeDelete, 0, 3, ExchangeDeleteParameters) {
        return no_keyword::AsyncSession_0_10::exchangeDelete(
            p[arg::exchange|string()],
            p[arg::ifUnused|false],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::ExchangeQueryResult>, exchangeQuery, 0, 2, ExchangeQueryParameters) {
        return no_keyword::AsyncSession_0_10::exchangeQuery(
            p[arg::name|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, exchangeBind, 0, 5, ExchangeBindParameters) {
        return no_keyword::AsyncSession_0_10::exchangeBind(
            p[arg::queue|string()],
            p[arg::exchange|string()],
            p[arg::bindingKey|string()],
            p[arg::arguments|FieldTable()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, exchangeUnbind, 0, 4, ExchangeUnbindParameters) {
        return no_keyword::AsyncSession_0_10::exchangeUnbind(
            p[arg::queue|string()],
            p[arg::exchange|string()],
            p[arg::bindingKey|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::ExchangeBoundResult>, exchangeBound, 0, 5, ExchangeBoundParameters) {
        return no_keyword::AsyncSession_0_10::exchangeBound(
            p[arg::exchange|string()],
            p[arg::queue|string()],
            p[arg::bindingKey|string()],
            p[arg::arguments|FieldTable()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, queueDeclare, 0, 8, QueueDeclareParameters) {
        return no_keyword::AsyncSession_0_10::queueDeclare(
            p[arg::queue|string()],
            p[arg::alternateExchange|string()],
            p[arg::passive|false],
            p[arg::durable|false],
            p[arg::exclusive|false],
            p[arg::autoDelete|false],
            p[arg::arguments|FieldTable()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, queueDelete, 0, 4, QueueDeleteParameters) {
        return no_keyword::AsyncSession_0_10::queueDelete(
            p[arg::queue|string()],
            p[arg::ifUnused|false],
            p[arg::ifEmpty|false],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(Completion, queuePurge, 0, 2, QueuePurgeParameters) {
        return no_keyword::AsyncSession_0_10::queuePurge(
            p[arg::queue|string()],
            p[arg::sync|false]);
    }
    
    BOOST_PARAMETER_MEMFUN(TypedResult<qpid::framing::QueueQueryResult>, queueQuery, 0, 2, QueueQueryParameters) {
        return no_keyword::AsyncSession_0_10::queueQuery(
            p[arg::queue|string()],
            p[arg::sync|false]);
    }
    
};
/** Conversion to AsyncSession_0_10 from another session type */
inline AsyncSession_0_10 async(const SessionBase_0_10& other) { return (other); }

inline AsyncSession_0_10::AsyncSession_0_10() {}
inline AsyncSession_0_10::AsyncSession_0_10(const SessionBase_0_10& other) {
    *this = other;
}
inline AsyncSession_0_10& AsyncSession_0_10::operator=(const SessionBase_0_10& other) {
    impl = static_cast<const AsyncSession_0_10&>(other).impl;
    return *this;
}

}} // namespace qpid::client

#endif  /*!QPID_CLIENT_ASYNCSESSION_0_10_H*/
