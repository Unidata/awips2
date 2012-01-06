#ifndef QPID_CLIENT_NO_KEYWORD_ASYNCSESSION_0_10_H
#define QPID_CLIENT_NO_KEYWORD_ASYNCSESSION_0_10_H
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


#include "qpid/client/SessionBase_0_10.h"
#include "qpid/client/ClientImportExport.h"

namespace qpid {
namespace client {
namespace no_keyword {

/**
 * AMQP 0-10 asynchronous session API.
 * 
 *       A session is a named interaction between two peers. Session names are chosen by the upper
 *       layers and may be used indefinitely. The model layer may associate long-lived or durable state
 *       with a given session name. The session layer provides transport of commands associated with
 *       this interaction.
 *     
 */
class AsyncSession_0_10:
    public SessionBase_0_10
{
  public:
    
    QPID_CLIENT_EXTERN AsyncSession_0_10();
    QPID_CLIENT_EXTERN AsyncSession_0_10(const SessionBase_0_10& other);
    QPID_CLIENT_EXTERN AsyncSession_0_10& operator=(const SessionBase_0_10& other);
    
    /**
     * 
     *         This command is complete when all prior commands are completed.
     *       
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion executionSync(bool sync=false);
    
    /**
     * 
     *         This command carries data resulting from the execution of a command.
     *       
     * 
     * @param commandId
     * 
     * @param value
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion executionResult(const SequenceNumber& commandId=SequenceNumber(), const string& value=string(), bool sync=false);
    
    /**
     * 
     *         This command informs a peer of an execution exception. The command-id, when given,
     *         correlates the error to a specific command.
     *       
     * 
     * @param errorCode
     * 
     * @param commandId
     * 
     *           The command-id of the command which caused the exception. If the exception was not caused
     *           by a specific command, this value is not set.
     *         
     * 
     * @param classCode
     * 
     * @param commandCode
     * 
     * @param fieldIndex
     * 
     *           The zero based index of the exceptional field within the arguments to the exceptional
     *           command. If the exception was not caused by a specific field, this value is not set.
     *         
     * 
     * @param description
     * 
     *           The description provided is implementation defined, but MUST be in the language
     *           appropriate for the selected locale.  The intention is that this description is suitable
     *           for logging or alerting output.
     *         
     * 
     * @param errorInfo
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion executionException(uint16_t errorCode=0, const SequenceNumber& commandId=SequenceNumber(), uint8_t classCode=0, uint8_t commandCode=0, uint8_t fieldIndex=0, const string& description=string(), const FieldTable& errorInfo=FieldTable(), bool sync=false);
    
    /**
     * 
     *         This command transfers a message between two peers. When a client uses this command to
     *         publish a message to a broker, the destination identifies a specific exchange. The message
     *         will then be routed to queues as defined by the exchange configuration.
     * 
     *         The client may request a broker to transfer messages to it, from a particular queue, by
     *         issuing a subscribe command. The subscribe command specifies the destination that the broker
     *         should use for any resulting transfers.
     *       
     * 
     * @param destination
     * 
     *           Specifies the destination to which the message is to be transferred.
     *         
     * 
     * @param acceptMode
     * 
     *           Indicates whether message.accept, session.complete, or nothing at all is required to
     *           indicate successful transfer of the message.
     *         
     * 
     * @param acquireMode
     * 
     *           Indicates whether or not the transferred message has been acquired.
     *         
     * 
     * @param content
     * Message content
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageTransfer(const string& destination=string(), uint8_t acceptMode=1, uint8_t acquireMode=0, const Message& content=Message(std::string()), bool sync=false);
    
    /**
     * 
     *         Accepts the message. Once a transfer is accepted, the command-id may no longer be referenced
     *         from other commands.
     *       
     * 
     * @param transfers
     * 
     *           Identifies the messages previously transferred that should be accepted.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageAccept(const SequenceSet& transfers=SequenceSet(), bool sync=false);
    
    /**
     * 
     *         Indicates that the message transfers are unprocessable in some way. A server may reject a
     *         message if it is unroutable. A client may reject a message if it is invalid. A message may
     *         be rejected for other reasons as well. Once a transfer is rejected, the command-id may no
     *         longer be referenced from other commands.
     *       
     * 
     * @param transfers
     * 
     *           Identifies the messages previously transferred that should be rejected.
     *         
     * 
     * @param code
     * 
     *           Code describing the reason for rejection.
     *         
     * 
     * @param text
     * 
     *           Text describing the reason for rejection.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageReject(const SequenceSet& transfers=SequenceSet(), uint16_t code=0, const string& text=string(), bool sync=false);
    
    /**
     * 
     *         Release previously transferred messages. When acquired messages are released, they become
     *         available for acquisition by any subscriber. Once a transfer is released, the command-id may
     *         no longer be referenced from other commands.
     *       
     * 
     * @param transfers
     * 
     *           Indicates the messages to be released.
     *         
     * 
     * @param setRedelivered
     * 
     *           By setting set-redelivered to true, any acquired messages released to a queue with this
     *           command will be marked as redelivered on their next transfer from that queue. If this flag
     *           is not set, then an acquired message will retain its original redelivered status on the
     *           queue. Messages that are not acquired are unaffected by the value of this flag.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageRelease(const SequenceSet& transfers=SequenceSet(), bool setRedelivered=false, bool sync=false);
    
    /**
     * 
     *         Acquires previously transferred messages for consumption. The acquired ids (if any) are
     *         sent via message.acquired.
     *       
     * 
     * @param transfers
     * 
     *           Indicates the messages to be acquired.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::MessageAcquireResult> messageAcquire(const SequenceSet& transfers=SequenceSet(), bool sync=false);
    
    /**
     * 
     *         This command resumes an interrupted transfer. The recipient should return the amount of
     *         partially transferred data associated with the given resume-id, or zero if there is no data
     *         at all. If a non-zero result is returned, the recipient should expect to receive message
     *         fragment(s) containing the remainder of the interrupted message.
     *       
     * 
     * @param destination
     * 
     *           The destination to which the remaining message fragments are transferred.
     *         
     * 
     * @param resumeId
     * 
     *           The name of the transfer being resumed.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::MessageResumeResult> messageResume(const string& destination=string(), const string& resumeId=string(), bool sync=false);
    
    /**
     *  This command asks the server to start a "subscription", which is a request for messages
     *         from a specific queue. Subscriptions last as long as the session they were created on, or
     *         until the client cancels them. 
     * 
     * @param queue
     *  Specifies the name of the subscribed queue. 
     * 
     * @param destination
     *  The client specified name for the subscription. This is used as the destination for
     *           all messages transferred from this subscription. The destination is scoped to the session.
     *         
     * 
     * @param acceptMode
     *  The accept-mode to use for messages transferred from this subscription. 
     * 
     * @param acquireMode
     *  The acquire-mode to use for messages transferred from this subscription. 
     * 
     * @param exclusive
     *  Request an exclusive subscription. This prevents other subscribers from subscribing to
     *           the queue. 
     * 
     * @param resumeId
     *  Requests that the broker use the supplied resume-id when transferring messages for
     *           this subscription. 
     * 
     * @param resumeTtl
     *  Requested duration in milliseconds for the broker use as resume-ttl when transferring
     *           messages for this subscription. 
     * 
     * @param arguments
     *  The syntax and semantics of these arguments depends on the providers implementation.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageSubscribe(const string& queue=string(), const string& destination=string(), uint8_t acceptMode=0, uint8_t acquireMode=0, bool exclusive=false, const string& resumeId=string(), uint64_t resumeTtl=0, const FieldTable& arguments=FieldTable(), bool sync=false);
    
    /**
     * 
     *         This command cancels a subscription. This does not affect already delivered messages, but it
     *         does mean the server will not send any more messages for that subscription. The client may
     *         receive an arbitrary number of messages in between sending the cancel command and receiving
     *         notification that the cancel command is complete.
     *       
     * 
     * @param destination
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageCancel(const string& destination=string(), bool sync=false);
    
    /**
     * 
     *         Sets the mode of flow control used for a given destination to either window or credit based
     *         flow control.
     * 
     *         With credit based flow control, the sender of messages continually maintains its current
     *         credit balance with the recipient. The credit balance consists of two values, a message
     *         count, and a byte count. Whenever message data is sent, both counts must be decremented.
     *         If either value reaches zero, the flow of message data must stop. Additional credit is
     *         received via the message.flow command.
     * 
     *         The sender MUST NOT send partial assemblies. This means that if there is not enough byte
     *         credit available to send a complete message, the sender must either wait or use message
     *         fragmentation (see the fragment-properties header struct) to send the first part of the
     *         message data in a complete assembly.
     * 
     *         Window based flow control is identical to credit based flow control, however message
     *         transfer completion implicitly grants a single unit of message credit, and the size of the
     *         message in byte credits for each completed message transfer. Completion of the transfer
     *         command with session.completed is the only way credit is implicitly updated; message.accept,
     *         message.release, message.reject, tx.commit and tx.rollback have no effect on the outstanding
     *         credit balances.
     *       
     * 
     * @param destination
     * 
     * @param flowMode
     * 
     *           The new flow control mode.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageSetFlowMode(const string& destination=string(), uint8_t flowMode=0, bool sync=false);
    
    /**
     * 
     *         This command controls the flow of message data to a given destination. It is used by the
     *         recipient of messages to dynamically match the incoming rate of message flow to its
     *         processing or forwarding capacity. Upon receipt of this command, the sender must add "value"
     *         number of the specified unit to the available credit balance for the specified destination.
     *         A value of (0xFFFFFFFF) indicates an infinite amount of credit. This disables any limit for
     *         the given unit until the credit balance is zeroed with message.stop or message.flush.
     *       
     * 
     * @param destination
     * 
     * @param unit
     * 
     *           The unit of value.
     *         
     * 
     * @param value
     * 
     *           If the value is not set then this indicates an infinite amount of credit.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageFlow(const string& destination=string(), uint8_t unit=0, uint32_t value=0, bool sync=false);
    
    /**
     * 
     *         Forces the sender to exhaust his credit supply. The sender's credit will always be zero when
     *         this command completes. The command completes when immediately available message data has
     *         been transferred, or when the credit supply is exhausted.
     *       
     * 
     * @param destination
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageFlush(const string& destination=string(), bool sync=false);
    
    /**
     * 
     *         On receipt of this command, a producer of messages MUST set his credit to zero for the given
     *         destination. When notifying of completion, credit MUST be zero and no further messages will
     *         be sent until such a time as further credit is received.
     *       
     * 
     * @param destination
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion messageStop(const string& destination=string(), bool sync=false);
    
    /**
     * 
     *         This command sets the session to use standard transactions. The client must use this command
     *         exactly once on a session before using the Commit or Rollback commands.
     *       
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion txSelect(bool sync=false);
    
    /**
     * 
     *         This command commits all messages published and accepted in the current transaction. A
     *         new transaction starts immediately after a commit.
     *       
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion txCommit(bool sync=false);
    
    /**
     * 
     *         This command abandons the current transaction.  In particular the transfers from Client to
     *         Server (publishes) and accepts of transfers from Server to Client which occurred in the
     *         current transaction are discarded.  A new transaction starts immediately after a rollback.
     *       
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion txRollback(bool sync=false);
    
    /**
     * 
     *         This command sets the session to use distributed transactions. The client must use this
     *         command at least once on a session before using XA demarcation operations.
     *       
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion dtxSelect(bool sync=false);
    
    /**
     * 
     *         This command is called when messages should be produced and consumed on behalf a transaction
     *         branch identified by xid.
     *       
     * 
     * @param xid
     * 
     *           Specifies the xid of the transaction branch to be started.
     *         
     * 
     * @param join
     * 
     *           Indicate whether this is joining an already associated xid. Indicate that the start
     *           applies to joining a transaction previously seen.
     *         
     * 
     * @param resume
     * 
     *           Indicate that the start applies to resuming a suspended transaction branch specified.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::XaResult> dtxStart(const Xid& xid=Xid(), bool join=false, bool resume=false, bool sync=false);
    
    /**
     * 
     *         This command is called when the work done on behalf a transaction branch finishes or needs
     *         to be suspended.
     *       
     * 
     * @param xid
     * 
     *           Specifies the xid of the transaction branch to be ended.
     *         
     * 
     * @param fail
     * 
     *           If set, indicates that this portion of work has failed; otherwise this portion of work has
     *           completed successfully.
     *         
     * 
     * @param suspend
     * 
     *           Indicates that the transaction branch is temporarily suspended in an incomplete state.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::XaResult> dtxEnd(const Xid& xid=Xid(), bool fail=false, bool suspend=false, bool sync=false);
    
    /**
     * 
     *         Commit the work done on behalf a transaction branch. This command commits the work
     *         associated with xid. Any produced messages are made available and any consumed messages are
     *         discarded.
     *       
     * 
     * @param xid
     * 
     *           Specifies the xid of the transaction branch to be committed.
     *         
     * 
     * @param onePhase
     * 
     *           Used to indicate whether one-phase or two-phase commit is used.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::XaResult> dtxCommit(const Xid& xid=Xid(), bool onePhase=false, bool sync=false);
    
    /**
     * 
     *         This command is called to forget about a heuristically completed transaction branch.
     *       
     * 
     * @param xid
     * 
     *           Specifies the xid of the transaction branch to be forgotten.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion dtxForget(const Xid& xid=Xid(), bool sync=false);
    
    /**
     * 
     *         This command obtains the current transaction timeout value in seconds. If set-timeout was
     *         not used prior to invoking this command, the return value is the default timeout; otherwise,
     *         the value used in the previous set-timeout call is returned.
     *       
     * 
     * @param xid
     * 
     *           Specifies the xid of the transaction branch for getting the timeout.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::DtxGetTimeoutResult> dtxGetTimeout(const Xid& xid=Xid(), bool sync=false);
    
    /**
     * 
     *         This command prepares for commitment any message produced or consumed on behalf of xid.
     *       
     * 
     * @param xid
     * 
     *           Specifies the xid of the transaction branch that can be prepared.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::XaResult> dtxPrepare(const Xid& xid=Xid(), bool sync=false);
    
    /**
     * 
     *         This command is called to obtain a list of transaction branches that are in a prepared or
     *         heuristically completed state.
     *       
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::DtxRecoverResult> dtxRecover(bool sync=false);
    
    /**
     * 
     *         This command rolls back the work associated with xid. Any produced messages are discarded
     *         and any consumed messages are re-enqueued.
     *       
     * 
     * @param xid
     * 
     *           Specifies the xid of the transaction branch that can be rolled back.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::XaResult> dtxRollback(const Xid& xid=Xid(), bool sync=false);
    
    /**
     * 
     *         Sets the specified transaction branch timeout value in seconds.
     *       
     * 
     * @param xid
     * 
     *           Specifies the xid of the transaction branch for setting the timeout.
     *         
     * 
     * @param timeout
     * 
     *           The transaction timeout value in seconds.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion dtxSetTimeout(const Xid& xid=Xid(), uint32_t timeout=0, bool sync=false);
    
    /**
     * 
     *         This command creates an exchange if it does not already exist, and if the exchange exists,
     *         verifies that it is of the correct and expected class.
     *       
     * 
     * @param exchange
     * 
     * @param type
     * 
     *           Each exchange belongs to one of a set of exchange types implemented by the server. The
     *           exchange types define the functionality of the exchange - i.e. how messages are routed
     *           through it. It is not valid or meaningful to attempt to change the type of an existing
     *           exchange.
     *         
     * 
     * @param alternateExchange
     * 
     *           In the event that a message cannot be routed, this is the name of the exchange to which
     *           the message will be sent. Messages transferred using message.transfer will be routed to
     *           the alternate-exchange only if they are sent with the "none" accept-mode, and the
     *           discard-unroutable delivery property is set to false, and there is no queue to route to
     *           for the given message according to the bindings on this exchange.
     *         
     * 
     * @param passive
     * 
     *           If set, the server will not create the exchange. The client can use this to check whether
     *           an exchange exists without modifying the server state.
     *         
     * 
     * @param durable
     * 
     *           If set when creating a new exchange, the exchange will be marked as durable. Durable
     *           exchanges remain active when a server restarts. Non-durable exchanges (transient
     *           exchanges) are purged if/when a server restarts.
     *         
     * 
     * @param autoDelete
     * 
     *           If set, the exchange is deleted automatically when there remain no bindings between the
     *           exchange and any queue. Such an exchange will not be automatically deleted until at least
     *           one binding has been made to prevent the immediate deletion of the exchange upon creation.
     *         
     * 
     * @param arguments
     * 
     *           A set of arguments for the declaration. The syntax and semantics of these arguments
     *           depends on the server implementation. This field is ignored if passive is 1.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion exchangeDeclare(const string& exchange=string(), const string& type=string(), const string& alternateExchange=string(), bool passive=false, bool durable=false, bool autoDelete=false, const FieldTable& arguments=FieldTable(), bool sync=false);
    
    /**
     * 
     *         This command deletes an exchange. When an exchange is deleted all queue bindings on the
     *         exchange are cancelled.
     *       
     * 
     * @param exchange
     * 
     * @param ifUnused
     * 
     *           If set, the server will only delete the exchange if it has no queue bindings. If the
     *           exchange has queue bindings the server does not delete it but raises an exception
     *           instead.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion exchangeDelete(const string& exchange=string(), bool ifUnused=false, bool sync=false);
    
    /**
     * 
     *         This command is used to request information on a particular exchange.
     *       
     * 
     * @param name
     * 
     *            The name of the exchange for which information is requested. If not specified explicitly
     *            the default exchange is implied.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::ExchangeQueryResult> exchangeQuery(const string& name=string(), bool sync=false);
    
    /**
     *  This command binds a queue to an exchange. Until a queue is bound it will not receive
     *         any messages. In a classic messaging model, store-and-forward queues are bound to a direct
     *         exchange and subscription queues are bound to a topic exchange. 
     * 
     * @param queue
     *  Specifies the name of the queue to bind. 
     * 
     * @param exchange
     * 
     * @param bindingKey
     *  The binding-key uniquely identifies a binding between a given (exchange, queue) pair.
     *           Depending on the exchange configuration, the binding key may be matched against the
     *           message routing key in order to make routing decisions. The match algorithm depends on the
     *           exchange type. Some exchange types may ignore the binding key when making routing
     *           decisions. Refer to the specific exchange type documentation. The meaning of an empty
     *           binding key depends on the exchange implementation. 
     * 
     * @param arguments
     *  A set of arguments for the binding. The syntax and semantics of these arguments
     *           depends on the exchange class. 
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion exchangeBind(const string& queue=string(), const string& exchange=string(), const string& bindingKey=string(), const FieldTable& arguments=FieldTable(), bool sync=false);
    
    /**
     * 
     *         This command unbinds a queue from an exchange.
     *       
     * 
     * @param queue
     * 
     *           Specifies the name of the queue to unbind.
     *         
     * 
     * @param exchange
     * 
     *           The name of the exchange to unbind from.
     *         
     * 
     * @param bindingKey
     * 
     *           Specifies the binding-key of the binding to unbind.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion exchangeUnbind(const string& queue=string(), const string& exchange=string(), const string& bindingKey=string(), bool sync=false);
    
    /**
     * 
     *         This command is used to request information on the bindings to a particular exchange.
     *       
     * 
     * @param exchange
     * 
     *            The name of the exchange for which binding information is being requested. If not
     *            specified explicitly the default exchange is implied.
     *         
     * 
     * @param queue
     * 
     *           If populated then determine whether the given queue is bound to the exchange.
     *         
     * 
     * @param bindingKey
     * 
     *           If populated defines the binding-key of the binding of interest, if not populated the
     *           request will ignore the binding-key on bindings when searching for a match.
     *         
     * 
     * @param arguments
     * 
     *           If populated defines the arguments of the binding of interest if not populated the request
     *           will ignore the arguments on bindings when searching for a match
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::ExchangeBoundResult> exchangeBound(const string& exchange=string(), const string& queue=string(), const string& bindingKey=string(), const FieldTable& arguments=FieldTable(), bool sync=false);
    
    /**
     * 
     *         This command creates or checks a queue. When creating a new queue the client can specify
     *         various properties that control the durability of the queue and its contents, and the level
     *         of sharing for the queue.
     *       
     * 
     * @param queue
     * 
     * @param alternateExchange
     * 
     *           The alternate-exchange field specifies how messages on this queue should be treated when
     *           they are rejected by a subscriber, or when they are orphaned by queue deletion. When
     *           present, rejected or orphaned messages MUST be routed to the alternate-exchange. In all
     *           cases the messages MUST be removed from the queue.
     *         
     * 
     * @param passive
     * 
     *           If set, the server will not create the queue. This field allows the client to assert the
     *           presence of a queue without modifying the server state.
     *         
     * 
     * @param durable
     * 
     *           If set when creating a new queue, the queue will be marked as durable. Durable queues
     *           remain active when a server restarts. Non-durable queues (transient queues) are purged
     *           if/when a server restarts. Note that durable queues do not necessarily hold persistent
     *           messages, although it does not make sense to send persistent messages to a transient
     *           queue.
     *         
     * 
     * @param exclusive
     * 
     *           Exclusive queues can only be used from one session at a time. Once a session
     *           declares an exclusive queue, that queue cannot be used by any other session until the
     *           declaring session closes.
     *         
     * 
     * @param autoDelete
     * 
     *             If this field is set and the exclusive field is also set, then the queue MUST be deleted
     *             when the session closes.
     * 
     *             If this field is set and the exclusive field is not set the queue is deleted when all
     *             the consumers have finished using it. Last consumer can be cancelled either explicitly
     *             or because its session is closed. If there was no consumer ever on the queue, it won't
     *             be deleted.
     *         
     * 
     * @param arguments
     * 
     *           A set of arguments for the declaration. The syntax and semantics of these arguments
     *           depends on the server implementation. This field is ignored if passive is 1.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion queueDeclare(const string& queue=string(), const string& alternateExchange=string(), bool passive=false, bool durable=false, bool exclusive=false, bool autoDelete=false, const FieldTable& arguments=FieldTable(), bool sync=false);
    
    /**
     * 
     *         This command deletes a queue. When a queue is deleted any pending messages are sent to the
     *         alternate-exchange if defined, or discarded if it is not.
     *       
     * 
     * @param queue
     * 
     *           Specifies the name of the queue to delete.
     *         
     * 
     * @param ifUnused
     * 
     *           If set, the server will only delete the queue if it has no consumers. If the queue has
     *           consumers the server does does not delete it but raises an exception instead.
     *         
     * 
     * @param ifEmpty
     * 
     *           If set, the server will only delete the queue if it has no messages.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion queueDelete(const string& queue=string(), bool ifUnused=false, bool ifEmpty=false, bool sync=false);
    
    /**
     * 
     *         This command removes all messages from a queue. It does not cancel subscribers. Purged
     *         messages are deleted without any formal "undo" mechanism.
     *       
     * 
     * @param queue
     * 
     *           Specifies the name of the queue to purge.
     *         
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN Completion queuePurge(const string& queue=string(), bool sync=false);
    
    /**
     * 
     *         This command requests information about a queue.
     *       
     * 
     * @param queue
     * 
     * @param sync
     * If true the broker will respond with completion status as soon as possible.
     * 
     */
    QPID_CLIENT_EXTERN TypedResult<qpid::framing::QueueQueryResult> queueQuery(const string& queue=string(), bool sync=false);
};

}}} // namespace qpid::client::no_keyword

#endif  /*!QPID_CLIENT_NO_KEYWORD_ASYNCSESSION_0_10_H*/
