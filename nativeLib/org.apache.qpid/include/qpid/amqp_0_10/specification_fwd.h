#ifndef QPID_AMQP_0_10_SPECIFICATION_FWD_H
#define QPID_AMQP_0_10_SPECIFICATION_FWD_H
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


#include "qpid/amqp_0_10/built_in_types.h"

namespace qpid {
namespace amqp_0_10 {


typedef ArrayDomain<Str16>  Str16Array;

namespace connection {

const uint8_t CODE=0x1;
extern const char* NAME;

enum CloseCode {
    NORMAL = 200,
    CONNECTION_FORCED = 320,
    INVALID_PATH = 402,
    FRAMING_ERROR = 501
};
inline SerializeAs<CloseCode, uint8_t> serializable(CloseCode& e) {
    return SerializeAs<CloseCode, uint8_t>(e);
}

typedef Str16 AmqpHostUrl;

typedef ArrayDomain<connection::AmqpHostUrl>  AmqpHostArray;
class Start;
class StartOk;
class Secure;
class SecureOk;
class Tune;
class TuneOk;
class Open;
class OpenOk;
class Redirect;
class Heartbeat;
class Close;
class CloseOk;

} // namespace connection


namespace session {

const uint8_t CODE=0x2;
extern const char* NAME;

typedef Vbin16 Name;

enum DetachCode {
    NORMAL = 0,
    SESSION_BUSY = 1,
    TRANSPORT_BUSY = 2,
    NOT_ATTACHED = 3,
    UNKNOWN_IDS = 4
};
inline SerializeAs<DetachCode, uint8_t> serializable(DetachCode& e) {
    return SerializeAs<DetachCode, uint8_t>(e);
}

typedef SequenceSet Commands;
class Header;
class CommandFragment;

typedef ArrayDomain<session::CommandFragment>  CommandFragments;
class Attach;
class Attached;
class Detach;
class Detached;
class RequestTimeout;
class Timeout;
class CommandPoint;
class Expected;
class Confirmed;
class Completed;
class KnownCompleted;
class Flush;
class Gap;

} // namespace session


namespace execution {

const uint8_t CODE=0x3;
extern const char* NAME;

enum ErrorCode {
    UNAUTHORIZED_ACCESS = 403,
    NOT_FOUND = 404,
    RESOURCE_LOCKED = 405,
    PRECONDITION_FAILED = 406,
    RESOURCE_DELETED = 408,
    ILLEGAL_STATE = 409,
    COMMAND_INVALID = 503,
    RESOURCE_LIMIT_EXCEEDED = 506,
    NOT_ALLOWED = 530,
    ILLEGAL_ARGUMENT = 531,
    NOT_IMPLEMENTED = 540,
    INTERNAL_ERROR = 541,
    INVALID_ARGUMENT = 542
};
inline SerializeAs<ErrorCode, uint8_t> serializable(ErrorCode& e) {
    return SerializeAs<ErrorCode, uint8_t>(e);
}
class Sync;
class Result;
class Exception;

} // namespace execution


namespace message {

const uint8_t CODE=0x4;
extern const char* NAME;

typedef Str8 Destination;

enum AcceptMode {
    EXPLICIT = 0,
    NONE = 1
};
inline SerializeAs<AcceptMode, uint8_t> serializable(AcceptMode& e) {
    return SerializeAs<AcceptMode, uint8_t>(e);
}

enum AcquireMode {
    PRE_ACQUIRED = 0,
    NOT_ACQUIRED = 1
};
inline SerializeAs<AcquireMode, uint8_t> serializable(AcquireMode& e) {
    return SerializeAs<AcquireMode, uint8_t>(e);
}

enum RejectCode {
    UNSPECIFIED = 0,
    UNROUTABLE = 1,
    IMMEDIATE = 2
};
inline SerializeAs<RejectCode, uint8_t> serializable(RejectCode& e) {
    return SerializeAs<RejectCode, uint8_t>(e);
}

typedef Str16 ResumeId;

enum DeliveryMode {
    NON_PERSISTENT = 1,
    PERSISTENT = 2
};
inline SerializeAs<DeliveryMode, uint8_t> serializable(DeliveryMode& e) {
    return SerializeAs<DeliveryMode, uint8_t>(e);
}

enum DeliveryPriority {
    LOWEST = 0,
    LOWER = 1,
    LOW = 2,
    BELOW_AVERAGE = 3,
    MEDIUM = 4,
    ABOVE_AVERAGE = 5,
    HIGH = 6,
    HIGHER = 7,
    VERY_HIGH = 8,
    HIGHEST = 9
};
inline SerializeAs<DeliveryPriority, uint8_t> serializable(DeliveryPriority& e) {
    return SerializeAs<DeliveryPriority, uint8_t>(e);
}
class DeliveryProperties;
class FragmentProperties;
class ReplyTo;
class MessageProperties;

enum FlowMode {
    CREDIT = 0,
    WINDOW = 1
};
inline SerializeAs<FlowMode, uint8_t> serializable(FlowMode& e) {
    return SerializeAs<FlowMode, uint8_t>(e);
}

enum CreditUnit {
    MESSAGE = 0,
    BYTE = 1
};
inline SerializeAs<CreditUnit, uint8_t> serializable(CreditUnit& e) {
    return SerializeAs<CreditUnit, uint8_t>(e);
}
class Transfer;
class Accept;
class Reject;
class Release;
class Acquire;
class Acquired;
class Resume;
class MessageResumeResult;
class Subscribe;
class Cancel;
class SetFlowMode;
class Flow;
class Flush;
class Stop;

} // namespace message


namespace tx {

const uint8_t CODE=0x5;
extern const char* NAME;
class Select;
class Commit;
class Rollback;

} // namespace tx


namespace dtx {

const uint8_t CODE=0x6;
extern const char* NAME;

enum XaStatus {
    XA_OK = 0,
    XA_RBROLLBACK = 1,
    XA_RBTIMEOUT = 2,
    XA_HEURHAZ = 3,
    XA_HEURCOM = 4,
    XA_HEURRB = 5,
    XA_HEURMIX = 6,
    XA_RDONLY = 7
};
inline SerializeAs<XaStatus, uint8_t> serializable(XaStatus& e) {
    return SerializeAs<XaStatus, uint8_t>(e);
}
class XaResult;
class Xid;
class Select;
class Start;
class End;
class Commit;
class Forget;
class GetTimeout;
class GetTimeoutResult;
class Prepare;
class Recover;
class RecoverResult;
class Rollback;
class SetTimeout;

} // namespace dtx


namespace exchange {

const uint8_t CODE=0x7;
extern const char* NAME;

typedef Str8 Name;
class Declare;
class Delete;
class Query;
class ExchangeQueryResult;
class Bind;
class Unbind;
class Bound;
class ExchangeBoundResult;

} // namespace exchange


namespace queue {

const uint8_t CODE=0x8;
extern const char* NAME;

typedef Str8 Name;
class Declare;
class Delete;
class Purge;
class Query;
class QueueQueryResult;

} // namespace queue


namespace file {

const uint8_t CODE=0x9;
extern const char* NAME;
class FileProperties;

enum ReturnCode {
    CONTENT_TOO_LARGE = 311,
    NO_ROUTE = 312,
    NO_CONSUMERS = 313
};
inline SerializeAs<ReturnCode, uint8_t> serializable(ReturnCode& e) {
    return SerializeAs<ReturnCode, uint8_t>(e);
}
class Qos;
class QosOk;
class Consume;
class ConsumeOk;
class Cancel;
class Open;
class OpenOk;
class Stage;
class Publish;
class Return;
class Deliver;
class Ack;
class Reject;

} // namespace file


namespace stream {

const uint8_t CODE=0xa;
extern const char* NAME;
class StreamProperties;

enum ReturnCode {
    CONTENT_TOO_LARGE = 311,
    NO_ROUTE = 312,
    NO_CONSUMERS = 313
};
inline SerializeAs<ReturnCode, uint8_t> serializable(ReturnCode& e) {
    return SerializeAs<ReturnCode, uint8_t>(e);
}
class Qos;
class QosOk;
class Consume;
class ConsumeOk;
class Cancel;
class Publish;
class Return;
class Deliver;

} // namespace stream


namespace cluster {

const uint8_t CODE=0x80;
extern const char* NAME;
class UpdateRequest;
class UpdateOffer;
class RetractOffer;

enum StoreState {
    NO_STORE = 0,
    EMPTY_STORE = 1,
    CLEAN_STORE = 2,
    DIRTY_STORE = 3
};
inline SerializeAs<StoreState, uint8_t> serializable(StoreState& e) {
    return SerializeAs<StoreState, uint8_t>(e);
}
class InitialStatus;
class Ready;
class ConfigChange;
class MessageExpired;

enum ErrorType {
    NONE = 0,
    SESSION = 1,
    CONNECTION = 2
};
inline SerializeAs<ErrorType, uint8_t> serializable(ErrorType& e) {
    return SerializeAs<ErrorType, uint8_t>(e);
}
class ErrorCheck;
class TimerWakeup;
class TimerDrop;
class Shutdown;
class DeliverToQueue;

} // namespace cluster


namespace cluster_connection {

const uint8_t CODE=0x81;
extern const char* NAME;
class Announce;
class DeliverClose;
class DeliverDoOutput;
class Abort;
class ShadowSetUser;
class ShadowPrepare;
class ConsumerState;
class DeliveryRecord;
class TxStart;
class TxAccept;
class TxDequeue;
class TxEnqueue;
class TxPublish;
class TxEnd;
class AccumulatedAck;
class OutputTask;
class SessionState;
class ShadowReady;
class Membership;
class RetractOffer;
class QueuePosition;
class Exchange;
class Queue;
class ExpiryId;
class AddQueueListener;
class ManagementSetupState;
class Config;

} // namespace cluster_connection


}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_SPECIFICATION_FWD_H*/
