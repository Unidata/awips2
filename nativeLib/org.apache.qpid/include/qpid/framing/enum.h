#ifndef QPID_FRAMING_ENUM_H
#define QPID_FRAMING_ENUM_H
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



namespace qpid {
namespace framing {

enum SegmentType {
    SEGMENT_TYPE_CONTROL=0,
    SEGMENT_TYPE_COMMAND=1,
    SEGMENT_TYPE_HEADER=2,
    SEGMENT_TYPE_BODY=3
};
enum Track {
    TRACK_CONTROL=0,
    TRACK_COMMAND=1
};

namespace connection {

enum CloseCode {
    CLOSE_CODE_NORMAL=200,
    CLOSE_CODE_CONNECTION_FORCED=320,
    CLOSE_CODE_INVALID_PATH=402,
    CLOSE_CODE_FRAMING_ERROR=501
};

} // namespace connection


namespace session {

enum DetachCode {
    DETACH_CODE_NORMAL=0,
    DETACH_CODE_SESSION_BUSY=1,
    DETACH_CODE_TRANSPORT_BUSY=2,
    DETACH_CODE_NOT_ATTACHED=3,
    DETACH_CODE_UNKNOWN_IDS=4
};

} // namespace session


namespace execution {

enum ErrorCode {
    ERROR_CODE_UNAUTHORIZED_ACCESS=403,
    ERROR_CODE_NOT_FOUND=404,
    ERROR_CODE_RESOURCE_LOCKED=405,
    ERROR_CODE_PRECONDITION_FAILED=406,
    ERROR_CODE_RESOURCE_DELETED=408,
    ERROR_CODE_ILLEGAL_STATE=409,
    ERROR_CODE_COMMAND_INVALID=503,
    ERROR_CODE_RESOURCE_LIMIT_EXCEEDED=506,
    ERROR_CODE_NOT_ALLOWED=530,
    ERROR_CODE_ILLEGAL_ARGUMENT=531,
    ERROR_CODE_NOT_IMPLEMENTED=540,
    ERROR_CODE_INTERNAL_ERROR=541,
    ERROR_CODE_INVALID_ARGUMENT=542
};

} // namespace execution


namespace message {

enum AcceptMode {
    ACCEPT_MODE_EXPLICIT=0,
    ACCEPT_MODE_NONE=1
};
enum AcquireMode {
    ACQUIRE_MODE_PRE_ACQUIRED=0,
    ACQUIRE_MODE_NOT_ACQUIRED=1
};
enum RejectCode {
    REJECT_CODE_UNSPECIFIED=0,
    REJECT_CODE_UNROUTABLE=1,
    REJECT_CODE_IMMEDIATE=2
};
enum DeliveryMode {
    DELIVERY_MODE_NON_PERSISTENT=1,
    DELIVERY_MODE_PERSISTENT=2
};
enum DeliveryPriority {
    DELIVERY_PRIORITY_LOWEST=0,
    DELIVERY_PRIORITY_LOWER=1,
    DELIVERY_PRIORITY_LOW=2,
    DELIVERY_PRIORITY_BELOW_AVERAGE=3,
    DELIVERY_PRIORITY_MEDIUM=4,
    DELIVERY_PRIORITY_ABOVE_AVERAGE=5,
    DELIVERY_PRIORITY_HIGH=6,
    DELIVERY_PRIORITY_HIGHER=7,
    DELIVERY_PRIORITY_VERY_HIGH=8,
    DELIVERY_PRIORITY_HIGHEST=9
};
enum FlowMode {
    FLOW_MODE_CREDIT=0,
    FLOW_MODE_WINDOW=1
};
enum CreditUnit {
    CREDIT_UNIT_MESSAGE=0,
    CREDIT_UNIT_BYTE=1
};

} // namespace message


namespace dtx {

enum XaStatus {
    XA_STATUS_XA_OK=0,
    XA_STATUS_XA_RBROLLBACK=1,
    XA_STATUS_XA_RBTIMEOUT=2,
    XA_STATUS_XA_HEURHAZ=3,
    XA_STATUS_XA_HEURCOM=4,
    XA_STATUS_XA_HEURRB=5,
    XA_STATUS_XA_HEURMIX=6,
    XA_STATUS_XA_RDONLY=7
};

} // namespace dtx


namespace file {

enum ReturnCode {
    RETURN_CODE_CONTENT_TOO_LARGE=311,
    RETURN_CODE_NO_ROUTE=312,
    RETURN_CODE_NO_CONSUMERS=313
};

} // namespace file


namespace stream {

enum ReturnCode {
    RETURN_CODE_CONTENT_TOO_LARGE=311,
    RETURN_CODE_NO_ROUTE=312,
    RETURN_CODE_NO_CONSUMERS=313
};

} // namespace stream


namespace cluster {

enum StoreState {
    STORE_STATE_NO_STORE=0,
    STORE_STATE_EMPTY_STORE=1,
    STORE_STATE_CLEAN_STORE=2,
    STORE_STATE_DIRTY_STORE=3
};
enum ErrorType {
    ERROR_TYPE_NONE=0,
    ERROR_TYPE_SESSION=1,
    ERROR_TYPE_CONNECTION=2
};

} // namespace cluster


}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_ENUM_H*/
