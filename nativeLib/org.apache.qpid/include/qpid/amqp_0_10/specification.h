#ifndef QPID_AMQP_0_10_SPECIFICATION_H
#define QPID_AMQP_0_10_SPECIFICATION_H
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


#include "qpid/amqp_0_10/specification_fwd.h"
#include "qpid/amqp_0_10/Map.h"
#include "qpid/amqp_0_10/Array.h"
#include "qpid/amqp_0_10/UnknownType.h"
#include "qpid/amqp_0_10/Struct32.h"
#include "qpid/amqp_0_10/Control.h"
#include "qpid/amqp_0_10/Command.h"
#include "qpid/amqp_0_10/Packer.h"
#include <iosfwd>

namespace qpid {
namespace amqp_0_10 {


namespace connection {


struct Start:
    public Control
{
    Map serverProperties;
    Str16Array mechanisms;
    Str16Array locales;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit Start(
        const Map& serverProperties_=Map(),
        const Str16Array& mechanisms_=Str16Array(),
        const Str16Array& locales_=Str16Array()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(serverProperties)(mechanisms)(locales);
    }
    
    struct Handler
    {
        void connectionStart(
            const Map& serverProperties_,
            const Str16Array& mechanisms_,
            const Str16Array& locales_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionStart(serverProperties, mechanisms, locales );
    }
};
inline Packer<Start> serializable(Start& x) { return Packer<Start>(x); }
std::ostream& operator << (std::ostream&, const Start&);
bool operator==(const Start&, const Start&);

struct StartOk:
    public Control
{
    Map clientProperties;
    Str8 mechanism;
    Vbin32 response;
    Str8 locale;
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit StartOk(
        const Map& clientProperties_=Map(),
        const Str8& mechanism_=Str8(),
        const Vbin32& response_=Vbin32(),
        const Str8& locale_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(clientProperties)(mechanism)(response)(locale);
    }
    
    struct Handler
    {
        void connectionStartOk(
            const Map& clientProperties_,
            const Str8& mechanism_,
            const Vbin32& response_,
            const Str8& locale_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionStartOk(clientProperties, mechanism, response, locale );
    }
};
inline Packer<StartOk> serializable(StartOk& x) { return Packer<StartOk>(x); }
std::ostream& operator << (std::ostream&, const StartOk&);
bool operator==(const StartOk&, const StartOk&);

struct Secure:
    public Control
{
    Vbin32 challenge;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit Secure(const Vbin32& challenge_=Vbin32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(challenge);
    }
    
    struct Handler
    {
        void connectionSecure(
            const Vbin32& challenge_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionSecure(challenge );
    }
};
inline Packer<Secure> serializable(Secure& x) { return Packer<Secure>(x); }
std::ostream& operator << (std::ostream&, const Secure&);
bool operator==(const Secure&, const Secure&);

struct SecureOk:
    public Control
{
    Vbin32 response;
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit SecureOk(const Vbin32& response_=Vbin32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(response);
    }
    
    struct Handler
    {
        void connectionSecureOk(
            const Vbin32& response_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionSecureOk(response );
    }
};
inline Packer<SecureOk> serializable(SecureOk& x) { return Packer<SecureOk>(x); }
std::ostream& operator << (std::ostream&, const SecureOk&);
bool operator==(const SecureOk&, const SecureOk&);

struct Tune:
    public Control
{
    Uint16 channelMax;
    Uint16 maxFrameSize;
    Uint16 heartbeatMin;
    Uint16 heartbeatMax;
    
    static const char* NAME;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit Tune(
        Uint16 channelMax_=Uint16(),
        Uint16 maxFrameSize_=Uint16(),
        Uint16 heartbeatMin_=Uint16(),
        Uint16 heartbeatMax_=Uint16()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(channelMax)(maxFrameSize)(heartbeatMin)(heartbeatMax);
    }
    
    struct Handler
    {
        void connectionTune(
            Uint16 channelMax_,
            Uint16 maxFrameSize_,
            Uint16 heartbeatMin_,
            Uint16 heartbeatMax_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionTune(channelMax, maxFrameSize, heartbeatMin, heartbeatMax );
    }
};
inline Packer<Tune> serializable(Tune& x) { return Packer<Tune>(x); }
std::ostream& operator << (std::ostream&, const Tune&);
bool operator==(const Tune&, const Tune&);

struct TuneOk:
    public Control
{
    Uint16 channelMax;
    Uint16 maxFrameSize;
    Uint16 heartbeat;
    
    static const char* NAME;
    static const uint8_t CODE=0x6;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit TuneOk(
        Uint16 channelMax_=Uint16(),
        Uint16 maxFrameSize_=Uint16(),
        Uint16 heartbeat_=Uint16()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(channelMax)(maxFrameSize)(heartbeat);
    }
    
    struct Handler
    {
        void connectionTuneOk(
            Uint16 channelMax_,
            Uint16 maxFrameSize_,
            Uint16 heartbeat_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionTuneOk(channelMax, maxFrameSize, heartbeat );
    }
};
inline Packer<TuneOk> serializable(TuneOk& x) { return Packer<TuneOk>(x); }
std::ostream& operator << (std::ostream&, const TuneOk&);
bool operator==(const TuneOk&, const TuneOk&);

struct Open:
    public Control
{
    Str8 virtualHost;
    Str16Array capabilities;
    Bit insist;
    
    static const char* NAME;
    static const uint8_t CODE=0x7;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit Open(
        const Str8& virtualHost_=Str8(),
        const Str16Array& capabilities_=Str16Array(),
        Bit insist_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(virtualHost)(capabilities)(insist);
    }
    
    struct Handler
    {
        void connectionOpen(
            const Str8& virtualHost_,
            const Str16Array& capabilities_,
            Bit insist_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionOpen(virtualHost, capabilities, insist );
    }
};
inline Packer<Open> serializable(Open& x) { return Packer<Open>(x); }
std::ostream& operator << (std::ostream&, const Open&);
bool operator==(const Open&, const Open&);

struct OpenOk:
    public Control
{
    AmqpHostArray knownHosts;
    
    static const char* NAME;
    static const uint8_t CODE=0x8;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit OpenOk(const connection::AmqpHostArray& knownHosts_=connection::AmqpHostArray());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(knownHosts);
    }
    
    struct Handler
    {
        void connectionOpenOk(
            const connection::AmqpHostArray& knownHosts_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionOpenOk(knownHosts );
    }
};
inline Packer<OpenOk> serializable(OpenOk& x) { return Packer<OpenOk>(x); }
std::ostream& operator << (std::ostream&, const OpenOk&);
bool operator==(const OpenOk&, const OpenOk&);

struct Redirect:
    public Control
{
    AmqpHostUrl host;
    AmqpHostArray knownHosts;
    
    static const char* NAME;
    static const uint8_t CODE=0x9;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit Redirect(
        const connection::AmqpHostUrl& host_=connection::AmqpHostUrl(),
        const connection::AmqpHostArray& knownHosts_=connection::AmqpHostArray()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(host)(knownHosts);
    }
    
    struct Handler
    {
        void connectionRedirect(
            const connection::AmqpHostUrl& host_,
            const connection::AmqpHostArray& knownHosts_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionRedirect(host, knownHosts );
    }
};
inline Packer<Redirect> serializable(Redirect& x) { return Packer<Redirect>(x); }
std::ostream& operator << (std::ostream&, const Redirect&);
bool operator==(const Redirect&, const Redirect&);

struct Heartbeat:
    public Control
{
    
    static const char* NAME;
    static const uint8_t CODE=0xa;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit Heartbeat();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void connectionHeartbeat(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionHeartbeat( );
    }
};
inline Packer<Heartbeat> serializable(Heartbeat& x) { return Packer<Heartbeat>(x); }
std::ostream& operator << (std::ostream&, const Heartbeat&);
bool operator==(const Heartbeat&, const Heartbeat&);

struct Close:
    public Control
{
    CloseCode replyCode;
    Str8 replyText;
    
    static const char* NAME;
    static const uint8_t CODE=0xb;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit Close(
        const connection::CloseCode& replyCode_=connection::CloseCode(),
        const Str8& replyText_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(replyCode)(replyText);
    }
    
    struct Handler
    {
        void connectionClose(
            const connection::CloseCode& replyCode_,
            const Str8& replyText_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionClose(replyCode, replyText );
    }
};
inline Packer<Close> serializable(Close& x) { return Packer<Close>(x); }
std::ostream& operator << (std::ostream&, const Close&);
bool operator==(const Close&, const Close&);

struct CloseOk:
    public Control
{
    
    static const char* NAME;
    static const uint8_t CODE=0xc;
    static const uint8_t CLASS_CODE=connection::CODE;
    static const char* CLASS_NAME;
    explicit CloseOk();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void connectionCloseOk(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.connectionCloseOk( );
    }
};
inline Packer<CloseOk> serializable(CloseOk& x) { return Packer<CloseOk>(x); }
std::ostream& operator << (std::ostream&, const CloseOk&);
bool operator==(const CloseOk&, const CloseOk&);

} // namespace connection


namespace session {


struct Attach:
    public Control
{
    Name name;
    Bit force;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Attach(
        const session::Name& name_=session::Name(),
        Bit force_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(name)(force);
    }
    
    struct Handler
    {
        void sessionAttach(
            const session::Name& name_,
            Bit force_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionAttach(name, force );
    }
};
inline Packer<Attach> serializable(Attach& x) { return Packer<Attach>(x); }
std::ostream& operator << (std::ostream&, const Attach&);
bool operator==(const Attach&, const Attach&);

struct Attached:
    public Control
{
    Name name;
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Attached(const session::Name& name_=session::Name());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(name);
    }
    
    struct Handler
    {
        void sessionAttached(
            const session::Name& name_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionAttached(name );
    }
};
inline Packer<Attached> serializable(Attached& x) { return Packer<Attached>(x); }
std::ostream& operator << (std::ostream&, const Attached&);
bool operator==(const Attached&, const Attached&);

struct Detach:
    public Control
{
    Name name;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Detach(const session::Name& name_=session::Name());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(name);
    }
    
    struct Handler
    {
        void sessionDetach(
            const session::Name& name_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionDetach(name );
    }
};
inline Packer<Detach> serializable(Detach& x) { return Packer<Detach>(x); }
std::ostream& operator << (std::ostream&, const Detach&);
bool operator==(const Detach&, const Detach&);

struct Detached:
    public Control
{
    Name name;
    DetachCode code;
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Detached(
        const session::Name& name_=session::Name(),
        const session::DetachCode& code_=session::DetachCode()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(name)(code);
    }
    
    struct Handler
    {
        void sessionDetached(
            const session::Name& name_,
            const session::DetachCode& code_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionDetached(name, code );
    }
};
inline Packer<Detached> serializable(Detached& x) { return Packer<Detached>(x); }
std::ostream& operator << (std::ostream&, const Detached&);
bool operator==(const Detached&, const Detached&);

struct RequestTimeout:
    public Control
{
    Uint32 timeout;
    
    static const char* NAME;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit RequestTimeout(Uint32 timeout_=Uint32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(timeout);
    }
    
    struct Handler
    {
        void sessionRequestTimeout(
            Uint32 timeout_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionRequestTimeout(timeout );
    }
};
inline Packer<RequestTimeout> serializable(RequestTimeout& x) { return Packer<RequestTimeout>(x); }
std::ostream& operator << (std::ostream&, const RequestTimeout&);
bool operator==(const RequestTimeout&, const RequestTimeout&);

struct Timeout:
    public Control
{
    Uint32 timeout;
    
    static const char* NAME;
    static const uint8_t CODE=0x6;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Timeout(Uint32 timeout_=Uint32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(timeout);
    }
    
    struct Handler
    {
        void sessionTimeout(
            Uint32 timeout_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionTimeout(timeout );
    }
};
inline Packer<Timeout> serializable(Timeout& x) { return Packer<Timeout>(x); }
std::ostream& operator << (std::ostream&, const Timeout&);
bool operator==(const Timeout&, const Timeout&);

struct CommandPoint:
    public Control
{
    SequenceNo commandId;
    Uint64 commandOffset;
    
    static const char* NAME;
    static const uint8_t CODE=0x7;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit CommandPoint(
        const SequenceNo& commandId_=SequenceNo(),
        Uint64 commandOffset_=Uint64()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commandId)(commandOffset);
    }
    
    struct Handler
    {
        void sessionCommandPoint(
            const SequenceNo& commandId_,
            Uint64 commandOffset_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionCommandPoint(commandId, commandOffset );
    }
};
inline Packer<CommandPoint> serializable(CommandPoint& x) { return Packer<CommandPoint>(x); }
std::ostream& operator << (std::ostream&, const CommandPoint&);
bool operator==(const CommandPoint&, const CommandPoint&);

struct Expected:
    public Control
{
    Commands commands;
    CommandFragments fragments;
    
    static const char* NAME;
    static const uint8_t CODE=0x8;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Expected(
        const session::Commands& commands_=session::Commands(),
        const session::CommandFragments& fragments_=session::CommandFragments()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commands)(fragments);
    }
    
    struct Handler
    {
        void sessionExpected(
            const session::Commands& commands_,
            const session::CommandFragments& fragments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionExpected(commands, fragments );
    }
};
inline Packer<Expected> serializable(Expected& x) { return Packer<Expected>(x); }
std::ostream& operator << (std::ostream&, const Expected&);
bool operator==(const Expected&, const Expected&);

struct Confirmed:
    public Control
{
    Commands commands;
    CommandFragments fragments;
    
    static const char* NAME;
    static const uint8_t CODE=0x9;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Confirmed(
        const session::Commands& commands_=session::Commands(),
        const session::CommandFragments& fragments_=session::CommandFragments()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commands)(fragments);
    }
    
    struct Handler
    {
        void sessionConfirmed(
            const session::Commands& commands_,
            const session::CommandFragments& fragments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionConfirmed(commands, fragments );
    }
};
inline Packer<Confirmed> serializable(Confirmed& x) { return Packer<Confirmed>(x); }
std::ostream& operator << (std::ostream&, const Confirmed&);
bool operator==(const Confirmed&, const Confirmed&);

struct Completed:
    public Control
{
    Commands commands;
    Bit timelyReply;
    
    static const char* NAME;
    static const uint8_t CODE=0xa;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Completed(
        const session::Commands& commands_=session::Commands(),
        Bit timelyReply_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commands)(timelyReply);
    }
    
    struct Handler
    {
        void sessionCompleted(
            const session::Commands& commands_,
            Bit timelyReply_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionCompleted(commands, timelyReply );
    }
};
inline Packer<Completed> serializable(Completed& x) { return Packer<Completed>(x); }
std::ostream& operator << (std::ostream&, const Completed&);
bool operator==(const Completed&, const Completed&);

struct KnownCompleted:
    public Control
{
    Commands commands;
    
    static const char* NAME;
    static const uint8_t CODE=0xb;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit KnownCompleted(const session::Commands& commands_=session::Commands());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commands);
    }
    
    struct Handler
    {
        void sessionKnownCompleted(
            const session::Commands& commands_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionKnownCompleted(commands );
    }
};
inline Packer<KnownCompleted> serializable(KnownCompleted& x) { return Packer<KnownCompleted>(x); }
std::ostream& operator << (std::ostream&, const KnownCompleted&);
bool operator==(const KnownCompleted&, const KnownCompleted&);

struct Flush:
    public Control
{
    Bit expected;
    Bit confirmed;
    Bit completed;
    
    static const char* NAME;
    static const uint8_t CODE=0xc;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Flush(
        Bit expected_=Bit(),
        Bit confirmed_=Bit(),
        Bit completed_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(expected)(confirmed)(completed);
    }
    
    struct Handler
    {
        void sessionFlush(
            Bit expected_,
            Bit confirmed_,
            Bit completed_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionFlush(expected, confirmed, completed );
    }
};
inline Packer<Flush> serializable(Flush& x) { return Packer<Flush>(x); }
std::ostream& operator << (std::ostream&, const Flush&);
bool operator==(const Flush&, const Flush&);

struct Gap:
    public Control
{
    Commands commands;
    
    static const char* NAME;
    static const uint8_t CODE=0xd;
    static const uint8_t CLASS_CODE=session::CODE;
    static const char* CLASS_NAME;
    explicit Gap(const session::Commands& commands_=session::Commands());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commands);
    }
    
    struct Handler
    {
        void sessionGap(
            const session::Commands& commands_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.sessionGap(commands );
    }
};
inline Packer<Gap> serializable(Gap& x) { return Packer<Gap>(x); }
std::ostream& operator << (std::ostream&, const Gap&);
bool operator==(const Gap&, const Gap&);

} // namespace session


namespace execution {


struct Sync:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=execution::CODE;
    static const char* CLASS_NAME;
    explicit Sync();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void executionSync(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.executionSync( );
    }
};
inline CommandPacker<Sync> serializable(Sync& x) { return CommandPacker<Sync>(x); }
std::ostream& operator << (std::ostream&, const Sync&);
bool operator==(const Sync&, const Sync&);

struct Result:
    public Command
{
    SequenceNo commandId;
    Struct32 value;
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=execution::CODE;
    static const char* CLASS_NAME;
    explicit Result(
        const SequenceNo& commandId_=SequenceNo(),
        const Struct32& value_=Struct32()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commandId)(value);
    }
    
    struct Handler
    {
        void executionResult(
            const SequenceNo& commandId_,
            const Struct32& value_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.executionResult(commandId, value );
    }
};
inline CommandPacker<Result> serializable(Result& x) { return CommandPacker<Result>(x); }
std::ostream& operator << (std::ostream&, const Result&);
bool operator==(const Result&, const Result&);

struct Exception:
    public Command
{
    ErrorCode errorCode;
    SequenceNo commandId;
    Uint8 classCode;
    Uint8 commandCode;
    Uint8 fieldIndex;
    Str16 description;
    Map errorInfo;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=execution::CODE;
    static const char* CLASS_NAME;
    explicit Exception(
        const execution::ErrorCode& errorCode_=execution::ErrorCode(),
        const SequenceNo& commandId_=SequenceNo(),
        Uint8 classCode_=Uint8(),
        Uint8 commandCode_=Uint8(),
        Uint8 fieldIndex_=Uint8(),
        const Str16& description_=Str16(),
        const Map& errorInfo_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(errorCode)(commandId)(classCode)(commandCode)(fieldIndex)(description)(errorInfo);
    }
    
    struct Handler
    {
        void executionException(
            const execution::ErrorCode& errorCode_,
            const SequenceNo& commandId_,
            Uint8 classCode_,
            Uint8 commandCode_,
            Uint8 fieldIndex_,
            const Str16& description_,
            const Map& errorInfo_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.executionException(errorCode, commandId, classCode, commandCode, fieldIndex, description, errorInfo );
    }
};
inline CommandPacker<Exception> serializable(Exception& x) { return CommandPacker<Exception>(x); }
std::ostream& operator << (std::ostream&, const Exception&);
bool operator==(const Exception&, const Exception&);

} // namespace execution


namespace message {


struct Transfer:
    public Command
{
    Destination destination;
    AcceptMode acceptMode;
    AcquireMode acquireMode;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Transfer(
        const message::Destination& destination_=message::Destination(),
        const message::AcceptMode& acceptMode_=message::AcceptMode(),
        const message::AcquireMode& acquireMode_=message::AcquireMode()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(destination)(acceptMode)(acquireMode);
    }
    
    struct Handler
    {
        void messageTransfer(
            const message::Destination& destination_,
            const message::AcceptMode& acceptMode_,
            const message::AcquireMode& acquireMode_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageTransfer(destination, acceptMode, acquireMode );
    }
};
inline CommandPacker<Transfer> serializable(Transfer& x) { return CommandPacker<Transfer>(x); }
std::ostream& operator << (std::ostream&, const Transfer&);
bool operator==(const Transfer&, const Transfer&);

struct Accept:
    public Command
{
    session::Commands transfers;
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Accept(const session::Commands& transfers_=session::Commands());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(transfers);
    }
    
    struct Handler
    {
        void messageAccept(
            const session::Commands& transfers_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageAccept(transfers );
    }
};
inline CommandPacker<Accept> serializable(Accept& x) { return CommandPacker<Accept>(x); }
std::ostream& operator << (std::ostream&, const Accept&);
bool operator==(const Accept&, const Accept&);

struct Reject:
    public Command
{
    session::Commands transfers;
    RejectCode code;
    Str8 text;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Reject(
        const session::Commands& transfers_=session::Commands(),
        const message::RejectCode& code_=message::RejectCode(),
        const Str8& text_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(transfers)(code)(text);
    }
    
    struct Handler
    {
        void messageReject(
            const session::Commands& transfers_,
            const message::RejectCode& code_,
            const Str8& text_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageReject(transfers, code, text );
    }
};
inline CommandPacker<Reject> serializable(Reject& x) { return CommandPacker<Reject>(x); }
std::ostream& operator << (std::ostream&, const Reject&);
bool operator==(const Reject&, const Reject&);

struct Release:
    public Command
{
    session::Commands transfers;
    Bit setRedelivered;
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Release(
        const session::Commands& transfers_=session::Commands(),
        Bit setRedelivered_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(transfers)(setRedelivered);
    }
    
    struct Handler
    {
        void messageRelease(
            const session::Commands& transfers_,
            Bit setRedelivered_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageRelease(transfers, setRedelivered );
    }
};
inline CommandPacker<Release> serializable(Release& x) { return CommandPacker<Release>(x); }
std::ostream& operator << (std::ostream&, const Release&);
bool operator==(const Release&, const Release&);

struct Acquire:
    public Command
{
    session::Commands transfers;
    
    static const char* NAME;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Acquire(const session::Commands& transfers_=session::Commands());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(transfers);
    }
    
    struct Handler
    {
        void messageAcquire(
            const session::Commands& transfers_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageAcquire(transfers );
    }
};
inline CommandPacker<Acquire> serializable(Acquire& x) { return CommandPacker<Acquire>(x); }
std::ostream& operator << (std::ostream&, const Acquire&);
bool operator==(const Acquire&, const Acquire&);

struct Resume:
    public Command
{
    Destination destination;
    ResumeId resumeId;
    
    static const char* NAME;
    static const uint8_t CODE=0x6;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Resume(
        const message::Destination& destination_=message::Destination(),
        const message::ResumeId& resumeId_=message::ResumeId()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(destination)(resumeId);
    }
    
    struct Handler
    {
        void messageResume(
            const message::Destination& destination_,
            const message::ResumeId& resumeId_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageResume(destination, resumeId );
    }
};
inline CommandPacker<Resume> serializable(Resume& x) { return CommandPacker<Resume>(x); }
std::ostream& operator << (std::ostream&, const Resume&);
bool operator==(const Resume&, const Resume&);

struct Subscribe:
    public Command
{
    queue::Name queue;
    Destination destination;
    AcceptMode acceptMode;
    AcquireMode acquireMode;
    Bit exclusive;
    ResumeId resumeId;
    Uint64 resumeTtl;
    Map arguments;
    
    static const char* NAME;
    static const uint8_t CODE=0x7;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Subscribe(
        const queue::Name& queue_=queue::Name(),
        const message::Destination& destination_=message::Destination(),
        const message::AcceptMode& acceptMode_=message::AcceptMode(),
        const message::AcquireMode& acquireMode_=message::AcquireMode(),
        Bit exclusive_=Bit(),
        const message::ResumeId& resumeId_=message::ResumeId(),
        Uint64 resumeTtl_=Uint64(),
        const Map& arguments_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(destination)(acceptMode)(acquireMode)(exclusive)(resumeId)(resumeTtl)(arguments);
    }
    
    struct Handler
    {
        void messageSubscribe(
            const queue::Name& queue_,
            const message::Destination& destination_,
            const message::AcceptMode& acceptMode_,
            const message::AcquireMode& acquireMode_,
            Bit exclusive_,
            const message::ResumeId& resumeId_,
            Uint64 resumeTtl_,
            const Map& arguments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageSubscribe(queue, destination, acceptMode, acquireMode, exclusive, resumeId, resumeTtl, arguments );
    }
};
inline CommandPacker<Subscribe> serializable(Subscribe& x) { return CommandPacker<Subscribe>(x); }
std::ostream& operator << (std::ostream&, const Subscribe&);
bool operator==(const Subscribe&, const Subscribe&);

struct Cancel:
    public Command
{
    Destination destination;
    
    static const char* NAME;
    static const uint8_t CODE=0x8;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Cancel(const message::Destination& destination_=message::Destination());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(destination);
    }
    
    struct Handler
    {
        void messageCancel(
            const message::Destination& destination_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageCancel(destination );
    }
};
inline CommandPacker<Cancel> serializable(Cancel& x) { return CommandPacker<Cancel>(x); }
std::ostream& operator << (std::ostream&, const Cancel&);
bool operator==(const Cancel&, const Cancel&);

struct SetFlowMode:
    public Command
{
    Destination destination;
    FlowMode flowMode;
    
    static const char* NAME;
    static const uint8_t CODE=0x9;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit SetFlowMode(
        const message::Destination& destination_=message::Destination(),
        const message::FlowMode& flowMode_=message::FlowMode()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(destination)(flowMode);
    }
    
    struct Handler
    {
        void messageSetFlowMode(
            const message::Destination& destination_,
            const message::FlowMode& flowMode_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageSetFlowMode(destination, flowMode );
    }
};
inline CommandPacker<SetFlowMode> serializable(SetFlowMode& x) { return CommandPacker<SetFlowMode>(x); }
std::ostream& operator << (std::ostream&, const SetFlowMode&);
bool operator==(const SetFlowMode&, const SetFlowMode&);

struct Flow:
    public Command
{
    Destination destination;
    CreditUnit unit;
    Uint32 value;
    
    static const char* NAME;
    static const uint8_t CODE=0xa;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Flow(
        const message::Destination& destination_=message::Destination(),
        const message::CreditUnit& unit_=message::CreditUnit(),
        Uint32 value_=Uint32()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(destination)(unit)(value);
    }
    
    struct Handler
    {
        void messageFlow(
            const message::Destination& destination_,
            const message::CreditUnit& unit_,
            Uint32 value_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageFlow(destination, unit, value );
    }
};
inline CommandPacker<Flow> serializable(Flow& x) { return CommandPacker<Flow>(x); }
std::ostream& operator << (std::ostream&, const Flow&);
bool operator==(const Flow&, const Flow&);

struct Flush:
    public Command
{
    Destination destination;
    
    static const char* NAME;
    static const uint8_t CODE=0xb;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Flush(const message::Destination& destination_=message::Destination());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(destination);
    }
    
    struct Handler
    {
        void messageFlush(
            const message::Destination& destination_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageFlush(destination );
    }
};
inline CommandPacker<Flush> serializable(Flush& x) { return CommandPacker<Flush>(x); }
std::ostream& operator << (std::ostream&, const Flush&);
bool operator==(const Flush&, const Flush&);

struct Stop:
    public Command
{
    Destination destination;
    
    static const char* NAME;
    static const uint8_t CODE=0xc;
    static const uint8_t CLASS_CODE=message::CODE;
    static const char* CLASS_NAME;
    explicit Stop(const message::Destination& destination_=message::Destination());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(destination);
    }
    
    struct Handler
    {
        void messageStop(
            const message::Destination& destination_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.messageStop(destination );
    }
};
inline CommandPacker<Stop> serializable(Stop& x) { return CommandPacker<Stop>(x); }
std::ostream& operator << (std::ostream&, const Stop&);
bool operator==(const Stop&, const Stop&);

} // namespace message


namespace tx {


struct Select:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=tx::CODE;
    static const char* CLASS_NAME;
    explicit Select();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void txSelect(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.txSelect( );
    }
};
inline CommandPacker<Select> serializable(Select& x) { return CommandPacker<Select>(x); }
std::ostream& operator << (std::ostream&, const Select&);
bool operator==(const Select&, const Select&);

struct Commit:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=tx::CODE;
    static const char* CLASS_NAME;
    explicit Commit();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void txCommit(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.txCommit( );
    }
};
inline CommandPacker<Commit> serializable(Commit& x) { return CommandPacker<Commit>(x); }
std::ostream& operator << (std::ostream&, const Commit&);
bool operator==(const Commit&, const Commit&);

struct Rollback:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=tx::CODE;
    static const char* CLASS_NAME;
    explicit Rollback();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void txRollback(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.txRollback( );
    }
};
inline CommandPacker<Rollback> serializable(Rollback& x) { return CommandPacker<Rollback>(x); }
std::ostream& operator << (std::ostream&, const Rollback&);
bool operator==(const Rollback&, const Rollback&);

} // namespace tx


namespace dtx {


struct Select:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit Select();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void dtxSelect(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxSelect( );
    }
};
inline CommandPacker<Select> serializable(Select& x) { return CommandPacker<Select>(x); }
std::ostream& operator << (std::ostream&, const Select&);
bool operator==(const Select&, const Select&);

struct Start:
    public Command
{
    Xid xid;
    Bit join;
    Bit resume;
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit Start(
        const dtx::Xid& xid_=dtx::Xid(),
        Bit join_=Bit(),
        Bit resume_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(xid)(join)(resume);
    }
    
    struct Handler
    {
        void dtxStart(
            const dtx::Xid& xid_,
            Bit join_,
            Bit resume_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxStart(xid, join, resume );
    }
};
inline CommandPacker<Start> serializable(Start& x) { return CommandPacker<Start>(x); }
std::ostream& operator << (std::ostream&, const Start&);
bool operator==(const Start&, const Start&);

struct End:
    public Command
{
    Xid xid;
    Bit fail;
    Bit suspend;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit End(
        const dtx::Xid& xid_=dtx::Xid(),
        Bit fail_=Bit(),
        Bit suspend_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(xid)(fail)(suspend);
    }
    
    struct Handler
    {
        void dtxEnd(
            const dtx::Xid& xid_,
            Bit fail_,
            Bit suspend_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxEnd(xid, fail, suspend );
    }
};
inline CommandPacker<End> serializable(End& x) { return CommandPacker<End>(x); }
std::ostream& operator << (std::ostream&, const End&);
bool operator==(const End&, const End&);

struct Commit:
    public Command
{
    Xid xid;
    Bit onePhase;
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit Commit(
        const dtx::Xid& xid_=dtx::Xid(),
        Bit onePhase_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(xid)(onePhase);
    }
    
    struct Handler
    {
        void dtxCommit(
            const dtx::Xid& xid_,
            Bit onePhase_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxCommit(xid, onePhase );
    }
};
inline CommandPacker<Commit> serializable(Commit& x) { return CommandPacker<Commit>(x); }
std::ostream& operator << (std::ostream&, const Commit&);
bool operator==(const Commit&, const Commit&);

struct Forget:
    public Command
{
    Xid xid;
    
    static const char* NAME;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit Forget(const dtx::Xid& xid_=dtx::Xid());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(xid);
    }
    
    struct Handler
    {
        void dtxForget(
            const dtx::Xid& xid_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxForget(xid );
    }
};
inline CommandPacker<Forget> serializable(Forget& x) { return CommandPacker<Forget>(x); }
std::ostream& operator << (std::ostream&, const Forget&);
bool operator==(const Forget&, const Forget&);

struct GetTimeout:
    public Command
{
    Xid xid;
    
    static const char* NAME;
    static const uint8_t CODE=0x6;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit GetTimeout(const dtx::Xid& xid_=dtx::Xid());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(xid);
    }
    
    struct Handler
    {
        void dtxGetTimeout(
            const dtx::Xid& xid_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxGetTimeout(xid );
    }
};
inline CommandPacker<GetTimeout> serializable(GetTimeout& x) { return CommandPacker<GetTimeout>(x); }
std::ostream& operator << (std::ostream&, const GetTimeout&);
bool operator==(const GetTimeout&, const GetTimeout&);

struct Prepare:
    public Command
{
    Xid xid;
    
    static const char* NAME;
    static const uint8_t CODE=0x7;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit Prepare(const dtx::Xid& xid_=dtx::Xid());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(xid);
    }
    
    struct Handler
    {
        void dtxPrepare(
            const dtx::Xid& xid_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxPrepare(xid );
    }
};
inline CommandPacker<Prepare> serializable(Prepare& x) { return CommandPacker<Prepare>(x); }
std::ostream& operator << (std::ostream&, const Prepare&);
bool operator==(const Prepare&, const Prepare&);

struct Recover:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x8;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit Recover();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void dtxRecover(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxRecover( );
    }
};
inline CommandPacker<Recover> serializable(Recover& x) { return CommandPacker<Recover>(x); }
std::ostream& operator << (std::ostream&, const Recover&);
bool operator==(const Recover&, const Recover&);

struct Rollback:
    public Command
{
    Xid xid;
    
    static const char* NAME;
    static const uint8_t CODE=0x9;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit Rollback(const dtx::Xid& xid_=dtx::Xid());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(xid);
    }
    
    struct Handler
    {
        void dtxRollback(
            const dtx::Xid& xid_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxRollback(xid );
    }
};
inline CommandPacker<Rollback> serializable(Rollback& x) { return CommandPacker<Rollback>(x); }
std::ostream& operator << (std::ostream&, const Rollback&);
bool operator==(const Rollback&, const Rollback&);

struct SetTimeout:
    public Command
{
    Xid xid;
    Uint32 timeout;
    
    static const char* NAME;
    static const uint8_t CODE=0xa;
    static const uint8_t CLASS_CODE=dtx::CODE;
    static const char* CLASS_NAME;
    explicit SetTimeout(
        const dtx::Xid& xid_=dtx::Xid(),
        Uint32 timeout_=Uint32()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(xid)(timeout);
    }
    
    struct Handler
    {
        void dtxSetTimeout(
            const dtx::Xid& xid_,
            Uint32 timeout_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.dtxSetTimeout(xid, timeout );
    }
};
inline CommandPacker<SetTimeout> serializable(SetTimeout& x) { return CommandPacker<SetTimeout>(x); }
std::ostream& operator << (std::ostream&, const SetTimeout&);
bool operator==(const SetTimeout&, const SetTimeout&);

} // namespace dtx


namespace exchange {


struct Declare:
    public Command
{
    Name exchange;
    Str8 type;
    Name alternateExchange;
    Bit passive;
    Bit durable;
    Bit autoDelete;
    Map arguments;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=exchange::CODE;
    static const char* CLASS_NAME;
    explicit Declare(
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& type_=Str8(),
        const exchange::Name& alternateExchange_=exchange::Name(),
        Bit passive_=Bit(),
        Bit durable_=Bit(),
        Bit autoDelete_=Bit(),
        const Map& arguments_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(exchange)(type)(alternateExchange)(passive)(durable)(autoDelete)(arguments);
    }
    
    struct Handler
    {
        void exchangeDeclare(
            const exchange::Name& exchange_,
            const Str8& type_,
            const exchange::Name& alternateExchange_,
            Bit passive_,
            Bit durable_,
            Bit autoDelete_,
            const Map& arguments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.exchangeDeclare(exchange, type, alternateExchange, passive, durable, autoDelete, arguments );
    }
};
inline CommandPacker<Declare> serializable(Declare& x) { return CommandPacker<Declare>(x); }
std::ostream& operator << (std::ostream&, const Declare&);
bool operator==(const Declare&, const Declare&);

struct Delete:
    public Command
{
    Name exchange;
    Bit ifUnused;
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=exchange::CODE;
    static const char* CLASS_NAME;
    explicit Delete(
        const exchange::Name& exchange_=exchange::Name(),
        Bit ifUnused_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(exchange)(ifUnused);
    }
    
    struct Handler
    {
        void exchangeDelete(
            const exchange::Name& exchange_,
            Bit ifUnused_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.exchangeDelete(exchange, ifUnused );
    }
};
inline CommandPacker<Delete> serializable(Delete& x) { return CommandPacker<Delete>(x); }
std::ostream& operator << (std::ostream&, const Delete&);
bool operator==(const Delete&, const Delete&);

struct Query:
    public Command
{
    Str8 name;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=exchange::CODE;
    static const char* CLASS_NAME;
    explicit Query(const Str8& name_=Str8());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(name);
    }
    
    struct Handler
    {
        void exchangeQuery(
            const Str8& name_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.exchangeQuery(name );
    }
};
inline CommandPacker<Query> serializable(Query& x) { return CommandPacker<Query>(x); }
std::ostream& operator << (std::ostream&, const Query&);
bool operator==(const Query&, const Query&);

struct Bind:
    public Command
{
    queue::Name queue;
    Name exchange;
    Str8 bindingKey;
    Map arguments;
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=exchange::CODE;
    static const char* CLASS_NAME;
    explicit Bind(
        const queue::Name& queue_=queue::Name(),
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& bindingKey_=Str8(),
        const Map& arguments_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(exchange)(bindingKey)(arguments);
    }
    
    struct Handler
    {
        void exchangeBind(
            const queue::Name& queue_,
            const exchange::Name& exchange_,
            const Str8& bindingKey_,
            const Map& arguments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.exchangeBind(queue, exchange, bindingKey, arguments );
    }
};
inline CommandPacker<Bind> serializable(Bind& x) { return CommandPacker<Bind>(x); }
std::ostream& operator << (std::ostream&, const Bind&);
bool operator==(const Bind&, const Bind&);

struct Unbind:
    public Command
{
    queue::Name queue;
    Name exchange;
    Str8 bindingKey;
    
    static const char* NAME;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=exchange::CODE;
    static const char* CLASS_NAME;
    explicit Unbind(
        const queue::Name& queue_=queue::Name(),
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& bindingKey_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(exchange)(bindingKey);
    }
    
    struct Handler
    {
        void exchangeUnbind(
            const queue::Name& queue_,
            const exchange::Name& exchange_,
            const Str8& bindingKey_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.exchangeUnbind(queue, exchange, bindingKey );
    }
};
inline CommandPacker<Unbind> serializable(Unbind& x) { return CommandPacker<Unbind>(x); }
std::ostream& operator << (std::ostream&, const Unbind&);
bool operator==(const Unbind&, const Unbind&);

struct Bound:
    public Command
{
    Str8 exchange;
    Str8 queue;
    Str8 bindingKey;
    Map arguments;
    
    static const char* NAME;
    static const uint8_t CODE=0x6;
    static const uint8_t CLASS_CODE=exchange::CODE;
    static const char* CLASS_NAME;
    explicit Bound(
        const Str8& exchange_=Str8(),
        const Str8& queue_=Str8(),
        const Str8& bindingKey_=Str8(),
        const Map& arguments_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(exchange)(queue)(bindingKey)(arguments);
    }
    
    struct Handler
    {
        void exchangeBound(
            const Str8& exchange_,
            const Str8& queue_,
            const Str8& bindingKey_,
            const Map& arguments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.exchangeBound(exchange, queue, bindingKey, arguments );
    }
};
inline CommandPacker<Bound> serializable(Bound& x) { return CommandPacker<Bound>(x); }
std::ostream& operator << (std::ostream&, const Bound&);
bool operator==(const Bound&, const Bound&);

} // namespace exchange


namespace queue {


struct Declare:
    public Command
{
    Name queue;
    exchange::Name alternateExchange;
    Bit passive;
    Bit durable;
    Bit exclusive;
    Bit autoDelete;
    Map arguments;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=queue::CODE;
    static const char* CLASS_NAME;
    explicit Declare(
        const queue::Name& queue_=queue::Name(),
        const exchange::Name& alternateExchange_=exchange::Name(),
        Bit passive_=Bit(),
        Bit durable_=Bit(),
        Bit exclusive_=Bit(),
        Bit autoDelete_=Bit(),
        const Map& arguments_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(alternateExchange)(passive)(durable)(exclusive)(autoDelete)(arguments);
    }
    
    struct Handler
    {
        void queueDeclare(
            const queue::Name& queue_,
            const exchange::Name& alternateExchange_,
            Bit passive_,
            Bit durable_,
            Bit exclusive_,
            Bit autoDelete_,
            const Map& arguments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.queueDeclare(queue, alternateExchange, passive, durable, exclusive, autoDelete, arguments );
    }
};
inline CommandPacker<Declare> serializable(Declare& x) { return CommandPacker<Declare>(x); }
std::ostream& operator << (std::ostream&, const Declare&);
bool operator==(const Declare&, const Declare&);

struct Delete:
    public Command
{
    Name queue;
    Bit ifUnused;
    Bit ifEmpty;
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=queue::CODE;
    static const char* CLASS_NAME;
    explicit Delete(
        const queue::Name& queue_=queue::Name(),
        Bit ifUnused_=Bit(),
        Bit ifEmpty_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(ifUnused)(ifEmpty);
    }
    
    struct Handler
    {
        void queueDelete(
            const queue::Name& queue_,
            Bit ifUnused_,
            Bit ifEmpty_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.queueDelete(queue, ifUnused, ifEmpty );
    }
};
inline CommandPacker<Delete> serializable(Delete& x) { return CommandPacker<Delete>(x); }
std::ostream& operator << (std::ostream&, const Delete&);
bool operator==(const Delete&, const Delete&);

struct Purge:
    public Command
{
    Name queue;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=queue::CODE;
    static const char* CLASS_NAME;
    explicit Purge(const queue::Name& queue_=queue::Name());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue);
    }
    
    struct Handler
    {
        void queuePurge(
            const queue::Name& queue_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.queuePurge(queue );
    }
};
inline CommandPacker<Purge> serializable(Purge& x) { return CommandPacker<Purge>(x); }
std::ostream& operator << (std::ostream&, const Purge&);
bool operator==(const Purge&, const Purge&);

struct Query:
    public Command
{
    Name queue;
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=queue::CODE;
    static const char* CLASS_NAME;
    explicit Query(const queue::Name& queue_=queue::Name());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue);
    }
    
    struct Handler
    {
        void queueQuery(
            const queue::Name& queue_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.queueQuery(queue );
    }
};
inline CommandPacker<Query> serializable(Query& x) { return CommandPacker<Query>(x); }
std::ostream& operator << (std::ostream&, const Query&);
bool operator==(const Query&, const Query&);

} // namespace queue


namespace file {


struct Qos:
    public Command
{
    Uint32 prefetchSize;
    Uint16 prefetchCount;
    Bit global;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Qos(
        Uint32 prefetchSize_=Uint32(),
        Uint16 prefetchCount_=Uint16(),
        Bit global_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(prefetchSize)(prefetchCount)(global);
    }
    
    struct Handler
    {
        void fileQos(
            Uint32 prefetchSize_,
            Uint16 prefetchCount_,
            Bit global_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileQos(prefetchSize, prefetchCount, global );
    }
};
inline CommandPacker<Qos> serializable(Qos& x) { return CommandPacker<Qos>(x); }
std::ostream& operator << (std::ostream&, const Qos&);
bool operator==(const Qos&, const Qos&);

struct QosOk:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit QosOk();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void fileQosOk(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileQosOk( );
    }
};
inline CommandPacker<QosOk> serializable(QosOk& x) { return CommandPacker<QosOk>(x); }
std::ostream& operator << (std::ostream&, const QosOk&);
bool operator==(const QosOk&, const QosOk&);

struct Consume:
    public Command
{
    queue::Name queue;
    Str8 consumerTag;
    Bit noLocal;
    Bit noAck;
    Bit exclusive;
    Bit nowait;
    Map arguments;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Consume(
        const queue::Name& queue_=queue::Name(),
        const Str8& consumerTag_=Str8(),
        Bit noLocal_=Bit(),
        Bit noAck_=Bit(),
        Bit exclusive_=Bit(),
        Bit nowait_=Bit(),
        const Map& arguments_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(consumerTag)(noLocal)(noAck)(exclusive)(nowait)(arguments);
    }
    
    struct Handler
    {
        void fileConsume(
            const queue::Name& queue_,
            const Str8& consumerTag_,
            Bit noLocal_,
            Bit noAck_,
            Bit exclusive_,
            Bit nowait_,
            const Map& arguments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileConsume(queue, consumerTag, noLocal, noAck, exclusive, nowait, arguments );
    }
};
inline CommandPacker<Consume> serializable(Consume& x) { return CommandPacker<Consume>(x); }
std::ostream& operator << (std::ostream&, const Consume&);
bool operator==(const Consume&, const Consume&);

struct ConsumeOk:
    public Command
{
    Str8 consumerTag;
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit ConsumeOk(const Str8& consumerTag_=Str8());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(consumerTag);
    }
    
    struct Handler
    {
        void fileConsumeOk(
            const Str8& consumerTag_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileConsumeOk(consumerTag );
    }
};
inline CommandPacker<ConsumeOk> serializable(ConsumeOk& x) { return CommandPacker<ConsumeOk>(x); }
std::ostream& operator << (std::ostream&, const ConsumeOk&);
bool operator==(const ConsumeOk&, const ConsumeOk&);

struct Cancel:
    public Command
{
    Str8 consumerTag;
    
    static const char* NAME;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Cancel(const Str8& consumerTag_=Str8());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(consumerTag);
    }
    
    struct Handler
    {
        void fileCancel(
            const Str8& consumerTag_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileCancel(consumerTag );
    }
};
inline CommandPacker<Cancel> serializable(Cancel& x) { return CommandPacker<Cancel>(x); }
std::ostream& operator << (std::ostream&, const Cancel&);
bool operator==(const Cancel&, const Cancel&);

struct Open:
    public Command
{
    Str8 identifier;
    Uint64 contentSize;
    
    static const char* NAME;
    static const uint8_t CODE=0x6;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Open(
        const Str8& identifier_=Str8(),
        Uint64 contentSize_=Uint64()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(identifier)(contentSize);
    }
    
    struct Handler
    {
        void fileOpen(
            const Str8& identifier_,
            Uint64 contentSize_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileOpen(identifier, contentSize );
    }
};
inline CommandPacker<Open> serializable(Open& x) { return CommandPacker<Open>(x); }
std::ostream& operator << (std::ostream&, const Open&);
bool operator==(const Open&, const Open&);

struct OpenOk:
    public Command
{
    Uint64 stagedSize;
    
    static const char* NAME;
    static const uint8_t CODE=0x7;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit OpenOk(Uint64 stagedSize_=Uint64());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(stagedSize);
    }
    
    struct Handler
    {
        void fileOpenOk(
            Uint64 stagedSize_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileOpenOk(stagedSize );
    }
};
inline CommandPacker<OpenOk> serializable(OpenOk& x) { return CommandPacker<OpenOk>(x); }
std::ostream& operator << (std::ostream&, const OpenOk&);
bool operator==(const OpenOk&, const OpenOk&);

struct Stage:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x8;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Stage();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void fileStage(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileStage( );
    }
};
inline CommandPacker<Stage> serializable(Stage& x) { return CommandPacker<Stage>(x); }
std::ostream& operator << (std::ostream&, const Stage&);
bool operator==(const Stage&, const Stage&);

struct Publish:
    public Command
{
    exchange::Name exchange;
    Str8 routingKey;
    Bit mandatory;
    Bit immediate;
    Str8 identifier;
    
    static const char* NAME;
    static const uint8_t CODE=0x9;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Publish(
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& routingKey_=Str8(),
        Bit mandatory_=Bit(),
        Bit immediate_=Bit(),
        const Str8& identifier_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(exchange)(routingKey)(mandatory)(immediate)(identifier);
    }
    
    struct Handler
    {
        void filePublish(
            const exchange::Name& exchange_,
            const Str8& routingKey_,
            Bit mandatory_,
            Bit immediate_,
            const Str8& identifier_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.filePublish(exchange, routingKey, mandatory, immediate, identifier );
    }
};
inline CommandPacker<Publish> serializable(Publish& x) { return CommandPacker<Publish>(x); }
std::ostream& operator << (std::ostream&, const Publish&);
bool operator==(const Publish&, const Publish&);

struct Return:
    public Command
{
    ReturnCode replyCode;
    Str8 replyText;
    exchange::Name exchange;
    Str8 routingKey;
    
    static const char* NAME;
    static const uint8_t CODE=0xa;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Return(
        const file::ReturnCode& replyCode_=file::ReturnCode(),
        const Str8& replyText_=Str8(),
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& routingKey_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(replyCode)(replyText)(exchange)(routingKey);
    }
    
    struct Handler
    {
        void fileReturn(
            const file::ReturnCode& replyCode_,
            const Str8& replyText_,
            const exchange::Name& exchange_,
            const Str8& routingKey_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileReturn(replyCode, replyText, exchange, routingKey );
    }
};
inline CommandPacker<Return> serializable(Return& x) { return CommandPacker<Return>(x); }
std::ostream& operator << (std::ostream&, const Return&);
bool operator==(const Return&, const Return&);

struct Deliver:
    public Command
{
    Str8 consumerTag;
    Uint64 deliveryTag;
    Bit redelivered;
    exchange::Name exchange;
    Str8 routingKey;
    Str8 identifier;
    
    static const char* NAME;
    static const uint8_t CODE=0xb;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Deliver(
        const Str8& consumerTag_=Str8(),
        Uint64 deliveryTag_=Uint64(),
        Bit redelivered_=Bit(),
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& routingKey_=Str8(),
        const Str8& identifier_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(consumerTag)(deliveryTag)(redelivered)(exchange)(routingKey)(identifier);
    }
    
    struct Handler
    {
        void fileDeliver(
            const Str8& consumerTag_,
            Uint64 deliveryTag_,
            Bit redelivered_,
            const exchange::Name& exchange_,
            const Str8& routingKey_,
            const Str8& identifier_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileDeliver(consumerTag, deliveryTag, redelivered, exchange, routingKey, identifier );
    }
};
inline CommandPacker<Deliver> serializable(Deliver& x) { return CommandPacker<Deliver>(x); }
std::ostream& operator << (std::ostream&, const Deliver&);
bool operator==(const Deliver&, const Deliver&);

struct Ack:
    public Command
{
    Uint64 deliveryTag;
    Bit multiple;
    
    static const char* NAME;
    static const uint8_t CODE=0xc;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Ack(
        Uint64 deliveryTag_=Uint64(),
        Bit multiple_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(deliveryTag)(multiple);
    }
    
    struct Handler
    {
        void fileAck(
            Uint64 deliveryTag_,
            Bit multiple_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileAck(deliveryTag, multiple );
    }
};
inline CommandPacker<Ack> serializable(Ack& x) { return CommandPacker<Ack>(x); }
std::ostream& operator << (std::ostream&, const Ack&);
bool operator==(const Ack&, const Ack&);

struct Reject:
    public Command
{
    Uint64 deliveryTag;
    Bit requeue;
    
    static const char* NAME;
    static const uint8_t CODE=0xd;
    static const uint8_t CLASS_CODE=file::CODE;
    static const char* CLASS_NAME;
    explicit Reject(
        Uint64 deliveryTag_=Uint64(),
        Bit requeue_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(deliveryTag)(requeue);
    }
    
    struct Handler
    {
        void fileReject(
            Uint64 deliveryTag_,
            Bit requeue_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.fileReject(deliveryTag, requeue );
    }
};
inline CommandPacker<Reject> serializable(Reject& x) { return CommandPacker<Reject>(x); }
std::ostream& operator << (std::ostream&, const Reject&);
bool operator==(const Reject&, const Reject&);

} // namespace file


namespace stream {


struct Qos:
    public Command
{
    Uint32 prefetchSize;
    Uint16 prefetchCount;
    Uint32 consumeRate;
    Bit global;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit Qos(
        Uint32 prefetchSize_=Uint32(),
        Uint16 prefetchCount_=Uint16(),
        Uint32 consumeRate_=Uint32(),
        Bit global_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(prefetchSize)(prefetchCount)(consumeRate)(global);
    }
    
    struct Handler
    {
        void streamQos(
            Uint32 prefetchSize_,
            Uint16 prefetchCount_,
            Uint32 consumeRate_,
            Bit global_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.streamQos(prefetchSize, prefetchCount, consumeRate, global );
    }
};
inline CommandPacker<Qos> serializable(Qos& x) { return CommandPacker<Qos>(x); }
std::ostream& operator << (std::ostream&, const Qos&);
bool operator==(const Qos&, const Qos&);

struct QosOk:
    public Command
{
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit QosOk();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void streamQosOk(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.streamQosOk( );
    }
};
inline CommandPacker<QosOk> serializable(QosOk& x) { return CommandPacker<QosOk>(x); }
std::ostream& operator << (std::ostream&, const QosOk&);
bool operator==(const QosOk&, const QosOk&);

struct Consume:
    public Command
{
    queue::Name queue;
    Str8 consumerTag;
    Bit noLocal;
    Bit exclusive;
    Bit nowait;
    Map arguments;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit Consume(
        const queue::Name& queue_=queue::Name(),
        const Str8& consumerTag_=Str8(),
        Bit noLocal_=Bit(),
        Bit exclusive_=Bit(),
        Bit nowait_=Bit(),
        const Map& arguments_=Map()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(consumerTag)(noLocal)(exclusive)(nowait)(arguments);
    }
    
    struct Handler
    {
        void streamConsume(
            const queue::Name& queue_,
            const Str8& consumerTag_,
            Bit noLocal_,
            Bit exclusive_,
            Bit nowait_,
            const Map& arguments_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.streamConsume(queue, consumerTag, noLocal, exclusive, nowait, arguments );
    }
};
inline CommandPacker<Consume> serializable(Consume& x) { return CommandPacker<Consume>(x); }
std::ostream& operator << (std::ostream&, const Consume&);
bool operator==(const Consume&, const Consume&);

struct ConsumeOk:
    public Command
{
    Str8 consumerTag;
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit ConsumeOk(const Str8& consumerTag_=Str8());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(consumerTag);
    }
    
    struct Handler
    {
        void streamConsumeOk(
            const Str8& consumerTag_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.streamConsumeOk(consumerTag );
    }
};
inline CommandPacker<ConsumeOk> serializable(ConsumeOk& x) { return CommandPacker<ConsumeOk>(x); }
std::ostream& operator << (std::ostream&, const ConsumeOk&);
bool operator==(const ConsumeOk&, const ConsumeOk&);

struct Cancel:
    public Command
{
    Str8 consumerTag;
    
    static const char* NAME;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit Cancel(const Str8& consumerTag_=Str8());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(consumerTag);
    }
    
    struct Handler
    {
        void streamCancel(
            const Str8& consumerTag_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.streamCancel(consumerTag );
    }
};
inline CommandPacker<Cancel> serializable(Cancel& x) { return CommandPacker<Cancel>(x); }
std::ostream& operator << (std::ostream&, const Cancel&);
bool operator==(const Cancel&, const Cancel&);

struct Publish:
    public Command
{
    exchange::Name exchange;
    Str8 routingKey;
    Bit mandatory;
    Bit immediate;
    
    static const char* NAME;
    static const uint8_t CODE=0x6;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit Publish(
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& routingKey_=Str8(),
        Bit mandatory_=Bit(),
        Bit immediate_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(exchange)(routingKey)(mandatory)(immediate);
    }
    
    struct Handler
    {
        void streamPublish(
            const exchange::Name& exchange_,
            const Str8& routingKey_,
            Bit mandatory_,
            Bit immediate_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.streamPublish(exchange, routingKey, mandatory, immediate );
    }
};
inline CommandPacker<Publish> serializable(Publish& x) { return CommandPacker<Publish>(x); }
std::ostream& operator << (std::ostream&, const Publish&);
bool operator==(const Publish&, const Publish&);

struct Return:
    public Command
{
    ReturnCode replyCode;
    Str8 replyText;
    exchange::Name exchange;
    Str8 routingKey;
    
    static const char* NAME;
    static const uint8_t CODE=0x7;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit Return(
        const stream::ReturnCode& replyCode_=stream::ReturnCode(),
        const Str8& replyText_=Str8(),
        const exchange::Name& exchange_=exchange::Name(),
        const Str8& routingKey_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(replyCode)(replyText)(exchange)(routingKey);
    }
    
    struct Handler
    {
        void streamReturn(
            const stream::ReturnCode& replyCode_,
            const Str8& replyText_,
            const exchange::Name& exchange_,
            const Str8& routingKey_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.streamReturn(replyCode, replyText, exchange, routingKey );
    }
};
inline CommandPacker<Return> serializable(Return& x) { return CommandPacker<Return>(x); }
std::ostream& operator << (std::ostream&, const Return&);
bool operator==(const Return&, const Return&);

struct Deliver:
    public Command
{
    Str8 consumerTag;
    Uint64 deliveryTag;
    exchange::Name exchange;
    queue::Name queue;
    
    static const char* NAME;
    static const uint8_t CODE=0x8;
    static const uint8_t CLASS_CODE=stream::CODE;
    static const char* CLASS_NAME;
    explicit Deliver(
        const Str8& consumerTag_=Str8(),
        Uint64 deliveryTag_=Uint64(),
        const exchange::Name& exchange_=exchange::Name(),
        const queue::Name& queue_=queue::Name()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(consumerTag)(deliveryTag)(exchange)(queue);
    }
    
    struct Handler
    {
        void streamDeliver(
            const Str8& consumerTag_,
            Uint64 deliveryTag_,
            const exchange::Name& exchange_,
            const queue::Name& queue_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.streamDeliver(consumerTag, deliveryTag, exchange, queue );
    }
};
inline CommandPacker<Deliver> serializable(Deliver& x) { return CommandPacker<Deliver>(x); }
std::ostream& operator << (std::ostream&, const Deliver&);
bool operator==(const Deliver&, const Deliver&);

} // namespace stream


namespace cluster {


struct UpdateRequest:
    public Control
{
    Str16 url;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit UpdateRequest(const Str16& url_=Str16());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(url);
    }
    
    struct Handler
    {
        void clusterUpdateRequest(
            const Str16& url_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterUpdateRequest(url );
    }
};
inline Packer<UpdateRequest> serializable(UpdateRequest& x) { return Packer<UpdateRequest>(x); }
std::ostream& operator << (std::ostream&, const UpdateRequest&);
bool operator==(const UpdateRequest&, const UpdateRequest&);

struct UpdateOffer:
    public Control
{
    Uint64 updatee;
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit UpdateOffer(Uint64 updatee_=Uint64());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(updatee);
    }
    
    struct Handler
    {
        void clusterUpdateOffer(
            Uint64 updatee_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterUpdateOffer(updatee );
    }
};
inline Packer<UpdateOffer> serializable(UpdateOffer& x) { return Packer<UpdateOffer>(x); }
std::ostream& operator << (std::ostream&, const UpdateOffer&);
bool operator==(const UpdateOffer&, const UpdateOffer&);

struct RetractOffer:
    public Control
{
    Uint64 updatee;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit RetractOffer(Uint64 updatee_=Uint64());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(updatee);
    }
    
    struct Handler
    {
        void clusterRetractOffer(
            Uint64 updatee_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterRetractOffer(updatee );
    }
};
inline Packer<RetractOffer> serializable(RetractOffer& x) { return Packer<RetractOffer>(x); }
std::ostream& operator << (std::ostream&, const RetractOffer&);
bool operator==(const RetractOffer&, const RetractOffer&);

struct InitialStatus:
    public Control
{
    Uint32 version;
    Bit active;
    Uuid clusterId;
    StoreState storeState;
    Uuid shutdownId;
    
    static const char* NAME;
    static const uint8_t CODE=0x5;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit InitialStatus(
        Uint32 version_=Uint32(),
        Bit active_=Bit(),
        const Uuid& clusterId_=Uuid(),
        const cluster::StoreState& storeState_=cluster::StoreState(),
        const Uuid& shutdownId_=Uuid()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(version)(active)(clusterId)(storeState)(shutdownId);
    }
    
    struct Handler
    {
        void clusterInitialStatus(
            Uint32 version_,
            Bit active_,
            const Uuid& clusterId_,
            const cluster::StoreState& storeState_,
            const Uuid& shutdownId_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterInitialStatus(version, active, clusterId, storeState, shutdownId );
    }
};
inline Packer<InitialStatus> serializable(InitialStatus& x) { return Packer<InitialStatus>(x); }
std::ostream& operator << (std::ostream&, const InitialStatus&);
bool operator==(const InitialStatus&, const InitialStatus&);

struct Ready:
    public Control
{
    Str16 url;
    
    static const char* NAME;
    static const uint8_t CODE=0x10;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit Ready(const Str16& url_=Str16());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(url);
    }
    
    struct Handler
    {
        void clusterReady(
            const Str16& url_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterReady(url );
    }
};
inline Packer<Ready> serializable(Ready& x) { return Packer<Ready>(x); }
std::ostream& operator << (std::ostream&, const Ready&);
bool operator==(const Ready&, const Ready&);

struct ConfigChange:
    public Control
{
    Vbin16 current;
    
    static const char* NAME;
    static const uint8_t CODE=0x11;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit ConfigChange(const Vbin16& current_=Vbin16());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(current);
    }
    
    struct Handler
    {
        void clusterConfigChange(
            const Vbin16& current_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConfigChange(current );
    }
};
inline Packer<ConfigChange> serializable(ConfigChange& x) { return Packer<ConfigChange>(x); }
std::ostream& operator << (std::ostream&, const ConfigChange&);
bool operator==(const ConfigChange&, const ConfigChange&);

struct MessageExpired:
    public Control
{
    Uint64 id;
    
    static const char* NAME;
    static const uint8_t CODE=0x12;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit MessageExpired(Uint64 id_=Uint64());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(id);
    }
    
    struct Handler
    {
        void clusterMessageExpired(
            Uint64 id_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterMessageExpired(id );
    }
};
inline Packer<MessageExpired> serializable(MessageExpired& x) { return Packer<MessageExpired>(x); }
std::ostream& operator << (std::ostream&, const MessageExpired&);
bool operator==(const MessageExpired&, const MessageExpired&);

struct ErrorCheck:
    public Control
{
    ErrorType type;
    SequenceNo frameSeq;
    
    static const char* NAME;
    static const uint8_t CODE=0x14;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit ErrorCheck(
        const cluster::ErrorType& type_=cluster::ErrorType(),
        const SequenceNo& frameSeq_=SequenceNo()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(type)(frameSeq);
    }
    
    struct Handler
    {
        void clusterErrorCheck(
            const cluster::ErrorType& type_,
            const SequenceNo& frameSeq_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterErrorCheck(type, frameSeq );
    }
};
inline Packer<ErrorCheck> serializable(ErrorCheck& x) { return Packer<ErrorCheck>(x); }
std::ostream& operator << (std::ostream&, const ErrorCheck&);
bool operator==(const ErrorCheck&, const ErrorCheck&);

struct Shutdown:
    public Control
{
    Uuid shutdownId;
    
    static const char* NAME;
    static const uint8_t CODE=0x20;
    static const uint8_t CLASS_CODE=cluster::CODE;
    static const char* CLASS_NAME;
    explicit Shutdown(const Uuid& shutdownId_=Uuid());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(shutdownId);
    }
    
    struct Handler
    {
        void clusterShutdown(
            const Uuid& shutdownId_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterShutdown(shutdownId );
    }
};
inline Packer<Shutdown> serializable(Shutdown& x) { return Packer<Shutdown>(x); }
std::ostream& operator << (std::ostream&, const Shutdown&);
bool operator==(const Shutdown&, const Shutdown&);

} // namespace cluster


namespace cluster_connection {


struct Announce:
    public Control
{
    Uint32 ssf;
    
    static const char* NAME;
    static const uint8_t CODE=0x1;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit Announce(Uint32 ssf_=Uint32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(ssf);
    }
    
    struct Handler
    {
        void clusterConnectionAnnounce(
            Uint32 ssf_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionAnnounce(ssf );
    }
};
inline Packer<Announce> serializable(Announce& x) { return Packer<Announce>(x); }
std::ostream& operator << (std::ostream&, const Announce&);
bool operator==(const Announce&, const Announce&);

struct DeliverClose:
    public Control
{
    
    static const char* NAME;
    static const uint8_t CODE=0x2;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit DeliverClose();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void clusterConnectionDeliverClose(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionDeliverClose( );
    }
};
inline Packer<DeliverClose> serializable(DeliverClose& x) { return Packer<DeliverClose>(x); }
std::ostream& operator << (std::ostream&, const DeliverClose&);
bool operator==(const DeliverClose&, const DeliverClose&);

struct DeliverDoOutput:
    public Control
{
    Uint32 limit;
    
    static const char* NAME;
    static const uint8_t CODE=0x3;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit DeliverDoOutput(Uint32 limit_=Uint32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(limit);
    }
    
    struct Handler
    {
        void clusterConnectionDeliverDoOutput(
            Uint32 limit_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionDeliverDoOutput(limit );
    }
};
inline Packer<DeliverDoOutput> serializable(DeliverDoOutput& x) { return Packer<DeliverDoOutput>(x); }
std::ostream& operator << (std::ostream&, const DeliverDoOutput&);
bool operator==(const DeliverDoOutput&, const DeliverDoOutput&);

struct Abort:
    public Control
{
    
    static const char* NAME;
    static const uint8_t CODE=0x4;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit Abort();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void clusterConnectionAbort(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionAbort( );
    }
};
inline Packer<Abort> serializable(Abort& x) { return Packer<Abort>(x); }
std::ostream& operator << (std::ostream&, const Abort&);
bool operator==(const Abort&, const Abort&);

struct ConsumerState:
    public Control
{
    Str8 name;
    Bit blocked;
    Bit notifyEnabled;
    SequenceNo position;
    
    static const char* NAME;
    static const uint8_t CODE=0x10;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit ConsumerState(
        const Str8& name_=Str8(),
        Bit blocked_=Bit(),
        Bit notifyEnabled_=Bit(),
        const SequenceNo& position_=SequenceNo()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(name)(blocked)(notifyEnabled)(position);
    }
    
    struct Handler
    {
        void clusterConnectionConsumerState(
            const Str8& name_,
            Bit blocked_,
            Bit notifyEnabled_,
            const SequenceNo& position_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionConsumerState(name, blocked, notifyEnabled, position );
    }
};
inline Packer<ConsumerState> serializable(ConsumerState& x) { return Packer<ConsumerState>(x); }
std::ostream& operator << (std::ostream&, const ConsumerState&);
bool operator==(const ConsumerState&, const ConsumerState&);

struct DeliveryRecord:
    public Control
{
    Str8 queue;
    SequenceNo position;
    Str8 tag;
    SequenceNo id;
    Bit acquired;
    Bit accepted;
    Bit cancelled;
    Bit completed;
    Bit ended;
    Bit windowing;
    Bit enqueued;
    Uint32 credit;
    
    static const char* NAME;
    static const uint8_t CODE=0x11;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit DeliveryRecord(
        const Str8& queue_=Str8(),
        const SequenceNo& position_=SequenceNo(),
        const Str8& tag_=Str8(),
        const SequenceNo& id_=SequenceNo(),
        Bit acquired_=Bit(),
        Bit accepted_=Bit(),
        Bit cancelled_=Bit(),
        Bit completed_=Bit(),
        Bit ended_=Bit(),
        Bit windowing_=Bit(),
        Bit enqueued_=Bit(),
        Uint32 credit_=Uint32()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(position)(tag)(id)(acquired)(accepted)(cancelled)(completed)(ended)(windowing)(enqueued)(credit);
    }
    
    struct Handler
    {
        void clusterConnectionDeliveryRecord(
            const Str8& queue_,
            const SequenceNo& position_,
            const Str8& tag_,
            const SequenceNo& id_,
            Bit acquired_,
            Bit accepted_,
            Bit cancelled_,
            Bit completed_,
            Bit ended_,
            Bit windowing_,
            Bit enqueued_,
            Uint32 credit_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionDeliveryRecord(queue, position, tag, id, acquired, accepted, cancelled, completed, ended, windowing, enqueued, credit );
    }
};
inline Packer<DeliveryRecord> serializable(DeliveryRecord& x) { return Packer<DeliveryRecord>(x); }
std::ostream& operator << (std::ostream&, const DeliveryRecord&);
bool operator==(const DeliveryRecord&, const DeliveryRecord&);

struct TxStart:
    public Control
{
    
    static const char* NAME;
    static const uint8_t CODE=0x12;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit TxStart();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void clusterConnectionTxStart(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionTxStart( );
    }
};
inline Packer<TxStart> serializable(TxStart& x) { return Packer<TxStart>(x); }
std::ostream& operator << (std::ostream&, const TxStart&);
bool operator==(const TxStart&, const TxStart&);

struct TxAccept:
    public Control
{
    SequenceSet commands;
    
    static const char* NAME;
    static const uint8_t CODE=0x13;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit TxAccept(const SequenceSet& commands_=SequenceSet());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commands);
    }
    
    struct Handler
    {
        void clusterConnectionTxAccept(
            const SequenceSet& commands_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionTxAccept(commands );
    }
};
inline Packer<TxAccept> serializable(TxAccept& x) { return Packer<TxAccept>(x); }
std::ostream& operator << (std::ostream&, const TxAccept&);
bool operator==(const TxAccept&, const TxAccept&);

struct TxDequeue:
    public Control
{
    Str8 queue;
    
    static const char* NAME;
    static const uint8_t CODE=0x14;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit TxDequeue(const Str8& queue_=Str8());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue);
    }
    
    struct Handler
    {
        void clusterConnectionTxDequeue(
            const Str8& queue_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionTxDequeue(queue );
    }
};
inline Packer<TxDequeue> serializable(TxDequeue& x) { return Packer<TxDequeue>(x); }
std::ostream& operator << (std::ostream&, const TxDequeue&);
bool operator==(const TxDequeue&, const TxDequeue&);

struct TxEnqueue:
    public Control
{
    Str8 queue;
    
    static const char* NAME;
    static const uint8_t CODE=0x15;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit TxEnqueue(const Str8& queue_=Str8());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue);
    }
    
    struct Handler
    {
        void clusterConnectionTxEnqueue(
            const Str8& queue_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionTxEnqueue(queue );
    }
};
inline Packer<TxEnqueue> serializable(TxEnqueue& x) { return Packer<TxEnqueue>(x); }
std::ostream& operator << (std::ostream&, const TxEnqueue&);
bool operator==(const TxEnqueue&, const TxEnqueue&);

struct TxPublish:
    public Control
{
    ArrayDomain<Str8>  queues;
    Bit delivered;
    
    static const char* NAME;
    static const uint8_t CODE=0x16;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit TxPublish(
        const ArrayDomain<Str8> & queues_=ArrayDomain<Str8> (),
        Bit delivered_=Bit()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queues)(delivered);
    }
    
    struct Handler
    {
        void clusterConnectionTxPublish(
            const ArrayDomain<Str8> & queues_,
            Bit delivered_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionTxPublish(queues, delivered );
    }
};
inline Packer<TxPublish> serializable(TxPublish& x) { return Packer<TxPublish>(x); }
std::ostream& operator << (std::ostream&, const TxPublish&);
bool operator==(const TxPublish&, const TxPublish&);

struct TxEnd:
    public Control
{
    
    static const char* NAME;
    static const uint8_t CODE=0x17;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit TxEnd();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void clusterConnectionTxEnd(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionTxEnd( );
    }
};
inline Packer<TxEnd> serializable(TxEnd& x) { return Packer<TxEnd>(x); }
std::ostream& operator << (std::ostream&, const TxEnd&);
bool operator==(const TxEnd&, const TxEnd&);

struct AccumulatedAck:
    public Control
{
    SequenceSet commands;
    
    static const char* NAME;
    static const uint8_t CODE=0x18;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit AccumulatedAck(const SequenceSet& commands_=SequenceSet());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(commands);
    }
    
    struct Handler
    {
        void clusterConnectionAccumulatedAck(
            const SequenceSet& commands_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionAccumulatedAck(commands );
    }
};
inline Packer<AccumulatedAck> serializable(AccumulatedAck& x) { return Packer<AccumulatedAck>(x); }
std::ostream& operator << (std::ostream&, const AccumulatedAck&);
bool operator==(const AccumulatedAck&, const AccumulatedAck&);

struct OutputTask:
    public Control
{
    Uint16 channel;
    Str8 name;
    
    static const char* NAME;
    static const uint8_t CODE=0x19;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit OutputTask(
        Uint16 channel_=Uint16(),
        const Str8& name_=Str8()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(channel)(name);
    }
    
    struct Handler
    {
        void clusterConnectionOutputTask(
            Uint16 channel_,
            const Str8& name_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionOutputTask(channel, name );
    }
};
inline Packer<OutputTask> serializable(OutputTask& x) { return Packer<OutputTask>(x); }
std::ostream& operator << (std::ostream&, const OutputTask&);
bool operator==(const OutputTask&, const OutputTask&);

struct SessionState:
    public Control
{
    SequenceNo replayStart;
    SequenceNo commandPoint;
    SequenceSet sentIncomplete;
    SequenceNo expected;
    SequenceNo received;
    SequenceSet unknownCompleted;
    SequenceSet receivedIncomplete;
    
    static const char* NAME;
    static const uint8_t CODE=0x1F;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit SessionState(
        const SequenceNo& replayStart_=SequenceNo(),
        const SequenceNo& commandPoint_=SequenceNo(),
        const SequenceSet& sentIncomplete_=SequenceSet(),
        const SequenceNo& expected_=SequenceNo(),
        const SequenceNo& received_=SequenceNo(),
        const SequenceSet& unknownCompleted_=SequenceSet(),
        const SequenceSet& receivedIncomplete_=SequenceSet()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(replayStart)(commandPoint)(sentIncomplete)(expected)(received)(unknownCompleted)(receivedIncomplete);
    }
    
    struct Handler
    {
        void clusterConnectionSessionState(
            const SequenceNo& replayStart_,
            const SequenceNo& commandPoint_,
            const SequenceSet& sentIncomplete_,
            const SequenceNo& expected_,
            const SequenceNo& received_,
            const SequenceSet& unknownCompleted_,
            const SequenceSet& receivedIncomplete_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionSessionState(replayStart, commandPoint, sentIncomplete, expected, received, unknownCompleted, receivedIncomplete );
    }
};
inline Packer<SessionState> serializable(SessionState& x) { return Packer<SessionState>(x); }
std::ostream& operator << (std::ostream&, const SessionState&);
bool operator==(const SessionState&, const SessionState&);

struct ShadowReady:
    public Control
{
    Uint64 memberId;
    Uint64 connectionId;
    Str8 userName;
    Str32 fragment;
    Uint32 sendMax;
    
    static const char* NAME;
    static const uint8_t CODE=0x20;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit ShadowReady(
        Uint64 memberId_=Uint64(),
        Uint64 connectionId_=Uint64(),
        const Str8& userName_=Str8(),
        const Str32& fragment_=Str32(),
        Uint32 sendMax_=Uint32()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(memberId)(connectionId)(userName)(fragment)(sendMax);
    }
    
    struct Handler
    {
        void clusterConnectionShadowReady(
            Uint64 memberId_,
            Uint64 connectionId_,
            const Str8& userName_,
            const Str32& fragment_,
            Uint32 sendMax_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionShadowReady(memberId, connectionId, userName, fragment, sendMax );
    }
};
inline Packer<ShadowReady> serializable(ShadowReady& x) { return Packer<ShadowReady>(x); }
std::ostream& operator << (std::ostream&, const ShadowReady&);
bool operator==(const ShadowReady&, const ShadowReady&);

struct Membership:
    public Control
{
    Map joiners;
    Map members;
    SequenceNo frameSeq;
    
    static const char* NAME;
    static const uint8_t CODE=0x21;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit Membership(
        const Map& joiners_=Map(),
        const Map& members_=Map(),
        const SequenceNo& frameSeq_=SequenceNo()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(joiners)(members)(frameSeq);
    }
    
    struct Handler
    {
        void clusterConnectionMembership(
            const Map& joiners_,
            const Map& members_,
            const SequenceNo& frameSeq_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionMembership(joiners, members, frameSeq );
    }
};
inline Packer<Membership> serializable(Membership& x) { return Packer<Membership>(x); }
std::ostream& operator << (std::ostream&, const Membership&);
bool operator==(const Membership&, const Membership&);

struct RetractOffer:
    public Control
{
    
    static const char* NAME;
    static const uint8_t CODE=0x22;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit RetractOffer();
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S&) {}
    
    struct Handler
    {
        void clusterConnectionRetractOffer(
            
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionRetractOffer( );
    }
};
inline Packer<RetractOffer> serializable(RetractOffer& x) { return Packer<RetractOffer>(x); }
std::ostream& operator << (std::ostream&, const RetractOffer&);
bool operator==(const RetractOffer&, const RetractOffer&);

struct QueuePosition:
    public Control
{
    Str8 queue;
    SequenceNo position;
    
    static const char* NAME;
    static const uint8_t CODE=0x30;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit QueuePosition(
        const Str8& queue_=Str8(),
        const SequenceNo& position_=SequenceNo()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(position);
    }
    
    struct Handler
    {
        void clusterConnectionQueuePosition(
            const Str8& queue_,
            const SequenceNo& position_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionQueuePosition(queue, position );
    }
};
inline Packer<QueuePosition> serializable(QueuePosition& x) { return Packer<QueuePosition>(x); }
std::ostream& operator << (std::ostream&, const QueuePosition&);
bool operator==(const QueuePosition&, const QueuePosition&);

struct Exchange:
    public Control
{
    Str32 encoded;
    
    static const char* NAME;
    static const uint8_t CODE=0x31;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit Exchange(const Str32& encoded_=Str32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(encoded);
    }
    
    struct Handler
    {
        void clusterConnectionExchange(
            const Str32& encoded_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionExchange(encoded );
    }
};
inline Packer<Exchange> serializable(Exchange& x) { return Packer<Exchange>(x); }
std::ostream& operator << (std::ostream&, const Exchange&);
bool operator==(const Exchange&, const Exchange&);

struct Queue:
    public Control
{
    Str32 encoded;
    
    static const char* NAME;
    static const uint8_t CODE=0x32;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit Queue(const Str32& encoded_=Str32());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(encoded);
    }
    
    struct Handler
    {
        void clusterConnectionQueue(
            const Str32& encoded_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionQueue(encoded );
    }
};
inline Packer<Queue> serializable(Queue& x) { return Packer<Queue>(x); }
std::ostream& operator << (std::ostream&, const Queue&);
bool operator==(const Queue&, const Queue&);

struct ExpiryId:
    public Control
{
    Uint64 expiryId;
    
    static const char* NAME;
    static const uint8_t CODE=0x33;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit ExpiryId(Uint64 expiryId_=Uint64());
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(expiryId);
    }
    
    struct Handler
    {
        void clusterConnectionExpiryId(
            Uint64 expiryId_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionExpiryId(expiryId );
    }
};
inline Packer<ExpiryId> serializable(ExpiryId& x) { return Packer<ExpiryId>(x); }
std::ostream& operator << (std::ostream&, const ExpiryId&);
bool operator==(const ExpiryId&, const ExpiryId&);

struct AddQueueListener:
    public Control
{
    Str8 queue;
    Uint32 consumer;
    
    static const char* NAME;
    static const uint8_t CODE=0x34;
    static const uint8_t CLASS_CODE=cluster_connection::CODE;
    static const char* CLASS_NAME;
    explicit AddQueueListener(
        const Str8& queue_=Str8(),
        Uint32 consumer_=Uint32()
    );
    void accept(Visitor&);
    void accept(ConstVisitor&) const;
    template <class S> void serialize(S& s) {
        s(queue)(consumer);
    }
    
    struct Handler
    {
        void clusterConnectionAddQueueListener(
            const Str8& queue_,
            Uint32 consumer_
        );
    };
    
    template <class T> void invoke(T& target)const
    {
        target.clusterConnectionAddQueueListener(queue, consumer );
    }
};
inline Packer<AddQueueListener> serializable(AddQueueListener& x) { return Packer<AddQueueListener>(x); }
std::ostream& operator << (std::ostream&, const AddQueueListener&);
bool operator==(const AddQueueListener&, const AddQueueListener&);

} // namespace cluster_connection


}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_SPECIFICATION_H*/
