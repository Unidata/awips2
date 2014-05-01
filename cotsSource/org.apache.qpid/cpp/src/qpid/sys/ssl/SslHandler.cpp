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
#include "qpid/sys/ssl/SslHandler.h"

#include "qpid/sys/ssl/SslIo.h"
#include "qpid/sys/ssl/SslSocket.h"
#include "qpid/framing/AMQP_HighestVersion.h"
#include "qpid/framing/ProtocolInitiation.h"
#include "qpid/log/Statement.h"

#include <boost/bind.hpp>

namespace qpid {
namespace sys {
namespace ssl {


// Buffer definition
struct Buff : public SslIO::BufferBase {
    Buff() :
        SslIO::BufferBase(new char[65536], 65536)
    {}
    ~Buff()
    { delete [] bytes;}
};

SslHandler::SslHandler(std::string id, ConnectionCodec::Factory* f) :
    identifier(id),
    aio(0),
    factory(f),
    codec(0),
    readError(false),
    isClient(false)
{}

SslHandler::~SslHandler() {
    if (codec)
        codec->closed();
    delete codec;
}

void SslHandler::init(SslIO* a, int numBuffs) {
    aio = a;

    // Give connection some buffers to use
    for (int i = 0; i < numBuffs; i++) {
        aio->queueReadBuffer(new Buff);
    }
}

void SslHandler::write(const framing::ProtocolInitiation& data)
{
    QPID_LOG(debug, "SENT [" << identifier << "] INIT(" << data << ")");
    SslIO::BufferBase* buff = aio->getQueuedBuffer();
    if (!buff)
        buff = new Buff;
    framing::Buffer out(buff->bytes, buff->byteCount);
    data.encode(out);
    buff->dataCount = data.encodedSize();
    aio->queueWrite(buff);
}

void SslHandler::abort() {
    // TODO: can't implement currently as underlying functionality not implemented
    // aio->requestCallback(boost::bind(&SslHandler::eof, this, _1));
}
void SslHandler::activateOutput() {
    aio->notifyPendingWrite();
}

void SslHandler::giveReadCredit(int32_t) {
    // FIXME aconway 2008-12-05: not yet implemented.
}

// Input side
void SslHandler::readbuff(SslIO& , SslIO::BufferBase* buff) {
    if (readError) {
        return;
    }
    size_t decoded = 0;
    if (codec) {                // Already initiated
        try {
            decoded = codec->decode(buff->bytes+buff->dataStart, buff->dataCount);
        }catch(const std::exception& e){
            QPID_LOG(error, e.what());
            readError = true;
            aio->queueWriteClose();
        }
    }else{
        framing::Buffer in(buff->bytes+buff->dataStart, buff->dataCount);
        framing::ProtocolInitiation protocolInit;
        if (protocolInit.decode(in)) {
            decoded = in.getPosition();
            QPID_LOG(debug, "RECV [" << identifier << "] INIT(" << protocolInit << ")");
            try {
                codec = factory->create(protocolInit.getVersion(), *this, identifier, aio->getKeyLen());
                if (!codec) {
                    //TODO: may still want to revise this...
                    //send valid version header & close connection.
                    write(framing::ProtocolInitiation(framing::highestProtocolVersion));
                    readError = true;
                    aio->queueWriteClose();
                }
            } catch (const std::exception& e) {
                QPID_LOG(error, e.what());
                readError = true;
                aio->queueWriteClose();
            }
        }
    }
    // TODO: unreading needs to go away, and when we can cope
    // with multiple sub-buffers in the general buffer scheme, it will
    if (decoded != size_t(buff->dataCount)) {
        // Adjust buffer for used bytes and then "unread them"
        buff->dataStart += decoded;
        buff->dataCount -= decoded;
        aio->unread(buff);
    } else {
        // Give whole buffer back to aio subsystem
        aio->queueReadBuffer(buff);
    }
}

void SslHandler::eof(SslIO&) {
    QPID_LOG(debug, "DISCONNECTED [" << identifier << "]");
    if (codec) codec->closed();
    aio->queueWriteClose();
}

void SslHandler::closedSocket(SslIO&, const SslSocket& s) {
    // If we closed with data still to send log a warning
    if (!aio->writeQueueEmpty()) {
        QPID_LOG(warning, "CLOSING [" << identifier << "] unsent data (probably due to client disconnect)");
    }
    delete &s;
    aio->queueForDeletion();
    delete this;
}

void SslHandler::disconnect(SslIO& a) {
    // treat the same as eof
    eof(a);
}

// Notifications
void SslHandler::nobuffs(SslIO&) {
}

void SslHandler::idle(SslIO&){
    if (isClient && codec == 0) {
        codec = factory->create(*this, identifier, aio->getKeyLen());
        write(framing::ProtocolInitiation(codec->getVersion()));
        return;
    }
    if (codec == 0) return;
    if (codec->canEncode()) {
        // Try and get a queued buffer if not then construct new one
        SslIO::BufferBase* buff = aio->getQueuedBuffer();
        if (!buff) buff = new Buff;
        size_t encoded=codec->encode(buff->bytes, buff->byteCount);
        buff->dataCount = encoded;
        aio->queueWrite(buff);
    }
    if (codec->isClosed())
        aio->queueWriteClose();
}


}}} // namespace qpid::sys::ssl
