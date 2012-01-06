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

#include "qpid/client/TCPConnector.h"

#include "qpid/client/ConnectionImpl.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/Codec.h"
#include "qpid/sys/Time.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/sys/AsynchIO.h"
#include "qpid/sys/Dispatcher.h"
#include "qpid/sys/Poller.h"
#include "qpid/sys/SecurityLayer.h"
#include "qpid/Msg.h"

#include <iostream>
#include <boost/bind.hpp>
#include <boost/format.hpp>

namespace qpid {
namespace client {

using namespace qpid::sys;
using namespace qpid::framing;
using boost::format;
using boost::str;

// Static constructor which registers connector here
namespace {
    Connector* create(framing::ProtocolVersion v, const ConnectionSettings& s, ConnectionImpl* c) {
        return new TCPConnector(v, s, c);
    }

    struct StaticInit {
        StaticInit() {
            Connector::registerFactory("tcp", &create);
        };
    } init;
}

struct TCPConnector::Buff : public AsynchIO::BufferBase {
    Buff(size_t size) : AsynchIO::BufferBase(new char[size], size) {}
    ~Buff() { delete [] bytes;}
};

TCPConnector::TCPConnector(ProtocolVersion ver,
                           const ConnectionSettings& settings,
                           ConnectionImpl* cimpl)
    : maxFrameSize(settings.maxFrameSize),
      lastEof(0),
      currentSize(0),
      bounds(cimpl),
      version(ver), 
      initiated(false),
      closed(true),
      joined(true),
      shutdownHandler(0),
      aio(0),
      impl(cimpl->shared_from_this())
{
    QPID_LOG(debug, "TCPConnector created for " << version.toString());
    settings.configureSocket(socket);
}

TCPConnector::~TCPConnector() {
    close();
}

void TCPConnector::connect(const std::string& host, int port) {
    Mutex::ScopedLock l(lock);
    assert(closed);
    assert(joined);
    poller = Poller::shared_ptr(new Poller);
    AsynchConnector::create(socket,
                            poller,
                            host, port,
                            boost::bind(&TCPConnector::connected, this, _1),
                            boost::bind(&TCPConnector::connectFailed, this, _3));
    closed = false;
    joined = false;
    receiver = Thread(this);
}

void TCPConnector::connected(const Socket&) {
    aio = AsynchIO::create(socket,
                       boost::bind(&TCPConnector::readbuff, this, _1, _2),
                       boost::bind(&TCPConnector::eof, this, _1),
                       boost::bind(&TCPConnector::eof, this, _1),
                       0, // closed
                       0, // nobuffs
                       boost::bind(&TCPConnector::writebuff, this, _1));
    for (int i = 0; i < 32; i++) {
        aio->queueReadBuffer(new Buff(maxFrameSize));
    }
    aio->start(poller);

    identifier = str(format("[%1% %2%]") % socket.getLocalPort() % socket.getPeerAddress());
    ProtocolInitiation init(version);
    writeDataBlock(init);
}

void TCPConnector::connectFailed(const std::string& msg) {
    QPID_LOG(warning, "Connecting failed: " << msg);
    closed = true;
    poller->shutdown();
    closeInternal();
    if (shutdownHandler)
        shutdownHandler->shutdown();
}

bool TCPConnector::closeInternal() {
    bool ret;
    {
    Mutex::ScopedLock l(lock);
    ret = !closed;
    if (!closed) {
        closed = true;
        aio->queueForDeletion();
        poller->shutdown();
    }
    if (joined || receiver.id() == Thread::current().id()) {
        return ret;
    }
    joined = true;
    }
    receiver.join();
    return ret;
}

void TCPConnector::close() {
    closeInternal();
}

void TCPConnector::abort() {
    // Can't abort a closed connection
    if (!closed) {
        if (aio) {
            // Established connection
            aio->requestCallback(boost::bind(&TCPConnector::eof, this, _1));
        } else {
            // We're still connecting
            connectFailed("Connection timedout");
        }
    }
}

void TCPConnector::setInputHandler(InputHandler* handler){
    input = handler;
}

void TCPConnector::setShutdownHandler(ShutdownHandler* handler){
    shutdownHandler = handler;
}

OutputHandler* TCPConnector::getOutputHandler() {
    return this; 
}

sys::ShutdownHandler* TCPConnector::getShutdownHandler() const {
    return shutdownHandler;
}

const std::string& TCPConnector::getIdentifier() const { 
    return identifier;
}

void TCPConnector::send(AMQFrame& frame) {
    Mutex::ScopedLock l(lock);
    frames.push_back(frame);
    //only ask to write if this is the end of a frameset or if we
    //already have a buffers worth of data
    currentSize += frame.encodedSize();
    bool notifyWrite = false;
    if (frame.getEof()) {
        lastEof = frames.size();
        notifyWrite = true;
    } else {
        notifyWrite = (currentSize >= maxFrameSize);
    }
    if (notifyWrite && !closed) aio->notifyPendingWrite();
}

void TCPConnector::handleClosed() {
    if (closeInternal() && shutdownHandler)
        shutdownHandler->shutdown();
}

void TCPConnector::writebuff(AsynchIO& /*aio*/) 
{
    Codec* codec = securityLayer.get() ? (Codec*) securityLayer.get() : (Codec*) this;
    if (codec->canEncode()) {
        std::auto_ptr<AsynchIO::BufferBase> buffer = std::auto_ptr<AsynchIO::BufferBase>(aio->getQueuedBuffer());
        if (!buffer.get()) buffer = std::auto_ptr<AsynchIO::BufferBase>(new Buff(maxFrameSize));

        size_t encoded = codec->encode(buffer->bytes, buffer->byteCount);

        buffer->dataStart = 0;
        buffer->dataCount = encoded;
        aio->queueWrite(buffer.release());
    }
}

// Called in IO thread.
bool TCPConnector::canEncode()
{
    Mutex::ScopedLock l(lock);
    //have at least one full frameset or a whole buffers worth of data
    return lastEof || currentSize >= maxFrameSize;
}

// Called in IO thread.
size_t TCPConnector::encode(const char* buffer, size_t size)
{
    framing::Buffer out(const_cast<char*>(buffer), size);
    size_t bytesWritten(0);
    {
        Mutex::ScopedLock l(lock);
        while (!frames.empty() && out.available() >= frames.front().encodedSize() ) {
            frames.front().encode(out);
            QPID_LOG(trace, "SENT " << identifier << ": " << frames.front());
            frames.pop_front();
            if (lastEof) --lastEof;
        }
        bytesWritten = size - out.available();
        currentSize -= bytesWritten;
    }
    if (bounds) bounds->reduce(bytesWritten);
    return bytesWritten;
}

bool TCPConnector::readbuff(AsynchIO& aio, AsynchIO::BufferBase* buff) 
{
    Codec* codec = securityLayer.get() ? (Codec*) securityLayer.get() : (Codec*) this;
    int32_t decoded = codec->decode(buff->bytes+buff->dataStart, buff->dataCount);
    // TODO: unreading needs to go away, and when we can cope
    // with multiple sub-buffers in the general buffer scheme, it will
    if (decoded < buff->dataCount) {
        // Adjust buffer for used bytes and then "unread them"
        buff->dataStart += decoded;
        buff->dataCount -= decoded;
        aio.unread(buff);
    } else {
        // Give whole buffer back to aio subsystem
        aio.queueReadBuffer(buff);
    }
    return true;
}

size_t TCPConnector::decode(const char* buffer, size_t size) 
{
    framing::Buffer in(const_cast<char*>(buffer), size);
    if (!initiated) {
        framing::ProtocolInitiation protocolInit;
        if (protocolInit.decode(in)) {
            QPID_LOG(debug, "RECV " << identifier << " INIT(" << protocolInit << ")");
            if(!(protocolInit==version)){
                throw Exception(QPID_MSG("Unsupported version: " << protocolInit
                                         << " supported version " << version));
            }
        }
        initiated = true;
    }
    AMQFrame frame;
    while(frame.decode(in)){
        QPID_LOG(trace, "RECV " << identifier << ": " << frame);
        input->received(frame);
    }
    return size - in.available();
}

void TCPConnector::writeDataBlock(const AMQDataBlock& data) {
    AsynchIO::BufferBase* buff = new Buff(maxFrameSize);
    framing::Buffer out(buff->bytes, buff->byteCount);
    data.encode(out);
    buff->dataCount = data.encodedSize();
    aio->queueWrite(buff);
}

void TCPConnector::eof(AsynchIO&) {
    handleClosed();
}

void TCPConnector::run() {
    // Keep the connection impl in memory until run() completes.
    boost::shared_ptr<ConnectionImpl> protect = impl.lock();
    assert(protect);
    try {
        Dispatcher d(poller);

        d.run();
    } catch (const std::exception& e) {
        QPID_LOG(error, QPID_MSG("FAIL " << identifier << ": " << e.what()));
        handleClosed();
    }
    try {
        socket.close();
    } catch (const std::exception&) {}
}

void TCPConnector::activateSecurityLayer(std::auto_ptr<qpid::sys::SecurityLayer> sl)
{
    securityLayer = sl;
    securityLayer->init(this);
}


}} // namespace qpid::client
