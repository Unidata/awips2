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
#include "qpid/client/Connector.h"

#include "config.h"
#include "qpid/client/Bounds.h"
#include "qpid/client/ConnectionImpl.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/Options.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/Time.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/sys/ssl/util.h"
#include "qpid/sys/ssl/SslIo.h"
#include "qpid/sys/ssl/SslSocket.h"
#include "qpid/sys/Dispatcher.h"
#include "qpid/sys/Poller.h"
#include "qpid/Msg.h"

#include <iostream>
#include <map>
#include <boost/bind.hpp>
#include <boost/format.hpp>

namespace qpid {
namespace client {

using namespace qpid::sys;
using namespace qpid::sys::ssl;
using namespace qpid::framing;
using boost::format;
using boost::str;


class SslConnector : public Connector, private sys::Runnable
{
    struct Buff;

    /** Batch up frames for writing to aio. */
    class Writer : public framing::FrameHandler {
        typedef sys::ssl::SslIOBufferBase BufferBase;
        typedef std::vector<framing::AMQFrame> Frames;

        const uint16_t maxFrameSize;
        sys::Mutex lock;
        sys::ssl::SslIO* aio;
        BufferBase* buffer;
        Frames frames;
        size_t lastEof; // Position after last EOF in frames
        framing::Buffer encode;
        size_t framesEncoded;
        std::string identifier;
        Bounds* bounds;        
        
        void writeOne();
        void newBuffer();

      public:
        
        Writer(uint16_t maxFrameSize, Bounds*);
        ~Writer();
        void init(std::string id, sys::ssl::SslIO*);
        void handle(framing::AMQFrame&);
        void write(sys::ssl::SslIO&);
    };
    
    const uint16_t maxFrameSize;
    framing::ProtocolVersion version;
    bool initiated;

    sys::Mutex closedLock;    
    bool closed;
    bool joined;

    sys::ShutdownHandler* shutdownHandler;
    framing::InputHandler* input;
    framing::InitiationHandler* initialiser;
    framing::OutputHandler* output;

    Writer writer;
    
    sys::Thread receiver;

    sys::ssl::SslSocket socket;

    sys::ssl::SslIO* aio;
    boost::shared_ptr<sys::Poller> poller;

    ~SslConnector();

    void run();
    void handleClosed();
    bool closeInternal();
    
    void readbuff(qpid::sys::ssl::SslIO&, qpid::sys::ssl::SslIOBufferBase*);
    void writebuff(qpid::sys::ssl::SslIO&);
    void writeDataBlock(const framing::AMQDataBlock& data);
    void eof(qpid::sys::ssl::SslIO&);

    std::string identifier;

    ConnectionImpl* impl;
    
    void connect(const std::string& host, int port);
    void init();
    void close();
    void send(framing::AMQFrame& frame);
    void abort() {} // TODO: Need to fix for heartbeat timeouts to work

    void setInputHandler(framing::InputHandler* handler);
    void setShutdownHandler(sys::ShutdownHandler* handler);
    sys::ShutdownHandler* getShutdownHandler() const;
    framing::OutputHandler* getOutputHandler();
    const std::string& getIdentifier() const;

public:
    SslConnector(framing::ProtocolVersion pVersion,
              const ConnectionSettings&, 
              ConnectionImpl*);
    unsigned int getSSF() { return socket.getKeyLen(); }
};

// Static constructor which registers connector here
namespace {
    Connector* create(framing::ProtocolVersion v, const ConnectionSettings& s, ConnectionImpl* c) {
        return new SslConnector(v, s, c);
    }

    struct StaticInit {
        StaticInit() {
            try {
                SslOptions options;
                options.parse (0, 0, QPIDC_CONF_FILE, true);
                if (options.certDbPath.empty()) {
                    QPID_LOG(info, "SSL connector not enabled, you must set QPID_SSL_CERT_DB to enable it.");                    
                } else {
                    initNSS(options);                
                    Connector::registerFactory("ssl", &create);
                }
            } catch (const std::exception& e) {
                QPID_LOG(error, "Failed to initialise SSL connector: " << e.what());
            }
        };

        ~StaticInit() { shutdownNSS(); }
    } init;
}

SslConnector::SslConnector(ProtocolVersion ver,
                     const ConnectionSettings& settings,
                     ConnectionImpl* cimpl)
    : maxFrameSize(settings.maxFrameSize),
      version(ver), 
      initiated(false),
      closed(true),
      joined(true),
      shutdownHandler(0),
      writer(maxFrameSize, cimpl),
      aio(0),
      impl(cimpl)
{
    QPID_LOG(debug, "SslConnector created for " << version.toString());
    //TODO: how do we want to handle socket configuration with ssl?
    //settings.configureSocket(socket);
}

SslConnector::~SslConnector() {
    close();
}

void SslConnector::connect(const std::string& host, int port){
    Mutex::ScopedLock l(closedLock);
    assert(closed);
    try {
        socket.connect(host, port);
    } catch (const std::exception& e) {
        socket.close();
        throw;
    }

    identifier = str(format("[%1% %2%]") % socket.getLocalPort() % socket.getPeerAddress());
    closed = false;
    poller = Poller::shared_ptr(new Poller);
    aio = new SslIO(socket,
                       boost::bind(&SslConnector::readbuff, this, _1, _2),
                       boost::bind(&SslConnector::eof, this, _1),
                       boost::bind(&SslConnector::eof, this, _1),
                       0, // closed
                       0, // nobuffs
                       boost::bind(&SslConnector::writebuff, this, _1));
    writer.init(identifier, aio);
}

void SslConnector::init(){
    Mutex::ScopedLock l(closedLock);
    assert(joined);
    ProtocolInitiation init(version);
    writeDataBlock(init);
    joined = false;
    receiver = Thread(this);
}

bool SslConnector::closeInternal() {
    Mutex::ScopedLock l(closedLock);
    bool ret = !closed;
    if (!closed) {
        closed = true;
        aio->queueForDeletion();
        poller->shutdown();
    }
    if (!joined && receiver.id() != Thread::current().id()) {
        joined = true;
        Mutex::ScopedUnlock u(closedLock);
        receiver.join();
    }
    return ret;
}
        
void SslConnector::close() {
    closeInternal();
}

void SslConnector::setInputHandler(InputHandler* handler){
    input = handler;
}

void SslConnector::setShutdownHandler(ShutdownHandler* handler){
    shutdownHandler = handler;
}

OutputHandler* SslConnector::getOutputHandler() {
    return this; 
}

sys::ShutdownHandler* SslConnector::getShutdownHandler() const {
    return shutdownHandler;
}

const std::string& SslConnector::getIdentifier() const { 
    return identifier;
}

void SslConnector::send(AMQFrame& frame) {
    writer.handle(frame);
}

void SslConnector::handleClosed() {
    if (closeInternal() && shutdownHandler)
        shutdownHandler->shutdown();
}

struct SslConnector::Buff : public SslIO::BufferBase {
    Buff(size_t size) : SslIO::BufferBase(new char[size], size) {}    
    ~Buff() { delete [] bytes;}
};

SslConnector::Writer::Writer(uint16_t s, Bounds* b) : maxFrameSize(s), aio(0), buffer(0), lastEof(0), bounds(b)
{
}

SslConnector::Writer::~Writer() { delete buffer; }

void SslConnector::Writer::init(std::string id, sys::ssl::SslIO* a) {
    Mutex::ScopedLock l(lock);
    identifier = id;
    aio = a;
    newBuffer();
}
void SslConnector::Writer::handle(framing::AMQFrame& frame) { 
    Mutex::ScopedLock l(lock);
    frames.push_back(frame);
    if (frame.getEof() || (bounds && bounds->getCurrentSize() >= maxFrameSize)) {
        lastEof = frames.size();
        aio->notifyPendingWrite();
    }
    QPID_LOG(trace, "SENT " << identifier << ": " << frame);
}

void SslConnector::Writer::writeOne() {
    assert(buffer);
    framesEncoded = 0;

    buffer->dataStart = 0;
    buffer->dataCount = encode.getPosition();
    aio->queueWrite(buffer);
    newBuffer();
}

void SslConnector::Writer::newBuffer() {
    buffer = aio->getQueuedBuffer();
    if (!buffer) buffer = new Buff(maxFrameSize);
    encode = framing::Buffer(buffer->bytes, buffer->byteCount);
    framesEncoded = 0;
}

// Called in IO thread.
void SslConnector::Writer::write(sys::ssl::SslIO&) {
    Mutex::ScopedLock l(lock);
    assert(buffer);
    size_t bytesWritten(0);
    for (size_t i = 0; i < lastEof; ++i) {
        AMQFrame& frame = frames[i];
        uint32_t size = frame.encodedSize();
        if (size > encode.available()) writeOne();
        assert(size <= encode.available());
        frame.encode(encode);
        ++framesEncoded;
        bytesWritten += size;
    }
    frames.erase(frames.begin(), frames.begin()+lastEof);
    lastEof = 0;
    if (bounds) bounds->reduce(bytesWritten);
    if (encode.getPosition() > 0) writeOne();
}

void SslConnector::readbuff(SslIO& aio, SslIO::BufferBase* buff) {
    framing::Buffer in(buff->bytes+buff->dataStart, buff->dataCount);

    if (!initiated) {
        framing::ProtocolInitiation protocolInit;
        if (protocolInit.decode(in)) {
            //TODO: check the version is correct
            QPID_LOG(debug, "RECV " << identifier << " INIT(" << protocolInit << ")");
        }
        initiated = true;
    }
    AMQFrame frame;
    while(frame.decode(in)){
        QPID_LOG(trace, "RECV " << identifier << ": " << frame);
        input->received(frame);
    }
    // TODO: unreading needs to go away, and when we can cope
    // with multiple sub-buffers in the general buffer scheme, it will
    if (in.available() != 0) {
        // Adjust buffer for used bytes and then "unread them"
        buff->dataStart += buff->dataCount-in.available();
        buff->dataCount = in.available();
        aio.unread(buff);
    } else {
        // Give whole buffer back to aio subsystem
        aio.queueReadBuffer(buff);
    }
}

void SslConnector::writebuff(SslIO& aio_) {
    writer.write(aio_);
}

void SslConnector::writeDataBlock(const AMQDataBlock& data) {
    SslIO::BufferBase* buff = new Buff(maxFrameSize);
    framing::Buffer out(buff->bytes, buff->byteCount);
    data.encode(out);
    buff->dataCount = data.encodedSize();
    aio->queueWrite(buff);
}

void SslConnector::eof(SslIO&) {
    handleClosed();
}

void SslConnector::run(){
    // Keep the connection impl in memory until run() completes.
    boost::shared_ptr<ConnectionImpl> protect = impl->shared_from_this();
    assert(protect);
    try {
        Dispatcher d(poller);
	
        for (int i = 0; i < 32; i++) {
            aio->queueReadBuffer(new Buff(maxFrameSize));
        }
	
        aio->start(poller);
        d.run();
        socket.close();
    } catch (const std::exception& e) {
        QPID_LOG(error, e.what());
        handleClosed();
    }
}


}} // namespace qpid::client
