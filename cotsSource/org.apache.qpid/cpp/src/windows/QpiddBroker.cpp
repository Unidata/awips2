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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#else
// These need to be made something sensible, like reading a value from
// the registry. But for now, get things going with a local definition.
namespace {
const char *QPIDD_CONF_FILE = "qpid_broker.conf";
const char *QPIDD_MODULE_DIR = ".";
}
#endif
#include "qpidd.h"
#include "qpid/Exception.h"
#include "qpid/Options.h"
#include "qpid/Plugin.h"
#include "qpid/sys/IntegerTypes.h"
#include "qpid/sys/windows/check.h"
#include "qpid/broker/Broker.h"

#include <iostream>
#include <windows.h>

using namespace qpid::broker;

BootstrapOptions::BootstrapOptions(const char* argv0)
  : qpid::Options("Options"),
    common("", QPIDD_CONF_FILE),
    module(QPIDD_MODULE_DIR),
    log(argv0)
{
    add(common);
    add(module);
    add(log);
}

// Local functions to set and get the pid via a LockFile.
namespace {

const std::string TCP = "tcp";

// ShutdownEvent maintains an event that can be used to ask the broker
// to stop. Analogous to sending SIGTERM/SIGINT to the posix broker.
// The signal() method signals the event.
class ShutdownEvent {
  public:
    ShutdownEvent(int port);
    ~ShutdownEvent();

    void create();
    void open();
    void signal();

  private:
    std::string eventName;

  protected:
    HANDLE event;
};

class ShutdownHandler : public ShutdownEvent, public qpid::sys::Runnable {
  public:
    ShutdownHandler(int port, const boost::intrusive_ptr<Broker>& b)
      : ShutdownEvent(port)  { broker = b; }

  private:
    virtual void run();     // Inherited from Runnable
    boost::intrusive_ptr<Broker> broker;
};

ShutdownEvent::ShutdownEvent(int port) : event(NULL) {
    std::ostringstream name;
    name << "qpidd_" << port << std::ends;
    eventName = name.str();
}

void ShutdownEvent::create() {
    // Auto-reset event in case multiple processes try to signal a
    // broker that doesn't respond for some reason. Initially not signaled.
    event = ::CreateEvent(NULL, false, false, eventName.c_str());
    QPID_WINDOWS_CHECK_NULL(event);
}

void ShutdownEvent::open() {
    // TODO: Might need to search Global\\ name if unadorned name fails
    event = ::OpenEvent(EVENT_MODIFY_STATE, false, eventName.c_str());
    QPID_WINDOWS_CHECK_NULL(event);
}

ShutdownEvent::~ShutdownEvent() {
    ::CloseHandle(event);
    event = NULL;
}

void ShutdownEvent::signal() {
    QPID_WINDOWS_CHECK_NOT(::SetEvent(event), 0);
}


void ShutdownHandler::run() {
    if (event == NULL)
        return;
    ::WaitForSingleObject(event, INFINITE);
    if (broker.get()) {
        broker->shutdown();
        broker = 0;             // Release the broker reference
    }
}

// Console control handler to properly handle ctl-c.
int ourPort;
BOOL CtrlHandler(DWORD ctl)
{
    ShutdownEvent shutter(ourPort);     // We have to have set up the port before interrupting
    shutter.open();
    shutter.signal();
    return ((ctl == CTRL_C_EVENT || ctl == CTRL_CLOSE_EVENT) ? TRUE : FALSE);
}

template <typename T>
class NamedSharedMemory {
    std::string name;
    HANDLE memory;
    T* data;

public:
    NamedSharedMemory(const std::string&);
    ~NamedSharedMemory();

    T& create();
    T& get();
};

template <typename T>
NamedSharedMemory<T>::NamedSharedMemory(const std::string& n) :
    name(n),
    memory(NULL),
    data(0)
{};

template <typename T>
NamedSharedMemory<T>::~NamedSharedMemory() {
    if (data)
        ::UnmapViewOfFile(data);
    if (memory != NULL)
        ::CloseHandle(memory);
};

template <typename T>
T& NamedSharedMemory<T>::create() {
    assert(memory == NULL);

    // Create named shared memory file
    memory = ::CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, sizeof(T), name.c_str());
    QPID_WINDOWS_CHECK_NULL(memory);

    // Map file into memory
    data = static_cast<T*>(::MapViewOfFile(memory, FILE_MAP_WRITE, 0, 0, 0));
    QPID_WINDOWS_CHECK_NULL(data);

    return *data;
}

template <typename T>
T& NamedSharedMemory<T>::get() {
    if (memory == NULL) {
        // TODO: Might need to search Global\\ name if unadorned name fails
        memory = ::OpenFileMapping(FILE_MAP_WRITE, FALSE, name.c_str());
        QPID_WINDOWS_CHECK_NULL(memory);

        data = static_cast<T*>(::MapViewOfFile(memory, FILE_MAP_WRITE, 0, 0, 0));
        QPID_WINDOWS_CHECK_NULL(data);
    }

    return *data;
}

std::string brokerInfoName(uint16_t port)
{
    std::ostringstream path;
    path << "qpidd_info_" << port;
    return path.str();
}

struct BrokerInfo {
    DWORD pid;
};

}

struct ProcessControlOptions : public qpid::Options {
    bool quit;
    bool check;
    //std::string transport;   No transport options yet - TCP is it.

    ProcessControlOptions()
        : qpid::Options("Process control options"),
          quit(false),
          check(false) //, transport(TCP)
    {
        // Only have TCP for now, so don't need this...
        //            ("transport", optValue(transport, "TRANSPORT"), "The transport for which to return the port")
        addOptions()
            ("check,c", qpid::optValue(check), "Prints the broker's process ID to stdout and returns 0 if the broker is running, otherwise returns 1")
            ("quit,q", qpid::optValue(quit), "Tells the broker to shut down");
    }
};

struct QpiddWindowsOptions : public QpiddOptionsPrivate {
    ProcessControlOptions control;
    QpiddWindowsOptions(QpiddOptions *parent) : QpiddOptionsPrivate(parent) {
        parent->add(control);
    }
};

QpiddOptions::QpiddOptions(const char* argv0)
  : qpid::Options("Options"),
    common("", QPIDD_CONF_FILE),
    module(QPIDD_MODULE_DIR),
    log(argv0)
{
    add(common);
    add(module);
    add(broker);
    add(log);

    platform.reset(new QpiddWindowsOptions(this));
    qpid::Plugin::addOptions(*this);
}

void QpiddOptions::usage() const {
    std::cout << "Usage: qpidd [OPTIONS]" << std::endl << std::endl
              << *this << std::endl;
}

int QpiddBroker::execute (QpiddOptions *options) {
    // Options that affect a running daemon.
    QpiddWindowsOptions *myOptions =
      reinterpret_cast<QpiddWindowsOptions *>(options->platform.get());
    if (myOptions == 0)
        throw qpid::Exception("Internal error obtaining platform options");

    if (myOptions->control.check || myOptions->control.quit) {
        // Relies on port number being set via --port or QPID_PORT env variable.
        NamedSharedMemory<BrokerInfo> info(brokerInfoName(options->broker.port));
        int pid = info.get().pid;
        if (pid < 0) 
            return 1;
        if (myOptions->control.check)
            std::cout << pid << std::endl;
        if (myOptions->control.quit) {
            ShutdownEvent shutter(options->broker.port);
            shutter.open();
            shutter.signal();
            HANDLE brokerHandle = ::OpenProcess(SYNCHRONIZE, false, pid);
            QPID_WINDOWS_CHECK_NULL(brokerHandle);
            ::WaitForSingleObject(brokerHandle, INFINITE);
            ::CloseHandle(brokerHandle);
        }
        return 0;
    }

    boost::intrusive_ptr<Broker> brokerPtr(new Broker(options->broker));

    // Need the correct port number to use in the pid file name.
    if (options->broker.port == 0)
        options->broker.port = brokerPtr->getPort("");

    BrokerInfo info;
    info.pid = ::GetCurrentProcessId();

    NamedSharedMemory<BrokerInfo> sharedInfo(brokerInfoName(options->broker.port));
    sharedInfo.create() = info;

    // Allow the broker to receive a shutdown request via a qpidd --quit
    // command. Note that when the broker is run as a service this operation
    // should not be allowed.
    ourPort = options->broker.port;
    ShutdownHandler waitShut(ourPort, brokerPtr);
    waitShut.create();
    qpid::sys::Thread waitThr(waitShut);   // Wait for shutdown event
    ::SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE);
    brokerPtr->accept();
    std::cout << options->broker.port << std::endl;
    brokerPtr->run();
    waitShut.signal();   // In case we shut down some other way
    waitThr.join();

    // CloseHandle(h);
    return 0;
}
