/*
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

#include "config.h"
#include "qpidd.h"
#include "qpid/Exception.h"
#include "qpid/broker/Broker.h"
#include "qpid/broker/Daemon.h"
#include "qpid/broker/SignalHandler.h"
#include "qpid/log/Logger.h"

#include <iostream>
#include <signal.h>
#include <unistd.h>
#include <sys/utsname.h>

using namespace std;
using namespace qpid;
using qpid::broker::Broker;
using qpid::broker::Daemon;

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

namespace {
const std::string TCP = "tcp";
}

struct DaemonOptions : public qpid::Options {
    bool daemon;
    bool quit;
    bool check;
    int wait;
    std::string piddir;
    std::string transport;

    DaemonOptions() : qpid::Options("Daemon options"), daemon(false), quit(false), check(false), wait(600), transport(TCP)
    {
        char *home = ::getenv("HOME");

        if (home == 0)
            piddir += "/tmp";
        else
            piddir += home;
        piddir += "/.qpidd";

        addOptions()
            ("daemon,d", optValue(daemon), "Run as a daemon. Logs to syslog by default in this mode.")
            ("transport", optValue(transport, "TRANSPORT"), "The transport for which to return the port")
            ("pid-dir", optValue(piddir, "DIR"), "Directory where port-specific PID file is stored")
            ("wait,w", optValue(wait, "SECONDS"), "Sets the maximum wait time to initialize the daemon. If the daemon fails to initialize, prints an error and returns 1")
            ("check,c", optValue(check), "Prints the daemon's process ID to stdout and returns 0 if the daemon is running, otherwise returns 1")
            ("quit,q", optValue(quit), "Tells the daemon to shut down");
    }
};

struct QpiddPosixOptions : public QpiddOptionsPrivate {
    DaemonOptions daemon;
    QpiddOptions *parent;

    QpiddPosixOptions(QpiddOptions *parent_) : parent(parent_) {
        parent->add(daemon);
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

    platform.reset(new QpiddPosixOptions(this));
    qpid::Plugin::addOptions(*this);
}

void QpiddOptions::usage() const {
    cout << "Usage: qpidd [OPTIONS]" << endl << endl << *this << endl;
}

struct QpiddDaemon : public Daemon {
    QpiddPosixOptions *options;
  
    QpiddDaemon(std::string pidDir, QpiddPosixOptions *opts)
      : Daemon(pidDir), options(opts) {}

    /** Code for parent process */
    void parent() {
        uint16_t port = wait(options->daemon.wait);
        if (options->parent->broker.port == 0 || options->daemon.transport != TCP)
            cout << port << endl; 
    }

    /** Code for forked child process */
    void child() {
        boost::intrusive_ptr<Broker> brokerPtr(new Broker(options->parent->broker));
        qpid::broker::SignalHandler::setBroker(brokerPtr);
        brokerPtr->accept();
        uint16_t port=brokerPtr->getPort(options->daemon.transport);
        ready(port);            // Notify parent.
        brokerPtr->run();
    }
};

int QpiddBroker::execute (QpiddOptions *options) {
    // Options that affect a running daemon.
    QpiddPosixOptions *myOptions =
      static_cast<QpiddPosixOptions *>(options->platform.get());
    if (myOptions == 0)
        throw Exception("Internal error obtaining platform options");

    if (myOptions->daemon.check || myOptions->daemon.quit) {
        pid_t pid = Daemon::getPid(myOptions->daemon.piddir,
                                   options->broker.port);
        if (pid < 0) 
            return 1;
        if (myOptions->daemon.check)
            cout << pid << endl;
        if (myOptions->daemon.quit) {
            if (kill(pid, SIGINT) < 0) 
                throw Exception("Failed to stop daemon: " + qpid::sys::strError(errno));
            // Wait for the process to die before returning
            int retry=10000;    // Try up to 10 seconds
            while (kill(pid,0) == 0 && --retry)
                sys::usleep(1000);
            if (retry == 0)
                throw Exception("Gave up waiting for daemon process to exit");
        }
        return 0;
    }

    // Starting the broker.
    if (myOptions->daemon.daemon) {
        // For daemon mode replace default stderr with syslog.
        options->log.sinkOptions->detached();
        qpid::log::Logger::instance().configure(options->log);
        // Fork the daemon
        QpiddDaemon d(myOptions->daemon.piddir, myOptions);
        d.fork();           // Broker is stared in QpiddDaemon::child()
    }
    else {                  // Non-daemon broker.
        boost::intrusive_ptr<Broker> brokerPtr(new Broker(options->broker));
        broker::SignalHandler::setBroker(brokerPtr);
        brokerPtr->accept();
        if (options->broker.port == 0 || myOptions->daemon.transport != TCP)
            cout << uint16_t(brokerPtr->getPort(myOptions->daemon.transport)) << endl;
        brokerPtr->run();
    }
    return 0;
}
