/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

/**@file

   The watchdog plug-in will kill the qpidd broker process if it
   becomes stuck for longer than a configured interval.

   If the watchdog plugin is loaded and the --watchdog-interval=N
   option is set then the broker starts a watchdog process and signals
   it every N/2 seconds.

   The watchdog process runs a very simple program that starts a timer
   for N seconds, and resets the timer to N seconds whenever it is
   signalled by the broker. If the timer ever reaches 0 the watchdog
   kills the broker process (with kill -9) and exits.

   This is useful in a cluster setting because in some insttances
   (e.g. while resolving an error) it's possible for a stuck process
   to hang other cluster members that are waiting for it to send a
   message.  Using the watchdog, the stuck process is terminated and 
   removed fromt the cluster allowing other members to continue and
   clients of the stuck process to fail over to other members.

*/
#include "qpid/Plugin.h"
#include "qpid/Options.h"
#include "qpid/log/Statement.h"
#include "qpid/broker/Broker.h"
#include "qpid/sys/Timer.h"
#include "qpid/sys/Fork.h"
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>

namespace qpid {
namespace cluster {

using broker::Broker;

struct Settings {
    Settings() : interval(0) {}
    int interval;
};

struct WatchDogOptions : public qpid::Options {
    Settings& settings;

    WatchDogOptions(Settings& s) : settings(s) {
        addOptions()
            ("watchdog-interval", optValue(settings.interval, "N"),
             "broker is automatically killed if it is hung for more than \
	      N seconds. 0 disables watchdog.");
    }
};

struct WatchDogTask : public sys::TimerTask {
    int pid;
    sys::Timer& timer;
    int interval;

    WatchDogTask(int pid_, sys::Timer& t, int _interval)
        : TimerTask(_interval*sys::TIME_SEC/2), pid(pid_), timer(t), interval(_interval) {}

    void fire() {
        timer.add (new WatchDogTask(pid, timer, interval));
        QPID_LOG(debug, "Sending keepalive signal to watchdog");
        ::kill(pid, SIGUSR1);
    }
};

struct WatchDogPlugin : public qpid::Plugin, public qpid::sys::Fork {
    Settings settings;
    WatchDogOptions options;
    Broker* broker;
    int watchdogPid;

    WatchDogPlugin() : options(settings), broker(0), watchdogPid(0) {}

    ~WatchDogPlugin() {
        if (watchdogPid) ::kill(watchdogPid, SIGTERM);
        ::waitpid(watchdogPid, 0, 0);
    }

    Options* getOptions() { return &options; }

    void earlyInitialize(qpid::Plugin::Target& target) {
        broker = dynamic_cast<Broker*>(&target);
        if (broker && settings.interval) {
            QPID_LOG(notice, "Starting watchdog process with interval of " <<
                     settings.interval << " seconds");
            fork();
        }
    }

    void initialize(Target&) {}

  protected:

    void child() {              // Child of fork
        const char* watchdog = ::getenv("QPID_WATCHDOG_EXEC"); // For use in tests
        if (!watchdog) watchdog=QPID_EXEC_DIR "/qpidd_watchdog";
        std::string interval = boost::lexical_cast<std::string>(settings.interval);
        ::execl(watchdog, watchdog, interval.c_str(), NULL);
        QPID_LOG(critical, "Failed to exec watchdog program " << watchdog );
        ::kill(::getppid(), SIGKILL);
        exit(1);
    }

    void parent(int pid) {          // Parent of fork
        watchdogPid = pid;
        broker->getTimer().add(
            new WatchDogTask(watchdogPid, broker->getTimer(), settings.interval));
        // TODO aconway 2009-08-10: to be extra safe, we could monitor
        // the watchdog child and re-start it if it exits.
    }
};

static WatchDogPlugin instance; // Static initialization.

}} // namespace qpid::cluster
