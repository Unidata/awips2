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

/*
 * TODO: Note this is really a Posix specific implementation and so should be
 * refactored together with windows/QpiddBroker into a more coherent daemon driver/
 * platform specific split
 */
#include "qpid/broker/Daemon.h"
#include "qpid/log/Statement.h"
#include "qpid/Exception.h"
#include "qpid/sys/posix/PidFile.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

namespace qpid {
namespace broker {

using namespace std;
using qpid::sys::PidFile;

Daemon::Daemon(std::string _pidDir) : pidDir(_pidDir) {
    struct stat s;
    pid = -1;
    pipeFds[0] = pipeFds[1] = -1;

    if (::stat(pidDir.c_str(), &s)) {
        if (errno == ENOENT) {
            if (::mkdir(pidDir.c_str(), 0755))
                throw Exception ("Can't create PID directory: " + pidDir);
        }
        else
            throw Exception ("PID directory not found: " + pidDir);
    }
}

string Daemon::pidFile(string pidDir, uint16_t port) {
    ostringstream path;
    path << pidDir << "/qpidd." << port << ".pid";
    return path.str();
}

/*
 * Rewritten using low-level IO, for compatibility 
 * with earlier Boost versions, i.e. 103200.
 */
void Daemon::fork()
{
    if(::pipe(pipeFds) < 0) throw ErrnoException("Can't create pipe");  
    if ((pid = ::fork()) < 0) throw ErrnoException("Daemon fork failed");
    if (pid == 0) {             // Child
        try {
            QPID_LOG(debug, "Forked daemon child process");
            
            // File descriptors
            if(::close(pipeFds[0])<0) throw ErrnoException("Cannot close read pipe");
            if(::close(0)<0) throw ErrnoException("Cannot close stdin");
            if(::close(1)<0) throw ErrnoException("Cannot close stdout");
            if(::close(2)<0) throw ErrnoException("Cannot close stderr");
            int fd=::open("/dev/null",O_RDWR); // stdin
            if(fd != 0) throw ErrnoException("Cannot re-open stdin");
            if(::dup(fd)<0) throw ErrnoException("Cannot re-open stdout");
            if(::dup(fd)<0) throw ErrnoException("Cannot re-open stderror");

            // Misc
            if(setsid()<0) throw ErrnoException("Cannot set session ID");
            if(chdir(pidDir.c_str()) < 0) throw ErrnoException("Cannot change directory to "+pidDir);
            umask(027);

            // Child behavior
            child();
        }
        catch (const exception& e) {
            QPID_LOG(critical, "Unexpected error: " << e.what());
            uint16_t port = 0;
            int unused_ret;    //Supress warning about ignoring return value.
            unused_ret = write(pipeFds[1], &port, sizeof(uint16_t));

            std::string pipeFailureMessage = e.what();
            unused_ret = write ( pipeFds[1], 
                    pipeFailureMessage.c_str(), 
                    strlen(pipeFailureMessage.c_str())
                  );
        }
    }
    else {                      // Parent
        close(pipeFds[1]);      // Write side.
        parent();
    }
}

Daemon::~Daemon() {
    if (!lockFile.empty()) 
        unlink(lockFile.c_str());
}

uint16_t Daemon::wait(int timeout) {            // parent waits for child.
    try {
        errno = 0;                  
        struct timeval tv;
        tv.tv_sec = timeout;
        tv.tv_usec = 0;

        /*
         * Rewritten using low-level IO, for compatibility 
         * with earlier Boost versions, i.e. 103200.
         */
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(pipeFds[0], &fds);
        int n=select(FD_SETSIZE, &fds, 0, 0, &tv);
        if(n==0) throw Exception("Timed out waiting for daemon (If store recovery is in progress, use longer wait time)");
        if(n<0) throw ErrnoException("Error waiting for daemon");
        uint16_t port = 0;
        /*
         * Read the child's port number from the pipe.
         */
        int desired_read = sizeof(uint16_t);
        if ( desired_read > ::read(pipeFds[0], & port, desired_read) ) 
            throw Exception("Cannot read from child process.");

        /*
         * If the port number is 0, the child has put an error message
         * on the pipe.  Get it and throw it.
         */
        if ( 0 == port ) {
            // Skip whitespace
            char c = ' ';
            while ( isspace(c) ) {
                if ( 1 > ::read(pipeFds[0], &c, 1) )
                    throw Exception("Child port == 0, and no error message on pipe.");
            }

            // Get Message
            string errmsg;
            do {
                errmsg += c;
            } while (::read(pipeFds[0], &c, 1));
            throw Exception("Daemon startup failed"+
                            (errmsg.empty() ? string(".") : ": " + errmsg));
        }
        return port;
    }
    catch (const std::exception& e) {
        // Print directly to cerr. The caller will catch and log the
        // exception, but in the case of a daemon parent process we
        // also need to be sure the error goes to stderr. A
        // dameon's logging configuration normally does not log to
        // stderr. 
        std::cerr << e.what() << endl;
        throw;
    }
}


/*
 * When the child is ready, it writes its pid to the
 * lockfile and its port number on the pipe back to
 * its parent process.  This indicates that the
 * child has successfully daemonized.  When the parent
 * hears the good news, it ill exit.
 */
void Daemon::ready(uint16_t port) { // child
    lockFile = pidFile(pidDir, port);
    PidFile lf(lockFile, true);

    /*
     * Write the PID to the lockfile.
     */
    lf.writePid();

    /*
     * Write the port number to the parent.
     */
     int desired_write = sizeof(uint16_t);
     if ( desired_write > ::write(pipeFds[1], & port, desired_write) ) {
       throw Exception("Error writing to parent." );
     }

     QPID_LOG(debug, "Daemon ready on port: " << port);
}

/*
 * The parent process reads the child's pid
 * from the lockfile.
 */
pid_t Daemon::getPid(string _pidDir, uint16_t port) {
    string name = pidFile(_pidDir, port);
    PidFile lf(name, false);
    pid_t pid = lf.readPid();
    if (kill(pid, 0) < 0 && errno != EPERM) {
        unlink(name.c_str());
        throw Exception("Removing stale lock file "+name);
    }
    return pid;
}


}} // namespace qpid::broker
