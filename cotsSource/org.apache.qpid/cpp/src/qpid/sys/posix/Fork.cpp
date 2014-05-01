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
#include "qpid/sys/Fork.h"
#include "qpid/log/Statement.h"
#include "qpid/Exception.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <sys/types.h>
#include <unistd.h>

namespace qpid {
namespace sys {

using namespace std;

namespace {

void writeStr(int fd, const std::string& str) {
    const char* WRITE_ERR = "Error writing to parent process";
    int size = str.size();
    if (int(sizeof(size)) > ::write(fd, &size, sizeof(size))) throw ErrnoException(WRITE_ERR);
    if (size > ::write(fd, str.data(), size)) throw ErrnoException(WRITE_ERR);
}

string readStr(int fd) {
    string value;
    const char* READ_ERR = "Error reading from forked process";
    int size;
    if (int(sizeof(size)) > ::read(fd, &size, sizeof(size))) throw ErrnoException(READ_ERR);
    if (size > 0) {          // Read string message
        value.resize(size);
        if (size > ::read(fd, const_cast<char*>(value.data()), size)) throw ErrnoException(READ_ERR);
    }
    return value;
}

} // namespace

Fork::Fork() {}
Fork::~Fork() {}

void Fork::fork() {
    pid_t pid = ::fork();
    if (pid < 0) throw ErrnoException("Failed to fork the process");
    if (pid == 0) child();
    else parent(pid);
}

ForkWithMessage::ForkWithMessage() {
    pipeFds[0] = pipeFds[1] = -1;
}

struct AutoCloseFd {
    int fd;
    AutoCloseFd(int d) : fd(d) {}
    ~AutoCloseFd() { ::close(fd); }
};

void ForkWithMessage::fork() {
    if(::pipe(pipeFds) < 0) throw ErrnoException("Can't create pipe");
    pid_t pid = ::fork();
    if(pid < 0) throw ErrnoException("Fork fork failed");
    if (pid == 0) {             // Child
        AutoCloseFd ac(pipeFds[1]); // Write side.
        ::close(pipeFds[0]); // Read side
        try {
            child();
        }
        catch (const std::exception& e) {
            QPID_LOG(error, "Error in forked child: " << e.what());
            std::string msg = e.what();
            if (msg.empty()) msg = " "; // Make sure we send a non-empty error string.
            writeStr(pipeFds[1], msg);
        }
    }
    else {                      // Parent
        close(pipeFds[1]);      // Write side.
        AutoCloseFd ac(pipeFds[0]); // Read side
        parent(pid);
    }
}

string ForkWithMessage::wait(int timeout) { // parent waits for child.
    errno = 0;                  
    struct timeval tv;
    tv.tv_sec = timeout;
    tv.tv_usec = 0;

    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(pipeFds[0], &fds);
    int n=select(FD_SETSIZE, &fds, 0, 0, &tv);
    if(n<0) throw ErrnoException("Error waiting for fork");
    if (n==0) throw Exception("Timed out waiting for fork");

    string error = readStr(pipeFds[0]);
    if (error.empty()) return readStr(pipeFds[0]);
    else throw Exception("Error in forked process: " + error);
}

// Write empty error string followed by value string to pipe.
void ForkWithMessage::ready(const string& value) { // child
    // Write empty string for error followed by value.
    writeStr(pipeFds[1], string()); // No error
    writeStr(pipeFds[1], value); 
}


}} // namespace qpid::sys
