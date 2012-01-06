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

#include "ForkedBroker.h"
#include "qpid/log/Statement.h"
#include <boost/bind.hpp>
#include <algorithm>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>

using namespace std;
using qpid::ErrnoException;

namespace std {
static ostream& operator<<(ostream& o, const qpid::tests::ForkedBroker::Args& a) {
copy(a.begin(), a.end(), ostream_iterator<string>(o, " "));
return o;
}
}

namespace qpid {
namespace tests {

ForkedBroker::ForkedBroker(const Args& constArgs) {
    Args args(constArgs);
    Args::iterator i = find(args.begin(), args.end(), string("TMP_DATA_DIR"));
    if (i != args.end()) {
        args.erase(i);
        char dd[] = "/tmp/ForkedBroker.XXXXXX";
        if (!mkdtemp(dd))
            throw qpid::ErrnoException("Can't create data dir");
        dataDir = dd;
        args.push_back("--data-dir");
        args.push_back(dataDir);
    }
    init(args);
}

ForkedBroker::~ForkedBroker() {
    try { kill(); }
    catch (const std::exception& e) {
        QPID_LOG(error, QPID_MSG("Killing forked broker: " << e.what()));
    }
    if (!dataDir.empty())
    {
        int unused_ret; // Suppress warnings about ignoring return value.
        unused_ret = ::system(("rm -rf "+dataDir).c_str());
    }
}

void ForkedBroker::kill(int sig) {
    if (pid == 0) return;
    int savePid = pid;
    pid = 0;                // Reset pid here in case of an exception.
    using qpid::ErrnoException;
    if (::kill(savePid, sig) < 0)
            throw ErrnoException("kill failed");
    int status;
    if (::waitpid(savePid, &status, 0) < 0 && sig != 9)
        throw ErrnoException("wait for forked process failed");
    if (WEXITSTATUS(status) != 0 && sig != 9)
        throw qpid::Exception(QPID_MSG("Forked broker exited with: " << WEXITSTATUS(status)));
}

bool isLogOption(const std::string& s) {
    const char * log_enable = "--log-enable",
               * trace      = "--trace";
    return( (! strncmp(s.c_str(), log_enable, strlen(log_enable))) ||
            (! strncmp(s.c_str(), trace,      strlen(trace)))
          );
}

void ForkedBroker::init(const Args& userArgs) {
    using qpid::ErrnoException;
    port = 0;
    int pipeFds[2];
    if(::pipe(pipeFds) < 0) throw ErrnoException("Can't create pipe");
    pid = ::fork();
    if (pid < 0) throw ErrnoException("Fork failed");
    if (pid) {              // parent
        ::close(pipeFds[1]);
        FILE* f = ::fdopen(pipeFds[0], "r");
        if (!f) throw ErrnoException("fopen failed");
        if (::fscanf(f, "%d", &port) != 1) {
            if (ferror(f)) throw ErrnoException("Error reading port number from child.");
            else throw qpid::Exception("EOF reading port number from child.");
        }
        ::close(pipeFds[0]);
    }
    else {                  // child
        ::close(pipeFds[0]);
        int fd = ::dup2(pipeFds[1], 1); // pipe stdout to the parent.
        if (fd < 0) throw ErrnoException("dup2 failed");
        const char* prog = ::getenv("QPIDD_EXEC");
        if (!prog) prog = "../qpidd"; // This only works from within svn checkout
        Args args(userArgs);
        args.push_back("--port=0");
        // Keep quiet except for errors.
        if (!::getenv("QPID_TRACE") && !::getenv("QPID_LOG_ENABLE")
            && find_if(userArgs.begin(), userArgs.end(), isLogOption) == userArgs.end())
            args.push_back("--log-enable=error+");
        std::vector<const char*> argv(args.size());
        std::transform(args.begin(), args.end(), argv.begin(), boost::bind(&std::string::c_str, _1));
        argv.push_back(0);
        QPID_LOG(debug, "ForkedBroker exec " << prog << ": " << args);
        execv(prog, const_cast<char* const*>(&argv[0]));
        QPID_LOG(critical, "execv failed to start broker: prog=\"" << prog << "\"; args=\"" << args << "\"; errno=" << errno << " (" << std::strerror(errno) << ")");
        ::exit(1);
    }
}

}} // namespace qpid::tests
