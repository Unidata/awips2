#ifndef QPID_LOG_POSIX_SINKOPTIONS_H
#define QPID_LOG_POSIX_SINKOPTIONS_H

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

#include "qpid/log/SinkOptions.h"
#include <string>

namespace qpid {
namespace log {
namespace posix {

/**
 * Provides a type that can be passed to << and >> operators to convert
 * syslog facility values to/from strings.
 */
struct SyslogFacility {
    int value;
    SyslogFacility(int i=0) : value(i) {}
};

struct SinkOptions : public qpid::log::SinkOptions {
    SinkOptions(const std::string& argv0);
    virtual ~SinkOptions() {}

    virtual qpid::log::SinkOptions& operator=(const qpid::log::SinkOptions& rhs);

    // This allows the caller to indicate that there's no normal outputs
    // available. For example, when running as a daemon. In these cases, the
    // platform's "syslog"-type output should replace the default stderr
    // unless some other sink has been selected.
    virtual void detached(void);

    // The Logger acting on these options calls setup() to request any
    // Sinks be set up and fed back to the logger.
    virtual void setup(qpid::log::Logger *logger);

    bool logToStderr;
    bool logToStdout;
    bool logToSyslog;
    std::string logFile;
    std::string syslogName;
    SyslogFacility syslogFacility;
};

}}} // namespace qpid::log::posix

#endif  /*!QPID_LOG_POSIX_SINKOPTIONS_H*/
