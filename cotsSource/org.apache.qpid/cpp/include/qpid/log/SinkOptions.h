#ifndef QPID_LOG_SINKOPTIONS_H
#define QPID_LOG_SINKOPTIONS_H

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

#include "qpid/Options.h"
#include <string>

namespace qpid {
namespace log {

class Logger;

/**
 * Logging sink options.
 *
 * Most logging sink options will be platform-specific, even if some are
 * duplicated. The range of platforms to which this code may be ported
 * can't be assumed to all have C++ iostreams or files. Thus, this class
 * is primarily for implementing in a platform-specific way.
 */
struct SinkOptions : public qpid::Options {

    // Create a platform's SinkOptions. Pass argv0 as the program name,
    // useful for syslog-type logging.
    static SinkOptions *create(const std::string& argv0=std::string());

    SinkOptions(const std::string& name="Logging sink options")
        : qpid::Options(name)
    {}
    virtual ~SinkOptions() {}

    virtual SinkOptions& operator=(const SinkOptions&) = 0;

    // This allows the caller to indicate that there's no normal outputs
    // available. For example, when running as a daemon. In these cases, the
    // platform's "syslog"-type output should replace the default stderr
    // unless some other sink has been selected.
    virtual void detached(void) = 0;

    // The Logger acting on these options calls setup() to request any
    // Sinks be set up and fed back to the logger.
    virtual void setup(Logger *logger) = 0;
};

}} // namespace qpid::log

#endif  /*!QPID_LOG_OPTIONS_H*/
