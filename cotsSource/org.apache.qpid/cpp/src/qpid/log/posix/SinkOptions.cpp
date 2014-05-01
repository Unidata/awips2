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

#include "qpid/log/posix/SinkOptions.h"
#include "qpid/log/SinkOptions.h"
#include "qpid/log/Logger.h"
#include "qpid/log/OstreamOutput.h"
#include "qpid/memory.h"
#include "qpid/Exception.h"
#include <iostream>
#include <map>
#include <string>
#include <syslog.h>

using std::string;
using qpid::Exception;

namespace {

// SyslogFacilities maps from syslog values to the text equivalents.
class SyslogFacilities {
public:
    typedef std::map<string, int> ByName;
    typedef std::map<int, string> ByValue;

    SyslogFacilities() {
        struct NameValue { const char* name; int value; };
        NameValue nameValue[] = {
            { "AUTH", LOG_AUTH },
#ifdef HAVE_LOG_AUTHPRIV            
            { "AUTHPRIV", LOG_AUTHPRIV },
#endif
            { "CRON", LOG_CRON },
            { "DAEMON", LOG_DAEMON },
#ifdef HAVE_LOG_FTP
            { "FTP", LOG_FTP },
#endif
            { "KERN", LOG_KERN },
            { "LOCAL0", LOG_LOCAL0 },
            { "LOCAL1", LOG_LOCAL1 },
            { "LOCAL2", LOG_LOCAL2 },
            { "LOCAL3", LOG_LOCAL3 },
            { "LOCAL4", LOG_LOCAL4 },
            { "LOCAL5", LOG_LOCAL5 },
            { "LOCAL6", LOG_LOCAL6 },
            { "LOCAL7", LOG_LOCAL7 },
            { "LPR", LOG_LPR },
            { "MAIL", LOG_MAIL },
            { "NEWS", LOG_NEWS },
            { "SYSLOG", LOG_SYSLOG },
            { "USER", LOG_USER },
            { "UUCP", LOG_UUCP }
        };
        for (size_t i = 0; i < sizeof(nameValue)/sizeof(nameValue[0]); ++i) {
            byName.insert(ByName::value_type(nameValue[i].name, nameValue[i].value));
            // Recognise with and without LOG_ prefix e.g.: AUTH and LOG_AUTH
            byName.insert(ByName::value_type(string("LOG_")+nameValue[i].name, nameValue[i].value));
            byValue.insert(ByValue::value_type(nameValue[i].value, string("LOG_")+nameValue[i].name));
        }
    }
    
    int value(const string& name) const {
        string key(name);
        std::transform(key.begin(), key.end(), key.begin(), ::toupper);        
        ByName::const_iterator i = byName.find(key);
        if (i == byName.end())
            throw Exception("Not a valid syslog facility: " + name);
        return i->second;
    }

    string name(int value) const {
        ByValue::const_iterator i = byValue.find(value);
        if (i == byValue.end())
            throw Exception("Not a valid syslog value: " + value);
        return i->second;
    }

  private:
    ByName byName;
    ByValue byValue;
};

// 'priorities' maps qpid log levels to syslog priorities. They are in
// order of qpid log levels and must map to:
// "trace", "debug", "info", "notice", "warning", "error", "critical"
static int priorities[qpid::log::LevelTraits::COUNT] = {
        LOG_DEBUG, LOG_DEBUG, LOG_INFO, LOG_NOTICE,
        LOG_WARNING, LOG_ERR, LOG_CRIT
};

std::string basename(const std::string path) {
    size_t i = path.find_last_of('/');
    return path.substr((i == std::string::npos) ? 0 : i+1);
}

} // namespace

namespace qpid {
namespace log {
namespace posix {

std::ostream& operator<<(std::ostream& o, const SyslogFacility& f) {
    return o << SyslogFacilities().name(f.value);
}

std::istream& operator>>(std::istream& i, SyslogFacility& f) {
    std::string name;
    i >> name;
    f.value = SyslogFacilities().value(name);
    return i;
}

class SyslogOutput : public qpid::log::Logger::Output {
public:
    SyslogOutput(const std::string& logName, const SyslogFacility& logFacility)
        : name(logName), facility(logFacility.value)
    {
        ::openlog(name.c_str(), LOG_PID, facility);
    }

    virtual ~SyslogOutput() {
        ::closelog();
    }
    
    virtual void log(const Statement& s, const std::string& m)
    {
        syslog(priorities[s.level], "%s", m.c_str());
    }

private:    
    std::string name;
    int facility;
};

SinkOptions::SinkOptions(const std::string& argv0)
    : qpid::log::SinkOptions(),
      logToStderr(true),
      logToStdout(false),
      logToSyslog(false),
      syslogName(basename(argv0)),
      syslogFacility(LOG_DAEMON) {

    addOptions()
      ("log-to-stderr", optValue(logToStderr, "yes|no"), "Send logging output to stderr")
      ("log-to-stdout", optValue(logToStdout, "yes|no"), "Send logging output to stdout")
      ("log-to-file", optValue(logFile, "FILE"), "Send log output to FILE.")
      ("log-to-syslog", optValue(logToSyslog, "yes|no"), "Send logging output to syslog;\n\tcustomize using --syslog-name and --syslog-facility")
      ("syslog-name", optValue(syslogName, "NAME"), "Name to use in syslog messages")
      ("syslog-facility", optValue(syslogFacility,"LOG_XXX"), "Facility to use in syslog messages")
      ;

}

qpid::log::SinkOptions& SinkOptions::operator=(const qpid::log::SinkOptions& rhs) {
    const SinkOptions *prhs = dynamic_cast<const SinkOptions*>(&rhs);
    if (this != prhs) {
        logToStderr = prhs->logToStderr;
        logToStdout = prhs->logToStdout;
        logToSyslog = prhs->logToSyslog;
        logFile = prhs->logFile;
        syslogName = prhs->syslogName;
        syslogFacility.value = prhs->syslogFacility.value;
    }
    return *this;
}

void SinkOptions::detached(void) {
    if (logToStderr && !logToStdout && !logToSyslog) {
        logToStderr = false;
        logToSyslog = true;
    }
}

// The Logger acting on these options calls setup() to request any
// Sinks be set up and fed back to the logger.
void SinkOptions::setup(qpid::log::Logger *logger) {
    if (logToStderr)
        logger->output(make_auto_ptr<qpid::log::Logger::Output>
                         (new qpid::log::OstreamOutput(std::clog)));
    if (logToStdout)
        logger->output(make_auto_ptr<qpid::log::Logger::Output>
                         (new qpid::log::OstreamOutput(std::cout)));

    if (logFile.length() > 0)
        logger->output(make_auto_ptr<qpid::log::Logger::Output>
                         (new qpid::log::OstreamOutput(logFile)));

    if (logToSyslog)
        logger->output(make_auto_ptr<qpid::log::Logger::Output>
                         (new SyslogOutput(syslogName, syslogFacility)));

}

} // namespace qpid::log::posix

SinkOptions* SinkOptions::create(const std::string& argv0) {
    return new qpid::log::posix::SinkOptions (argv0);
}

}} // namespace qpid::log
