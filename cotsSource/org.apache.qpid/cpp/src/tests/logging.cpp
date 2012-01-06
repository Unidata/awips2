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

#include "test_tools.h"
#include "qpid/log/Logger.h"
#include "qpid/log/Options.h"
#include "qpid/log/OstreamOutput.h"
#include "qpid/memory.h"
#include "qpid/Options.h"
#if defined (_WIN32)
#  include "qpid/log/windows/SinkOptions.h"
#else
#  include "qpid/log/posix/SinkOptions.h"
#endif

#include <boost/test/floating_point_comparison.hpp>
#include <boost/format.hpp>
#include "unit_test.h"

#include <exception>
#include <fstream>
#include <time.h>


namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(loggingTestSuite)

using namespace std;
using namespace boost;
using namespace qpid::log;

QPID_AUTO_TEST_CASE(testStatementInit) {
    Statement s=QPID_LOG_STATEMENT_INIT(debug); int line=__LINE__;
    BOOST_CHECK(!s.enabled);
    BOOST_CHECK_EQUAL(string(__FILE__), s.file);
    BOOST_CHECK_EQUAL(line, s.line);
    BOOST_CHECK_EQUAL(debug, s.level);
}


QPID_AUTO_TEST_CASE(testSelector_enable) {
    Selector s;
    // Simple enable
    s.enable(debug,"foo");
    BOOST_CHECK(s.isEnabled(debug,"foo"));
    BOOST_CHECK(!s.isEnabled(error,"foo"));
    BOOST_CHECK(!s.isEnabled(error,"bar"));

    // Substring match
    BOOST_CHECK(s.isEnabled(debug, "bazfoobar"));
    BOOST_CHECK(!s.isEnabled(debug, "bazbar"));

    // Different levels for different substrings.
    s.enable(info, "bar");
    BOOST_CHECK(s.isEnabled(debug, "foobar"));
    BOOST_CHECK(s.isEnabled(info, "foobar"));
    BOOST_CHECK(!s.isEnabled(debug, "bar"));
    BOOST_CHECK(!s.isEnabled(info, "foo"));

    // Enable-strings
    s.enable("notice:blob");
    BOOST_CHECK(s.isEnabled(notice, "blob"));
    s.enable("error+:oops");
    BOOST_CHECK(s.isEnabled(error, "oops"));
    BOOST_CHECK(s.isEnabled(critical, "oops"));
}

QPID_AUTO_TEST_CASE(testStatementEnabled) {
    // Verify that the singleton enables and disables static
    // log statements.
    Logger& l = Logger::instance();
    ScopedSuppressLogging ls(l);
    l.select(Selector(debug));
    static Statement s=QPID_LOG_STATEMENT_INIT(debug);
    BOOST_CHECK(!s.enabled);
    static Statement::Initializer init(s);
    BOOST_CHECK(s.enabled);

    static Statement s2=QPID_LOG_STATEMENT_INIT(warning);
    static Statement::Initializer init2(s2);
    BOOST_CHECK(!s2.enabled);

    l.select(Selector(warning));
    BOOST_CHECK(!s.enabled);
    BOOST_CHECK(s2.enabled);
}

struct TestOutput : public Logger::Output {
    vector<string> msg;
    vector<Statement> stmt;

    TestOutput(Logger& l) {
        l.output(std::auto_ptr<Logger::Output>(this));
    }

    void log(const Statement& s, const string& m) {
        msg.push_back(m);
        stmt.push_back(s);
    }
    string last() { return msg.back(); }
};

using boost::assign::list_of;

QPID_AUTO_TEST_CASE(testLoggerOutput) {
    Logger l;
    l.clear();
    l.select(Selector(debug));
    Statement s=QPID_LOG_STATEMENT_INIT(debug);

    TestOutput* out=new TestOutput(l);

    // Verify message is output.
    l.log(s, "foo");
    vector<string> expect=list_of("foo\n");
    BOOST_CHECK_EQUAL(expect, out->msg);

    // Verify multiple outputs
    TestOutput* out2=new TestOutput(l);
    l.log(Statement(), "baz");
    expect.push_back("baz\n");
    BOOST_CHECK_EQUAL(expect, out->msg);
    expect.erase(expect.begin());
    BOOST_CHECK_EQUAL(expect, out2->msg);
}

QPID_AUTO_TEST_CASE(testMacro) {
    Logger& l=Logger::instance();
    ScopedSuppressLogging ls(l);
    l.select(Selector(info));
    TestOutput* out=new TestOutput(l);
    QPID_LOG(info, "foo");
    vector<string> expect=list_of("foo\n");
    BOOST_CHECK_EQUAL(expect, out->msg);
    BOOST_CHECK_EQUAL(__FILE__, out->stmt.front().file);

    // Not enabled:
    QPID_LOG(debug, "bar");
    BOOST_CHECK_EQUAL(expect, out->msg);

    QPID_LOG(info, 42 << " bingo");
    expect.push_back("42 bingo\n");
    BOOST_CHECK_EQUAL(expect, out->msg);
}

QPID_AUTO_TEST_CASE(testLoggerFormat) {
    Logger& l = Logger::instance();
    ScopedSuppressLogging ls(l);
    l.select(Selector(critical));
    TestOutput* out=new TestOutput(l);

    l.format(Logger::FILE);
    QPID_LOG(critical, "foo");
    BOOST_CHECK_EQUAL(out->last(), string(__FILE__)+": foo\n");

    l.format(Logger::FILE|Logger::LINE);
    QPID_LOG(critical, "foo");
    BOOST_CHECK_REGEX(string(__FILE__)+":\\d+: foo\n", out->last());

    l.format(Logger::FUNCTION);
    QPID_LOG(critical, "foo");
    BOOST_CHECK_EQUAL(string(BOOST_CURRENT_FUNCTION) + ": foo\n", out->last());

    l.format(Logger::LEVEL);
    QPID_LOG(critical, "foo");
    BOOST_CHECK_EQUAL("critical foo\n", out->last());
}

QPID_AUTO_TEST_CASE(testOstreamOutput) {
    Logger& l=Logger::instance();
    ScopedSuppressLogging ls(l);
    l.select(Selector(error));
    ostringstream os;
    l.output(qpid::make_auto_ptr<Logger::Output>(new OstreamOutput(os)));
    QPID_LOG(error, "foo");
    QPID_LOG(error, "bar");
    QPID_LOG(error, "baz");
    BOOST_CHECK_EQUAL("foo\nbar\nbaz\n", os.str());
}

#if 0 // This test requires manual intervention. Normally disabled.
QPID_AUTO_TEST_CASE(testSyslogOutput) {
    Logger& l=Logger::instance();
    Logger::StateSaver ls(l);
    l.clear();
    l.select(Selector(info));
    l.syslog("qpid_test");
    QPID_LOG(info, "Testing QPID");
    BOOST_ERROR("Manually verify that /var/log/messages contains a recent line 'Testing QPID'");
}
#endif // 0

int count() {
    static int n = 0;
    return n++;
}

int loggedCount() {
    static int n = 0;
    QPID_LOG(debug, "counting: " << n);
    return n++;
}


using namespace qpid::sys;

// Measure CPU time.
clock_t timeLoop(int times, int (*fp)()) {
    clock_t start=clock();
    while (times-- > 0)
        (*fp)();
    return clock() - start;
}

// Overhead test disabled because it consumes a ton of CPU and takes
// forever under valgrind. Not friendly for regular test runs.
//
#if 0
QPID_AUTO_TEST_CASE(testOverhead) {
    // Ensure that the ratio of CPU time for an incrementing loop
    // with and without disabled log statements is in  acceptable limits.
    //
    int times=100000000;
    clock_t noLog=timeLoop(times, count);
    clock_t withLog=timeLoop(times, loggedCount);
    double ratio=double(withLog)/double(noLog);

    // NB: in initial tests the ratio was consistently below 1.5,
    // 2.5 is reasonable and should avoid spurios failures
    // due to machine load.
    //
    BOOST_CHECK_SMALL(ratio, 2.5);
}
#endif // 0

Statement statement(
    Level level, const char* file="", int line=0, const char* fn=0)
{
    Statement s={0, file, line, fn, level};
    return s;
}


#define ARGC(argv) (sizeof(argv)/sizeof(char*))

QPID_AUTO_TEST_CASE(testOptionsParse) {
    const char* argv[]={
        0,
        "--log-enable", "error+:foo",
        "--log-enable", "debug:bar",
        "--log-enable", "info",
        "--log-to-stderr", "no",
        "--log-to-file", "logout",
        "--log-level", "yes",
        "--log-source", "1",
        "--log-thread", "true",
        "--log-function", "YES"
    };
    qpid::log::Options opts("");
#ifdef _WIN32
    qpid::log::windows::SinkOptions sinks("test");
#else
    qpid::log::posix::SinkOptions sinks("test");
#endif
    opts.parse(ARGC(argv), const_cast<char**>(argv));
    sinks = *opts.sinkOptions;
    vector<string> expect=list_of("error+:foo")("debug:bar")("info");
    BOOST_CHECK_EQUAL(expect, opts.selectors);
    BOOST_CHECK(!sinks.logToStderr);
    BOOST_CHECK(!sinks.logToStdout);
    BOOST_CHECK(sinks.logFile == "logout");
    BOOST_CHECK(opts.level);
    BOOST_CHECK(opts.source);
    BOOST_CHECK(opts.function);
    BOOST_CHECK(opts.thread);
}

QPID_AUTO_TEST_CASE(testOptionsDefault) {
    qpid::log::Options opts("");
#ifdef _WIN32
    qpid::log::windows::SinkOptions sinks("test");
#else
    qpid::log::posix::SinkOptions sinks("test");
#endif
    sinks = *opts.sinkOptions;
    BOOST_CHECK(sinks.logToStderr);
    BOOST_CHECK(!sinks.logToStdout);
    BOOST_CHECK(sinks.logFile.length() == 0);
    vector<string> expect=list_of("notice+");
    BOOST_CHECK_EQUAL(expect, opts.selectors);
    BOOST_CHECK(opts.time && opts.level);
    BOOST_CHECK(!(opts.source || opts.function || opts.thread));
}

QPID_AUTO_TEST_CASE(testSelectorFromOptions) {
    const char* argv[]={
        0,
        "--log-enable", "error+:foo",
        "--log-enable", "debug:bar",
        "--log-enable", "info"
    };
    qpid::log::Options opts("");
    opts.parse(ARGC(argv), const_cast<char**>(argv));
    vector<string> expect=list_of("error+:foo")("debug:bar")("info");
    BOOST_CHECK_EQUAL(expect, opts.selectors);
    Selector s(opts);
    BOOST_CHECK(!s.isEnabled(warning, "x"));
    BOOST_CHECK(!s.isEnabled(debug, "x"));
    BOOST_CHECK(s.isEnabled(debug, "bar"));
    BOOST_CHECK(s.isEnabled(error, "foo"));
    BOOST_CHECK(s.isEnabled(critical, "foo"));
}

QPID_AUTO_TEST_CASE(testLoggerStateure) {
    Logger& l=Logger::instance();
    ScopedSuppressLogging ls(l);
    qpid::log::Options opts("test");
    const char* argv[]={
        0,
        "--log-time", "no",
        "--log-source", "yes",
        "--log-to-stderr", "no",
        "--log-to-file", "logging.tmp",
        "--log-enable", "critical"
    };
    opts.parse(ARGC(argv), const_cast<char**>(argv));
    l.configure(opts);
    QPID_LOG(critical, "foo"); int srcline=__LINE__;
    ifstream log("logging.tmp");
    string line;
    getline(log, line);
    string expect=(format("critical %s:%d: foo")%__FILE__%srcline).str();
    BOOST_CHECK_EQUAL(expect, line);
    log.close();
    unlink("logging.tmp");
}

QPID_AUTO_TEST_CASE(testQuoteNonPrintable) {
    Logger& l=Logger::instance();
    ScopedSuppressLogging ls(l);
    qpid::log::Options opts("test");
    opts.time=false;
#ifdef _WIN32
    qpid::log::windows::SinkOptions *sinks =
      dynamic_cast<qpid::log::windows::SinkOptions *>(opts.sinkOptions.get());
#else
    qpid::log::posix::SinkOptions *sinks =
      dynamic_cast<qpid::log::posix::SinkOptions *>(opts.sinkOptions.get());
#endif
    sinks->logToStderr = false;
    sinks->logFile = "logging.tmp";
    l.configure(opts);

    char s[] = "null\0tab\tspace newline\nret\r\x80\x99\xff";
    string str(s, sizeof(s));
    QPID_LOG(critical, str);
    ifstream log("logging.tmp");
    string line;
    getline(log, line, '\0');
    string expect="critical null\\x00tab\tspace newline\nret\r\\x80\\x99\\xFF\\x00\n";
    BOOST_CHECK_EQUAL(expect, line);
    log.close();
    unlink("logging.tmp");
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
