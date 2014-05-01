#ifndef TEST_TOOLS_H
#define TEST_TOOLS_H

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
#include "qpid/log/Logger.h"

#include <limits.h>             // Include before boost/test headers.
#include <boost/test/test_tools.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/regex.hpp>
#include <boost/assign/list_of.hpp>
#include <vector>
#include <set>
#include <ostream>
#include <sstream>
#include <exception>

// Print a sequence
template <class T> std::ostream& seqPrint(std::ostream& o, const T& seq) {
    std::copy(seq.begin(), seq.end(), std::ostream_iterator<typename T::value_type>(o, " "));
    return o;
}

// Compare sequences
template <class T, class U>
bool seqEqual(const T& a, const U& b) {
    typename T::const_iterator i = a.begin();
    typename U::const_iterator j = b.begin();
    while (i != a.end() && j != b.end() && *i == *j) { ++i; ++j; }
    return (i == a.end()) && (j == b.end());
}

// ostream and == operators so we can compare vectors and sets with
// boost::assign::list_of with BOOST_CHECK_EQUALS
namespace std {                 // In namespace std so boost can find them.

template <class T>
ostream& operator<<(ostream& o, const vector<T>& v) { return seqPrint(o, v); }

template <class T>
ostream& operator<<(ostream& o, const set<T>& v) { return seqPrint(o, v); }

template <class T>
ostream& operator<<(ostream& o, const boost::assign_detail::generic_list<T>& l) { return seqPrint(o, l); }

template <class T>
bool operator == (const vector<T>& a, const boost::assign_detail::generic_list<T>& b) { return seqEqual(a, b); }

template <class T>
bool operator == (const boost::assign_detail::generic_list<T>& b, const vector<T>& a) { return seqEqual(a, b); }

template <class T>
bool operator == (const set<T>& a, const boost::assign_detail::generic_list<T>& b) { return seqEqual(a, b); }

template <class T>
bool operator == (const boost::assign_detail::generic_list<T>& b, const set<T>& a) { return seqEqual(a, b); }
}

namespace qpid {
namespace tests {

/** NB: order of parameters is regex first, in line with
 * CHECK(expected, actual) convention.
 */
inline bool regexPredicate(const std::string& re, const std::string& text) {
    return boost::regex_match(text, boost::regex(re));
}

/** Check for regular expression match. You must #include <boost/regex.hpp> */
#if (BOOST_VERSION < 103300)
  #define BOOST_CHECK_REGEX(re, text)
#else
  #define BOOST_CHECK_REGEX(re, text) \
    BOOST_CHECK_PREDICATE(regexPredicate, (re)(text))
#endif

/** Check if types of two objects (as given by typeinfo::name()) match. */
#define BOOST_CHECK_TYPEID_EQUAL(a,b) BOOST_CHECK_EQUAL(typeid(a).name(),typeid(b).name())

/**
 * Supress all logging in a scope, restore to previous configuration in destructor.
 */
struct ScopedSuppressLogging {
    typedef qpid::log::Logger  Logger;
    ScopedSuppressLogging(Logger& l=Logger::instance()) : logger(l), opts(l.getOptions()) { l.clear(); }
    ~ScopedSuppressLogging() { logger.configure(opts); }
    Logger& logger;
    qpid::log::Options opts;
};

inline std::string getLibPath(const char* envName, const char* defaultPath = 0) {
    const char* p = std::getenv(envName);
    if (p != 0)
        return p;
    if (defaultPath == 0) {
        std::ostringstream msg;
        msg << "Environment variable " << envName << " not set.";
        throw std::runtime_error(msg.str());
    }
    return defaultPath;
}

}} // namespace qpid::tests

#endif  /*!TEST_TOOLS_H*/

