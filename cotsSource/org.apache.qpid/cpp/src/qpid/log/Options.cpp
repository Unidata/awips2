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

#include "qpid/log/Options.h"
#include "qpid/log/SinkOptions.h"
#include "qpid/log/Statement.h"
#include "qpid/Options.h"
#include <map>
#include <string>
#include <algorithm>

namespace qpid {
namespace log {

using namespace std;

Options::Options(const std::string& argv0_, const std::string& name_) :
    qpid::Options(name_),
    argv0(argv0_),
    name(name_),
    time(true),
    level(true),
    thread(false),
    source(false),
    function(false),
    trace(false),
    sinkOptions (SinkOptions::create(argv0_))
{
    selectors.push_back("notice+");

    ostringstream levels;
    levels << LevelTraits::name(Level(0));
    for (int i = 1; i < LevelTraits::COUNT; ++i)
        levels << " " << LevelTraits::name(Level(i));

    addOptions()
        ("trace,t", optValue(trace), "Enables all logging" )
        ("log-enable", optValue(selectors, "RULE"),
         ("Enables logging for selected levels and components. " 
          "RULE is in the form 'LEVEL[+][:PATTERN]' "
          "Levels are one of: \n\t "+levels.str()+"\n"
          "For example:\n"
          "\t'--log-enable warning+' "
          "logs all warning, error and critical messages.\n"
          "\t'--log-enable debug:framing' "
          "logs debug messages from the framing namespace. "
          "This option can be used multiple times").c_str())
        ("log-time", optValue(time, "yes|no"), "Include time in log messages")
        ("log-level", optValue(level,"yes|no"), "Include severity level in log messages")
        ("log-source", optValue(source,"yes|no"), "Include source file:line in log messages")
        ("log-thread", optValue(thread,"yes|no"), "Include thread ID in log messages")
        ("log-function", optValue(function,"yes|no"), "Include function signature in log messages")
        ("log-prefix", optValue(prefix,"STRING"), "Prefix to append to all log messages")
        ;
    add(*sinkOptions);
}

Options::Options(const Options &o) :
    qpid::Options(o.name),
    argv0(o.argv0),
    name(o.name),
    selectors(o.selectors),
    time(o.time),
    level(o.level),
    thread(o.thread),
    source(o.source),
    function(o.function),
    trace(o.trace),
    prefix(o.prefix),
    sinkOptions (SinkOptions::create(o.argv0))
{
    *sinkOptions = *o.sinkOptions;
}

Options& Options::operator=(const Options& x) {
    if (this != &x) {
        argv0 = x.argv0;
        name = x.name;
        selectors = x.selectors;
        time = x.time;
        level= x.level;
        thread = x.thread;
        source = x.source;
        function = x.function;
        trace = x.trace;
        prefix = x.prefix;
        *sinkOptions = *x.sinkOptions;
    }
    return *this;
}
        
}} // namespace qpid::log
