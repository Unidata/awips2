/*
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

#include "qpid/Plugin.h"
#include "qpid/Options.h"
#include <boost/bind.hpp>
#include <algorithm>

namespace qpid {

namespace {

Plugin::Plugins& thePlugins() {
    // This is a single threaded singleton implementation so
    // it is important to be sure that the first use of this
    // singleton is when the program is still single threaded
    static Plugin::Plugins plugins;
    return plugins;
}

void invoke(boost::function<void()> f) { f(); }

} // namespace

Plugin::Target::~Target() { finalize(); }

void Plugin::Target::finalize() {
    std::for_each(finalizers.begin(), finalizers.end(), invoke);
    finalizers.clear();
}

void Plugin::Target::addFinalizer(const boost::function<void()>& f) {
    finalizers.push_back(f);
}

namespace {
bool initBefore(const Plugin* a, const Plugin* b) {
    return a->initOrder() < b->initOrder();
}
}

Plugin::Plugin() {
    // Register myself.
    thePlugins().push_back(this);
    std::sort(thePlugins().begin(), thePlugins().end(), &initBefore);
}

Plugin::~Plugin() {}

Options*  Plugin::getOptions() { return 0; }

const Plugin::Plugins& Plugin::getPlugins() { return thePlugins(); }

namespace {
template <class F> void each_plugin(const F& f) {
    std::for_each(Plugin::getPlugins().begin(), Plugin::getPlugins().end(), f);
}
}

void Plugin::addOptions(Options& opts) {
    for (Plugins::const_iterator i = getPlugins().begin(); i != getPlugins().end(); ++i) {
        if ((*i)->getOptions())
            opts.add(*(*i)->getOptions());
    }
}

int Plugin::initOrder() const { return DEFAULT_INIT_ORDER; }

void Plugin::earlyInitAll(Target& t) {
    each_plugin(boost::bind(&Plugin::earlyInitialize, _1, boost::ref(t)));
}

void Plugin::initializeAll(Target& t) {
    each_plugin(boost::bind(&Plugin::initialize, _1, boost::ref(t)));
}

} // namespace qpid
