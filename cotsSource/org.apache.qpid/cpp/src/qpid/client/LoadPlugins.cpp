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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "qpid/Modules.h"
#include "qpid/sys/Shlib.h"
#include <string>
#include <vector>
using std::vector;
using std::string;

namespace {

struct LoadtimeInitialise {
    LoadtimeInitialise() {
        qpid::ModuleOptions moduleOptions(QPIDC_MODULE_DIR);
        string              defaultPath (moduleOptions.loadDir);
        moduleOptions.parse (0, 0, QPIDC_CONF_FILE, true);
    
        for (vector<string>::iterator iter = moduleOptions.load.begin();
             iter != moduleOptions.load.end();
             iter++)
            qpid::tryShlib (iter->data(), false);
    
        if (!moduleOptions.noLoad) {
            bool isDefault = defaultPath == moduleOptions.loadDir;
            qpid::loadModuleDir (moduleOptions.loadDir, isDefault);
        }
    }
} init;

} // namespace
