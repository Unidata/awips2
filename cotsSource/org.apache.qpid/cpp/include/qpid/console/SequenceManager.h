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
#ifndef _QPID_CONSOLE_SEQUENCEMANAGER_H_
#define _QPID_CONSOLE_SEQUENCEMANAGER_H_

#include "qpid/console/ConsoleImportExport.h"
#include "qpid/sys/Mutex.h"
#include <map>
#include <string>
#include <set>

namespace qpid {
namespace console {

    /**
     *
     * \ingroup qpidconsoleapi
     */
    class SequenceManager {
    public:
        typedef std::set<uint32_t> set;

        SequenceManager() : sequence(0) {}
        QPID_CONSOLE_EXTERN uint32_t reserve(const std::string& context = "");
        QPID_CONSOLE_EXTERN std::string release(uint32_t seq);

    private:
        sys::Mutex lock;
        uint32_t sequence;
        std::map<uint32_t, std::string> pending;
    };
}
}


#endif
