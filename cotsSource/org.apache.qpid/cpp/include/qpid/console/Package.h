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
#ifndef _QPID_CONSOLE_PACKAGE_H_
#define _QPID_CONSOLE_PACKAGE_H_

#include <string>
#include <map>
#include "qpid/console/ConsoleImportExport.h"
#include "qpid/sys/IntegerTypes.h"

namespace qpid {
namespace console {
    struct SchemaClass;

    /**
     *
     * \ingroup qmfconsoleapi
     */
    class Package {
    public:
        Package(const std::string& n) : name(n) {}
        const std::string& getName() const { return name; }

    private:
        friend class SessionManager;
        struct NameHash {
            std::string name;
            uint8_t     hash[16];
            NameHash(const std::string& n, const uint8_t* h) : name(n) {
                for (int i = 0; i < 16; i++)
                    hash[i] = h[i];
            }
        };

        struct NameHashComp {
            bool operator() (const NameHash& lhs, const NameHash& rhs) const
            {
                if (lhs.name != rhs.name)
                    return lhs.name < rhs.name;
                else
                    for (int i = 0; i < 16; i++)
                        if (lhs.hash[i] != rhs.hash[i])
                            return lhs.hash[i] < rhs.hash[i];
                return false;
            }
        };

        typedef std::map<NameHash, SchemaClass*, NameHashComp> ClassMap;

        const std::string name;
        ClassMap classes;

        SchemaClass* getClass(const std::string& className, uint8_t* hash);
        void addClass(const std::string& className, uint8_t* hash,
                      SchemaClass* schemaClass);
    };
}
}

#endif
