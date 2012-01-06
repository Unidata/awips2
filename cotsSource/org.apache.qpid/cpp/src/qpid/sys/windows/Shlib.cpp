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

#include "qpid/sys/Shlib.h"
#include "qpid/Exception.h"
#include "qpid/sys/windows/check.h"
#include <windows.h>

namespace qpid {
namespace sys {

void Shlib::load(const char* name) {
    HMODULE h = LoadLibrary(name);
    if (h == NULL) {
        throw QPID_WINDOWS_ERROR(GetLastError());
    }
    handle = static_cast<void*>(h);
}

void  Shlib::unload() {
    if (handle) {
        if (FreeLibrary(static_cast<HMODULE>(handle)) == 0) {
            throw QPID_WINDOWS_ERROR(GetLastError());
        }
        handle = 0;
    }
}

void*  Shlib::getSymbol(const char* name) {
    void* sym = GetProcAddress(static_cast<HMODULE>(handle), name);
    if (sym == NULL)
        throw QPID_WINDOWS_ERROR(GetLastError());
    return sym;
}

}} // namespace qpid::sys
