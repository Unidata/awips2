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
#include <dlfcn.h>


namespace qpid {
namespace sys {

void Shlib::load(const char* name) {
    ::dlerror();
    handle = ::dlopen(name, RTLD_NOW);
    const char* error = ::dlerror();
    if (error) {
        throw Exception(QPID_MSG(error << ": " << name));
    }
}

void  Shlib::unload() {
    if (handle) {
        ::dlerror();
        ::dlclose(handle);
        const char* error = ::dlerror();
        if (error) {
            throw Exception(QPID_MSG(error));
        }
        handle = 0;
    }
}

void*  Shlib::getSymbol(const char* name) {
    ::dlerror();
    void* sym = ::dlsym(handle, name);
    const char* error = ::dlerror();
    if (error) 
        throw Exception(QPID_MSG(error << ": " << name));
    return sym;
}

}} // namespace qpid::sys
