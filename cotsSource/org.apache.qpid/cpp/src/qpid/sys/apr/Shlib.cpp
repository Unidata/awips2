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
#include "qpid/sys/apr/APRBase.h"
#include "qpid/sys/apr/APRPool.h"
#include <apr_dso.h>

namespace qpid {
namespace sys {

void Shlib::load(const char* libname) {
    apr_dso_handle_t* aprHandle; 
    CHECK_APR_SUCCESS(
        apr_dso_load(&aprHandle, libname, APRPool::get()));
    handle=aprHandle;
}

void  Shlib::unload() {
    CHECK_APR_SUCCESS(
        apr_dso_unload(static_cast<apr_dso_handle_t*>(handle)));
}

void*  Shlib::getSymbol(const char* name) {
    apr_dso_handle_sym_t symbol;
    CHECK_APR_SUCCESS(apr_dso_sym(&symbol,
                                  static_cast<apr_dso_handle_t*>(handle),
                                  name));
    return (void*) symbol;
}

}} // namespace qpid::sys
