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

#include "State.h"
#include "DatabaseConnection.h"
#include "Exception.h"
#include <comdef.h>

namespace qpid {
namespace store {
namespace ms_sql {

State::State() : dbConn(0)
{
    HRESULT hr = ::CoInitializeEx(NULL, COINIT_MULTITHREADED);
    if (hr != S_OK && hr != S_FALSE)
        throw Exception("Error initializing COM");
}

State::~State()
{
    if (dbConn)
        delete dbConn;
    ::CoUninitialize();
}

}}}  // namespace qpid::store::ms_sql
