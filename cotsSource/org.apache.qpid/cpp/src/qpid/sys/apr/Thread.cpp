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

#include "qpid/sys/apr/Thread.h"
#include "qpid/sys/Runnable.h"

using namespace qpid::sys;
using qpid::sys::Runnable;

void* APR_THREAD_FUNC Thread::runRunnable(apr_thread_t* thread, void *data) {
    reinterpret_cast<Runnable*>(data)->run();
    CHECK_APR_SUCCESS(apr_thread_exit(thread, APR_SUCCESS));
    return NULL;
} 


