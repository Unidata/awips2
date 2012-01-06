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
#include "qpid/sys/Mutex.h"

namespace qpid {
namespace sys {

/**
 * Initialise a recursive mutex attr for use in creating mutexes later
 * (we use pthread_once to make sure it is initialised exactly once)
 */

namespace {
pthread_once_t  onceControl = PTHREAD_ONCE_INIT;
pthread_mutexattr_t mutexattr;
	
void initMutexattr()  {
    pthread_mutexattr_init(&mutexattr);
    pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE);
}
}

const pthread_mutexattr_t* Mutex::getAttribute() {
    pthread_once(&onceControl, initMutexattr);
    return &mutexattr;
}

}} // namespace qpid::sys
