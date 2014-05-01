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
#include <iostream>
#include "qpid/log/Statement.h"
#include "qpid/sys/apr/APRBase.h"

using namespace qpid::sys;

APRBase* APRBase::instance = 0;

APRBase* APRBase::getInstance(){
    if(instance == 0){
	instance = new APRBase();
    }
    return instance;
}


APRBase::APRBase() : count(0){
    apr_initialize();
    CHECK_APR_SUCCESS(apr_pool_create(&pool, 0));
    CHECK_APR_SUCCESS(apr_thread_mutex_create(&mutex, APR_THREAD_MUTEX_NESTED, pool));
}

APRBase::~APRBase(){
    CHECK_APR_SUCCESS(apr_thread_mutex_destroy(mutex));
    apr_pool_destroy(pool);
    apr_terminate();  
}

bool APRBase::_increment(){
    bool deleted(false);
    CHECK_APR_SUCCESS(apr_thread_mutex_lock(mutex));
    if(this == instance){
	count++;
    }else{
	deleted = true;
    }
    CHECK_APR_SUCCESS(apr_thread_mutex_unlock(mutex));
    return !deleted;
}

void APRBase::_decrement(){
    APRBase* copy = 0;
    CHECK_APR_SUCCESS(apr_thread_mutex_lock(mutex));
    if(--count == 0){
	copy = instance;
	instance = 0;
    }
    CHECK_APR_SUCCESS(apr_thread_mutex_unlock(mutex));
    if(copy != 0){
	delete copy;
    }
}

void APRBase::increment(){
    int count = 0;
    while(count++ < 2 && !getInstance()->_increment())
        QPID_LOG(warning, "APR initialization triggered concurrently with termination.");
}

void APRBase::decrement(){
    getInstance()->_decrement();
}

std::string qpid::sys::get_desc(apr_status_t status){
    const int size = 50;
    char tmp[size];
    return std::string(apr_strerror(status, tmp, size));
}

