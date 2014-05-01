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

#include "qpid/sys/apr/APRPool.h"
#include "qpid/sys/apr/APRBase.h"
#include <boost/pool/detail/singleton.hpp>

using namespace qpid::sys;

APRPool::APRPool(){
    APRBase::increment();
    CHECK_APR_SUCCESS(apr_pool_create(&pool, NULL));
}

APRPool::~APRPool(){
    apr_pool_destroy(pool);
    APRBase::decrement();
}

apr_pool_t* APRPool::get() {
    return boost::details::pool::singleton_default<APRPool>::instance().pool;
}

