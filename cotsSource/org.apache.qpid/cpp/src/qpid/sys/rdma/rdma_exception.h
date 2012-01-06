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
#ifndef RDMA_EXCEPTION_H
#define RDMA_EXCEPTION_H

#include <exception>

#include <errno.h>
#include <string.h>

namespace Rdma {
    static __thread char s[50];
    class Exception : public std::exception {
        int err;

    public:
        Exception(int e) : err(e) {}
        int getError() { return err; }
        const char* what() const throw() {
            return ::strerror_r(err, s, 50);
        }
    };

    inline void THROW_ERRNO() {
        throw Rdma::Exception(errno);
    }

    inline void CHECK(int rc) {
        if (rc != 0)
            throw Rdma::Exception((rc == -1) ? errno : rc >0 ? rc : -rc);
    }

    inline void CHECK_IBV(int rc) {
        if (rc != 0)
            throw Rdma::Exception(rc);
    }

    template <typename T>
    inline
    T* CHECK_NULL(T* rc) {
        if (rc == 0)
            THROW_ERRNO();
        return rc;
    }
}

#endif // RDMA_EXCEPTION_H
