#ifndef _sys_PipeHandle_h
#define _sys_PipeHandle_h

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

#include "qpid/sys/IntegerTypes.h"
#include "qpid/CommonImportExport.h"
#include <string>

// This class is a portability wrapper around pipe fds. 
// It currently exists primarily and solely for the purpose of 
// integration with single-threaded components that require QMF 
// integration through a signalling fd.

namespace qpid {
namespace sys {

    class PipeHandle {
    private:
        int writeFd; 
        int readFd;
    public:
        QPID_COMMON_EXTERN  PipeHandle(bool nonBlocking=true);
        QPID_COMMON_EXTERN  ~PipeHandle();
        QPID_COMMON_EXTERN  int read(void* buf, size_t bufSize);
        QPID_COMMON_EXTERN  int write(const void* buf, size_t bufSize);
        QPID_COMMON_EXTERN  int getReadHandle();
    };

}}

#endif  /*!_sys_PipeHandle_h*/
