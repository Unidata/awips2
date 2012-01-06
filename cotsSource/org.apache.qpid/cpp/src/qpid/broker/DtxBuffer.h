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
#ifndef _DtxBuffer_
#define _DtxBuffer_

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/broker/TxBuffer.h"
#include "qpid/sys/Mutex.h"

namespace qpid {
    namespace broker {
        class DtxBuffer : public TxBuffer{
            sys::Mutex lock;
            const std::string xid;
            bool ended;
            bool suspended;           
            bool failed;
            bool expired;

        public:
            typedef boost::shared_ptr<DtxBuffer> shared_ptr;

            QPID_BROKER_EXTERN DtxBuffer(const std::string& xid = "");
            QPID_BROKER_EXTERN ~DtxBuffer();
            QPID_BROKER_EXTERN void markEnded();
            bool isEnded();
            void setSuspended(bool suspended);
            bool isSuspended();
            void fail();
            bool isRollbackOnly();
            void timedout();
            bool isExpired();
            const std::string& getXid();
        };
    }
}


#endif
