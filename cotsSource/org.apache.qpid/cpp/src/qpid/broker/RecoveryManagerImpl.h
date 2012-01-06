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
#ifndef _RecoveryManagerImpl_
#define _RecoveryManagerImpl_

#include <list>
#include "qpid/broker/DtxManager.h"
#include "qpid/broker/ExchangeRegistry.h"
#include "qpid/broker/QueueRegistry.h"
#include "qpid/broker/LinkRegistry.h"
#include "qpid/broker/RecoveryManager.h"

namespace qpid {
namespace broker {

    class RecoveryManagerImpl : public RecoveryManager{
        QueueRegistry& queues;
        ExchangeRegistry& exchanges;
        LinkRegistry& links;
        DtxManager& dtxMgr;
        const uint64_t stagingThreshold;
    public:
        RecoveryManagerImpl(QueueRegistry& queues, ExchangeRegistry& exchanges, LinkRegistry& links,
                            DtxManager& dtxMgr, uint64_t stagingThreshold);
        ~RecoveryManagerImpl();

        RecoverableExchange::shared_ptr recoverExchange(framing::Buffer& buffer);
        RecoverableQueue::shared_ptr recoverQueue(framing::Buffer& buffer);
        RecoverableMessage::shared_ptr recoverMessage(framing::Buffer& buffer);
        RecoverableTransaction::shared_ptr recoverTransaction(const std::string& xid, 
                                                              std::auto_ptr<TPCTransactionContext> txn);
        RecoverableConfig::shared_ptr recoverConfig(framing::Buffer& buffer);
        void recoveryComplete();
    };

    
}
}


#endif
