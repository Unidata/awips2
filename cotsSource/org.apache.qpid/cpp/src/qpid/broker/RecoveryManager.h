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
#ifndef _RecoveryManager_
#define _RecoveryManager_

#include "qpid/broker/RecoverableExchange.h"
#include "qpid/broker/RecoverableQueue.h"
#include "qpid/broker/RecoverableMessage.h"
#include "qpid/broker/RecoverableTransaction.h"
#include "qpid/broker/RecoverableConfig.h"
#include "qpid/broker/TransactionalStore.h"
#include "qpid/framing/Buffer.h"

namespace qpid {
namespace broker {

class RecoveryManager{
  public:
    virtual ~RecoveryManager(){}
    virtual RecoverableExchange::shared_ptr recoverExchange(framing::Buffer& buffer) = 0;
    virtual RecoverableQueue::shared_ptr recoverQueue(framing::Buffer& buffer) = 0;
    virtual RecoverableMessage::shared_ptr recoverMessage(framing::Buffer& buffer) = 0;
    virtual RecoverableTransaction::shared_ptr recoverTransaction(const std::string& xid, 
                                                                  std::auto_ptr<TPCTransactionContext> txn) = 0;
    virtual RecoverableConfig::shared_ptr recoverConfig(framing::Buffer& buffer) = 0;

    virtual void recoveryComplete() = 0;
};

class Recoverable {
  public:
    virtual ~Recoverable() {}

    /**
     * Request recovery of queue and message state.
     */
    virtual void recover(RecoveryManager& recoverer) = 0;
};

}}


#endif
