/*
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
*/

#pragma once

#include "MessageWaiter.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Threading;
using namespace System::Runtime::InteropServices;

using namespace qpid::client;
using namespace std;

// smart pointer to the low level AMQP 0-10 frames of the message
typedef qpid::framing::FrameSet::shared_ptr QpidFrameSetPtr;

public ref class InputLink
{
private:
    AmqpSession^ amqpSession;
    Subscription* subscriptionp;
    LocalQueue* localQueuep;
    Demux::QueuePtr* queuePtrp;
    Collections::Generic::List<MessageWaiter^>^ waiters;
    bool disposed;
    bool finalizing;
    Object^ linkLock;
    Object^ subscriptionLock;
    QpidFrameSetPtr* dequeuedFrameSetpp;
    ManualResetEvent^ asyncHelperWaitHandle;
    // number of messages to buffer locally for future consumption
    int prefetchLimit;
    // the number of messages requested and not yet processed
    int workingCredit;
    // stopping and restarting the message flow
    bool creditSyncPending;
    // working credit low water mark
    int minWorkingCredit;

    void Cleanup();
    void ReleaseNative();
    bool haveMessage();
    void addWaiter(MessageWaiter^ waiter);
    void asyncHelper();
    AmqpMessage^ createAmqpMessage(IntPtr msgp);
    void AdjustCredit();
    void SyncCredit(Object ^);

internal:
    InputLink(AmqpSession^ session, System::String^ sourceQueue, qpid::client::AsyncSession *qpidSessionp,
	      qpid::client::SubscriptionManager *qpidSubsMgrp, bool exclusive, bool temporary, System::String^ filterKey,
	      System::String^ exchange);

    bool internalWaitForMessage();
    void unblockWaiter();
    void resetQueue();
    IntPtr nextLocalMessage();
    void removeWaiter(MessageWaiter^ waiter);
    void sync();

public:  
    ~InputLink();
    !InputLink();
    void Close();

    bool TryReceive(TimeSpan timeout, [Out] AmqpMessage ^% amqpMessage);
    IAsyncResult^ BeginTryReceive(TimeSpan timeout, AsyncCallback^ callback, Object^ state);
    bool EndTryReceive(IAsyncResult^ result, [Out] AmqpMessage^% amqpMessage);

    bool WaitForMessage(TimeSpan timeout);
    IAsyncResult^ BeginWaitForMessage(TimeSpan timeout, AsyncCallback^ callback, Object^ state);
    bool EndWaitForMessage(IAsyncResult^ result);

    property int PrefetchLimit {
	int get () { return prefetchLimit; }
	void set (int value);
    }

};

}}} // namespace Apache::Qpid::Interop
