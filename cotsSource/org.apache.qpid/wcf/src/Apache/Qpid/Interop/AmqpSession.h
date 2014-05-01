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

#include "AmqpConnection.h"
#include "MessageBodyStream.h"
#include "CompletionWaiter.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Runtime::InteropServices;
using namespace System::Transactions;
using namespace System::Diagnostics;


using namespace qpid::client;
using namespace std;

ref class InputLink;
ref class OutputLink;
ref class XaTransaction;

public ref class AmqpSession
{
private:
    Object^ sessionLock;
    Object^ openCloseLock;
    AmqpConnection^ connection;
    AsyncSession* sessionp;
    SessionImpl* sessionImplp;
    SubscriptionManager* subs_mgrp;
    Collections::Generic::List<CompletionWaiter^>^ waiters;
    bool helperRunning;

    // number of active InputLinks and OutputLinks
    int openCount;

    // the number of async commands sent to the broker that need completion confirmation
    int syncCount;

    bool closing;
    ManualResetEvent^ closeWaitHandle;
    bool dtxEnabled;
    Transaction^ openSystemTransaction;
    XaTransaction^ openXaTransaction;
    Collections::Generic::List<XaTransaction^>^ pendingTransactions;

    void Cleanup();
    void CheckOpen();
    void asyncHelper(Object ^);
    void addWaiter(CompletionWaiter^ waiter);
    void UpdateTransactionState(msclr::lock^ sessionLock);
    void IncrementSyncs();
    void DecrementSyncs();
    void WaitLastSync(msclr::lock^ l);

public:  
    OutputLink^ CreateOutputLink(System::String^ targetQueue);
    InputLink^ CreateInputLink(System::String^ sourceQueue);

    // 0-10 specific support
    InputLink^ CreateInputLink(System::String^ sourceQueue, bool exclusive, bool temporary, System::String^ filterKey, System::String^ exchange);
    void Bind(System::String^ queue, System::String^ exchange, System::String^ filterKey);

internal:
    AmqpSession(AmqpConnection^ connection, qpid::client::Connection* qpidConnection);
    void NotifyClosed();
    CompletionWaiter^ SendMessage (System::String^ queue, MessageBodyStream ^mbody, TimeSpan timeout, bool async, AsyncCallback^ callback, Object^ state);
    void ConnectionClosed();
    void internalWaitForCompletion(IntPtr future);
    void removeWaiter(CompletionWaiter^ waiter);
    bool MessageStop(std::string &name);
    void AcceptAndComplete(SequenceSet& transfers);
    IntPtr BeginPhase0Flush(XaTransaction^);
    void EndPhase0Flush(XaTransaction^, IntPtr);
    IntPtr DtxStart(IntPtr xidp, bool, bool);
    IntPtr DtxPrepare(IntPtr xidp);
    IntPtr DtxCommit(IntPtr xidp, bool onePhase);
    IntPtr DtxRollback(IntPtr xidp);
    void ReleaseCompletion(IntPtr completion);

    property AmqpConnection^ Connection {
	AmqpConnection^ get () { return connection; }
    }
};

}}} // namespace Apache::Qpid::Interop
