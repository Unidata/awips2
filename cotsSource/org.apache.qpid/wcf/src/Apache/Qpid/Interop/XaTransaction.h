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

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Threading;
using namespace System::Transactions;

enum DtcCallbackType{
    DTC_PREPARE,
    DTC_COMMIT,
    DTC_ABORT,
    DTC_SINGLE_PHASE,
    DTC_TMDOWN
};


ref class DtxResourceManager;
class DtcCallbackHandler;

// Function pointer declaratiom for managed space delegate
typedef bool (__stdcall *DtcCallbackFp)(DtcCallbackType);

// and the delegate with the same signature
public delegate bool DtcCallbackDelegate(DtcCallbackType);



public ref class XaTransaction
{
private:
    bool active;
    DtxResourceManager^ resourceManager;
    Transaction^ systemTransaction;
    AmqpSession^ controlSession;
    Collections::Generic::List<AmqpSession^>^ enlistedSessions;
    qpid::framing::Xid* xidp;
    DtcCallbackHandler* nativeHandler;
    bool preparing;
    DtcCallbackDelegate^ inboundDelegate;
    // the Qpid async result of the AMQP dtx prepare/commit commands
    TypedResult<qpid::framing::XaResult>* commandCompletionp;
    // the Qpid async result of the first session to do dtx start
    TypedResult<qpid::framing::XaResult>* firstDtxStartCompletionp;
    ManualResetEvent^ completionHandle;

    AmqpSession^ firstEnlistedSession;
    DtcCallbackType currentCommand;
    void AsyncCompleter(Object ^);
    void Phase0Flush();
    void Cleanup();
    void Complete();

internal:
    XaTransaction(Transaction^ t, IDtcToXaHelperSinglePipe *pXaHelper, DWORD rmCookie, DtxResourceManager^ rm);
    XaTransaction^ Enlist (AmqpSession ^session);
    bool DtcCallback (DtcCallbackType callback);
    void NotifyPhase0();
    void ChildFinalize();
    void SessionClosing(AmqpSession^ session);
    void WaitForCompletion();

    property IntPtr XidHandle {
	IntPtr get () { return (IntPtr) xidp; }
    }

#ifdef QPID_RECOVERY_TEST_HOOK
    void ForceRecovery();
    bool debugFailMode;
#endif

};

}}} // namespace Apache::Qpid::Interop
 
