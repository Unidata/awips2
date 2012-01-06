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

ref class XaTransaction;

public ref class DtxResourceManager
{
private:
    // Receive() or WaitForMessage()
    AmqpSession^ dtxControlSession;
    String^ dataSourceName;
    bool consumed;
    DWORD rmCookie;
    void* xaHelperp;
    void* dtcComp;
    int doubtCount;
    DtxResourceManager(AmqpConnection^);
    XaTransaction^ InternalGetXaTransaction (AmqpSession^ session, Transaction^ transaction);
    bool tmDown;

    // The active transactions
    Collections::Generic::Dictionary<Transaction^, XaTransaction^>^ transactionMap;

    // one resource manager per AMQP broker per process
    static Collections::Generic::Dictionary<System::String^, DtxResourceManager^>^ resourceManagerMap =
	gcnew Collections::Generic::Dictionary<System::String^, DtxResourceManager^>();

    void Cleanup();
    ~DtxResourceManager();
    !DtxResourceManager();

internal:
    static XaTransaction^ GetXaTransaction (AmqpSession^ session, Transaction^ transaction);
    void Complete(Transaction ^tx);
    void TmDown();

    property AmqpSession^ DtxControlSession {
	AmqpSession^ get () { return dtxControlSession; }
    }

    void IncrementDoubt();
    void DecrementDoubt();

#ifdef QPID_RECOVERY_TEST_HOOK
public:
    static void ForceRecovery(Transaction ^tx);
#endif
};

}}} // namespace Apache::Qpid::Interop
