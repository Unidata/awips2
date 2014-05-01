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

#include <windows.h>
#include <msclr\lock.h>
#include <transact.h>
#include <xolehlp.h>
#include <txdtc.h>
#include <oletx2xa.h>
#include <iostream>
#include <fstream>

#include "qpid/client/AsyncSession.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/Connection.h"
#include "qpid/client/Message.h"
#include "qpid/client/MessageListener.h"
#include "qpid/framing/FrameSet.h"

#include "AmqpConnection.h"
#include "AmqpSession.h"
#include "DtxResourceManager.h"
#include "XaTransaction.h"
#include "QpidException.h"
#include "QpidMarshal.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Runtime::InteropServices;
using namespace System::Transactions;
using namespace msclr;


/*
 * There is one DtxResourceManager per broker and per application process.
 *
 * Each RM manages a collection of active XaTransaction objects.  Participating AmqpSessions enlist
 * (or re-enlist) with an XaTransaction indexed by the corresponding System.Transaction object.  The
 * RM maintains its own AmqpSession for sending 2PC commnds (dtxPrepare, dtxCommit etc.).  The
 * XaTransaction object works through the lifecycle of the Transaction, including prompting the
 * enlisted sessions to send their delimiting dtxEnd commands.
 *
 * A separate DtcPlugin.cpp file provides the recovery logic when needed in a library named
 * qpidxarm.dll.  The MSDTC maintans recovery info in its log and tracks when there may be
 * transactions in doubt.  See the documentation for IDtcToXaHelperSinglePipe.
 *
 * To enable transaction support:
 *    DTC requires a registry key to find the plugin
 *      [HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\MSDTC\XADLL] qpidxarm.dll -> [path to qpidxarm.dll]
 *    DTC needs to be configured for XA
 *      cmdprompt -> dcomcnfg -> Component services -> My Computer -> DTC -> Local DTC -> right click properties -> Security -> Enable XA Transactions
 *
 */

// TODO: provide shutdown mechanism, perhaps callback from Connection Idle for enlisted connections.
// But note that a new RM registration with the DTC is very expensive.


DtxResourceManager::DtxResourceManager(AmqpConnection^ appConnection) {
    dtcComp = NULL;
    xaHelperp = NULL;
    rmCookie = 0;
    doubtCount = 0;
    tmDown = false;
    AmqpConnection^ clonedCon = appConnection->Clone();
    dtxControlSession = clonedCon->CreateSession();
    dataSourceName = clonedCon->DataSourceName;
    transactionMap = gcnew Collections::Generic::Dictionary<Transaction^, XaTransaction^>();

    HRESULT hr;

    try {
	// instead of pinning this instance, just use tmp stack variables for small stuff
	IUnknown* tmp = NULL;
	// request the default DTC
	hr = DtcGetTransactionManager(NULL, NULL, IID_IUnknown, 0, 0, 0, (void **)&tmp);
    	if (hr != S_OK)
	    throw gcnew QpidException("connection failure to DTC service");
	dtcComp = tmp;

	IDtcToXaHelperSinglePipe *tmp2 = NULL;
	hr = ((IUnknown *)dtcComp)->QueryInterface(IID_IDtcToXaHelperSinglePipe, (void**) &tmp2);
    	if (hr != S_OK)
	    throw gcnew QpidException("DTC XA unavailable");
	xaHelperp = tmp2;

	std::string native_dsn = QpidMarshal::ToNative(dataSourceName);
	DWORD tmp3;

	// This call doesn't return until the DTC has opened and closed a connection to the broker
	// and written a recovery entry in its log.
	hr = ((IDtcToXaHelperSinglePipe *) xaHelperp)->XARMCreate(const_cast<char *>(native_dsn.c_str()), "qpidxarm.dll", &tmp3);
	if (hr != S_OK) {
	    switch (hr) {
	    case E_FAIL:
		throw gcnew QpidException("Resource Manager DLL configuration error");
	    case E_INVALIDARG:
		throw gcnew QpidException("Resource Manager internal error");
	    case E_OUTOFMEMORY:
		throw gcnew QpidException("Resource Manager out of memory");
	    case E_UNEXPECTED:
		throw gcnew QpidException("Resource Manager internal failure");
	    case XACT_E_TMNOTAVAILABLE:
	    case XACT_E_CONNECTION_DOWN:
		throw gcnew QpidException("MSDTC unavailable");

	    default:
		throw gcnew QpidException("Resource Manager Registration failed");
	    }
	}

	rmCookie = tmp3;
    }
    finally {
	if (rmCookie == 0) {
	    // undo partial construction
	    Cleanup();
	}
    }
}


DtxResourceManager::!DtxResourceManager() {
    Cleanup();
}


DtxResourceManager::~DtxResourceManager() {
    GC::SuppressFinalize(this);
    Cleanup();
}


// Called when the DTC COM proxy sends TMDOWN to a pending XaTransaction
// called once for each outstanding tx

void DtxResourceManager::TmDown() {
    // this block is the only place where both locks are held
    lock l1(transactionMap);
    lock l2(resourceManagerMap);
    if (tmDown)
	return;

    tmDown = true;
    resourceManagerMap->Remove(this->dataSourceName);
    // defer cleanup until last TmDown notification received
}



void DtxResourceManager::Cleanup() {
    for each (Collections::Generic::KeyValuePair<Transaction^, XaTransaction^> kvp in transactionMap) {
	XaTransaction^ xaTr = kvp.Value;
	xaTr->ChildFinalize();
    }

    try {
	if (rmCookie != 0) {
	    // implies no recovery needed
	    bool cleanSession = (doubtCount == 0) && (transactionMap->Count == 0);
	    ((IDtcToXaHelperSinglePipe *)xaHelperp)->ReleaseRMCookie(rmCookie, cleanSession);
	    rmCookie = 0;
	}
	    

	if (xaHelperp != NULL) {
	    ((IDtcToXaHelperSinglePipe *) xaHelperp)->Release();
	    xaHelperp = NULL;
	}

	if (dtcComp != NULL) {
	    ((IUnknown *) dtcComp)->Release();
	    dtcComp = NULL;
	}

	if (dtxControlSession != nullptr) {
	    dtxControlSession->Connection->Close();
	}

    }
    catch (Exception^) {}
}


XaTransaction^ DtxResourceManager::GetXaTransaction(AmqpSession^ appSession, Transaction^ transaction) {
    // find or create the RM instance associated with the session's broker
    AmqpConnection^ connection = appSession->Connection;
    DtxResourceManager^ instance = connection->CachedResourceManager;

    // try cached rm first
    if (instance != nullptr) {
	XaTransaction^ xaTx = instance->InternalGetXaTransaction(appSession, transaction);
	if (xaTx != nullptr)
	    return xaTx;
	else {
	    // cached version no longer available, force new rm creation
	    connection->CachedResourceManager = nullptr;
	}
    }

    lock l(resourceManagerMap);
    String^ dsn = connection->DataSourceName;
    if (!resourceManagerMap->TryGetValue(dsn, instance)) {
	instance = gcnew DtxResourceManager(connection->Clone());
	resourceManagerMap->Add(dsn, instance);
	connection->CachedResourceManager = instance;
    }
    l.release();

    return instance->InternalGetXaTransaction(appSession, transaction);
}


XaTransaction^ DtxResourceManager::InternalGetXaTransaction(AmqpSession^ appSession, Transaction^ transaction) {
    // find or create the tx proxy instance associated with the DTC transaction
    lock l(transactionMap);
    if (tmDown)
	return nullptr;
    
    XaTransaction^ xaTransaction = nullptr;
    if (!transactionMap->TryGetValue(transaction, xaTransaction)) {
	xaTransaction = gcnew XaTransaction(transaction, (IDtcToXaHelperSinglePipe *) xaHelperp, rmCookie, this);
	transactionMap->Add(transaction, xaTransaction);
    }

    return xaTransaction;
}

void DtxResourceManager::Complete(Transaction ^tx) {
    lock l(transactionMap);
    transactionMap->Remove(tx);

    if (tmDown && (transactionMap->Count == 0)) {
	// no more activity on this instance
	GC::SuppressFinalize(this);
	Cleanup();
    }
}


void DtxResourceManager::IncrementDoubt() {
    Interlocked::Increment(doubtCount);
}


void DtxResourceManager::DecrementDoubt() {
    Interlocked::Decrement(doubtCount);
}


#ifdef QPID_RECOVERY_TEST_HOOK
void DtxResourceManager::ForceRecovery(Transaction ^tx) {
    lock l(resourceManagerMap);
    for each (Collections::Generic::KeyValuePair<System::String^, DtxResourceManager^> kvp in resourceManagerMap) {

	Collections::Generic::Dictionary<Transaction^, XaTransaction^>^ txmap = kvp.Value->transactionMap;
	XaTransaction^ xaTransaction = nullptr;
	lock l2(txmap);
	if (txmap->TryGetValue(tx, xaTransaction)) {
	    xaTransaction->ForceRecovery();
	}
    }
}
#endif
    
}}} // namespace Apache::Qpid::Interop
