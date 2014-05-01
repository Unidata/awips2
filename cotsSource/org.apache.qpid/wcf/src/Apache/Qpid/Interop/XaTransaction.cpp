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
#include "qpid/framing/FrameSet.h"
#include "qpid/framing/Xid.h"

#include "QpidException.h"
#include "AmqpConnection.h"
#include "AmqpSession.h"
#include "DtxResourceManager.h"
#include "XaTransaction.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Runtime::InteropServices;
using namespace System::Transactions;
using namespace msclr;

using namespace qpid::framing::dtx;

// ------------------------------------------------------------------------
// Start of a pure native code section
#pragma unmanaged
// ------------------------------------------------------------------------

// This is the native COM object the DTC expects to talk to for coordination.
// There is exactly one native instance of this for each managed XaTransaction object.


class DtcCallbackHandler : public ITransactionResourceAsync
{
private:
    long useCount;
    DtcCallbackFp managedCallback;
public:
    ITransactionEnlistmentAsync *txHandle;
    DtcCallbackHandler(DtcCallbackFp cbp) : managedCallback(cbp), useCount(0) {}
    ~DtcCallbackHandler() {}
    virtual HRESULT __stdcall PrepareRequest(BOOL unused, DWORD grfrm, BOOL unused2, BOOL singlePhase);
    virtual HRESULT __stdcall CommitRequest(DWORD grfrm, XACTUOW *unused);
    virtual HRESULT __stdcall AbortRequest(BOID *unused, BOOL unused2, XACTUOW *unused3);

    virtual HRESULT __stdcall TMDown();
    virtual HRESULT __stdcall DtcCallbackHandler::QueryInterface (REFIID riid, void **ppvObject);
    virtual ULONG __stdcall DtcCallbackHandler::AddRef();
    virtual ULONG __stdcall DtcCallbackHandler::Release();
    void __stdcall AbortRequestDone();
};


HRESULT DtcCallbackHandler::PrepareRequest(BOOL unused, DWORD grfrm, BOOL unused2, BOOL singlePhase)
{
    if (singlePhase) {
	return managedCallback(DTC_SINGLE_PHASE) ? S_OK : E_FAIL;
    }

    return managedCallback(DTC_PREPARE) ? S_OK : E_FAIL;
}


HRESULT DtcCallbackHandler::CommitRequest(DWORD grfrm, XACTUOW *unused)
{
    return managedCallback(DTC_COMMIT) ? S_OK : E_FAIL;
}

HRESULT DtcCallbackHandler::AbortRequest(BOID *unused, BOOL unused2, XACTUOW *unused3)
{
    return managedCallback(DTC_ABORT) ? S_OK : E_FAIL;
}


HRESULT DtcCallbackHandler::TMDown()
{
    return managedCallback(DTC_TMDOWN) ? S_OK : E_FAIL;
}


HRESULT DtcCallbackHandler::QueryInterface (REFIID riid, void **ppvObject)
{
    *ppvObject = NULL;
 	
    if ((riid == IID_IUnknown) || (riid == IID_IResourceManagerSink))
	*ppvObject = this;
    else
	return ResultFromScode(E_NOINTERFACE);
 	
    this->AddRef();
    return S_OK;
}

 	
ULONG DtcCallbackHandler::AddRef()
{
    return InterlockedIncrement(&useCount);
}

 	
ULONG DtcCallbackHandler::Release()
{
    long uc = InterlockedDecrement(&useCount);

    if (uc)
	return uc;
 	
    delete this;
    return 0;
}


// ------------------------------------------------------------------------
// End of pure native code section
#pragma managed
// ------------------------------------------------------------------------

#ifdef QPID_RECOVERY_TEST_HOOK
void XaTransaction::ForceRecovery() {
    debugFailMode = true;
}
#endif

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------


XaTransaction::XaTransaction(Transaction^ t, IDtcToXaHelperSinglePipe *xaHelperp, DWORD rmCookie, DtxResourceManager^ rm) {
    bool success = false;
    xidp = NULL;
    commandCompletionp = NULL;
    firstDtxStartCompletionp = NULL;
    nativeHandler = NULL;
    resourceManager = rm;
    controlSession = rm->DtxControlSession;
    active = true;
    preparing = false;
    systemTransaction = t;
    IntPtr comTxp = IntPtr::Zero;
    completionHandle = gcnew ManualResetEvent(false);

    try {
	enlistedSessions = gcnew Collections::Generic::List<AmqpSession^>();

	// take a System.Transactions.Transaction and obtain
	// the corresponding DTC COM object.
	IDtcTransaction^ dtcTransaction = TransactionInterop::GetDtcTransaction(t);
	comTxp = Marshal::GetIUnknownForObject(dtcTransaction);
	XID winXid;
	HRESULT hr = xaHelperp->ConvertTridToXID((DWORD *)comTxp.ToPointer(), rmCookie, &winXid);
	if (hr != S_OK)
	    throw gcnew QpidException("get XA XID");

	// Convert the X/Open format to the internal Qpid format
	xidp = new qpid::framing::Xid();
	xidp->setFormat((uint32_t) winXid.formatID);
	int bqualPos = 0;
	if (winXid.gtrid_length > 0) {
	    xidp->setGlobalId(std::string(winXid.data, winXid.gtrid_length));
	    bqualPos = winXid.gtrid_length;
	}
	if (winXid.bqual_length > 0) {
	    xidp->setBranchId(std::string(winXid.data + bqualPos, winXid.bqual_length));
	}
	
	// create the callback chain: DTC proxy -> DtcCallbackHandler -> this
	inboundDelegate = gcnew DtcCallbackDelegate(this, &XaTransaction::DtcCallback);
	IntPtr ip = Marshal::GetFunctionPointerForDelegate(inboundDelegate);
	nativeHandler = new DtcCallbackHandler(static_cast<DtcCallbackFp>(ip.ToPointer()));
	// add myself for later smart pointer destruction
	nativeHandler->AddRef();

	hr = xaHelperp->EnlistWithRM(rmCookie, (ITransaction *)comTxp.ToPointer(), nativeHandler, &(nativeHandler->txHandle));

	if (hr != S_OK)
	    throw gcnew QpidException("Enlist");

	success = true;
    }
    finally {
	if (!success)
	    Cleanup();
	if (comTxp != IntPtr::Zero)
	    ((IUnknown *) comTxp.ToPointer())->Release();
    }
}


void XaTransaction::Cleanup() {
    if (firstDtxStartCompletionp != NULL) {
	try {
	    firstEnlistedSession->ReleaseCompletion((IntPtr) firstDtxStartCompletionp);
	}
	catch (...) {
	    // TODO: log it?
	}

	firstDtxStartCompletionp = NULL;
    }

    if (nativeHandler != NULL) {
	nativeHandler->Release();
	nativeHandler = NULL;
    }
    if (xidp != NULL) {
	delete xidp;
	xidp = NULL;
    }
}


XaTransaction^ XaTransaction::Enlist (AmqpSession ^session) {
    lock l(enlistedSessions);
    if (!active)
	throw gcnew QpidException("transaction enlistment internal error");
    if (!enlistedSessions->Contains(session)) {
	enlistedSessions->Add(session);
	if (firstEnlistedSession == nullptr) {
	    firstEnlistedSession = session;
	    IntPtr intptr = session->DtxStart((IntPtr) xidp, false, false);
	    firstDtxStartCompletionp = (TypedResult<qpid::framing::XaResult> *) intptr.ToPointer();
	}
	else {
	    // the broker must see the dtxStart as a join operation, and it must arrive
	    // at the broker after the first dtx start
	    if (firstDtxStartCompletionp != NULL)
		firstDtxStartCompletionp->wait();
	    session->DtxStart((IntPtr) xidp, true, false);
	}
    }
    else {
	// already started once, so resume is true
	session->DtxStart((IntPtr) xidp, false, true);
    }
    return this;
}


void XaTransaction::SessionClosing(AmqpSession^ session) {
    lock l(enlistedSessions);
    if (!enlistedSessions->Contains(session))
	return;

    enlistedSessions->Remove(session);
    if (!active) {
	// Phase0Flush already done on all sessions
	l.release();
	return;
    }

    IntPtr completion = session->BeginPhase0Flush(this);
    session->EndPhase0Flush(this, completion);

    if (session == firstEnlistedSession) {
	// if we just completed the dtxEnd, we know the dtxStart completed before that
	if (firstDtxStartCompletionp != NULL) {
	    firstEnlistedSession->ReleaseCompletion((IntPtr) firstDtxStartCompletionp);
	    firstDtxStartCompletionp = NULL;
	}
    }
}


void XaTransaction::Phase0Flush() {
    // let each session delimit their transactional work with an AMQP dtx.end protocol frame
    lock l(enlistedSessions);
    if (!active)
	return;

    active = false;		// no more enlistments
    int scount = enlistedSessions->Count;

    if (scount > 0) {
	array<IntPtr> ^completions = gcnew array<IntPtr>(scount);
	for (int i = 0; i < scount; i++) {

	    // TODO: skip phase0 flush for rollback case

	    completions[i] = enlistedSessions[i]->BeginPhase0Flush(this);
	}

	for (int i = 0; i < scount; i++) {
	    // without each session.sync(), session commands are queued up in the right order,
	    // but on their separate outbound channels, and destined for receipt at separate Broker inbound
	    // channels.  It is not clear how to be sure Phase 0 dtx.End is processed in the
	    // correct order before commit on the broker without the sync.
	    enlistedSessions[i]->EndPhase0Flush(this, completions[i]);
	}
    }

    // since all dtxEnds have completed, we know all starts have too
    if (firstDtxStartCompletionp != NULL) {
	try {
	    firstEnlistedSession->ReleaseCompletion((IntPtr) firstDtxStartCompletionp);
	}
	catch (...) {
	    // TODO: log it?
	}

	firstDtxStartCompletionp = NULL;
    }
}


bool XaTransaction::DtcCallback (DtcCallbackType callback) {
    // called by the DTC proxy thread.  Be brief and don't block (but Phase0Flush?)

    if (AppDomain::CurrentDomain->IsFinalizingForUnload())
	return false;

    IntPtr intptr = IntPtr::Zero;
    currentCommand = callback;

    try {
	switch (callback) {
	case DTC_PREPARE:
	    Phase0Flush();
	    try {
		intptr = controlSession->DtxPrepare((IntPtr) xidp);
		preparing = true;
		resourceManager->IncrementDoubt();
	    }
	    catch (System::Exception^ ) {
		// intptr remains nullptr
	    }
	    commandCompletionp = (TypedResult<qpid::framing::XaResult> *) intptr.ToPointer();
	    ThreadPool::QueueUserWorkItem(gcnew WaitCallback(this, &XaTransaction::AsyncCompleter));
	    break;

	case DTC_COMMIT:
#ifdef QPID_RECOVERY_TEST_HOOK
	if (debugFailMode){ return; }
#endif
	    // no phase 0 required.  always preceded by a prepare
	    try {
		intptr = controlSession->DtxCommit((IntPtr) xidp, false);
	    }
	    catch (System::Exception^ ) {
		// intptr remains nullptr
	    }
	    commandCompletionp = (TypedResult<qpid::framing::XaResult> *) intptr.ToPointer();
	    ThreadPool::QueueUserWorkItem(gcnew WaitCallback(this, &XaTransaction::AsyncCompleter));
	    break;

	case DTC_ABORT:
	    Phase0Flush();
#ifdef QPID_RECOVERY_TEST_HOOK
	if (debugFailMode){ return; }
#endif
	    try {
		intptr = controlSession->DtxRollback((IntPtr) xidp);
	    }
	    catch (System::Exception^ ) {
		// intptr remains nullptr
	    }
	    commandCompletionp = (TypedResult<qpid::framing::XaResult> *) intptr.ToPointer();
	    ThreadPool::QueueUserWorkItem(gcnew WaitCallback(this, &XaTransaction::AsyncCompleter));
	    break;

	case DTC_SINGLE_PHASE:
	    Phase0Flush();
	    try {
		intptr = controlSession->DtxCommit((IntPtr) xidp, true);
	    }
	    catch (System::Exception^ ) {
		// intptr remains nullptr
	    }
	    commandCompletionp = (TypedResult<qpid::framing::XaResult> *) intptr.ToPointer();
	    ThreadPool::QueueUserWorkItem(gcnew WaitCallback(this, &XaTransaction::AsyncCompleter));
	    break;

	case DTC_TMDOWN:
	    commandCompletionp = NULL;
	    ThreadPool::QueueUserWorkItem(gcnew WaitCallback(this, &XaTransaction::AsyncCompleter));
	    break;
	}
	return true;
    }
    catch (System::Exception^ e) {
	// TODO: log it
	Console::WriteLine("Unexpected DtcCallback exception: {0}", e->ToString());
    }
    catch (...) {
	// TODO: log it
    }
    return false;
}


// this handles the case where the application regains control for
// a new transaction before we are notified (abort/rollback
// optimization in DTC).

void XaTransaction::NotifyPhase0() {
    if (active)
	Phase0Flush();
}


void XaTransaction::AsyncCompleter(Object ^unused) {
    bool success = false;

    if (commandCompletionp != NULL) {
	try {
	    // waits for the AMQP broker's response and returns the decoded content
	    XaResult& xaResult = commandCompletionp->get();
	    if (xaResult.hasStatus()) {
		if (xaResult.getStatus() == XaStatus::XA_STATUS_XA_OK) {
		    success = true;
		}
	    }
	}
	catch (...) {
	    // TODO: log it?
	}
	try {
	    controlSession->ReleaseCompletion((IntPtr) commandCompletionp);
	}
	catch (...) {
	    // TODO: log it?
	}

	commandCompletionp = NULL;
    }

    ITransactionEnlistmentAsync *dtcTxHandle = nativeHandler->txHandle;

    HRESULT hr = success ? S_OK : E_FAIL;

    switch (currentCommand) {
    case DTC_PREPARE:
	dtcTxHandle->PrepareRequestDone(hr, NULL, NULL);
	break;

    case DTC_COMMIT:
	dtcTxHandle->CommitRequestDone(hr);
	if (success)
	    resourceManager->DecrementDoubt();
	Complete();
	break;

    case DTC_ABORT:
	dtcTxHandle->AbortRequestDone(hr);
	if (success) {
	    if (preparing) {
		preparing = false;
		resourceManager->DecrementDoubt();
	    }
	}
	Complete();
	break;

    case DTC_SINGLE_PHASE:
	if (success)
	    hr = XACT_S_SINGLEPHASE;
	dtcTxHandle->PrepareRequestDone(hr, NULL, NULL);
	Complete();
	break;

    case DTC_TMDOWN:
	// Stop the RM from accepting new enlistments
	resourceManager->TmDown();
	Complete();
	break;
    }
}


void XaTransaction::Complete() {
    Cleanup();
    resourceManager->Complete(systemTransaction);
    completionHandle->Set();
}


void XaTransaction::WaitForCompletion() {
    completionHandle->WaitOne();
}


    /*
void XaTransaction::WaitForFlush() {
    isFlushedHandle->WaitOne();
}
    */

// called from DtxResourceManager Finalize

void XaTransaction::ChildFinalize() {
    lock l(enlistedSessions);
    Phase0Flush();
    Cleanup();
}



}}} // namespace Apache::Qpid::Interop
