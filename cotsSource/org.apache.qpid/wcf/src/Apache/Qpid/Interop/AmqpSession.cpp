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
#include <oletx2xa.h>

#include "qpid/client/AsyncSession.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/Connection.h"
#include "qpid/client/SessionImpl.h"
#include "qpid/client/SessionBase_0_10Access.h"
#include "qpid/client/Message.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/client/Future.h"
#include "qpid/framing/Xid.h"

#include "AmqpConnection.h"
#include "AmqpSession.h"
#include "AmqpMessage.h"
#include "MessageBodyStream.h"
#include "InputLink.h"
#include "OutputLink.h"
#include "QpidMarshal.h"
#include "QpidException.h"
#include "XaTransaction.h"
#include "DtxResourceManager.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Runtime::InteropServices;
using namespace System::Transactions;
using namespace msclr;

using namespace qpid::client;
using namespace std;


AmqpSession::AmqpSession(AmqpConnection^ conn, qpid::client::Connection* qpidConnectionp) :
    connection(conn),
    sessionp(NULL),
    sessionImplp(NULL),
    subs_mgrp(NULL),
    helperRunning(false),
    openCount(0),
    syncCount(0),
    closing(false),
    dtxEnabled(false)    
{
    bool success = false;
    try {
	sessionp = new qpid::client::AsyncSession;
	*sessionp = qpidConnectionp->newSession();
	subs_mgrp = new SubscriptionManager (*sessionp);
	waiters = gcnew Collections::Generic::List<CompletionWaiter^>();
	sessionLock = waiters;  // waiters convenient and not publicly visible
	openCloseLock = gcnew Object();
	success = true;
    } finally {
        if (!success) {
 	    Cleanup();
	    // TODO: include inner exception information
	    throw gcnew QpidException ("session creation failure");
	}
    }
}


void AmqpSession::Cleanup()
{
    bool connected = connection->IsOpen;

    if (subs_mgrp != NULL) {
	if (connected)
	    subs_mgrp->stop();
	delete subs_mgrp;
	subs_mgrp = NULL;
    }

    if (sessionp != NULL) {
	if (connected) {
	    sessionp->close();
	}
	delete sessionp;
	sessionp = NULL;
	sessionImplp = NULL;
    }
}


static qpid::framing::Xid& getXid(XaTransaction^ xaTx)
{
    return *((qpid::framing::Xid *)xaTx->XidHandle.ToPointer());
}


void AmqpSession::CheckOpen()
{
    if (closing)
	throw gcnew ObjectDisposedException("AmqpSession");
}


// Called by the parent AmqpConnection

void AmqpSession::ConnectionClosed()
{
    lock l(sessionLock);

    if (closing)
	return;

    closing = true;

    if (connection->IsOpen) {
	// send closing handshakes...

	if (dtxEnabled) {
	    // session may close before all its transactions complete, at least force the phase 0 flush
	    if (pendingTransactions->Count > 0) {
		array<XaTransaction^>^ txArray = pendingTransactions->ToArray();
		l.release();
		for each (XaTransaction^ xaTx in txArray) {
			//xaTx->SessionClosing(this);
			xaTx->WaitForCompletion();
		}
		l.acquire();
	    }
	}
	    
	WaitLastSync (%l);
	// Assert pendingTransactions->Count == 0

	if (openXaTransaction != nullptr) {
	    // send final dtxend
	    sessionp->dtxEnd(getXid(openXaTransaction), false, true, false);
	    openXaTransaction = nullptr;
	    openSystemTransaction = nullptr;
	    // this operation will complete by the time Cleanup() returns
	}
    }

    Cleanup();
}

InputLink^ AmqpSession::CreateInputLink(System::String^ sourceQueue)
{
    return CreateInputLink(sourceQueue, true, false, nullptr, nullptr);
}

InputLink^ AmqpSession::CreateInputLink(System::String^ sourceQueue, bool exclusive, bool temporary, 
					System::String^ filterKey, System::String^ exchange)
{
    lock ocl(openCloseLock);
    lock l(sessionLock);
    CheckOpen();

    InputLink^ link = gcnew InputLink (this, sourceQueue, sessionp, subs_mgrp, exclusive, temporary, filterKey, exchange);
    {
	if (openCount == 0) {
	    l.release();
	    connection->NotifyBusy();
	}
	openCount++;
    }
    return link;
}

OutputLink^ AmqpSession::CreateOutputLink(System::String^ targetQueue)
{
    lock ocl(openCloseLock);
    lock l(sessionLock);
    CheckOpen();

    OutputLink^ link = gcnew OutputLink (this, targetQueue);

    if (sessionImplp == NULL) {
	// not needed unless sending messages
	SessionBase_0_10Access sa(*sessionp);
	boost::shared_ptr<SessionImpl> sip = sa.get();
	sessionImplp = sip.get();
    }

    if (openCount == 0) {
	l.release();
	connection->NotifyBusy();
    }
    openCount++;

    return link;
}


// called whenever a child InputLink or OutputLink is closed or finalized
void AmqpSession::NotifyClosed()
{
    lock ocl(openCloseLock);
    openCount--;
    if (openCount == 0) {
	connection->NotifyIdle();
    }
}    


CompletionWaiter^ AmqpSession::SendMessage (System::String^ queue, MessageBodyStream ^mbody, TimeSpan timeout, bool async, AsyncCallback^ callback, Object^ state)
{
    lock l(sessionLock);

    // delimit with session dtx commands depending on the transaction context
    UpdateTransactionState(%l);

    CheckOpen();

    bool syncPending = false;

    // create an AMQP message.transfer command to use with the partial frameset from the MessageBodyStream

    std::string exname = QpidMarshal::ToNative(queue);
    FrameSet *framesetp = (FrameSet *) mbody->GetFrameSet().ToPointer();
    uint8_t acceptMode=1;
    uint8_t acquireMode=0;
    MessageTransferBody mtcmd(ProtocolVersion(0,10), exname, acceptMode, acquireMode);
    // ask for a command completion
    mtcmd.setSync(true);

    //send it

    Future *futurep = NULL;
    try {
	futurep = new Future(sessionImplp->send(mtcmd, *framesetp));

	CompletionWaiter^ waiter = nullptr;
	if (async || (timeout != TimeSpan::MaxValue)) {
	    waiter = gcnew CompletionWaiter(this, timeout, (IntPtr) futurep, callback, state);
	    // waiter is responsible for releasing the Future native resource
	    futurep = NULL;
	    addWaiter(waiter);
	    return waiter;
	}

	// synchronous send with no timeout: no need to involve the asyncHelper thread

	IncrementSyncs();
	syncPending = true;
	l.release();
	internalWaitForCompletion((IntPtr) futurep);
    }
    finally {
	if (syncPending) {
	    if (!l.is_locked())
		l.acquire();
	    DecrementSyncs();
	}
	if (futurep != NULL)
	    delete (futurep);
    }
    return nullptr;
}
    

void AmqpSession::Bind(System::String^ queue, System::String^ exchange, System::String^ filterKey)
{
    lock l(sessionLock);
    CheckOpen();

    sessionp->exchangeBind(arg::queue=QpidMarshal::ToNative(queue),
			       arg::exchange=QpidMarshal::ToNative(exchange),
			       arg::bindingKey=QpidMarshal::ToNative(filterKey));    

}


void AmqpSession::internalWaitForCompletion(IntPtr fp)
{
    Debug::Assert(syncCount > 0, "sync counter mismatch");

    // Qpid native lib call to wait for the command completion
    ((Future *)fp.ToPointer())->wait(*sessionImplp);
}

// call with lock held
void AmqpSession::addWaiter(CompletionWaiter^ waiter)
{
    IncrementSyncs();
    waiters->Add(waiter);
    if (!helperRunning) {
	helperRunning = true;
	ThreadPool::QueueUserWorkItem(gcnew WaitCallback(this, &AmqpSession::asyncHelper));
    }
}


void AmqpSession::removeWaiter(CompletionWaiter^ waiter)
{
    // a waiter can be removed from anywhere in the list if timed out

    lock l(sessionLock);
    int idx = waiters->IndexOf(waiter);
    if (idx == -1) {
	// TODO: assert or log
    }
    else {
	waiters->RemoveAt(idx);
	DecrementSyncs();
    }
}


// process CompletionWaiter list one at a time.

void AmqpSession::asyncHelper(Object ^unused)
{
    lock l(sessionLock);

    while (true) {
	if (waiters->Count == 0) {
	    helperRunning = false;
	    return;
	}

	CompletionWaiter^ waiter = waiters[0];
	l.release();
	// can block, but for short time
	// the waiter removes itself from the list, possibly as the timer thread on timeout
	waiter->Run();
	l.acquire();
    }
}

bool AmqpSession::MessageStop(std::string &name)
{
    lock l(sessionLock);

    if (closing)
	return false;

    sessionp->messageStop(name, true);
    return true;
}

void AmqpSession::AcceptAndComplete(SequenceSet& transfers)
{
    lock l(sessionLock);

    // delimit with session dtx commands depending on the transaction context
    UpdateTransactionState(%l);

    CheckOpen();

    sessionp->markCompleted(transfers, false);
    sessionp->messageAccept(transfers, false);
}


// call with session lock held

void AmqpSession::UpdateTransactionState(lock^ slock)
{
    Transaction^ currentTx = Transaction::Current;
    if ((currentTx == nullptr) && !dtxEnabled) {
	// no transaction scope and no previous dtx work to monitor
	return;
    }

    if (currentTx == openSystemTransaction) {
	// no change
	return;
    }

    if (!dtxEnabled) {
	// AMQP requires that this be the first dtx-related command on the session
	sessionp->dtxSelect(false);
	dtxEnabled = true;
	pendingTransactions = gcnew Collections::Generic::List<XaTransaction^>();
    }
 
    bool notify = false; // unless the System.Transaction is no longer active
    XaTransaction^ oldXaTx = openXaTransaction;
    if (openSystemTransaction != nullptr) {
	// The application may start a new transaction before the phase0 on rollback
	try {
	    if (openSystemTransaction->TransactionInformation->Status != TransactionStatus::Active) {
		notify = true;
	    }
	} catch (System::ObjectDisposedException^) {
	    notify = true;
	}
    }

    slock->release();
    // only use stack variables until lock re-acquired

    if (notify) {
	// will do call back to all enlisted sessions.  call with session lock released.
	// If NotifyPhase0() wins the race to start phase 0, openXaTransaction will be null
	oldXaTx->NotifyPhase0();
    }
    
    XaTransaction^ newXaTx = nullptr;
    if (currentTx != nullptr) {
	// This must be called with locks released.  The DTC and System.Transactions methods that
	// will be called hold locks that interfere with the ITransactionResourceAsync callbacks.
	newXaTx = DtxResourceManager::GetXaTransaction(this, currentTx);
    }

    slock->acquire();

    if (closing)
	return;

    if (openSystemTransaction != nullptr) {
	// some other transaction has the dtx window open
	// close the XID window, suspend = true... in case it is used again
	sessionp->dtxEnd(getXid(openXaTransaction), false, true, false);
	openSystemTransaction = nullptr;
	openXaTransaction = nullptr;
    }


    // Call enlist with session lock held.  The XaTransaction will call DtxStart before returning.
    if (newXaTx != nullptr) {
	if (!pendingTransactions->Contains(newXaTx)) {
	    pendingTransactions->Add(newXaTx);
	}

	newXaTx->Enlist(this);
    }

    openXaTransaction = newXaTx;
    openSystemTransaction = currentTx;
}


typedef TypedResult<qpid::framing::XaResult> XaResultCompletion;


// send the required closing dtx.End before Phase 1

IntPtr AmqpSession::BeginPhase0Flush(XaTransaction ^xaTx) {
    
    lock l(sessionLock);
    IntPtr completionp = IntPtr::Zero;
    try {
	if (sessionp != NULL) {

	    // proceed even if "closing == true", the phase 0 is part of the transition from closing to closed

	    if (xaTx != openXaTransaction) {
		// a different transaction (or none) is in scope, so xaTx was previously suspended.
		// must re-open it to close it properly
		if (openXaTransaction != nullptr) {
		    // suspend the session's current pending transaction
		    // it wil be reopened in a future enlistment or phase 0 flush.
		    sessionp->dtxEnd(getXid(openXaTransaction), false, true, false);
		}
		// resuming
		sessionp->dtxStart(getXid(xaTx), false, true, false);
	    }
	
	    // the closing (i.e. non-suspended) dtxEnd happens here (exactly once for a given transaction)
	    // set the sync bit since phase0 is a precondition to prepare or abort
	    completionp = (IntPtr) new XaResultCompletion(sessionp->dtxEnd(getXid(xaTx), false, false, true));
	    IncrementSyncs();
	}
    }
    catch (System::Exception^ ) {
	// all the caller wants to know is if completionp is non-null
    }

    openXaTransaction = nullptr;
    openSystemTransaction = nullptr;
    return completionp;
}


void AmqpSession::EndPhase0Flush(XaTransaction ^xaTx, IntPtr intptr) {
    XaResultCompletion *completionp = (XaResultCompletion *) intptr.ToPointer();
    lock l(sessionLock);

    if (completionp != NULL) {
	try {
	    l.release();
	    completionp->wait();
	    pendingTransactions->Remove(xaTx);
	}
	catch (System::Exception^) {
	    // connection closed or network drop
	}
	finally {
	    l.acquire();
	    DecrementSyncs();
	    delete completionp;
	}
    }
}


IntPtr AmqpSession::DtxStart(IntPtr ip, bool join, bool resume) {
    // called with session lock held (as a callback from the Enlist())
    // The XaTransaction knows if this should be the originating dtxStart, or a join/resume
    IntPtr rv = IntPtr::Zero;
    qpid::framing::Xid* xidp = (qpid::framing::Xid *) ip.ToPointer();
    if (join || resume) {
	sessionp->dtxStart(*xidp, join, resume, false);
    }
    else {
	// The XaTransaction needs to track when the first dtxStart completes to safely request a join
	IncrementSyncs();  // caller must use ReleaseCompletion() for corresponding DecrementSyncs
	rv = (IntPtr) new XaResultCompletion(sessionp->dtxStart(*xidp, join, resume, false));
    }

    return rv;
}


IntPtr AmqpSession::DtxPrepare(IntPtr ip) {
    qpid::framing::Xid* xidp = (qpid::framing::Xid *) ip.ToPointer();
    lock l(sessionLock);

    if (closing)
	return IntPtr::Zero;

    IncrementSyncs();  // caller must use ReleaseCompletion() for corresponding DecrementSyncs
    return (IntPtr) new XaResultCompletion(sessionp->dtxPrepare(*xidp, true));
}


IntPtr AmqpSession::DtxCommit(IntPtr ip, bool onePhase) {
    qpid::framing::Xid* xidp = (qpid::framing::Xid *) ip.ToPointer();
    lock l(sessionLock);

    if (closing)
	return IntPtr::Zero;

    IncrementSyncs();  // caller must use ReleaseCompletion() for corresponding DecrementSyncs
    return (IntPtr) new XaResultCompletion(sessionp->dtxCommit(*xidp, onePhase, true));
}


IntPtr AmqpSession::DtxRollback(IntPtr ip) {
    qpid::framing::Xid* xidp = (qpid::framing::Xid *) ip.ToPointer();
    lock l(sessionLock);
    if (closing)
	return IntPtr::Zero;

    IncrementSyncs();  // caller must use ReleaseCompletion() for corresponding DecrementSyncs

    return (IntPtr) new XaResultCompletion(sessionp->dtxRollback(*xidp, true));
}


//call with lock held
void AmqpSession::IncrementSyncs() {
    syncCount++;
}


//call with lock held
void AmqpSession::DecrementSyncs() {
    syncCount--;
    Debug::Assert(syncCount >= 0, "sync counter underrun");
    if (syncCount == 0) {
	if (closeWaitHandle != nullptr) {
	    // now OK to move from closing to closed
	    closeWaitHandle->Set();
	}
    }
}


// call with lock held
void AmqpSession::WaitLastSync(lock ^l) {
    if (syncCount == 0)
	return;
    if (AppDomain::CurrentDomain->IsFinalizingForUnload()) {
	// a wait would be a hang.  No more syncs coming
	return;
    }
    if (closeWaitHandle == nullptr)
	closeWaitHandle = gcnew ManualResetEvent(false);
    l->release();
    closeWaitHandle->WaitOne();
    l->acquire();
}


void AmqpSession::ReleaseCompletion(IntPtr completion) {
    lock l(sessionLock);
    DecrementSyncs();
    delete completion.ToPointer();
}

}}} // namespace Apache::Qpid::Cli
