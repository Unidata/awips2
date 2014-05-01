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

#include "qpid/client/AsyncSession.h"
#include "qpid/framing/FrameSet.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/Connection.h"
#include "qpid/client/Message.h"
#include "qpid/client/MessageListener.h"
#include "qpid/client/Demux.h"
#include "qpid/client/SessionImpl.h"
#include "qpid/client/SessionBase_0_10Access.h"

#include "MessageBodyStream.h"
#include "AmqpMessage.h"
#include "AmqpSession.h"
#include "InputLink.h"
#include "CompletionWaiter.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Threading;
using namespace msclr;

// A class to provide IAsyncResult semantics for a qpid AsyncSession command (i.e. 0-10 messageTransfer) 
// when the client session receives a "Completion" notification from the Broker.


CompletionWaiter::CompletionWaiter(AmqpSession^ parent, TimeSpan timeSpan, IntPtr future, AsyncCallback^ callback, Object^ state)
{
    this->qpidFuture = future;
    this->asyncCallback = callback;
    this->state = state;
    this->parent = parent;
    this->thisLock = gcnew Object();
    // do this after the Completion Waiter is fully initialized, in case of
    // very small timespan 
    if (timeSpan != TimeSpan::MaxValue) {
	this->timer = gcnew Timer(timeoutCallback, this, timeSpan, TimeSpan::FromMilliseconds(-1));
    }
}


void CompletionWaiter::WaitForCompletion()
{
    if (isCompleted)
	return;

    lock l(thisLock);
    while (!isCompleted) {
	Monitor::Wait(thisLock);
    }
}

void CompletionWaiter::Run()
{
    // no locks required in this method
    if (isCompleted)
	return;

    try {
	// Wait for the arrival of the "AMQP Completion" indication from the Broker
	parent->internalWaitForCompletion(qpidFuture);
    }
    catch (System::Exception^ e) {
	runException = e;
    }
    finally {
	delete(qpidFuture.ToPointer());
	qpidFuture = (IntPtr) NULL;
    }

    if (timer != nullptr) {
	timer->~Timer();
	timer = nullptr;
    }

    Complete(false);
}


// "Complete" here means complete the AsyncResult, which may precede broker "command completion" if timed out

void CompletionWaiter::Complete(bool isTimerThread)
{
    lock l(thisLock);
    if (isCompleted)
	return;

    isCompleted = true;
    if (isTimerThread)
	timedOut = true;

    Monitor::PulseAll(thisLock);

    // do this check and signal while locked
    if (asyncWaitHandle != nullptr)
	asyncWaitHandle->Set();

    l.release();

    parent->removeWaiter(this);

    if (asyncCallback != nullptr) {
	// guard against application callback exception
	try {
	    asyncCallback(this);
	}
	catch (System::Exception^) {
	    // log it?
	}
    }
}


void CompletionWaiter::TimeoutCallback(Object^ state)
{
    CompletionWaiter^ waiter = (CompletionWaiter^) state;
    waiter->Complete(true);
}


}}} // namespace Apache::Qpid::Interop
