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
#include "MessageWaiter.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Threading;
using namespace msclr;


MessageWaiter::MessageWaiter(InputLink^ parent, TimeSpan timeSpan, bool consuming, bool async, AsyncCallback ^callback, Object^ state)
{
    this->consuming = consuming;
    if (!consuming) {
	GC::SuppressFinalize(this);
    }

    if (async) {
	this->async = true;
	this->asyncCallback = callback;
	this->state = state;
    }
    else {
	this->assigned = true;
    }
    this->parent = parent;
    this->thisLock = gcnew Object();

    // do this after the Message Waiter is fully initialized, in case of
    // very small timespan 
    if (timeSpan != TimeSpan::MaxValue) {
	this->timer = gcnew Timer(timeoutCallback, this, timeSpan, TimeSpan::FromMilliseconds(-1));
    }
}

MessageWaiter::~MessageWaiter()
{
    if (message != IntPtr::Zero) {
	try{}
	finally {
	    delete message.ToPointer();
	    message = IntPtr::Zero;
	}
    }
}

MessageWaiter::!MessageWaiter()
{
    this->~MessageWaiter();
}


void MessageWaiter::WaitForCompletion()
{
    if (isCompleted)
	return;

    lock l(thisLock);
    while (!isCompleted) {
	Monitor::Wait(thisLock);
    }
}

void MessageWaiter::Activate()
{
    if (activated)
	return;

    lock l(thisLock);
    if (!activated) {
	activated = true;
	Monitor::PulseAll(thisLock);
    }
}


void MessageWaiter::Run()
{
    lock l(thisLock);

    // wait until Activate(), i.e. our turn in the waiter list or a timeout
    while (!activated) {
	Monitor::Wait(thisLock);
    }
    bool haveMessage = false;
    bool mustReset = false;

    if (!timedOut)
	blocking = true;

    if (blocking) {
	l.release();

	try {
	    haveMessage = parent->internalWaitForMessage();
	}
	catch (System::Exception^ e) {
	    runException = e;
	}

	l.acquire();
	blocking = false;
	if (timedOut) {
	    // TimeoutCallback() called parent->unblockWaiter()
	    mustReset = true;
	    // let the timer thread move past critical region
	    while (processingTimeout) {
		Monitor::Wait(thisLock);
	    }
	}
    }

    if (timer != nullptr) {
	timer->~Timer();
	timer = nullptr;
    }

    if (haveMessage) {
	timedOut = false;	// for the case timeout and message arrival are essentially tied
	if (!consuming) {
	    // just waiting
	    haveMessage = false;
	}
    }

    if (haveMessage || mustReset) {
	l.release();
	if (haveMessage) {
	    // hang on to it for when the async caller gets around to retrieving
	    message = parent->nextLocalMessage();
	}
	if (mustReset) {
	    parent->resetQueue();
	}
	l.acquire();
    }

    isCompleted = true;
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

bool MessageWaiter::AcceptForWork()
{
    lock l(thisLock);
    if (!assigned) {
	assigned = true;
	return true;
    }
    return false;
}

void MessageWaiter::TimeoutCallback(Object^ state)
{
    MessageWaiter^ waiter = (MessageWaiter^) state;
    if (waiter->isCompleted)
	return;

    // make sure parent has finished initializing us before we get going
    waiter->parent->sync();

    lock l(waiter->thisLock);
    if (waiter->timer == nullptr) {
	// the waiter is in the clean up phase and doesn't need a wakeup
	return;
    }

    // timedOut, blocking and processingTimeout work as a unit
    waiter->timedOut = true;
    if (waiter->blocking) {
	// let the waiter know that we are busy with an upcoming unblock operation
	waiter->processingTimeout = true;
    }

    waiter->Activate();

    if (waiter->processingTimeout) {
	// call with lock off
	l.release();
	waiter->parent->unblockWaiter();

	// synchronize with blocked thread
	l.acquire();
	waiter->processingTimeout = false;
	Monitor::PulseAll(waiter->thisLock);
    }

    l.release();

    // If waiter has no associated thread, we must move it to completion
    if (waiter->AcceptForWork()) {
	waiter->Run();	// does not block since timedOut == true
    }
}

}}} // namespace Apache::Qpid::Interop
