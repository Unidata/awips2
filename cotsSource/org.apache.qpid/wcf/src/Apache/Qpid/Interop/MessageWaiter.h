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

public ref class MessageWaiter : IAsyncResult
{
private:
    // Receive() or WaitForMessage()
    bool consuming;
    bool consumed;
    bool timedOut;
    bool async;
    // has an owner thread
    bool assigned;
    // can Run (i.e. earlier MessageWaiters in the queue have completed)
    bool activated;
    // is making a call to internalWaitForMessage() which (usually) blocks
    bool blocking;
    // the timeout timer thread is lurking
    bool processingTimeout;
    // the saved exception from within Run() for async delivery
    System::Exception^ runException;
    AsyncCallback^ asyncCallback;
    Threading::Timer ^timer;
    bool isCompleted;
    bool completedSynchronously;
    Object^ state;
    Object^ thisLock;
    ManualResetEvent^ asyncWaitHandle;
    InputLink^ parent;
    static void TimeoutCallback(Object^ state);
    static TimerCallback^ timeoutCallback = gcnew TimerCallback(MessageWaiter::TimeoutCallback);
    IntPtr message;
    !MessageWaiter();
    ~MessageWaiter();

 internal:
    MessageWaiter(InputLink^ parent, TimeSpan timeSpan, bool consuming, bool async, AsyncCallback ^callback, Object^ state);

    void Run();
    bool AcceptForWork();
    void Activate();
    void WaitForCompletion();


    property IntPtr Message {
	IntPtr get () { 
	    if (!consuming || consumed)
		throw gcnew InvalidOperationException("Message property");
	    consumed = true;
	    IntPtr v = message;
	    message = IntPtr::Zero;
	    GC::SuppressFinalize(this);
	    return v;
	}
    }

    property bool Assigned {
	bool get () { return assigned; }
    }

    property bool TimedOut {
	bool get () { return timedOut; }
    }

    property System::Exception^ RunException {
	System::Exception^ get() { return runException; }
    }


 public:

    virtual property bool IsCompleted {
	bool get () { return isCompleted; }
    }

    virtual property bool CompletedSynchronously {
	bool get () { return completedSynchronously; }
    }

    virtual property WaitHandle^ AsyncWaitHandle {
	WaitHandle^ get () {
	    if (asyncWaitHandle != nullptr) {
		return asyncWaitHandle;
	    }

	    msclr::lock l(thisLock);
	    if (asyncWaitHandle == nullptr) {
		asyncWaitHandle = gcnew ManualResetEvent(isCompleted);
	    }
	    return asyncWaitHandle;
	}
    }

    virtual property Object^ AsyncState {
	Object^ get () { return state; }
    }
};

}}} // namespace Apache::Qpid::Interop

