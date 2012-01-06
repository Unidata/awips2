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
#include "QpidMarshal.h"
#include "QpidException.h"

namespace Apache {
namespace Qpid {
namespace Interop {


using namespace System;
using namespace System::Runtime::InteropServices;
using namespace System::Threading;
using namespace msclr;

using namespace qpid::client;
using namespace qpid::framing;

using namespace std;

using namespace Apache::Qpid::AmqpTypes;

// Scalability note: When using async methods, an async helper thread is created
// to block on the Demux BlockingQueue.  This design should be revised in line
// with proposed changes to the native library to reduce the number of servicing
// threads for large numbers of subscriptions.

// synchronization is accomplished with locks, but also by ensuring that only one
// MessageWaiter (the one at the front of the line) is ever active.  
// async threads to watch for: Close/finalizer, Timers, SyncCredit and the native Dispatch
// thread (who deposits FrameSets into the local queue and is oblivious to the
// managed space locks).


// The folowing def must match the "Frames" private typedef.
// TODO, make Qpid-cpp "Frames" definition visible.
typedef qpid::InlineVector<AMQFrame, 4> FrameSetFrames;

InputLink::InputLink(AmqpSession^ session, System::String^ sourceQueue,
		     qpid::client::AsyncSession *qpidSessionp, qpid::client::SubscriptionManager *qpidSubsMgrp,
		     bool exclusive,
		     bool temporary, System::String^ filterKey, System::String^ exchange) :
    amqpSession(session),
    subscriptionp(NULL),
    localQueuep(NULL),
    queuePtrp(NULL),
    dequeuedFrameSetpp(NULL),
    disposed(false),
    finalizing(false)
{
    bool success = false;
    System::Exception^ linkException = nullptr;

    waiters = gcnew Collections::Generic::List<MessageWaiter^>();
    linkLock = waiters;  // private and available
    subscriptionLock = gcnew Object();

    try {
	std::string qname = QpidMarshal::ToNative(sourceQueue);

	if (temporary) {
	    qpidSessionp->queueDeclare(arg::queue=qname, arg::durable=false, arg::autoDelete=true, arg::exclusive=true);
	    qpidSessionp->exchangeBind(arg::exchange=QpidMarshal::ToNative(exchange),
				       arg::queue=qname, arg::bindingKey=QpidMarshal::ToNative(filterKey));
	    qpidSessionp->sync();
	}

	localQueuep = new LocalQueue;
	SubscriptionSettings settings;
	settings.flowControl = FlowControl::messageCredit(0);
	settings.completionMode = CompletionMode::MANUAL_COMPLETION;

	Subscription sub = qpidSubsMgrp->subscribe(*localQueuep, qname, settings);
	subscriptionp = new Subscription (sub); // copy smart pointer for later IDisposable cleanup

	// the roundabout way to obtain localQueuep->queue
	SessionBase_0_10Access sa(*qpidSessionp);
	boost::shared_ptr<SessionImpl> simpl = sa.get();
	queuePtrp = new Demux::QueuePtr(simpl->getDemux().get(sub.getName()));

	success = true;
    } finally {
        if (!success) {
 	    Cleanup();
	    linkException = gcnew QpidException ("InputLink creation failure");
	    throw linkException;
	}
    }
}

// called with lock held
void InputLink::ReleaseNative()
{
    // involves talking to the Broker unless the connection is broken

    if ((subscriptionp != NULL) && !finalizing) {
	// TODO: find boost time error on cleanup when in finalizer thread
        try {
	    subscriptionp->cancel();
	}
	catch (const std::exception& error) {
	    // TODO: log this properly
	    std::cout << "shutdown error " << error.what() << std::endl;
	}
    }

    // free native mem (or smart pointers) that we own
    if (subscriptionp != NULL) {
	delete subscriptionp;
	subscriptionp = NULL;
    }
    if (queuePtrp != NULL) {
	delete queuePtrp;
	queuePtrp = NULL;
    }
    if (localQueuep != NULL) {
	if (!finalizing) {
	    // TODO: find boost time error on cleanup when in finalizer thread
	    delete localQueuep;
	    localQueuep = NULL;
	}
    }
    if (dequeuedFrameSetpp != NULL) {
	delete dequeuedFrameSetpp;
	dequeuedFrameSetpp = NULL;
    }
}

void InputLink::Cleanup()
{
    {
        lock l(linkLock);
        if (disposed)
	    return;

	disposed = true;

	// if the asyncHelper exists and is idle, unblock it
	if (asyncHelperWaitHandle != nullptr) {
	    asyncHelperWaitHandle->Set();
	}

	// wakeup anyone waiting for messages
	if (queuePtrp != NULL)
	    (*queuePtrp)->close();
  
	// wait for any sync operations on the subscription to complete before ReleaseNative
	lock l2(subscriptionLock);

	try {}
	finally
	{
	    ReleaseNative();
	}

    }
    amqpSession->NotifyClosed();
}

InputLink::~InputLink()
{
    Cleanup();
}

InputLink::!InputLink()
{
    finalizing = true;
    Cleanup();
}

void InputLink::Close()
{
    // Simulate Dispose()...
    Cleanup();
    GC::SuppressFinalize(this);
}

// call with lock held
bool InputLink::haveMessage()
{
    if (dequeuedFrameSetpp != NULL)
	return true;

    if (queuePtrp != NULL) {
	if ((*queuePtrp)->size() > 0)
	    return true;
    }
    return false;
}
	
IntPtr InputLink::nextLocalMessage()
{
    lock l(linkLock);

    if (disposed)
	return (IntPtr) NULL;

    // A message already pulled off BlockingQueue?
    if (dequeuedFrameSetpp != NULL) {
	QpidFrameSetPtr* rv = dequeuedFrameSetpp;
	dequeuedFrameSetpp = NULL;
	return (IntPtr) rv;
    }

    if ((*queuePtrp)->empty())
	return (IntPtr) NULL;

    bool received = false;
    QpidFrameSetPtr* frameSetpp = new QpidFrameSetPtr;

    try {
	received = (*queuePtrp)->pop(*frameSetpp, qpid::sys::TIME_INFINITE);
	if (received) {
	    QpidFrameSetPtr* rv = frameSetpp;
	    // no need to free native in finally block
	    frameSetpp = NULL;
	    return (IntPtr) rv;
	}
    } catch(const std::exception& error) {
	// should be no async tampering with queue since we hold the lock and have a
	// smart pointer ref to the native LocalQueue, even if the network connection fails...
	cout << "unknown exception in InputLink.nextLocalMessage() " << error.what() <<endl;
	// TODO: log this
    }
    finally {
	if (frameSetpp != NULL) {
	    delete frameSetpp;
	}
    }

    return (IntPtr) NULL;
}

    

void InputLink::unblockWaiter()
{
    // to be followed by resetQueue() below
    lock l(linkLock);
    if (disposed)
	return;
    (*queuePtrp)->close();
}



// Set things right after unblockWaiter(). Closing and opening a Qpid BlockingQueue unsticks 
// a blocking thread without interefering with queue contents or the ability to push
// new incoming messages.

void InputLink::resetQueue()
{
    lock l(linkLock);
    if (disposed)
	return;
    if ((*queuePtrp)->isClosed()) {
	(*queuePtrp)->open();
    }
}


// returns true if there is a message to consume, i.e. nextLocalMessage() won't block

bool InputLink::internalWaitForMessage()
{
    Demux::QueuePtr demuxQueuePtr;

    bool received = false;
    QpidFrameSetPtr* frameSetpp = NULL;
    try	{
	lock l(linkLock);
	if (disposed)
	    return false;
	if (haveMessage())
	    return true;

	AdjustCredit();

	// get a scoped smart ptr ref to guard against async close or hangup
	demuxQueuePtr = *queuePtrp;
	frameSetpp = new QpidFrameSetPtr;

	l.release();
	// Async cleanup is now possible.  Only use demuxQueuePtr until lock reacquired.
	received = demuxQueuePtr->pop(*frameSetpp, qpid::sys::TIME_INFINITE);
	l.acquire();

	if (received) {
	    dequeuedFrameSetpp = frameSetpp;
	    frameSetpp = NULL;	// native will eventually be freed in Cleanup or MessageBodyStream
	}

	return true;
    } catch(const std::exception& ) {
	// timeout or connection closed
	return false;
    }
    finally {
	if (frameSetpp != NULL) {
	    delete frameSetpp;
	}
    }

    return false;
}


// call with lock held
void InputLink::addWaiter(MessageWaiter^ waiter)
{
    waiters->Add(waiter);
    if (waiters->Count == 1) {
	// mark this waiter as ready to run
	// Only the waiter at the head of the queue is active.
	waiter->Activate();
    }
    
    if (waiter->Assigned)
	return;

    if (asyncHelperWaitHandle == nullptr) {
	asyncHelperWaitHandle = gcnew ManualResetEvent(false);
	ThreadStart^ threadDelegate = gcnew ThreadStart(this, &InputLink::asyncHelper);
	(gcnew Thread(threadDelegate))->Start();
    }

    if (waiters->Count == 1) {
	// wake up the asyncHelper
	asyncHelperWaitHandle->Set();
    }
}


void InputLink::removeWaiter(MessageWaiter^ waiter) {
    // a waiter can be removed from anywhere in the list if timed out

    lock l(linkLock);
    int idx = waiters->IndexOf(waiter);
    if (idx == -1) {
	// TODO: assert or log
	if (asyncHelperWaitHandle != nullptr) {
	    // just in case.
	    asyncHelperWaitHandle->Set();
	}
	return;
    }

    waiters->RemoveAt(idx);
    if (waiter->TimedOut) {
	// may have to give back message if it arrives momentarily
	AdjustCredit();
    }

    // let the next waiter know it's his turn. 
    if (waiters->Count > 0) {
	MessageWaiter^ nextWaiter = waiters[0];

	// wakeup the asyncHelper thread to help out if necessary.
	if (!nextWaiter->Assigned) {
	    asyncHelperWaitHandle->Set();
	}

	l.release();
	nextWaiter->Activate();
	return;
    }
    else {
	if (disposed && (asyncHelperWaitHandle != nullptr)) {
	    asyncHelperWaitHandle->Set();
	}
    }
}


void InputLink::asyncHelper()
{
    lock l(linkLock);

    while (true) {
	if (disposed && (waiters->Count == 0)) {
	    asyncHelperWaitHandle = nullptr;
	    return;
	}

	if (waiters->Count > 0) {
	    MessageWaiter^ waiter = waiters[0];

	    l.release();
	    if (waiter->AcceptForWork()) {
		waiter->Run();
	    }
	    l.acquire();
	}

	// sleep if more work may be coming or it is currently someone else's turn
	if (((waiters->Count == 0) && !disposed) || ((waiters->Count != 0) && waiters[0]->Assigned)) {
	    // wait for something to do
	    asyncHelperWaitHandle->Reset();
	    l.release();
	    asyncHelperWaitHandle->WaitOne();
	    l.acquire();
	}
    }
}

void InputLink::sync()
{
    // used by the MessageWaiter timeout thread to not run before fully initialized
    lock l(linkLock);
}


void InputLink::PrefetchLimit::set(int value)
{
    lock l(linkLock);
    prefetchLimit = value;

    int delta = 0;

    // rough rule of thumb to keep the flow, but reduce chatter.
    // for small messages, the credit request is almost as expensive as the transfer itself.
    // experience may suggest a better heuristic or require a property for the low water mark
    if (prefetchLimit >= 3) {
	delta = prefetchLimit / 3;
    }
    minWorkingCredit = prefetchLimit - delta;
    AdjustCredit();
}


// call with lock held
void InputLink::AdjustCredit()
{
    if (creditSyncPending || disposed)
	return;

    // low watermark check
    if ((prefetchLimit != 0) &&
	(workingCredit >= minWorkingCredit) &&
	(workingCredit >= waiters->Count))
	return;

    // should have enough for all waiters or to satisfy the prefetch window
    int targetCredit = waiters->Count;
    if (targetCredit < prefetchLimit)
	targetCredit = prefetchLimit;

    if (targetCredit > workingCredit) {
	subscriptionp->grantMessageCredit(targetCredit - workingCredit);
	workingCredit = targetCredit;
	return;
    }
    if (targetCredit < workingCredit) {
	if ((targetCredit == 0) && (prefetchLimit == 0)) {
	    creditSyncPending = true;
	    ThreadPool::QueueUserWorkItem(gcnew WaitCallback(this, &InputLink::SyncCredit));
	}
	// TODO: also shrink credit when prefetchLimit != 0
    }
}

void InputLink::SyncCredit(Object ^unused)
{
    lock l(linkLock);

    try {
	if (disposed)
	    return;

	if (!amqpSession->MessageStop(subscriptionp->getName())) {
	    // connection closed
	    return;
	}

	l.release();
	// use setFlowControl to re-enable credit flow on the broker.
	// setFlowControl is a sync operation
	{
	    lock l2(subscriptionLock);
	    if (subscriptionp != NULL) {
		subscriptionp->setFlowControl(subscriptionp->getSettings().flowControl);
	    }
	}
	l.acquire();

	if (disposed)
	    return;

	// let existing waiters use up any messages that arrived.
	// local queue size can only decrease until more credit is issued
	while (true) {
	    if ((waiters->Count > 0) && ((*queuePtrp)->size() > 0)) {
		l.release();
		// a rare use case and not used in performance oriented code.
		// optimization can wait until the qpid/messaging api is used
		Thread::Sleep(10);
		l.acquire();
		if (disposed)
		    return;
	    }
	    else {
		break;
	    }
	}

	// At this point, the lock is held and we are fully synced with the broker
	// so we have a valid snapshot 

	if ((prefetchLimit == 0) && ((*queuePtrp)->size() > 0)) {
	    // can't be sure application will request a message again any time soon
	    QpidFrameSetPtr frameSetp;
	    while (!(*queuePtrp)->empty()) {
		(*queuePtrp)->pop(frameSetp);
		SequenceSet frameSetID(frameSetp->getId());
		subscriptionp->release(frameSetID);
	    }

	    // don't touch dequeuedFrameSetpp.  It is spoken for: explicitely from a
	    // MessageWaiter about to to get the nextLocalMessage(), or implicitely
	    // from a WaitForMessage().
	}
	// TODO: if prefetchLimit != 0, release messages from back of the queue that exceed targetCredit

	workingCredit = (*queuePtrp)->size();
	if (dequeuedFrameSetpp != NULL) {
	    workingCredit++;
	}
    }
    finally {
	creditSyncPending = false;
    }

    AdjustCredit();
}


AmqpMessage^ InputLink::createAmqpMessage(IntPtr msgp)
{
    QpidFrameSetPtr* fspp = (QpidFrameSetPtr*) msgp.ToPointer();
    bool ownFrameSet = true;
    bool haveProperties = false;

    try {
	MessageBodyStream^  mstream = gcnew MessageBodyStream(fspp);
	ownFrameSet = false;	// stream releases on close/dispose

	AmqpMessage^ amqpMessage = gcnew AmqpMessage(mstream);

	AMQHeaderBody* headerBodyp = (*fspp)->getHeaders();
	uint64_t contentSize = (*fspp)->getContentSize();
	SequenceSet frameSetID((*fspp)->getId());

	// target managed representation
	AmqpProperties^ amqpProperties = gcnew AmqpProperties();

	// source native representation
	const DeliveryProperties* deliveryProperties = headerBodyp->get<DeliveryProperties>();
	const qpid::framing::MessageProperties* messageProperties = headerBodyp->get<qpid::framing::MessageProperties>();

	if (deliveryProperties) {
	    if (deliveryProperties->hasRoutingKey()) {
		haveProperties = true;

		amqpProperties->RoutingKey = gcnew String(deliveryProperties->getRoutingKey().c_str());
	    }

	    if (deliveryProperties->hasDeliveryMode()) {
		if (deliveryProperties->getDeliveryMode() == qpid::framing::PERSISTENT)
		    amqpProperties->Durable = true;
	    }

	    if (deliveryProperties->hasTtl()) {
		long long ticks = deliveryProperties->getTtl() * TimeSpan::TicksPerMillisecond;
		amqpProperties->TimeToLive = Nullable<TimeSpan>(TimeSpan::FromTicks(ticks));
	    }
	}

	if (messageProperties) {

	    if (messageProperties->hasReplyTo()) {
		haveProperties = true;
		const ReplyTo& rpto = messageProperties->getReplyTo();
		String^ rk = nullptr;
		String^ ex = nullptr;
		if (rpto.hasRoutingKey()) {
		    rk = gcnew String(rpto.getRoutingKey().c_str());
		}
		if (rpto.hasExchange()) {
		    ex = gcnew String(rpto.getExchange().c_str());
		}
		amqpProperties->SetReplyTo(ex,rk);
	    }
	    
	    if (messageProperties->hasContentType()) {
		haveProperties = true;
		amqpProperties->ContentType = gcnew String(messageProperties->getContentType().c_str());

		if (messageProperties->hasContentEncoding()) {
		    String^ enc = gcnew String(messageProperties->getContentEncoding().c_str());
		    if (!String::IsNullOrEmpty(enc)) {
			// TODO: properly assemble 1.0 style to 0-10 for all cases
			amqpProperties->ContentType += "; charset=" + enc;
		    }
		}
	    }

	    if (messageProperties->hasCorrelationId()) {
		haveProperties = true;
		const std::string& ncid = messageProperties->getCorrelationId();
		int len = ncid.size();
		array<unsigned char>^ mcid = gcnew array<unsigned char>(len);
		Marshal::Copy ((IntPtr) (void *) ncid.data(), mcid, 0, len);
		amqpProperties->CorrelationId = mcid;
	    }

	    if (messageProperties->hasUserId()) {
		haveProperties = true;
		const std::string& nuid = messageProperties->getUserId();
		int len = nuid.size();
		array<unsigned char>^ muid = gcnew array<unsigned char>(len);
		Marshal::Copy ((IntPtr) (void *) nuid.data(), muid, 0, len);
		amqpProperties->UserId = muid;
	    }

	    if (messageProperties->hasApplicationHeaders()) {
		haveProperties = true;
		const qpid::framing::FieldTable& fieldTable = messageProperties->getApplicationHeaders();
		int count = fieldTable.count();

		if (count > 0) {
		    haveProperties = true;
		    Collections::Generic::Dictionary<System::String^, AmqpType^>^ mmap = 
			gcnew Collections::Generic::Dictionary<System::String^, AmqpType^>(count);

		    for(qpid::framing::FieldTable::ValueMap::const_iterator i = fieldTable.begin(); i != fieldTable.end(); i++) {

			qpid::framing::FieldValue::Data &data = i->second->getData();
			
			// TODO: replace these generic int/string conversions with handler for each AMQP specific type:
			// uint8_t dataType = i->second->getType();
			// switch (dataType) { case TYPE_CODE_STR8: ... }

			if (data.convertsToInt()) {
			    mmap->Add (gcnew String(i->first.data()), gcnew AmqpInt((int) i->second->getData().getInt()));
			}
			if (data.convertsToString()) {
			    std::string ns = data.getString();
			    String^ ms = gcnew String(ns.data(), 0, ns.size());
			    mmap->Add (gcnew String(i->first.data()), gcnew AmqpString(ms));
			}
		    }
		
		    amqpProperties->PropertyMap = mmap;
		}

	    }
	}

	if (haveProperties) {
	    amqpMessage->Properties = amqpProperties;
	}

	// We have a message we can return to the caller.
	// Tell the broker we got it.
	
	// subscriptionp->accept(frameSetID) is a slow sync operation in the native API
	// so do it within the AsyncSession directly
	amqpSession->AcceptAndComplete(frameSetID);

	workingCredit--;
	// check if more messages need to be requested from broker
	AdjustCredit();

	return amqpMessage;
    }
    finally {
	if (ownFrameSet)
	    delete (fspp);
    }
}

    // As for IInputChannel:
    //   if success, return true + amqpMessage
    //   elseif timeout, return false
    //   elseif closed/EOF, return true and amqpMessage = null
    //   else throw an Exception

bool InputLink::TryReceive(TimeSpan timeout, [Out] AmqpMessage^% amqpMessage)
{
    lock l(linkLock);

    if (waiters->Count == 0) {
	// see if there is a message already available without blocking
	IntPtr fspp = nextLocalMessage();
	if (fspp.ToPointer() != NULL) {
	    amqpMessage = createAmqpMessage(fspp);
	    return true;
	}
    }

    MessageWaiter^ waiter = gcnew MessageWaiter(this, timeout, true, false, nullptr, nullptr);
    addWaiter(waiter);

    l.release();
    waiter->Run();
    l.acquire();
    
    if (waiter->TimedOut) {
	return false;
    }

    IntPtr waiterMsg = waiter->Message;
    if (waiterMsg.ToPointer() == NULL) {
	if (disposed) {
	    // indicate normal EOF on channel
	    amqpMessage = nullptr;
	    return true;
	}
    }

    amqpMessage = createAmqpMessage(waiterMsg);
    return true;
}

IAsyncResult^ InputLink::BeginTryReceive(TimeSpan timeout, AsyncCallback^ callback, Object^ state)
{

    //TODO: if haveMessage() complete synchronously

    lock l(linkLock);
    MessageWaiter^ waiter = gcnew MessageWaiter(this, timeout, true, true, callback, state);
    addWaiter(waiter);
    return waiter;
}

bool InputLink::EndTryReceive(IAsyncResult^ result, [Out] AmqpMessage^% amqpMessage)
{

    // TODO: validate result

    MessageWaiter^ waiter = (MessageWaiter ^) result;

    waiter->WaitForCompletion();

    if (waiter->RunException != nullptr)
	throw waiter->RunException;

    if (waiter->TimedOut) {
	amqpMessage = nullptr;
	return false;
    }

    IntPtr waiterMsg = waiter->Message;
    if (waiterMsg.ToPointer() == NULL) {
	if (disposed) {
	    // indicate normal EOF on channel
	    amqpMessage = nullptr;
	    return true;
	}
    }

    amqpMessage = createAmqpMessage(waiterMsg);
    return true;
}


bool InputLink::WaitForMessage(TimeSpan timeout)
{
    lock l(linkLock);

    if (disposed)
	return false;

    if (waiters->Count == 0) {
	// see if there is a message already available without blocking
	if (haveMessage())
	    return true;
    }

    // Same as for TryReceive, except consuming = false
    MessageWaiter^ waiter = gcnew MessageWaiter(this, timeout, false, false, nullptr, nullptr);
    addWaiter(waiter);

    l.release();
    waiter->Run();
    l.acquire();
    
    if (waiter->TimedOut) {
	return false;
    }

    return haveMessage();
}

IAsyncResult^ InputLink::BeginWaitForMessage(TimeSpan timeout, AsyncCallback^ callback, Object^ state)
{
    lock l(linkLock);

    // Same as for BeginTryReceive, except consuming = false
    MessageWaiter^ waiter = gcnew MessageWaiter(this, timeout, false, true, callback, state);
    addWaiter(waiter);
    return waiter;
}

bool InputLink::EndWaitForMessage(IAsyncResult^ result)
{
    MessageWaiter^ waiter = (MessageWaiter ^) result;

    waiter->WaitForCompletion();

    if (waiter->TimedOut) {
	return false;
    }

    return haveMessage();
}


}}} // namespace Apache::Qpid::Interop
