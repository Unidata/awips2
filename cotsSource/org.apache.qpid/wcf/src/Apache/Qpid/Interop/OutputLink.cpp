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


#include "AmqpSession.h"
#include "AmqpMessage.h"
#include "OutputLink.h"
#include "QpidMarshal.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Runtime::InteropServices;
using namespace System::Threading;
using namespace msclr;

using namespace qpid::client;
using namespace std;

using namespace Apache::Qpid::AmqpTypes;


OutputLink::OutputLink(AmqpSession^ session, String^ defaultQueue) :
    amqpSession(session),
    queue(defaultQueue),
    disposed(false),
    maxFrameSize(session->Connection->MaxFrameSize),
    finalizing(false)
{
}

void OutputLink::Cleanup()
{
    {
        lock l(this);
        if (disposed)
	    return;

	disposed = true;
    }

    amqpSession->NotifyClosed();
}

OutputLink::~OutputLink()
{
    Cleanup();
}

OutputLink::!OutputLink()
{
    Cleanup();
}

void OutputLink::Close()
{
    // Simulate Dispose()...
    Cleanup();
    GC::SuppressFinalize(this);
}


AmqpMessage^ OutputLink::CreateMessage()
{
    MessageBodyStream ^mbody = gcnew MessageBodyStream(maxFrameSize);
    AmqpMessage ^amqpm = gcnew AmqpMessage(mbody);
    return amqpm;
}


void OutputLink::ManagedToNative(AmqpMessage^ m)
{
    MessageBodyStream^ messageBodyStream = (MessageBodyStream^ ) m->BodyStream;

    AmqpProperties^ mprops = m->Properties;

    if (mprops != nullptr) {
	AMQHeaderBody* bodyp = (AMQHeaderBody*) messageBodyStream->GetHeader().ToPointer();

	if (mprops->HasDeliveryProperties) {
	    DeliveryProperties* deliveryPropertiesp = bodyp->get<DeliveryProperties>(true);

	    if (mprops->RoutingKey != nullptr) {
		deliveryPropertiesp->setRoutingKey(QpidMarshal::ToNative(mprops->RoutingKey));
	    }

	    if (mprops->Durable) {
		deliveryPropertiesp->setDeliveryMode(qpid::framing::PERSISTENT);
	    }

	    if (mprops->TimeToLive.HasValue) {
		long long ttl = mprops->TimeToLive.Value.Ticks;
		bool was_positive = (ttl > 0);
		if (ttl < 0)
		    ttl = 0;
		ttl = ttl / TimeSpan::TicksPerMillisecond;
		if ((ttl == 0) && was_positive)
		    ttl = 1;
		deliveryPropertiesp->setTtl(ttl);
	    }
	}
	
	if (mprops->HasMessageProperties) {
	    qpid::framing::MessageProperties* messagePropertiesp =
		bodyp->get<qpid::framing::MessageProperties>(true);

	    String^ replyToExchange = mprops->ReplyToExchange;
	    String^ replyToRoutingKey = mprops->ReplyToRoutingKey;
	    if ((replyToExchange != nullptr) || (replyToRoutingKey != nullptr)) {
		qpid::framing::ReplyTo nReplyTo;
		if (replyToExchange != nullptr) {
		    nReplyTo.setExchange(QpidMarshal::ToNative(replyToExchange));
		}
		if (replyToRoutingKey != nullptr) {
		    nReplyTo.setRoutingKey(QpidMarshal::ToNative(replyToRoutingKey));
		}
		messagePropertiesp->setReplyTo(nReplyTo);
	    }

	    // TODO: properly split 1.0 style to 0-10 content type + encoding

	    String^ contentType = mprops->ContentType;
	    if (contentType != nullptr) {
		String^ type = nullptr;
		String^ enc = nullptr;
		int idx = contentType->IndexOf(';');
		if (idx == -1) {
		    type = contentType;
		}
		else {
		    type = contentType->Substring(0, idx);
		    contentType = contentType->Substring(idx + 1);
		    idx = contentType->IndexOf('=');
		    if (idx != -1) {
			enc = contentType->Substring(idx + 1);
			enc = enc->Trim();
		    }
		}
		if (!String::IsNullOrEmpty(type)) {
		    messagePropertiesp->setContentType(QpidMarshal::ToNative(type));
		}
		if (!String::IsNullOrEmpty(enc)) {
		    messagePropertiesp->setContentEncoding(QpidMarshal::ToNative(enc));
		}
	    }
		

	    array<unsigned char>^ mbytes = mprops->CorrelationId;
	    if (mbytes != nullptr) {
		pin_ptr<unsigned char> pinnedBuf = &mbytes[0];
		std::string s((char *) pinnedBuf, mbytes->Length);
		messagePropertiesp->setCorrelationId(s);
	    }

	    mbytes = mprops->UserId;
	    if (mbytes != nullptr) {
		pin_ptr<unsigned char> pinnedBuf = &mbytes[0];
		std::string s((char *) pinnedBuf, mbytes->Length);
		messagePropertiesp->setUserId(s);
	    }
		
	    if (mprops->HasMappedProperties) {
		qpid::framing::FieldTable fieldTable;
		// TODO: add support for abitrary AMQP types
		for each (Collections::Generic::KeyValuePair<System::String^, AmqpType^> kvp in mprops->PropertyMap) {
		    Type^ type = kvp.Value->GetType();
		    if (type == AmqpInt::typeid) {
			fieldTable.setInt(QpidMarshal::ToNative(kvp.Key), 
					  ((AmqpInt ^) kvp.Value)->Value);
		    }
		    else if (type == AmqpString::typeid) {
			AmqpString^ str = (AmqpString ^) kvp.Value;
			// For now, FieldTable supports a single string type
			fieldTable.setString(QpidMarshal::ToNative(kvp.Key), QpidMarshal::ToNative(str->Value));
		    }
		}

		messagePropertiesp->setApplicationHeaders(fieldTable);
	    }
	}
    }
}



void OutputLink::Send(AmqpMessage^ amqpMessage, TimeSpan timeout)
{
    // copy properties from managed space to the native counterparts
    ManagedToNative(amqpMessage);

    MessageBodyStream^ messageBodyStream = (MessageBodyStream^ ) amqpMessage->BodyStream;
    CompletionWaiter^ waiter = amqpSession->SendMessage(queue, messageBodyStream, timeout, false, nullptr, nullptr);

    if (waiter != nullptr) {
	waiter->WaitForCompletion();
	if (waiter->TimedOut) {
	    throw gcnew TimeoutException("Receive");
	}
    }
    // else: SendMessage() has already waited for the Completion

}

IAsyncResult^ OutputLink::BeginSend(AmqpMessage^ amqpMessage, TimeSpan timeout, AsyncCallback^ callback, Object^ state)
{
    ManagedToNative(amqpMessage);

    MessageBodyStream^ messageBodyStream = (MessageBodyStream^ ) amqpMessage->BodyStream;
    CompletionWaiter^ waiter = amqpSession->SendMessage(queue, messageBodyStream, timeout, true, callback, state);
    return waiter;
}

void OutputLink::EndSend(IAsyncResult^ result)
{
    CompletionWaiter^ waiter = (CompletionWaiter ^) result;
    waiter->WaitForCompletion();
    if (waiter->TimedOut) {
	throw gcnew TimeoutException("Receive");
    }
}


}}} // namespace Apache::Qpid::Interop
