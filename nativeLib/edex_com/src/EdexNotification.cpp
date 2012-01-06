/*****************************************************************************************
 * COPYRIGHT (c), 2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

/*
 * Notification capability to retrieve DataURIs from EDEX notification.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/2/09       3375       brockwoo    Initial Creation
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1
 */

#include <qpid/client/QueueOptions.h>
#include <qpid/client/Connection.h>
#include <qpid/Url.h>
#include "EdexNotification.h"

using qpid::Url;

EdexNotification::EdexNotification(const string & brokerURI) {
	this->sessionTransacted = false;
	this->brokerURI = brokerURI;
	this->isConnected = false;
	this->listSize = 0;
	this->mess = new DataURINotificationMessage();
	this->timeout = false;
	this->timeoutLength = 999999;
	this->subman = NULL;
	this->localQueue = NULL;
}

EdexNotification::~EdexNotification() {
	this->cleanup();
	delete this->mess;
}

bool EdexNotification::hasMessages() {
	if (listSize < 1) {
		return false;
	} else {
		return (myStringIterator != mess->dataURIs.end());
	}
}

string EdexNotification::getDataUri() {
	string returnString = (*myStringIterator);
	myStringIterator++;
	return returnString;
}

int EdexNotification::getMessageCount() {
	return listSize;
}

void EdexNotification::clearListenTimeout() {
	this->timeout = false;
}

void EdexNotification::setListenTimeout(int length) {
	this->timeout = true;
	this->timeoutLength = length;
}

void EdexNotification::listen() {
	this->connect();
	if (isConnected) {
		Message message;
		bool result;
		try {
			result = localQueue->get(message, 120 * qpid::sys::TIME_SEC);
		} catch(...) {
			cleanup();
			throw;
		}
		if (result) {
			std::string output = message.getData();
			/* Message * message;
			 if(timeout) {
			 message = this->consumer->receive();
			 }
			 else {
			 message = this->consumer->receive(timeoutLength);
			 }
			 if (message == NULL) {
			 listSize = 0;
			 } else {
			 */
			uint8_t * data = (uint8_t *) output.c_str();
			TMemoryBuffer * buffer = new TMemoryBuffer(data,
					output.length(),
					apache::thrift::transport::TMemoryBuffer::COPY);
			shared_ptr<TMemoryBuffer> ptrBuffer = shared_ptr<TMemoryBuffer> (
					buffer);
			NotificationProtocol * prot = new NotificationProtocol(ptrBuffer);

			int32_t info = 0;
			TMessageType type;
			string messageName;
			prot->readMessageBegin(messageName, type, info);
			mess->read(prot);
			listSize = (int) mess->dataURIs.size();
			if (listSize > 0) {
				myStringIterator = mess->dataURIs.begin();
			}
			delete prot;
		} else {
			listSize = -1;
		}
	}
}

void EdexNotification::cleanup() {
	// Destroy resources.
	try {
		delete subman;
		subman = NULL;
		delete localQueue;
		localQueue = NULL;
		session.close();
		connection.close();
	} catch (const std::exception& error) {
		this->isConnected = false;
	}
	this->isConnected = false;
}

bool EdexNotification::connect() {
	if (this->isConnected) {
		return this->isConnected;
	}
	try {
		this->connection.open(Url(brokerURI));
		this->session = this->connection.newSession();
		queue = "_edex.alert-edex_com@amq.topic_";
		queue += session.getId().getName();
		QueueOptions qo;
		qo.setSizePolicy(RING, 100 * 1024 * 1024, 5000);
		session.queueDeclare(arg::queue = queue, arg::exclusive = true,
				arg::autoDelete = true, arg::arguments=qo);
		session.exchangeBind(arg::exchange = "amq.topic", arg::queue = queue,
				arg::bindingKey = "edex.alerts");
		subman = new SubscriptionManager(session);
		localQueue = new LocalQueue();
		subman->subscribe(*localQueue, queue);
		this->isConnected = true;
	} catch (const std::exception& error) {
		this->isConnected = false;
		cleanup();
		throw;
	}
	return this->isConnected;
}

CEdexNotification * get_notification_instance(char * address) {
	EdexNotification * test = new EdexNotification(address);
	return (CEdexNotification *) test;
}

void get_notification(CEdexNotification * cedex) {
	EdexNotification * edex = (EdexNotification *) cedex;
	edex->listen();
}

void set_timeout(CEdexNotification * cedex, int length) {
	EdexNotification * edex = (EdexNotification *) cedex;
	edex->setListenTimeout(length);
}

void clear_timeout(CEdexNotification * cedex) {
	EdexNotification * edex = (EdexNotification *) cedex;
	edex->clearListenTimeout();
}

int has_messages(CEdexNotification * cedex) {
	EdexNotification * edex = (EdexNotification *) cedex;
	return edex->hasMessages() ? 1 : 0;
}

int get_number_messages(CEdexNotification * cedex) {
	EdexNotification * edex = (EdexNotification *) cedex;
	return edex->getMessageCount();
}

const char * get_datauri(CEdexNotification * cedex) {
	EdexNotification * edex = (EdexNotification *) cedex;
	return edex->getDataUri().c_str();
}

void delete_notification_instance(CEdexNotification * cedex) {
	EdexNotification * edex = (EdexNotification *) cedex;
	delete edex;
}

