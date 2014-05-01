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
 * 08/13/13      2257       bkowal      Update for qpid 0.18.
 * 08/15/13      2169       bkowal      Qpid messages are now decompressed
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <uuid/uuid.h>
#include <qpid/messaging/Connection.h>
#include <qpid/messaging/Duration.h>
#include <qpid/Url.h>
#include "EdexNotification.h"

using qpid::Url;

EdexNotification::EdexNotification(const string & brokerURI) :
	duration(Duration(1000 * 120)) {
	this->sessionTransacted = false;
	this->brokerURI = brokerURI;
	this->isConnected = false;
	this->listSize = 0;
	this->mess = new com_raytheon_uf_common_dataplugin_message_DataURINotificationMessage();
	this->timeout = false;
	this->timeoutLength = 999999;
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
			result = this->receiver.fetch(message, this->duration);
		} catch(...) {
			cleanup();
			throw;
		}
		if (result) {
			this->session.acknowledge(message);
			std::string output = message.getContent();

			std::stringstream compressedData(output);
			std::stringstream decompressedData;

			/* decompress the data. */
			try
			{
				boost::iostreams::filtering_streambuf<boost::iostreams::input> in;
				in.push(boost::iostreams::gzip_decompressor());
				in.push(compressedData);
				boost::iostreams::copy(in, decompressedData);
			}
			catch (const boost::iostreams::gzip_error& error)
			{
				std::cout << "ERROR: Failed to decompress gzip data - "
					<< error.error() << std::endl;
				this->cleanup();
				throw;
			}

			output = decompressedData.str();
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

	// attempt to close the receiver
	if (this->receiver != 0)
	{
		try
		{
			this->receiver.close();
			this->receiver = 0;
		}
		catch (const std::exception& error)
		{
			std::cout << "WARNING: Failed to close the receiver -"
				<< error.what() << std::endl;
		}
	}

	// attempt to close the session
	if (this->session != 0)
	{
		try
		{
			this->session.close();
			this->session = 0;
		}
		catch (const std::exception& error)
		{
			std::cout << "WARNING: Failed to close the session -"
				<< error.what() << std::endl;
		}
	}

	// attempt to close the connection
	if (this->connection != 0)
	{
		try
		{
			this->connection.close();
			this->connection = 0;
		}
		catch (const std::exception& error)
		{
			std::cout << "WARNING: Failed to close the connection -"
				<< error.what() << std::endl;
		}
	}

	this->isConnected = false;
}

bool EdexNotification::connect() {
	if (this->isConnected) {
		return this->isConnected;
	}
	try {
		// initialize
		this->connection = 0;
		this->session = 0;
		this->receiver = 0;

		char uuidBuff[37];
		uuid_t uuidGenerated;
		uuid_generate_random(uuidGenerated);
		uuid_unparse(uuidGenerated, uuidBuff);

		std::string connectionOptions = "{sasl-mechanism:PLAIN,"
			"username:guest,password:guest}";

		this->connection = Connection(this->brokerURI, connectionOptions);
		this->connection.open();
		queue = "_edex.alert-edex_com@amq.topic_";
		queue += std::string(uuidBuff);

		std::stringstream addressBuilder;
		addressBuilder << queue;
		addressBuilder << "; {create:always,delete:always,node:{type:queue,";
		addressBuilder << "x-bindings:[{exchange:amq.topic,queue:";
		addressBuilder << queue;
		addressBuilder << ",key:edex.alerts}]}}";
		const std::string address = addressBuilder.str();

		this->session = this->connection.createSession();
		this->receiver = this->session.createReceiver(address);

		this->isConnected = true;
	} catch (const std::exception& error) {
		this->cleanup();
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

