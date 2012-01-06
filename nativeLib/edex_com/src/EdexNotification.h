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
 * Notification capability header file to retrieve DataURIs from EDEX
 * notification.
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

#ifndef EDEX_NOTIFICATION_H_
#define EDEX_NOTIFICATION_H_

typedef void CEdexNotification;

#ifdef __cplusplus

#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>
#include <qpid/client/Message.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/SubscriptionManager.h>
#include <qpid/sys/Time.h>
#include <transport/TBufferTransports.h>
#include <sys/timeb.h>
#include "Notification_types.h"
#include "NotificationProtocol.h"

using namespace qpid::client;
using namespace qpid::framing;
using namespace std;
using apache::thrift::transport::TMemoryBuffer;
using boost::shared_ptr;

class EdexNotification {
private:

	Connection connection;
	Session session;
	bool useTopic;
	bool sessionTransacted;
	bool isConnected;
	int listSize;
	DataURINotificationMessage * mess;
	SubscriptionManager * subman;
	vector<string>::iterator myStringIterator;
	std::string brokerURI;
	std::string queue;
	LocalQueue * localQueue;
	bool timeout;
	int timeoutLength;

public:

	EdexNotification(const std::string& brokerURI);
	~EdexNotification();
	void listen();
	void setListenTimeout(int length);
	void clearListenTimeout();
	bool hasMessages();
	int getMessageCount();
	string getDataUri();

private:
	void cleanup();
	bool connect();
};

extern "C" {
#endif

// The following function calls are to be used on the C side

/**
 * Creates an instance of the notification object and passes that pointer
 * back to the C application.  This pointer will be required in any
 * subsequent function calls.
 */
extern CEdexNotification * get_notification_instance(char * address);

/**
 * A blocking function call that will listen for a message to be sent from
 * EDEX.  See the set_timeout and clear_timeout functions to set the timeout
 * length for this function.  Additionally, this call will reset the iterator
 * and clear the messages from the previous call.
 */
extern void get_notification(CEdexNotification * cedex);

/**
 * Allows a program to set a timeout for the get_notification function.  This
 * should be in milliseconds.  Any subsequent calls to get_notification could
 * potential timeout.  Call clear_timeout to disable.
 */
extern void set_timeout(CEdexNotification * cedex, int length);

/**
 * Disables timeouts when calling get_notification.
 */
extern void clear_timeout(CEdexNotification * cedex);

/**
 * Returns the total number of messages for the present datauri notification.
 */
extern int get_number_messages(CEdexNotification * cedex);

/**
 * An interator that can be used to retrieve the individual datauris.  It will
 * return 1 if there are messages that can be pulled off the list.  0 is
 * returned if a) no messages were received or b) no more messages can be
 * retrieved from the list.
 */
extern int has_messages(CEdexNotification * cedex);

/**
 * Returns the current datauri and increments the iterator by 1.
 */
extern const char * get_datauri(CEdexNotification * cedex);

/**
 * Must be called to delete the instance of the notification object.
 */
extern void delete_notification_instance(CEdexNotification * cedex);

#ifdef __cplusplus
}
#endif

#endif /* EDEX_NOTIFICATION_H_ */
