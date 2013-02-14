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
 * Example application which demonstrates how to call the C++ code for
 * EDEX notification from C.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/9/09       3375       brockwoo    Initial Creation
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "EdexNotification.h"

int main(int argc, char *argv[]) {
	if (argc != 2) {
		printf("Usage: %s <URL amqp:tcp:HOST:PORT >\n", argv[0]);
		return 1;
	} else {
		printf("Connecting to %s\n", argv[1]);
	}
	CEdexNotification * cedex = NULL;
	cedex = get_notification_instance(argv[1]);
	int looper = 0;
	while (looper < 20) {
		get_notification(cedex); // Will block until a notification is sent
		int counter = 0;
		while (has_messages(cedex)) { // A single message will contain 0-n datauris
			const char * uri = get_datauri(cedex);
			if(strstr(uri, "radar") != NULL) {
				printf("%s\n", uri);
			}
			counter++;
		}
		printf("total number of messages that trip:  %d from an expected %d\n", counter, get_number_messages(cedex));
		if(counter > 0) {
			looper++;
		}
	}
	delete_notification_instance(cedex);
	return EXIT_SUCCESS;
}
