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
 * Extended thrift protocol to handle messages from edex.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/9/09       3375       brockwoo    Initial Creation
 * 07/29/13      2215       bkowal      Update for thrift 0.9.0
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1
 */

#ifndef NOTIFICATIONPROTOCOL_H_
#define NOTIFICATIONPROTOCOL_H_

#include "thrift/TReflectionLocal.h"
#include "thrift/protocol/TProtocol.h"
#include "thrift/protocol/TBinaryProtocol.h"
#include "thrift/protocol/TVirtualProtocol.h"
#include "NotificationProtocol.h"

using apache::thrift::protocol::TType;
using apache::thrift::protocol::TMessageType;
using apache::thrift::protocol::TVirtualProtocol;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::transport::TTransport;

class NotificationProtocol : public TVirtualProtocol<NotificationProtocol, TBinaryProtocol> {
public:
	NotificationProtocol(boost::shared_ptr<TTransport> trans) : TVirtualProtocol<NotificationProtocol, TBinaryProtocol>(trans) {}

	uint32_t readFieldBegin(std::string& name, TType& fieldType,
			int16_t& fieldId);

	uint32_t readStructBegin(std::string& name);

	uint32_t writeFieldBegin(const char* name,
	                           const TType fieldType,
	                           const int16_t fieldId);

	uint32_t writeStructBegin(const char* name);
};

#endif /* NOTIFICATIONPROTOCOL_H_ */
