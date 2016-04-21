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
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1
 */

#include "NotificationProtocol.h"

uint32_t NotificationProtocol::readFieldBegin(std::string& name,
		TType& fieldType, int16_t& fieldId) {
	uint32_t result = 0;
	int8_t type;

	result += readByte(type);
	fieldType = (TType) type;
	if (fieldType == apache::thrift::protocol::T_STOP) {
		fieldId = 0;
		return result;
	}
	result += readString(name);
	result += readI16(fieldId);

	return result;
}

uint32_t NotificationProtocol::readStructBegin(std::string& name) {
	uint32_t result = 0;
	result += readString(name);
	return result;
}

uint32_t NotificationProtocol::writeFieldBegin(const char* name,
		const TType fieldType, const int16_t fieldId) {
	uint32_t result = 0;
	result += writeByte(fieldType);
	result += writeString(std::string(name));
	result += writeI16(fieldId);
	return result;
}

uint32_t NotificationProtocol::writeStructBegin(const char* name) {
	uint32_t result = 0;
	result += writeString(std::string(name));
	return 0;
}
