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
 * Support for point data request capability to retrieve point data and metadata
 * from EDEX Thrift service.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/08/11     9696        gzhou       Initial Creation
 *
 * </pre>
 *
 * @author gzhou
 * @version 1
 */


#ifndef POINTDATASERVERREQUEST_H_
#define POINTDATASERVERREQUEST_H_

typedef void CPointDataServerRequest;

#define STRING 1
#define INT 2
#define LONG 3
#define FLOAT 4
#define MISSING 5

#define INTMISSING -9999999
#define LONGMISSING -9999999
#define FLOATMISSING -9999999.0f

#ifdef __cplusplus

#include "Notification_types.h"
#include "NotificationProtocol.h"
#include "transport/THttpClient.h"
#include "PointDataServer_types.h"
#include <iostream>
#include <stdint.h>
#include <string>

using apache::thrift::transport::THttpClient;
using namespace apache::thrift::protocol;
using boost::shared_ptr;
using namespace std;

class PointDataServerRequest {
private:
	string hostname;
	int port;
	std::list<string> requestedParameters;
	com_raytheon_uf_common_pointdata_PointDataThriftContainer pdtc;
	map<string, int> lookup;
	map<string, int> position;
	map<string, com_raytheon_uf_common_dataquery_requests_RequestConstraint>
			rcMap;

public:
	PointDataServerRequest(const string& host, int port = 9581);
	~PointDataServerRequest();
	void addConstraintParameter(const string& param,
			const string& constraintValue, const string& constraintType);
	void addRequestedParameter(const string& param);
	void clearParameters();
	int requestData(const string& station);
	string getParameterUnit(const string& parameter);
	int getParameterType(const string& parameter);
	string getStringData(const string& parameter, int offset);
	int32_t getIntData(const string& parameter, int offset);
	int64_t getLongData(const string& parameter, int offset);
	float getFloatData(const string& parameter, int offset);
};

extern "C" {
#endif

// The following function calls are to be used on the C side

/**
 * Creates an instance of the point data server request object and passes that pointer
 * back to the C application.  This pointer will be required in any
 * subsequent function calls.  plugin is the plugin name desired, host is the
 * name of the machine running edex, port should be the port of the thrift service
 * on EDEX (typically 9581).
 */
extern CPointDataServerRequest * get_pdsr_instance(char * host, int port);

/**
 * Must be called to delete the instance of the point data server request object.
 */
extern void delete_pdsr_instance(CPointDataServerRequest * cpdsr);

/**
 * Adds a constraint parameter to constraint the queried.
 */
extern void pdsr_add_constraint_parameter(CPointDataServerRequest * cpdsr,
		char * parameter, char * constraintValue, char * constraintType);

/**
 * Adds a requested parameter to the list of those to be queried for.
 */
extern void pdsr_add_requested_parameter(CPointDataServerRequest * cpdsr,
		char * parameter);

/**
 * Clears out the list of parameters.
 */
extern void pdsr_clear_parameters(CPointDataServerRequest * cpdsr);

/**
 * Performs the request for point data to EDEX for the specific station.
 * Returns the number of observations that were returned.
 */
extern int pdsr_request_data(CPointDataServerRequest * cpdsr, char * station);

/**
 * Returns the unit of the requested parameter.
 */
extern const char * pdsr_get_parameter_unit(CPointDataServerRequest * cpdsr,
		char * parameter);

/**
 * Returns the type of the requested parameter defined in PointDataServerRequest.h.
 * 1 -- STRING
 * 2 -- INT
 * 3 -- LONG
 * 4 -- FLOAT
 * 5 -- MISSING
 */
extern int pdsr_get_parameter_type(CPointDataServerRequest * cpdsr,
		char * parameter);

/**
 * If the parameter is of type STRING, use this function to get the value.  Offset is
 * the observation desired (0 is first ob in the list, etc).  Returns "MISSING" if
 * parameter is not found.
 */
extern const char * pdsr_get_string_data(CPointDataServerRequest * cpdsr,
		char * parameter, int offset);

/**
 * If the parameter is of type INT, use this function to get the value.  Offset is
 * the observation desired (0 is first ob in the list, etc).  Returns INTMISSING if
 * parameter is not found.
 */
extern int32_t pdsr_get_int_data(CPointDataServerRequest * cpdsr,
		char * parameter, int offset);

/**
 * If the parameter is of type LONG, use this function to get the value.  Offset is
 * the observation desired (0 is first ob in the list, etc).  Returns LONGMISSING if
 * parameter is not found.
 */
extern int64_t pdsr_get_long_data(CPointDataServerRequest * cpdsr,
		char * parameter, int offset);

/**
 * If the parameter is of type FLOAT, use this function to get the value.  Offset is
 * the observation desired (0 is first ob in the list, etc).  Returns FLOATMISSING if
 * parameter is not found.
 */
extern float pdsr_get_float_data(CPointDataServerRequest * cpdsr,
		char * parameter, int offset);

#ifdef __cplusplus
}
#endif

#endif /* POINTDATASERVERREQUEST_H_ */
