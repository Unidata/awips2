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
 * Point data request capability header file to retrieve point data
 * from EDEX Thrift service.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 01/18/10     3747        brockwoo    Initial Creation
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1
 */

#ifndef POINTDATAREQUEST_H_
#define POINTDATAREQUEST_H_

typedef void CPointDataRequest;

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
#include "thrift/transport/THttpClient.h"
#include "PointData_types.h"
#include <iostream>
#include <stdint.h>

using apache::thrift::transport::THttpClient;
using namespace apache::thrift::protocol;
using boost::shared_ptr;
using namespace std;

class PointDataRequest {
private:
	string hostname;
	int port;
	string plugin;
	vector<string> parameters;
	string datatime;
	com_raytheon_uf_common_pointdata_PointDataThriftContainer pdtc;
	map<string, int> lookup;
	map<string, int> position;

public:
	PointDataRequest(const string& host, int port = 9581);
	~PointDataRequest();
	void addParameter(const string& param);
	void clearParameters();
	void setPlugin(const string& plugin);
	string getPlugin();
	void setDataTime(const string& dt);
	void clearDataTime();
	//void setStations(vector<string>);
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
 * Creates an instance of the point data request object and passes that pointer
 * back to the C application.  This pointer will be required in any
 * subsequent function calls.  plugin is the plugin name desired, host is the
 * name of the machine running edex, port should be the port of the thrift service
 * on EDEX (typically 9581).
 */
extern CPointDataRequest * get_pdr_instance(char * host, int port);

/**
 * Must be called to delete the instance of the point data request object.
 */
extern void delete_pdr_instance(CPointDataRequest * cpdr);

/**
 * Adds a parameter to the list of those to be queried for.
 */
extern void pdr_add_parameter(CPointDataRequest * cpdr, char * parameter);

/**
 * Clears out the list of parameters.
 */
extern void pdr_clear_parameters(CPointDataRequest * cpdr);

/**
 * Sets the plugin to request data against.
 */
extern void pdr_set_plugin(CPointDataRequest * cpdr, char * plugin);

/**
 * Returns the plugin being queried against.
 */
extern const char * pdr_get_plugin(CPointDataRequest * cpdr);

/**
 * When looking for an observation at a specific time, use this to set the
 * datatime.
 */
extern void pdr_set_datatime(CPointDataRequest * cpdr, char * dt);

/**
 * Clears the set datatime.
 */
extern void pdr_clear_datatime(CPointDataRequest * cpdr);

/**
 * Performs the request for point data to EDEX for the specific station.
 * Returns the number of observations that were returned.
 */
extern int pdr_request_data(CPointDataRequest * cpdr, char * station);

/**
 * Returns the unit of the requested parameter.
 */
extern const char * pdr_get_parameter_unit(CPointDataRequest * cpdr,
		char * parameter);

/**
 * Returns the type of the requested parameter defined in PointDataRequest.h.
 * 1 -- STRING
 * 2 -- INT
 * 3 -- LONG
 * 4 -- FLOAT
 * 5 -- MISSING
 */
extern int pdr_get_parameter_type(CPointDataRequest * cpdr, char * parameter);

/**
 * If the parameter is of type STRING, use this function to get the value.  Offset is
 * the observation desired (0 is first ob in the list, etc).  Returns "MISSING" if
 * parameter is not found.
 */
extern const char * pdr_get_string_data(CPointDataRequest * cpdr,
		char * parameter, int offset);

/**
 * If the parameter is of type INT, use this function to get the value.  Offset is
 * the observation desired (0 is first ob in the list, etc).  Returns INTMISSING if
 * parameter is not found.
 */
extern int32_t pdr_get_int_data(CPointDataRequest * cpdr, char * parameter,
		int offset);

/**
 * If the parameter is of type LONG, use this function to get the value.  Offset is
 * the observation desired (0 is first ob in the list, etc).  Returns LONGMISSING if
 * parameter is not found.
 */
extern int64_t pdr_get_long_data(CPointDataRequest * cpdr, char * parameter,
		int offset);

/**
 * If the parameter is of type FLOAT, use this function to get the value.  Offset is
 * the observation desired (0 is first ob in the list, etc).  Returns FLOATMISSING if
 * parameter is not found.
 */
extern float pdr_get_float_data(CPointDataRequest * cpdr, char * parameter,
		int offset);

#ifdef __cplusplus
}
#endif

#endif /* POINTDATAREQUEST_H_ */
