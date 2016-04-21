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
 * 11/30/10     4463        bfarmer     Initial Creation
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1
 */

#ifndef GRIDDATAREQUEST_H_
#define GRIDDATAREQUEST_H_

typedef void CGridDataRequest;
typedef void CGribThriftContainer;
typedef void CGribThriftRecord;

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
#include "GridDataRequestMessage_types.h"
#include "GribThriftContainer_types.h"
#include <iostream>
#include <stdint.h>

using apache::thrift::transport::THttpClient;
using namespace apache::thrift::protocol;
using boost::shared_ptr;
using namespace std;

class GridDataRequest {
private:
	com_raytheon_uf_common_dataplugin_grib_request_GridDataRequestMessage gdrm;
	string hostname;
	int port;
	com_raytheon_uf_common_dataplugin_grib_GribThriftContainer retval;

public:
	GridDataRequest(const string& host, int port = 9581);
	~GridDataRequest();
	void setForecastTime(int fct);
	void setLevelOne(double lo);
	void setLevelTwo(double lt);
	void setLevelType(string lType);
	void setModelName(string mName);
	void setParameterAbbreviation(string pAbbr);
	void setPert(int pert);
	void setStartTime(int sTime);
	void setVersion(int version);
	int getforecastTime();
	double getLevelOne();
	double getLevelTwo();
	string getLevelType();
	string getModelName();
	string getParameterAbbreviation();
	int getPert();
	int getStartTime();
	int getVersion();
	com_raytheon_uf_common_dataplugin_grib_GribThriftContainer * getReturns();

	// Request Data
	int requestData();
	int testFromFile();
};

extern "C" {
#endif

// The following function calls are to be used on the C side

/**
 * Creates an instance of the grid data request object and passes that pointer
 * back to the C application.  This pointer will be required in any
 * subsequent function calls.  host is the
 * name of the machine running edex, port should be the port of the thrift service
 * on EDEX (typically 9581).  Will delete an existing pointer in order to make
 * the new object.
 */
extern CGridDataRequest * get_gdr_instance(char * host, int port);

/**
 * Must be called to delete the instance of the grid data request object.
 */
extern void delete_gdr_instance(CGridDataRequest * gdr);

extern void gdr_set_forecasttime(CGridDataRequest * gdr, int fct);
extern void gdr_set_levelone(CGridDataRequest * gdr, double lo);
extern void gdr_set_leveltwo(CGridDataRequest * gdr, double lt);
extern void gdr_set_leveltype(CGridDataRequest * gdr, char* lType);
extern void gdr_set_modelname(CGridDataRequest * gdr, char*  mName);
extern void gdr_set_parameterabbreviation(CGridDataRequest * gdr, char* pAbbr);
extern void gdr_set_pert(CGridDataRequest * gdr, int pert);
extern void gdr_set_starttime(CGridDataRequest * gdr, int sTime);
extern void gdr_set_version(CGridDataRequest * gdr, int version);
extern int gdr_get_forecasttime(CGridDataRequest * gdr);
extern double gdr_get_levelone(CGridDataRequest * gdr);
extern double gdr_get_leveltwo(CGridDataRequest * gdr);
extern const char* gdr_get_leveltype(CGridDataRequest * gdr);
extern const char* gdr_get_modelname(CGridDataRequest * gdr);
extern const char* gdr_get_parameterabbreviation(CGridDataRequest * gdr);
extern int gdr_get_pert(CGridDataRequest * gdr);
extern int gdr_get_starttime(CGridDataRequest * gdr);
extern int gdr_get_version(CGridDataRequest * gdr);

// Request Data
extern int gdr_request_data(CGridDataRequest * gdr);
extern CGribThriftContainer * gdr_get_results(CGridDataRequest * cgdr);

extern int gtc_get_num_records(CGribThriftContainer * cgtc);
extern CGribThriftRecord * gtc_get_record_at_index(CGribThriftContainer * cgtc, int index);

extern int gtr_is_hybrid_grid(CGribThriftRecord * cgtr);
extern int gtr_is_local_section(CGribThriftRecord * cgtr);
extern int gtr_is_thinned_grid(CGribThriftRecord * cgtr);

extern int gtr_get_localsectiondata_max(CGribThriftRecord * cgtr);
extern int gtr_get_localsectiondata_at_index(CGribThriftRecord * cgtr, int index);

extern int gtr_get_thinnedgriddata_max(CGribThriftRecord * cgtr);
extern int gtr_get_thinnedgriddata_at_index(CGribThriftRecord * cgtr, int index);

extern int gtr_get_data_max(CGribThriftRecord * cgtr);
extern float gtr_get_data_at_index(CGribThriftRecord * cgtr, int index);

extern int gtr_get_hybridgriddata_max(CGribThriftRecord * cgtr);
extern float gtr_get_hybridgriddata_at_index(CGribThriftRecord * cgtr, int index);

#ifdef __cplusplus
}
#endif


#endif /* GRIDDATAREQUEST_H_ */
