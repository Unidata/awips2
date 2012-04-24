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

#include "PointDataServerRequest.h"

PointDataServerRequest::PointDataServerRequest(const string& host, int port) {
	this->hostname = host;
	this->port = port;
}

PointDataServerRequest::~PointDataServerRequest() {
}

void PointDataServerRequest::addConstraintParameter(const string& param,
		const string& value, const string& type) {
	com_raytheon_uf_common_dataquery_requests_RequestConstraint constraint;
	com_raytheon_uf_common_dataquery_requests_ConstraintType cType;

	//change the operand string to upper case
	std::string sType = type;
	std::transform(sType.begin(), sType.end(), sType.begin(), ::toupper);
	if (sType.empty()) {
		cType.operand = "EQUALS";
	} else {
		cType.operand = sType;
	}

	constraint.constraintValue = value;
	constraint.constraintType = cType;

	rcMap.insert(make_pair(param, constraint));
}

void PointDataServerRequest::addRequestedParameter(const string& param) {
	if (!param.empty()) {
		//check if there exists duplicate parameter(s)
		list<string>::const_iterator pos;
		bool found = false;
		for (pos = requestedParameters.begin(); pos
				!= requestedParameters.end(); ++pos) {
			if ((*pos).compare(0, param.length(), param) == 0) {
				//the param exists in the list
				found = true;
				break;
			}
		}

		if (found == false) {
			requestedParameters.push_back(param);
		}
	}
}

void PointDataServerRequest::clearParameters() {
	requestedParameters.clear();
	rcMap.clear();
}

int PointDataServerRequest::getParameterType(const string& parameter) {
	if (lookup.find(parameter) != lookup.end()) {
		return lookup[parameter];
	}
	return MISSING;
}

string PointDataServerRequest::getParameterUnit(const string& parameter) {
	if (lookup.find(parameter) != lookup.end()) {
		int pos = position[parameter];
		switch (lookup[parameter]) {
		case STRING:
			return pdtc.stringData.at(pos).description.unit;
			break;
		case INT:
			return pdtc.intData.at(pos).description.unit;
			break;
		case LONG:
			return pdtc.longData.at(pos).description.unit;
			break;
		case FLOAT:
			return pdtc.floatData.at(pos).description.unit;
			break;
		}
	}
	return "MISSING";
}

string PointDataServerRequest::getStringData(const string& parameter,
		int offset) {
	if (position.find(parameter) != position.end()) {
		if (lookup[parameter] != STRING) {
			return "MISSING";
		}
		int pos = position[parameter];
		return pdtc.stringData.at(pos).stringData.at(offset);
	}
	return "MISSING";
}

int32_t PointDataServerRequest::getIntData(const string& parameter, int offset) {
	if (position.find(parameter) != position.end()) {
		if (lookup[parameter] != INT) {
			return INTMISSING;
		}
		int pos = position[parameter];
		return pdtc.intData.at(pos).intData.at(offset);
	}
	return INTMISSING;
}

int64_t PointDataServerRequest::getLongData(const string& parameter, int offset) {
	if (position.find(parameter) != position.end()) {
		if (lookup[parameter] != LONG) {
			return LONGMISSING;
		}
		int pos = position[parameter];
		return pdtc.longData.at(pos).longData.at(offset);
	}
	return LONGMISSING;
}

float PointDataServerRequest::getFloatData(const string& parameter, int offset) {
	if (position.find(parameter) != position.end()) {
		if (lookup[parameter] != FLOAT) {
			return FLOATMISSING;
		}
		int pos = position[parameter];
		return pdtc.floatData.at(pos).trueFloatData.at(offset);
	}
	return FLOATMISSING;
}

int PointDataServerRequest::requestData(const string& station) {
	this->lookup.clear();
	this->position.clear();
	THttpClient *client = new THttpClient(hostname, port, "/services/thrift");
	client->open();
	shared_ptr<THttpClient> httpBuffer = shared_ptr<THttpClient> (client);
	NotificationProtocol * np = new NotificationProtocol(httpBuffer);
	np->writeMessageBegin("dynamicSerialize", T_CALL, 0);

	com_raytheon_uf_common_pointdata_PointDataServerRequest m;
	com_raytheon_uf_common_dataquery_requests_RequestConstraint consStation,
			consMode, consParam;
	com_raytheon_uf_common_dataquery_requests_ConstraintType cType;

	//add constraint parameter
	consStation.constraintValue = station;
	cType.operand = "EQUALS";
	consStation.constraintType = cType;
	m.rcMap.insert(make_pair("location.stationId", consStation));

	string params = "";
	if (requestedParameters.empty()) {
		cout << "ERROR: there is no requested parameter....EXIT" << endl;
		return -1;
	} else {
		list<string>::const_iterator pos;
		for (pos = requestedParameters.begin(); pos
				!= requestedParameters.end(); ++pos) {
			params += *pos;
			params += ",";
		}

		//erase the extra comma
		params.erase(params.length() - 1);
	}

	consParam.constraintValue = params;
	cType.operand = "EQUALS";
	consParam.constraintType = cType;
	m.rcMap.insert(make_pair("requestedParameters", consParam));

	//request for thrift serialization point data container
	consMode.constraintValue = "convertToThrift";
	cType.operand = "EQUALS";
	consMode.constraintType = cType;
	m.rcMap.insert(make_pair("mode", consMode));

	//copy the constraint parameters
	std::map<std::string,
			com_raytheon_uf_common_dataquery_requests_RequestConstraint>::iterator
			pos;
	for (pos = rcMap.begin(); pos != rcMap.end(); ++pos) {
		m.rcMap.insert(make_pair(pos->first, pos->second));
	}

	m.write(np);
	np->writeMessageEnd();
	client->flush();
	int32_t info = 0;
	TMessageType type;
	string messageName;
	np->readMessageBegin(messageName, type, info);
	pdtc.read(np);
	delete np;
	client->close();

	for (int i = 0; i < (int) pdtc.stringData.size(); i++) {
		lookup[pdtc.stringData.at(i).description.parameterName] = STRING;
		position[pdtc.stringData.at(i).description.parameterName] = i;
	}
	for (int i = 0; i < (int) pdtc.intData.size(); i++) {
		lookup[pdtc.intData.at(i).description.parameterName] = INT;
		position[pdtc.intData.at(i).description.parameterName] = i;
	}
	for (int i = 0; i < (int) pdtc.longData.size(); i++) {

		lookup[pdtc.longData.at(i).description.parameterName] = LONG;
		position[pdtc.longData.at(i).description.parameterName] = i;
	}

	for (int i = 0; i < (int) pdtc.floatData.size(); i++) {

		lookup[pdtc.floatData.at(i).description.parameterName] = FLOAT;
		position[pdtc.floatData.at(i).description.parameterName] = i;
	}

	return pdtc.size;
}

CPointDataServerRequest * get_pdsr_instance(char * host, int port) {
	PointDataServerRequest * pdsr = new PointDataServerRequest(host, port);
	return (CPointDataServerRequest *) pdsr;
}

void delete_pdsr_instance(CPointDataServerRequest * cpdsr) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	delete pdsr;
}

void pdsr_add_constraint_parameter(CPointDataServerRequest * cpdsr,
		char * parameter, char * constraintValue, char * constraintType) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	pdsr->addConstraintParameter(parameter, constraintValue, constraintType);
}

void pdsr_add_requested_parameter(CPointDataServerRequest * cpdsr,
		char * parameter) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	pdsr->addRequestedParameter(parameter);
}

void pdsr_clear_parameters(CPointDataServerRequest * cpdsr) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	pdsr->clearParameters();
}

int pdsr_request_data(CPointDataServerRequest * cpdsr, char * station) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	return pdsr->requestData(station);
}

const char * pdsr_get_parameter_unit(CPointDataServerRequest * cpdsr,
		char * parameter) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	return pdsr->getParameterUnit(parameter).c_str();
}

int pdsr_get_parameter_type(CPointDataServerRequest * cpdsr, char * parameter) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	return pdsr->getParameterType(parameter);
}

const char * pdsr_get_string_data(CPointDataServerRequest * cpdsr,
		char * parameter, int offset) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	return pdsr->getStringData(parameter, offset).c_str();
}

int32_t pdsr_get_int_data(CPointDataServerRequest * cpdsr, char * parameter,
		int offset) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	return pdsr->getIntData(parameter, offset);
}

int64_t pdsr_get_long_data(CPointDataServerRequest * cpdsr, char * parameter,
		int offset) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	return pdsr->getLongData(parameter, offset);
}

float pdsr_get_float_data(CPointDataServerRequest * cpdsr, char * parameter,
		int offset) {
	PointDataServerRequest * pdsr = (PointDataServerRequest *) cpdsr;
	return pdsr->getFloatData(parameter, offset);
}

