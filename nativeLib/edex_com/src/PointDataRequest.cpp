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
 * Point data request capability file to retrieve point data
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

#include "PointDataRequest.h"

PointDataRequest::PointDataRequest(const string& host, int port) {
	this->hostname = host;
	this->port = port;
}

PointDataRequest::~PointDataRequest() {
}

void PointDataRequest::addParameter(const string& param) {
	parameters.push_back(param);
}

void PointDataRequest::clearParameters() {
	parameters.clear();
}

void PointDataRequest::setDataTime(const string& dt) {
	datatime = dt;
}

void PointDataRequest::clearDataTime() {
	datatime.clear();
}

void PointDataRequest::setPlugin(const string & plugin) {
	this->plugin = plugin;
}

string PointDataRequest::getPlugin() {
	return this->plugin;
}

int PointDataRequest::getParameterType(const string& parameter) {
	if (lookup.find(parameter) != lookup.end()) {
		return lookup[parameter];
	}
	return MISSING;
}

string PointDataRequest::getParameterUnit(const string& parameter) {
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

string PointDataRequest::getStringData(const string& parameter, int offset) {
	if (position.find(parameter) != position.end()) {
		if (lookup[parameter] != STRING) {
			return "MISSING";
		}
		int pos = position[parameter];
		return pdtc.stringData.at(pos).stringData.at(offset);
	}
	return "MISSING";
}

int32_t PointDataRequest::getIntData(const string& parameter, int offset) {
	if (position.find(parameter) != position.end()) {
		if (lookup[parameter] != INT) {
			return INTMISSING;
		}
		int pos = position[parameter];
		return pdtc.intData.at(pos).intData.at(offset);
	}
	return INTMISSING;
}

int64_t PointDataRequest::getLongData(const string& parameter, int offset) {
	if (position.find(parameter) != position.end()) {
		if (lookup[parameter] != LONG) {
			return LONGMISSING;
		}
		int pos = position[parameter];
		return pdtc.longData.at(pos).longData.at(offset);
	}
	return LONGMISSING;
}

float PointDataRequest::getFloatData(const string& parameter, int offset) {
	if (position.find(parameter) != position.end()) {
		if (lookup[parameter] != FLOAT) {
			return FLOATMISSING;
		}
		int pos = position[parameter];
		return pdtc.floatData.at(pos).trueFloatData.at(offset);
	}
	return FLOATMISSING;
}

int PointDataRequest::requestData(const string& station) {
	this->lookup.clear();
	this->position.clear();
	THttpClient *client = new THttpClient(hostname, port, "/services/thrift");
	client->open();
	shared_ptr<THttpClient> httpBuffer = shared_ptr<THttpClient> (client);
	NotificationProtocol * np = new NotificationProtocol(httpBuffer);
	np->writeMessageBegin("dynamicSerialize", T_CALL, 0);
	com_raytheon_uf_common_pointdata_PointDataRequestMessageConstraint mc;
	mc.parameter = "location.stationId";
	mc.value = station;
	mc.constraintType = 7;
	com_raytheon_uf_common_pointdata_PointDataRequestMessage m;

	if (datatime.length() > 0) {
		com_raytheon_uf_common_pointdata_PointDataRequestMessageConstraint dt;
		dt.parameter = "dataTime";
		dt.value = datatime;
		dt.constraintType = 0;
		m.constraints.push_back(dt);
	}
	m.allLevels = true;
	m.pluginName = plugin;
	m.parameters = parameters;
	m.constraints.push_back(mc);

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

CPointDataRequest * get_pdr_instance(char * host, int port) {
	PointDataRequest * pdr = new PointDataRequest(host, port);
	return (CPointDataRequest *) pdr;
}

void delete_pdr_instance(CPointDataRequest * cpdr) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	delete pdr;
}

void pdr_set_plugin(CPointDataRequest * cpdr, char * plugin){
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	pdr->setPlugin(plugin);
}

const char * pdr_get_plugin(CPointDataRequest * cpdr){
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	return pdr->getPlugin().c_str();
}

void pdr_add_parameter(CPointDataRequest * cpdr, char * parameter) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	pdr->addParameter(parameter);
}

void pdr_clear_parameters(CPointDataRequest * cpdr) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	pdr->clearParameters();
}

void pdr_set_datatime(CPointDataRequest * cpdr, char * dt) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	pdr->setDataTime(dt);
}

void pdr_clear_datatime(CPointDataRequest * cpdr) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	pdr->clearDataTime();
}

int pdr_request_data(CPointDataRequest * cpdr, char * station) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	return pdr->requestData(station);
}

const char * pdr_get_parameter_unit(CPointDataRequest * cpdr, char * parameter) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	return pdr->getParameterUnit(parameter).c_str();
}

int pdr_get_parameter_type(CPointDataRequest * cpdr, char * parameter) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	return pdr->getParameterType(parameter);
}

const char * pdr_get_string_data(CPointDataRequest * cpdr, char * parameter,
		int offset) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	return pdr->getStringData(parameter, offset).c_str();
}

int32_t pdr_get_int_data(CPointDataRequest * cpdr, char * parameter, int offset) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	return pdr->getIntData(parameter, offset);
}

int64_t pdr_get_long_data(CPointDataRequest * cpdr, char * parameter,
		int offset) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	return pdr->getLongData(parameter, offset);
}

float pdr_get_float_data(CPointDataRequest * cpdr, char * parameter, int offset) {
	PointDataRequest * pdr = (PointDataRequest *) cpdr;
	return pdr->getFloatData(parameter, offset);
}

