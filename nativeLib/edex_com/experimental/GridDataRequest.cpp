/*
 * GridDataRequest.cpp
 *
 *  Created on: Nov 30, 2010
 *      Author: bfarmer
 */

#include "GridDataRequest.h"
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include "EdexNotification.h"
#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>
#include <curlpp/Exception.hpp>


GridDataRequest::GridDataRequest(const string& host, int port) {
	this->hostname = host;
	this->port = port;
	gdrm.levelOne = -999999.0;
	gdrm.startTime = -999;
}

GridDataRequest::~GridDataRequest() {
}

void GridDataRequest::setForecastTime(int fct)
{
	gdrm.forecastTime = fct;
}
void GridDataRequest::setLevelOne(double lo)
{
	gdrm.levelOne = lo;
}
void GridDataRequest::setLevelTwo(double lt)
{
	gdrm.levelTwo = lt;
}
void GridDataRequest::setLevelType(string lType)
{
	gdrm.levelType = lType;
}
void GridDataRequest::setModelName(string mName)
{
	gdrm.modelName = mName;
}
void GridDataRequest::setParameterAbbreviation(string pAbbr)
{
	gdrm.parameterAbbreviation = pAbbr;
}
void GridDataRequest::setPert(int pert)
{
	gdrm.pert = pert;
}
void GridDataRequest::setStartTime(int sTime)
{
	gdrm.startTime = sTime;
}
void GridDataRequest::setVersion(int version)
{
	gdrm.version = version;
}
int GridDataRequest::getforecastTime()
{
	return gdrm.forecastTime;
}
double GridDataRequest::getLevelOne()
{
	return gdrm.levelOne;
}
double GridDataRequest::getLevelTwo()
{
	return gdrm.levelTwo;
}
string GridDataRequest::getLevelType()
{
	return gdrm.levelType;
}
string GridDataRequest::getModelName()
{
	return gdrm.modelName;
}
string GridDataRequest::getParameterAbbreviation()
{
	return gdrm.parameterAbbreviation;
}
int GridDataRequest::getPert()
{
	return gdrm.pert;
}
int GridDataRequest::getStartTime()
{
	return gdrm.startTime;
}
int GridDataRequest::getVersion()
{
	return gdrm.version;
}
com_raytheon_uf_common_dataplugin_grib_GribThriftContainer * GridDataRequest::getReturns()
{
	return &retval;
}

int GridDataRequest::requestData()
{
	// The HTTP connection handling in thrift is terrible, so we're
	// going to bypass it entirely and use cURL and cURLpp instead.
	// In order to do this, we tell thrift to write to a TMemoryBuffer
	// instead of a THttpClient.
	int32_t info = 0;
	TMessageType type;
	string messageName;
	TMemoryBuffer * buffer = new TMemoryBuffer();
	shared_ptr<TMemoryBuffer> ptrBuffer = shared_ptr<TMemoryBuffer> (
			buffer);
	NotificationProtocol * np = new NotificationProtocol(ptrBuffer);
	np->writeMessageBegin("dynamicSerialize", T_CALL, 0);
	gdrm.write(np);
	np->writeMessageEnd();

	//Then we can extract the buffer, as a string, from the TMemoryBuffer,
	// construct the URL we're sending to, and get a receiver stream ready.
	std::istringstream sendObject(buffer->getBufferAsString());
	int sendSize = sendObject.str().size();
	std::stringstream receiveObject;
	std::stringstream url;
	url << "http://" << hostname << ":" << port << "/services/thrift";

	//Turns out that these headers are not actually necessary for connecting to
	// awips, but I am keeping the comments here in case they are ever MADE necessary,
	// because it was a pain to look them up the first time, and I'd prefer
	// nobody ever have to do it again.
//	std::list< std::string > headers;
//	std::stringstream headermaker;
//	headermaker << "POST " << "/services/thrift" << " HTTP/1.1";
//	headers.push_back(headermaker.str());
//	headermaker.clear();
//	headermaker << "Host: " << hostname;
//	headers.push_back(headermaker.str());
//	headermaker.clear();
//	headermaker << "Content-Type: application/x-thrift";
//	headers.push_back(headermaker.str());
//	headermaker.clear();
//	headermaker << "Content-Length: " << sendSize;
//	headers.push_back(headermaker.str());
//	headermaker.clear();
//	headermaker << "Accept: application/x-thrift";
//	headers.push_back(headermaker.str());
//	headermaker.clear();
//	headermaker << "User-Agent: Thrift/" << " (C++/THttpClient)";
//	headers.push_back(headermaker.str());
//	headermaker.clear();

	try
	{
		//Then we just set up an Easy request, turn off
		// verboseness so it doesn't print garbage to cout,
		// tell it to read from the stream we made above, and
		// write our response to another stream, and feed it our url.
		// We would also tell it to use headers, but they're not
		// actually necessary.
		curlpp::Cleanup cleaner;
		curlpp::Easy request;

		using namespace curlpp::Options;
		request.setOpt(new ReadStream(&sendObject));
		request.setOpt(new InfileSize(sendSize));
		request.setOpt(new Upload(true));
		request.setOpt(new WriteStream(&receiveObject));
		request.setOpt(new Url(url.str()));
//		request.setOpt(new HttpHeader(headers));

		// Then perform the request.
		request.perform();

	}
	catch ( curlpp::LogicError & e )
	 {
	   std::cout << e.what() << std::endl;
	 }
   catch ( curlpp::RuntimeError & e )
	 {
	   std::cout << e.what() << std::endl;
	 }
   // Once the request is finished, we reverse the above process, making a TMemoryBuffer
   // from the stringbuffer we used for the WriteStream option.  Build a NotificationProtocol
   // around that, tell it to read the message beginning, then tell our retval object
   // to read itself from the NotificationProtocol object and we're done.
   	uint8_t * data = (uint8_t *) receiveObject.str().c_str();
   	std::cerr << receiveObject.str() << std::endl;
   	TMemoryBuffer * ibuffer = new TMemoryBuffer(data,
   			receiveObject.str().length(),
   			apache::thrift::transport::TMemoryBuffer::COPY);
   	shared_ptr<TMemoryBuffer> iptrBuffer = shared_ptr<TMemoryBuffer> (
   			ibuffer);
   	NotificationProtocol * prot = new NotificationProtocol(iptrBuffer);

   	prot->readMessageBegin(messageName, type, info);
   	retval.read(prot);
   	delete np;
   	delete prot;

	return 1;
}

/**
 * Creates an instance of the grid data request object and passes that pointer
 * back to the C application.  This pointer will be required in any
 * subsequent function calls.  host is the
 * name of the machine running edex, port should be the port of the thrift service
 * on EDEX (typically 9581).
 */
CGridDataRequest * get_gdr_instance(char * host, int port) {
	GridDataRequest * gdr = new GridDataRequest(host, port);
	return (CGridDataRequest *)gdr;
}
/**
 * Must be called to delete the instance of the grid data request object.
 */
void delete_gdr_instance(CGridDataRequest * gdrm) {
	GridDataRequest* gdr = (GridDataRequest *) gdrm;
	delete gdr;
}

void gdr_set_forecasttime(CGridDataRequest * gdr, int fct){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setForecastTime(fct);
}
void gdr_set_levelone(CGridDataRequest * gdr, double lo){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setLevelOne(lo);
}
void gdr_set_leveltwo(CGridDataRequest * gdr, double lt){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setLevelTwo(lt);
}
void gdr_set_leveltype(CGridDataRequest * gdr, char* lType){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setLevelType(lType);
}
void gdr_set_modelname(CGridDataRequest * gdr, char*  mName){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setModelName(mName);
}
void gdr_set_parameterabbreviation(CGridDataRequest * gdr, char* pAbbr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setParameterAbbreviation(pAbbr);
}
void gdr_set_pert(CGridDataRequest * gdr, int pert){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setPert(pert);
}
void gdr_set_starttime(CGridDataRequest * gdr, int sTime){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setStartTime(sTime);
}
void gdr_set_version(CGridDataRequest * gdr, int version){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	rgdr->setVersion(version);
}
int gdr_get_forecasttime(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getforecastTime();
}
double gdr_get_levelone(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getLevelOne();
}
double gdr_get_leveltwo(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getLevelTwo();
}
const char* gdr_get_leveltype(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getLevelType().c_str();
}
const char* gdr_get_modelname(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getModelName().c_str();
}
const char* gdr_get_parameterabbreviation(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getParameterAbbreviation().c_str();
}
int gdr_get_pert(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getPert();
}
int gdr_get_starttime(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getStartTime();
}
int gdr_get_version(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->getVersion();
}

// Request Data
int gdr_request_data(CGridDataRequest * gdr){
	GridDataRequest * rgdr = (GridDataRequest *) gdr;
	return rgdr->requestData();
}
CGribThriftContainer * gdr_get_results(CGridDataRequest * cgdr) {
	GridDataRequest * gdr = (GridDataRequest *) cgdr;
	com_raytheon_uf_common_dataplugin_grib_GribThriftContainer * gdc = gdr->getReturns();
	CGribThriftContainer * ret = (CGribThriftContainer *) gdc;
	return ret;
}

int gtc_get_num_records(CGribThriftContainer * cgtc) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftContainer * gtc = (com_raytheon_uf_common_dataplugin_grib_GribThriftContainer *) cgtc;
	return gtc->numOfRecords;
}
CGribThriftRecord * gtc_get_record_at_index(CGribThriftContainer * cgtc, int index) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftContainer * gtc = (com_raytheon_uf_common_dataplugin_grib_GribThriftContainer *) cgtc;
	CGribThriftRecord * retval = (CGribThriftRecord *) &(gtc->records[index]);
	return retval;
}

int gtr_is_hybrid_grid(CGribThriftRecord * cgtr) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	if (gtr->hybridGrid)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}
int gtr_is_local_section(CGribThriftRecord * cgtr) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	if (gtr->localSection)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}
int gtr_is_thinned_grid(CGribThriftRecord * cgtr) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	if (gtr->thinnedGrid)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

int gtr_get_localsectiondata_max(CGribThriftRecord * cgtr) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	int retval = gtr->localSectionData.size();
	return retval;
}
int gtr_get_localsectiondata_at_index(CGribThriftRecord * cgtr, int index) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	int retval = gtr->localSectionData[index];
	return retval;
}

int gtr_get_thinnedgriddata_max(CGribThriftRecord * cgtr) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	int retval = gtr->thinnedGridData.size();
	return retval;
}
int gtr_get_thinnedgriddata_at_index(CGribThriftRecord * cgtr, int index) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	int retval = gtr->thinnedGridData[index];
	return retval;
}

int gtr_get_data_max(CGribThriftRecord * cgtr) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	int retval = gtr->trueData.size();
	return retval;
}
float gtr_get_data_at_index(CGribThriftRecord * cgtr, int index) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	float retval = gtr->trueData[index];
	return retval;
}

int gtr_get_hybridgriddata_max(CGribThriftRecord * cgtr) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	int retval = gtr->trueHybridGridData.size();
	return retval;
}
float gtr_get_hybridgriddata_at_index(CGribThriftRecord * cgtr, int index) {
	com_raytheon_uf_common_dataplugin_grib_GribThriftRecord * gtr = (com_raytheon_uf_common_dataplugin_grib_GribThriftRecord *) cgtr;
	float retval = gtr->trueHybridGridData[index];
	return retval;
}
