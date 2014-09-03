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
 * 07/29/13     2215        bkowal      Regenerated for thrift 0.9.0
 * 07/22/14     15649       lshi        Rollback to Initial Creation
 *
 * </pre>
 *
 * @author gzhou
 * @version 1
 */

#include "PointDataServer_types.h"

const char
		* com_raytheon_uf_common_dataquery_requests_ConstraintType::ascii_fingerprint =
				"EFB929595D312AC8F305D5A794CFEDA1";
const uint8_t
		com_raytheon_uf_common_dataquery_requests_ConstraintType::binary_fingerprint[16] =
				{ 0xEF, 0xB9, 0x29, 0x59, 0x5D, 0x31, 0x2A, 0xC8, 0xF3, 0x05,
						0xD5, 0xA7, 0x94, 0xCF, 0xED, 0xA1 };

uint32_t com_raytheon_uf_common_dataquery_requests_ConstraintType::read(
		::apache::thrift::protocol::TProtocol* iprot) {

	uint32_t xfer = 0;
	std::string fname;
	::apache::thrift::protocol::TType ftype;
	int16_t fid;

	xfer += iprot->readStructBegin(fname);

	using ::apache::thrift::protocol::TProtocolException;

	xfer += iprot->readFieldBegin(fname, ftype, fid);
	if (ftype == ::apache::thrift::protocol::T_STRING) {
		xfer += iprot->readString(operand);
		this->__isset.operand = true;
	} else
		xfer += iprot->skip(ftype);
	xfer += iprot->readFieldEnd();

	xfer += iprot->readStructEnd();

	return xfer;
}

uint32_t com_raytheon_uf_common_dataquery_requests_ConstraintType::write(
		::apache::thrift::protocol::TProtocol* oprot) const {
	uint32_t xfer = 0;
	xfer
			+= oprot->writeStructBegin(
					"com_raytheon_uf_common_dataquery_requests_RequestConstraint$ConstraintType");
	xfer += oprot->writeFieldBegin("__enumValue__",
			::apache::thrift::protocol::T_STRING, 1);
	xfer += oprot->writeString(this->operand);
	xfer += oprot->writeFieldEnd();
	xfer += oprot->writeStructEnd();
	return xfer;
}

const char
		* com_raytheon_uf_common_dataquery_requests_RequestConstraint::ascii_fingerprint =
				"BDAF55DAA660FA1ADBE30760752211A8";
const uint8_t
		com_raytheon_uf_common_dataquery_requests_RequestConstraint::binary_fingerprint[16] =
				{ 0xBD, 0xAF, 0x55, 0xDA, 0xA6, 0x60, 0xFA, 0x1A, 0xDB, 0xE3,
						0x07, 0x60, 0x75, 0x22, 0x11, 0xA8 };

uint32_t com_raytheon_uf_common_dataquery_requests_RequestConstraint::read(
		::apache::thrift::protocol::TProtocol* iprot) {

	uint32_t xfer = 0;
	std::string fname;
	::apache::thrift::protocol::TType ftype;
	int16_t fid;

	xfer += iprot->readStructBegin(fname);

	using ::apache::thrift::protocol::TProtocolException;

	while (true) {
		xfer += iprot->readFieldBegin(fname, ftype, fid);
		if (ftype == ::apache::thrift::protocol::T_STOP) {
			break;
		}
		switch (fid) {
		case 1:
			if (ftype == ::apache::thrift::protocol::T_STRUCT) {
				xfer += this->constraintType.read(iprot);
				this->__isset.constraintType = true;
			} else {
				xfer += iprot->skip(ftype);
			}
			break;
		case 2:
			if (ftype == ::apache::thrift::protocol::T_STRING) {
				xfer += iprot->readString(this->constraintValue);
				this->__isset.constraintValue = true;
			} else {
				xfer += iprot->skip(ftype);
			}
			break;
		default:
			xfer += iprot->skip(ftype);
			break;
		}
		xfer += iprot->readFieldEnd();
	}

	xfer += iprot->readStructEnd();

	return xfer;
}

uint32_t com_raytheon_uf_common_dataquery_requests_RequestConstraint::write(
		::apache::thrift::protocol::TProtocol* oprot) const {
	uint32_t xfer = 0;
	xfer += oprot->writeStructBegin(
			"com_raytheon_uf_common_dataquery_requests_RequestConstraint");
	xfer += oprot->writeFieldBegin("constraintType",
			::apache::thrift::protocol::T_STRUCT, 1);
	xfer += this->constraintType.write(oprot);
	xfer += oprot->writeFieldEnd();
	xfer += oprot->writeFieldBegin("constraintValue",
			::apache::thrift::protocol::T_STRING, 2);
	xfer += oprot->writeString(this->constraintValue);
	xfer += oprot->writeFieldEnd();
	xfer += oprot->writeFieldStop();
	xfer += oprot->writeStructEnd();
	return xfer;
}

const char
		* com_raytheon_uf_common_pointdata_PointDataServerRequest::ascii_fingerprint =
				"C9F1CC9CD1A896EE01B7C5215E2BE99F";
const uint8_t
		com_raytheon_uf_common_pointdata_PointDataServerRequest::binary_fingerprint[16] =
				{ 0xC9, 0xF1, 0xCC, 0x9C, 0xD1, 0xA8, 0x96, 0xEE, 0x01, 0xB7,
						0xC5, 0x21, 0x5E, 0x2B, 0xE9, 0x9F };

uint32_t com_raytheon_uf_common_pointdata_PointDataServerRequest::read(
		::apache::thrift::protocol::TProtocol* iprot) {

	uint32_t xfer = 0;
	std::string fname;
	::apache::thrift::protocol::TType ftype;
	int16_t fid;

	xfer += iprot->readStructBegin(fname);

	using ::apache::thrift::protocol::TProtocolException;

	while (true) {
		xfer += iprot->readFieldBegin(fname, ftype, fid);
		if (ftype == ::apache::thrift::protocol::T_STOP) {
			break;
		}
		switch (fid) {
		case 1:
			if (ftype == ::apache::thrift::protocol::T_MAP) {
				{
					this->rcMap.clear();
					uint32_t _size0;
					::apache::thrift::protocol::TType _ktype1;
					::apache::thrift::protocol::TType _vtype2;
					iprot->readMapBegin(_ktype1, _vtype2, _size0);
					uint32_t _i4;
					for (_i4 = 0; _i4 < _size0; ++_i4) {
						std::string _key5;
						xfer += iprot->readString(_key5);
						com_raytheon_uf_common_dataquery_requests_RequestConstraint
								& _val6 = this->rcMap[_key5];
						xfer += _val6.read(iprot);
					}
					iprot->readMapEnd();
				}
				this->__isset.rcMap = true;
			} else {
				xfer += iprot->skip(ftype);
			}
			break;
		default:
			xfer += iprot->skip(ftype);
			break;
		}
		xfer += iprot->readFieldEnd();
	}

	xfer += iprot->readStructEnd();

	return xfer;
}

uint32_t com_raytheon_uf_common_pointdata_PointDataServerRequest::write(
		::apache::thrift::protocol::TProtocol* oprot) const {
	uint32_t xfer = 0;
	xfer += oprot->writeStructBegin(
			"com_raytheon_uf_common_pointdata_PointDataServerRequest");
	xfer += oprot->writeFieldBegin("rcMap", ::apache::thrift::protocol::T_MAP,
			1);
	{
		xfer += oprot->writeMapBegin(::apache::thrift::protocol::T_STRING,
				::apache::thrift::protocol::T_STRUCT, this->rcMap.size());
		std::map<std::string,
				com_raytheon_uf_common_dataquery_requests_RequestConstraint>::const_iterator
				_iter7;
		for (_iter7 = this->rcMap.begin(); _iter7 != this->rcMap.end(); ++_iter7) {
			xfer += oprot->writeStructBegin("11");
			xfer += oprot->writeString(_iter7->first);
			xfer += oprot->writeStructEnd();
			xfer += _iter7->second.write(oprot);
		}
		xfer += oprot->writeMapEnd();
	}
	xfer += oprot->writeFieldEnd();
	xfer += oprot->writeFieldStop();
	xfer += oprot->writeStructEnd();
	return xfer;
}

