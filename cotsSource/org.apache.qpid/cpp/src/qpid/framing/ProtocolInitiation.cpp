/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
#include "qpid/framing/ProtocolInitiation.h"

namespace qpid {
namespace framing {

ProtocolInitiation::ProtocolInitiation(){}

ProtocolInitiation::ProtocolInitiation(uint8_t _major, uint8_t _minor) : version(_major, _minor) {}

ProtocolInitiation::ProtocolInitiation(ProtocolVersion p) : version(p) {}

ProtocolInitiation::~ProtocolInitiation(){}

void ProtocolInitiation::encode(Buffer& buffer) const {
    buffer.putOctet('A');
    buffer.putOctet('M');
    buffer.putOctet('Q');
    buffer.putOctet('P');
    buffer.putOctet(1);//class
    buffer.putOctet(1);//instance
    buffer.putOctet(version.getMajor());
    buffer.putOctet(version.getMinor());    
}

bool ProtocolInitiation::decode(Buffer& buffer){
    if(buffer.available() >= 8){
	buffer.getOctet();//A
	buffer.getOctet();//M
	buffer.getOctet();//Q
	buffer.getOctet();//P
	buffer.getOctet();//class
	buffer.getOctet();//instance
	version.setMajor(buffer.getOctet());
	version.setMinor(buffer.getOctet());
	return true;
    }else{
	return false;
    }
}


std::ostream& operator<<(std::ostream& o, const framing::ProtocolInitiation& pi) {
    return o << int(pi.getMajor()) << "-" << int(pi.getMinor());
}

}} // namespace qpid::framing
