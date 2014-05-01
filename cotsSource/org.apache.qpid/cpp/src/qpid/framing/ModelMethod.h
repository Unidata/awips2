#ifndef _ModelMethod_
#define _ModelMethod_

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
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/Header.h"

namespace qpid {
namespace framing {


class ModelMethod : public AMQMethodBody 
{
    mutable Header header;
public:    
    virtual ~ModelMethod() {}
    virtual void encodeHeader(Buffer& buffer) const { header.encode(buffer); }
    virtual void decodeHeader(Buffer& buffer, uint32_t size=0) { header.decode(buffer, size); }
    virtual uint32_t headerSize() const { return header.encodedSize(); } 
    virtual bool isSync() const { return header.getSync(); }
    virtual void setSync(bool on) const { header.setSync(on); }
    Header& getHeader() { return header; } 
    const Header& getHeader()  const { return header; } 
};


}} // namespace qpid::framing


#endif
