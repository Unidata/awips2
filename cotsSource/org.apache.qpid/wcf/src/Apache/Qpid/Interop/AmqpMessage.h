/*
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
*/

#pragma once

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Runtime::InteropServices;

using namespace qpid::client;
using namespace std;



public ref class AmqpMessage
{
private:
    MessageBodyStream^ messageBodyStream;
    AmqpTypes::AmqpProperties^ amqpProperties;
    bool disposed;
    void Cleanup();

internal:
    AmqpMessage(MessageBodyStream ^bstream);

public:  
    ~AmqpMessage();
    !AmqpMessage();
    void Close();

    property AmqpTypes::AmqpProperties^ Properties {
	AmqpTypes::AmqpProperties^ get () { return amqpProperties; }
	void set(AmqpTypes::AmqpProperties^ p) { amqpProperties = p; }
    }

    property System::IO::Stream^ BodyStream {
	System::IO::Stream^ get() { return messageBodyStream; }
    }
};
	    

}}} // namespace Apache::Qpid::Interop
