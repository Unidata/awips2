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


public ref class OutputLink
{
private:
    AmqpSession^ amqpSession;
    String^ queue;
    bool disposed;
    bool finalizing;
    void Cleanup();
    AmqpTypes::AmqpProperties^ defaultProperties;
    void ManagedToNative(AmqpMessage^ m);
    int maxFrameSize;

internal:
    OutputLink(AmqpSession^ session, String^ defaultQueue);

public:  
    ~OutputLink();
    !OutputLink();
    void Close();
    AmqpMessage^ CreateMessage();
    void Send(AmqpMessage^ m, TimeSpan timeout);
    IAsyncResult^ BeginSend(AmqpMessage^ amqpMessage, TimeSpan timeout, AsyncCallback^ callback, Object^ state);
    void EndSend(IAsyncResult^ result);

    property AmqpTypes::AmqpProperties^ DefaultProperties {
	AmqpTypes::AmqpProperties^ get () { return defaultProperties; }
	void set(AmqpTypes::AmqpProperties^ p) { defaultProperties = p; }
    }
};


}}} // namespace Apache::Qpid::Interop
