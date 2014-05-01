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
using namespace std;
using namespace qpid::client;

ref class AmqpSession;
ref class DtxResourceManager;

public delegate void ConnectionIdleEventHandler(Object^ sender, EventArgs^ eventArgs);

public ref class AmqpConnection
{
private:
    Connection* connectionp;
    String^ host;
    int port;
    bool disposed;
    Collections::Generic::List<AmqpSession^>^ sessions;
    bool isOpen;
    int busyCount;
    int maxFrameSize;
    DtxResourceManager^ dtxResourceManager;
    void Cleanup();
    // unique string used for distributed transactions
    String^ dataSourceName;

 internal:
    void NotifyBusy();
    void NotifyIdle();
    AmqpConnection^ Clone();

    property int MaxFrameSize {
	int get () { return maxFrameSize; }
    }

    property DtxResourceManager^ CachedResourceManager {
	DtxResourceManager^ get () { return dtxResourceManager; }
	void set (DtxResourceManager^ value) { dtxResourceManager = value; }
    }

    property String^ DataSourceName {
	// Note: any change to this format has to be reflected in the DTC plugin's xa_open()
	String^ get() {
	    if (dataSourceName == nullptr) {
		dataSourceName = String::Format("{0}.{1}..AMQP.{2}.{3}", port, host, 
						System::Diagnostics::Process::GetCurrentProcess()->Id, 
						AppDomain::CurrentDomain->Id);
	    }
	    return dataSourceName;
	}
    }

public:  
    AmqpConnection(System::String^ server, int port);
    ~AmqpConnection();
    !AmqpConnection();
    void Close();
    AmqpSession^ CreateSession();
    event ConnectionIdleEventHandler^ OnConnectionIdle;

    property bool IsOpen {
	bool get() { return isOpen; }
    };

    property bool IsIdle {
	bool get() { return (busyCount == 0); }
    }
};


}}} // namespace Apache::Qpid::Interop
