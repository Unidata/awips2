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

#include <windows.h>
#include <msclr\lock.h>

#include "qpid/client/AsyncSession.h"
#include "qpid/framing/FrameSet.h"
#include "qpid/framing/AMQFrame.h"

#include "MessageBodyStream.h"
#include "AmqpMessage.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Threading;
using namespace msclr;

using namespace Apache::Qpid::AmqpTypes;

AmqpMessage::AmqpMessage(MessageBodyStream ^mbs) :
    messageBodyStream(mbs),
    disposed(false)
{
}

void AmqpMessage::Cleanup()
{
    {
        lock l(this);
        if (disposed)
	    return;

	disposed = true;
    }

    messageBodyStream->Close();
}

AmqpMessage::~AmqpMessage()
{
    Cleanup();
}

AmqpMessage::!AmqpMessage()
{
    Cleanup();
}

void AmqpMessage::Close()
{
    // Simulate Dispose()...
    Cleanup();
    GC::SuppressFinalize(this);
}

}}} // namespace Apache::Qpid::Interop
