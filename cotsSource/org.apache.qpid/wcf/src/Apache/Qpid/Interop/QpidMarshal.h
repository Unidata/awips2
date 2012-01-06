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
using namespace System::Text;


// Helper functions for marshaling.

private ref class QpidMarshal
{
 public:

    // marshal_as<T> not available in all Visual Studio editions.

    static std::string ToNative (System::String^ managed) {
	if (managed->Length == 0) {
	    return std::string();
	}
	array<unsigned char>^ mbytes = Encoding::UTF8->GetBytes(managed);
	if (mbytes->Length == 0) {
	    return std::string();
	}

	pin_ptr<unsigned char> pinnedBuf = &mbytes[0];
	std::string native((char *) pinnedBuf, mbytes->Length);
	return native;
    }
};

}}} // namespace Apache::Qpid::Interop
