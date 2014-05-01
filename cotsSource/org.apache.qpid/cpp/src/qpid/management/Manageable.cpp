//
// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
// 
//   http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//

#include "qpid/management/Manageable.h"

using namespace qpid::management;
using std::string;

string Manageable::StatusText (status_t status, string text)
{
    if ((status & STATUS_USER) == STATUS_USER)
        return text;

    switch (status)
    {
    case STATUS_OK                      : return "OK";
    case STATUS_UNKNOWN_OBJECT          : return "UnknownObject";
    case STATUS_UNKNOWN_METHOD          : return "UnknownMethod";
    case STATUS_NOT_IMPLEMENTED         : return "NotImplemented";
    case STATUS_PARAMETER_INVALID       : return "InvalidParameter";
    case STATUS_FEATURE_NOT_IMPLEMENTED : return "FeatureNotImplemented";
    case STATUS_FORBIDDEN               : return "Forbidden";
    }

    return "??";
}

Manageable::status_t Manageable::ManagementMethod (uint32_t, Args&, std::string&)
{
    return STATUS_UNKNOWN_METHOD;
}

