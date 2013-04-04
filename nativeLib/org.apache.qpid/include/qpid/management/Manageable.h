#ifndef _Manageable_
#define _Manageable_

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

#include "qpid/management/ManagementObject.h"
#include "qpid/management/Args.h"
#include <string>
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace management {

class QPID_COMMON_EXTERN Manageable
{
  public:

    virtual ~Manageable(void) = 0;

    //  status_t is a type used to pass completion status from the method handler.
    //
    typedef uint32_t status_t;
    static std::string StatusText(status_t status, std::string text = std::string());

    static const status_t STATUS_OK                      = 0;
    static const status_t STATUS_UNKNOWN_OBJECT          = 1;
    static const status_t STATUS_UNKNOWN_METHOD          = 2;
    static const status_t STATUS_NOT_IMPLEMENTED         = 3;
    static const status_t STATUS_PARAMETER_INVALID       = 4;
    static const status_t STATUS_FEATURE_NOT_IMPLEMENTED = 5;
    static const status_t STATUS_FORBIDDEN               = 6;
    static const status_t STATUS_EXCEPTION               = 7;
    static const status_t STATUS_USER                    = 0x00010000;

    //  Every "Manageable" object must hold a reference to exactly one
    //  management object.  This object is always of a class derived from
    //  the pure-virtual "ManagementObject".
    //
    //  This accessor function returns a pointer to the management object.
    //
    virtual ManagementObject* GetManagementObject(void) const = 0;

    //  Every "Manageable" object must implement ManagementMethod.  This
    //  function is called when a remote management client invokes a method
    //  on this object.  The input and output arguments are specific to the
    //  method being called and must be down-cast to the appropriate sub class
    //  before use.
    virtual status_t ManagementMethod(uint32_t methodId, Args& args, std::string& text);

    //  This optional method can be overridden to allow the agent application to
    //  authorize method invocations.  Return true iff the authenticated user identified
    //  in userId us authorized to execute the method.
    virtual bool AuthorizeMethod(uint32_t methodId, Args& args, const std::string& userId);
};

inline Manageable::~Manageable(void) {}

}}

#endif  /*!_Manageable_*/
