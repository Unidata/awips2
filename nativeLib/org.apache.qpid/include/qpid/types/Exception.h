#ifndef QPID_TYPES_EXCEPTION_H
#define QPID_TYPES_EXCEPTION_H

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

#include <string>
#include "qpid/types/ImportExport.h"

namespace qpid {
namespace types {

class Exception : public std::exception
{
  public:
    QPID_TYPES_EXTERN explicit Exception(const std::string& message=std::string()) throw();
    QPID_TYPES_EXTERN virtual ~Exception() throw();
    QPID_TYPES_EXTERN virtual const char* what() const throw();

  private:
    const std::string message;
};

}} // namespace qpid::types

#endif  /*!QPID_TYPES_EXCEPTION_H*/
