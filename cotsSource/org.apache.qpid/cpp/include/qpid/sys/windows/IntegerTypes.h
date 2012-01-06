#ifndef QPID_SYS_WINDOWS_INTEGERTYPES_H
#define QPID_SYS_WINDOWS_INTEGERTYPES_H

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
 *
 */

typedef unsigned char    uint8_t;
typedef char             int8_t;
typedef unsigned short   uint16_t;
typedef short            int16_t;
typedef unsigned int     uint32_t;
typedef int              int32_t;
typedef unsigned __int64 uint64_t;
typedef          __int64 int64_t;

// Visual Studio doesn't define other common types, so set them up here too.
typedef unsigned int     uint;

#endif  /*!QPID_SYS_WINDOWS_INTEGERTYPES_H*/
