#ifndef _QmfEngineTypecode_
#define _QmfEngineTypecode_

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

namespace qmf {
namespace engine {

    enum Typecode {
        TYPE_UINT8      = 1,
        TYPE_UINT16     = 2,
        TYPE_UINT32     = 3,
        TYPE_UINT64     = 4,
        TYPE_SSTR       = 6,
        TYPE_LSTR       = 7,
        TYPE_ABSTIME    = 8,
        TYPE_DELTATIME  = 9,
        TYPE_REF        = 10,
        TYPE_BOOL       = 11,
        TYPE_FLOAT      = 12,
        TYPE_DOUBLE     = 13,
        TYPE_UUID       = 14,
        TYPE_MAP        = 15,
        TYPE_INT8       = 16,
        TYPE_INT16      = 17,
        TYPE_INT32      = 18,
        TYPE_INT64      = 19,
        TYPE_OBJECT     = 20,
        TYPE_LIST       = 21,
        TYPE_ARRAY      = 22
    };
}
}

#endif

