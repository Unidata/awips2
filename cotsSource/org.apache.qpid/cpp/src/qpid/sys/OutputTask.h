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
#ifndef _OutputTask_
#define _OutputTask_

namespace qpid {
namespace sys {

    class OutputTask 
    {
    public:
        virtual ~OutputTask() {}
        /** Generate some  output.
         *@return true if output was generated, false if there is no work to do.
         */
        virtual bool doOutput() = 0;

        /** Check if there may be work to do, but don't do it.
         * @return True if there may be work to do, false if there is none.
         * Can to return a false positive, to allow implementations to do a
         * faster check than doOutput(). Must never return a false negative.
         */
        virtual bool hasOutput() = 0;
    };

}
}


#endif
