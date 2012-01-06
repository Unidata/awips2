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
#ifndef _APRBase_
#define _APRBase_

#include <string>
#include <apr_thread_mutex.h>
#include <apr_errno.h>

namespace qpid {
namespace sys {

    /**
     * Use of APR libraries necessitates explicit init and terminate
     * calls. Any class using APR libs should obtain the reference to
     * this singleton and increment on construction, decrement on
     * destruction. This class can then correctly initialise apr
     * before the first use and terminate after the last use.
     */
    class APRBase{
	static APRBase* instance;
	apr_pool_t* pool;
	apr_thread_mutex_t* mutex;
	int count;

	APRBase();
	~APRBase();
	static APRBase* getInstance();
	bool _increment();
	void _decrement();
    public:
	static void increment();
	static void decrement();
    };

    //this is also a convenient place for a helper function for error checking:
    void check(apr_status_t status, const char* file, const int line);
    std::string get_desc(apr_status_t status);

#define CHECK_APR_SUCCESS(A) qpid::sys::check(A, __FILE__, __LINE__); 

}
}

// Inlined as it is called *a lot*
void inline qpid::sys::check(apr_status_t status, const char* file, const int line){
    if (status != APR_SUCCESS){
        char tmp[256];
        throw Exception(QPID_MSG(apr_strerror(status, tmp, size)))
    }
}




#endif
