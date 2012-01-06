#ifndef _sys_PidFile_h
#define _sys_PidFile_h

/*
 *
 * Copyright (c) 2008 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/sys/LockFile.h"

#include "qpid/CommonImportExport.h"
#include "qpid/sys/IntegerTypes.h"

#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <string>

namespace qpid {
namespace sys {

class PidFile : public LockFile
{
public:
    QPID_COMMON_EXTERN PidFile(const std::string& path_, bool create);

    /**
     * Read the process ID from the lock file. This method assumes that
     * if there is a process ID in the file, it was written there by
     * writePid(); thus, it's at the start of the file.
     *
     * Throws an exception if there is an error reading the file.
     *
     * @returns The stored process ID. No validity check is done on it.
     */
    QPID_COMMON_EXTERN pid_t readPid(void) const;

    /**
     * Write the current process's ID to the lock file. It's written at
     * the start of the file and will overwrite any other content that
     * may be in the file.
     *
     * Throws an exception if the write fails.
     */
    QPID_COMMON_EXTERN void writePid(void);
};
 
}}   /* namespace qpid::sys */

#endif /*!_sys_PidFile_h*/
