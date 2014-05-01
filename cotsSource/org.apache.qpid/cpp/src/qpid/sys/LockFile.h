#ifndef _sys_LockFile_h
#define _sys_LockFile_h

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

#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <string>

#include "qpid/CommonImportExport.h"
#include "qpid/sys/IntegerTypes.h"

namespace qpid {
namespace sys {

class LockFilePrivate;

/**
 * @class LockFile
 *
 * LockFile represents a locked file suitable for a coarse-grain system
 * lock. For example, the broker uses this to ensure that only one broker
 * runs. A common usage idiom is to store the current "owner" process ID
 * in the lock file - if the lock file exists, but the stored process ID
 * doesn't, the old owner has probably died without cleaning up the lock
 * file.
 */
class LockFile : private boost::noncopyable
{
    std::string path;
    bool created;
    boost::shared_ptr<LockFilePrivate> impl;

protected:
    int read(void*, size_t) const;
    int write(void*, size_t) const;

public:
    QPID_COMMON_EXTERN LockFile(const std::string& path_, bool create);
    QPID_COMMON_EXTERN ~LockFile();
};
 
}}   /* namespace qpid::sys */

#endif /*!_sys_LockFile_h*/


       
