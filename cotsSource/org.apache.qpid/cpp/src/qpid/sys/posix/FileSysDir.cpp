/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
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

#include "qpid/sys/FileSysDir.h"
#include "qpid/sys/StrError.h"
#include "qpid/Exception.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <cerrno>
#include <unistd.h>

namespace qpid {
namespace sys {

bool FileSysDir::exists (void) const
{
    const  char *cpath = dirPath.c_str ();
    struct stat  s;
    if (::stat(cpath, &s)) {
        if (errno == ENOENT) {
            return false;
        }
        throw qpid::Exception (strError(errno) +
                               ": Can't check directory: " + dirPath);
    }
    if (S_ISDIR(s.st_mode))
        return true;
    throw qpid::Exception(dirPath + " is not a directory");
}

void FileSysDir::mkdir(void)
{
    if (::mkdir(dirPath.c_str(), 0755))
        throw Exception ("Can't create directory: " + dirPath);
}

}} // namespace qpid::sys
