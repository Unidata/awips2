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
#include "qpid/sys/windows/check.h"

#include <windows.h>

namespace qpid {
namespace sys {

class LockFilePrivate {
    friend class LockFile;

    HANDLE fd;

public:
    LockFilePrivate(HANDLE f) : fd(f) {}
};

LockFile::LockFile(const std::string& path_, bool create)
  : path(path_), created(create) {

    HANDLE h = ::CreateFile(path.c_str(),
                          create ? (GENERIC_READ|GENERIC_WRITE) : GENERIC_READ,
                          FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,
                          0, /* Default security */
                          create ? OPEN_ALWAYS : OPEN_EXISTING,
                          FILE_FLAG_DELETE_ON_CLOSE, /* Delete file when closed */
                          NULL);
    if (h == INVALID_HANDLE_VALUE)
        throw qpid::Exception(path + ": " + qpid::sys::strError(GetLastError()));

    // Lock up to 4Gb
    if (!::LockFile(h, 0, 0, 0xffffffff, 0))
        throw qpid::Exception(path + ": " + qpid::sys::strError(GetLastError()));
    impl.reset(new LockFilePrivate(h));
}

LockFile::~LockFile() {
    if (impl) {
        if (impl->fd != INVALID_HANDLE_VALUE) {
            ::UnlockFile(impl->fd, 0, 0, 0xffffffff, 0);
            ::CloseHandle(impl->fd);
        }
    }
}

}}  /* namespace qpid::sys */
