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

#include "qpid/Exception.h"
#include "qpid/DataDir.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/FileSysDir.h"
#include "qpid/sys/LockFile.h"

namespace qpid {

DataDir::DataDir (std::string path) :
    enabled (!path.empty ()),
    dirPath (path)
{
    if (!enabled)
    {
        QPID_LOG (info, "No data directory - Disabling persistent configuration");
        return;
    }

    sys::FileSysDir dir(dirPath);
    if (!dir.exists())
        dir.mkdir();
    std::string lockFileName(path);
    lockFileName += "/lock";
    lockFile = std::auto_ptr<sys::LockFile>(new sys::LockFile(lockFileName, true));
}

DataDir::~DataDir () {}

} // namespace qpid

