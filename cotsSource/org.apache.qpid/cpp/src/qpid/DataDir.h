#ifndef QPID_DATADIR_H
#define QPID_DATADIR_H

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

#include <string>
#include <memory>
#include "qpid/CommonImportExport.h"

namespace qpid {

    namespace sys {
        class LockFile;
    }

/**
 * DataDir class.
 */
class DataDir
{
    const bool        enabled;
    const std::string dirPath;
    std::auto_ptr<qpid::sys::LockFile> lockFile;

  public:

	QPID_COMMON_EXTERN DataDir (std::string path);
	QPID_COMMON_EXTERN ~DataDir ();

    bool isEnabled() { return enabled; }
    const std::string& getPath() { return dirPath; }
};
 
} // namespace qpid

#endif  /*!QPID_DATADIR_H*/
