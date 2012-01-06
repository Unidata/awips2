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

#include "config.h"
#include "qpid/Modules.h"
#include "qpid/Exception.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/Shlib.h"

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

namespace fs=boost::filesystem;

namespace {

// CMake sets QPID_MODULE_SUFFIX; Autoconf doesn't, so assume Linux .so
#if defined (QPID_MODULE_SUFFIX)
    std::string suffix(QPID_MODULE_SUFFIX);
#else
    std::string suffix(".so");
#endif

bool isShlibName(const std::string& name) {
    return name.find (suffix) == name.length() - suffix.length();
}

}

namespace qpid {

ModuleOptions::ModuleOptions(const std::string& defaultModuleDir)
    : qpid::Options("Module options"), loadDir(defaultModuleDir), noLoad(false)
{
    addOptions()
        ("module-dir",    optValue(loadDir, "DIR"),  "Load all shareable modules in this directory")
        ("load-module",   optValue(load,    "FILE"), "Specifies additional module(s) to be loaded")
        ("no-module-dir", optValue(noLoad),          "Don't load modules from module directory");
}

void tryShlib(const char* libname_, bool noThrow) {
    std::string libname(libname_);
    if (!isShlibName(libname)) libname += suffix;
    try {
        sys::Shlib shlib(libname);
        QPID_LOG (info, "Loaded Module: " << libname);
    }
    catch (const std::exception& /*e*/) {
        if (!noThrow)
            throw;
    }
}

void loadModuleDir (std::string dirname, bool isDefault)
{
    fs::path dirPath (dirname, fs::native);

    if (!fs::exists (dirPath))
    {
        if (isDefault)
            return;
        throw Exception ("Directory not found: " + dirname);
    }
    if (!fs::is_directory(dirPath)) 
    {
        throw Exception ("Invalid value for module-dir: " + dirname + " is not a directory");
    }

    fs::directory_iterator endItr;
    for (fs::directory_iterator itr (dirPath); itr != endItr; ++itr)
    {
        if (!fs::is_directory(*itr) && isShlibName(itr->string()))
            tryShlib (itr->string().data(), true);
    }
}

} // namespace qpid
