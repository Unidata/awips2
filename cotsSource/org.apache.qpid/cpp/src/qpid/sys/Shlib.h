#ifndef QPID_SYS_SHLIB_H
#define QPID_SYS_SHLIB_H

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

#include "qpid/CommonImportExport.h" 
#include <boost/noncopyable.hpp>
#include <iostream>

namespace qpid {
namespace sys {

/** Encapsulates a shared library handle.
 *@see AutoShlib
 */
class Shlib {
  public:
    /** Load a shared library */
    Shlib(const char* libname) { load(libname); }

    /** Load a shared library */
    Shlib(const std::string& libname) { load(libname.c_str()); }

    /** Unload shared library. */
    QPID_COMMON_EXTERN void unload();

    /** Look up symbol. */
    QPID_COMMON_EXTERN void* getSymbol(const char* symbol);

    /** Look up symbol in shared library, cast it to the desired
     * pointer type, void* by default.
     */
    template <class T>
    T getSymbol(const char* symbol) {
        // Double cast avoids warning about casting object to function pointer
        return reinterpret_cast<T>(reinterpret_cast<intptr_t>(
                                       this->getSymbol(symbol)));
    }
    
  private:
    void* handle;
    QPID_COMMON_EXTERN void load(const char* libname);
};

/** A shared library handle that unloads the shlib in it's dtor */
class AutoShlib : public Shlib {
  public:
    /** Load shared library */
    AutoShlib(const std::string& libname) : Shlib(libname) {}
    /** Calls unload() */
    QPID_COMMON_EXTERN ~AutoShlib() throw();
};

    
}} // namespace qpid::sys

#endif  /*!QPID_SYS_SHLIB_H*/
