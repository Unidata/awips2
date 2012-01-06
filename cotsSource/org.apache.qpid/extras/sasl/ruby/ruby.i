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
 */

%module saslwrapper
%include "typemaps.i"
%include "stl.i"

%{
#include "saslwrapper.h"
%}

%typemap (in) void *
{
    $1 = (void *) $input;
}

%typemap (out) void *
{
    $result = (VALUE) $1;
}

%typemap (in) uint16_t
{
    $1 = NUM2UINT($input);
}

%typemap (out) uint16_t
{
    $result = UINT2NUM((uint16_t) $1);
}

%typemap (in) uint32_t
{
    if (TYPE($input) == T_BIGNUM)
        $1 = NUM2UINT($input);
    else
        $1 = FIX2UINT($input);
}

%typemap (out) uint32_t
{
    $result = UINT2NUM((uint32_t) $1);
}

%typemap (in) int32_t
{
    if (TYPE($input) == T_BIGNUM)
        $1 = NUM2INT($input);
    else
        $1 = FIX2INT($input);
}

%typemap (out) int32_t
{
    $result = INT2NUM((int32_t) $1);
}

%typemap (typecheck, precedence=SWIG_TYPECHECK_INTEGER) uint32_t {
   $1 = FIXNUM_P($input);
}

%typemap (in) uint64_t
{
    if (TYPE($input) == T_BIGNUM)
        $1 = NUM2ULL($input);
    else
        $1 = (uint64_t) FIX2LONG($input);
}

%typemap (out) uint64_t
{
    $result = ULL2NUM((uint64_t) $1);
}

%typemap (in) int64_t
{
    if (TYPE($input) == T_BIGNUM)
        $1 = NUM2LL($input);
    else
        $1 = (int64_t) FIX2LONG($input);
}

%typemap (out) int64_t
{
    $result = LL2NUM((int64_t) $1);
}

%typemap (typecheck, precedence=SWIG_TYPECHECK_INTEGER) uint64_t {
   $1 = FIXNUM_P($input);
}

namespace saslwrapper {
    class Client {
    public:

        Client();
        ~Client();
        bool setAttr(const std::string& INPUT, const std::string& INPUT);
        bool setAttr(const std::string& INPUT, uint32_t INPUT);
        bool init();
        bool start(const std::string& INPUT, std::string& OUTPUT, std::string& OUTPUT);
        bool step(const std::string& INPUT, std::string& OUTPUT);
        bool encode(const std::string& INPUT, std::string& OUTPUT);
        bool decode(const std::string& INPUT, std::string& OUTPUT);
        bool getUserId(std::string& OUTPUT);
        void getError(std::string& OUTPUT);
    };
}
