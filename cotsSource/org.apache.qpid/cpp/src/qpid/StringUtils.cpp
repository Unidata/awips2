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
#include "qpid/StringUtils.h"

namespace qpid {

using std::string;
using std::vector;

void split(vector<string>& out, const string& in, const string& delims)
{
    string::size_type start = in.find_first_not_of(delims);
    if (start == string::npos) return;

    string::size_type end = in.find_first_of(delims, start);
    while (end != string::npos) {
        out.push_back(in.substr(start, end - start));
        start = in.find_first_not_of(delims, end);
        if (start == string::npos) return;
        end = in.find_first_of(delims, start);
    }
    out.push_back(in.substr(start));
}

vector<string> split(const string& in, const string& delims)
{
    vector<string> out;
    split(out, in, delims);
    return out;
}

} // namespace qpid
