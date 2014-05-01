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

#include "qpid/log/OstreamOutput.h"
#include <stdexcept>

using namespace std;

namespace qpid {
namespace log {

OstreamOutput::OstreamOutput(std::ostream& o) : out(&o) {}

OstreamOutput::OstreamOutput(const std::string& file)
  : out(new ofstream(file.c_str(), ios_base::out | ios_base::app)),
    mine(out)
{
    if (!out->good())
        throw std::runtime_error("Can't open log file: "+file);
}

void OstreamOutput::log(const Statement&, const std::string& m) {
    *out << m << flush;
}

}} // namespace qpid::log
