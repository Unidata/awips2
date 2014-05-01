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

#include "qpid/log/Selector.h"
#include "qpid/log/Options.h"
#include <boost/bind.hpp>
#include <algorithm>
#include <string.h>

namespace qpid {
namespace log {

using namespace std;

void Selector::enable(const string& enableStr) {
    string level, pattern;
    size_t c=enableStr.find(':');
    if (c==string::npos) {
        level=enableStr;
    }
    else {
        level=enableStr.substr(0,c);
        pattern=enableStr.substr(c+1);
    }
    if (!level.empty() && level[level.size()-1]=='+') {
        for (int i =  LevelTraits::level(level.substr(0,level.size()-1));
             i < LevelTraits::COUNT;
             ++i)
            enable(Level(i), pattern);
    }
    else {
        enable(LevelTraits::level(level), pattern);
    }
}

Selector::Selector(const Options& opt){
    for_each(opt.selectors.begin(), opt.selectors.end(),
             boost::bind(&Selector::enable, this, _1));
}

bool Selector::isEnabled(Level level, const char* function) {
    const char* functionEnd = function+::strlen(function);
    for (std::vector<std::string>::iterator i=substrings[level].begin();
         i != substrings[level].end();
         ++i)
    {
        if (std::search(function, functionEnd, i->begin(), i->end()) != functionEnd)
            return true;
    }
    return false;
}

}} // namespace qpid::log
