#ifndef SELECTOR_H
#define SELECTOR_H

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

#include "qpid/log/Statement.h"
#include "qpid/CommonImportExport.h"
#include <vector>

namespace qpid {
namespace log {
struct Options;

/**
 * A selector identifies the set of log messages to enable.
 *
 * Thread object unsafe, pass-by-value type.
 */
class Selector {
  public:
    /** Empty selector selects nothing */
    Selector() {}

    /** Set selector from Options */
    QPID_COMMON_EXTERN Selector(const Options&);

    /** Equavlient to: Selector s; s.enable(l, s) */
    Selector(Level l, const std::string& s=std::string()) {
        enable(l,s);
    }

    Selector(const std::string& enableStr) { enable(enableStr); }
    /**
     * Enable messages with level in levels where the file
     * name contains substring. Empty string matches all.
     */
    void enable(Level level, const std::string& substring=std::string()) {
        substrings[level].push_back(substring);
    }

    /** Enable based on a 'level[+]:file' string */
    QPID_COMMON_EXTERN void enable(const std::string& enableStr);

    /** True if level is enabled for file. */
    QPID_COMMON_EXTERN bool isEnabled(Level level, const char* function);

  private:
    std::vector<std::string> substrings[LevelTraits::COUNT];
};


}} // namespace qpid::log


#endif  /*!SELECTOR_H*/
