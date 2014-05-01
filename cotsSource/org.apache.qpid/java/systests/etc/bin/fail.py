#!/usr/bin/env python
#
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

import os
import re
import datetime

from optparse import OptionParser

BASE_CMD = "mvn -Dskip.python.test=true %s test"

def main():
    parser = OptionParser()
    parser.add_option("-t", "--test", dest="test",
                      action="store", type="string",
                      help="run specific tests")
    parser.add_option("-c", "--continuous", dest="continuous",
                      action="store_true", default=False,
                      help="run tests after failures, don't stop")


    (options, args) = parser.parse_args()

    # determine command to run
    if (options.test != None):
        cmd = (BASE_CMD % ("-Dtest="+options.test))
    else:
        cmd = (BASE_CMD % (""))

    run_forever = options.continuous


    failed_runs = []
    iteration = 0
    fail_match = re.compile("BUILD SUCCESSFUL")
    done = False

    while (run_forever or not (len(failed_runs) > 0)):
        iteration = iteration + 1
        if (run_forever):
            extra_text = (", %d failures so far: %s:" % (len(failed_runs), failed_runs))
        else:
            extra_text = ""
        print ("%s Test run %d%s" % (datetime.datetime.today().isoformat(), iteration, extra_text))
        (child_stdin, child_stdout_and_stderr) = os.popen4(cmd)
        output = child_stdout_and_stderr.read()
        child_stdin.close()
        child_stdout_and_stderr.close()
        matches = fail_match.search(output)
        if (matches == None):
            failed_runs.append(iteration)
            output_name = ("test-run-%d.out" % (iteration))
            #write testouput
            test_output = file(output_name, "w")
            test_output.write(output)
            test_output.close()
            #tar test-output and surefire reports together
            find_stdout = os.popen("find . -type d -name surefire-reports")
            surefire_dirs = find_stdout.read().replace('\n', ' ')
            find_stdout.close()
            tarcmd = ("tar -zcf test-failures-%d.tar.gz %s %s" % (iteration, output_name, surefire_dirs))
            tar_stdout = os.popen(tarcmd)
            tar_output = tar_stdout.read()
            tar_exitstatus = tar_stdout.close()
            print ("Something failed! Check %s" % (output_name))
            if (tar_exitstatus != None):
                print ("tar exited abornmally, aborting\n %s"  % (tar_output))
                run_forever = False

if __name__ == "__main__":
    main()