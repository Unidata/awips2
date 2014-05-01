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

import sys, re
from popen2 import popen2, popen3
from optparse import OptionParser
from xml.dom.minidom import parse, parseString

prereqs = ["tr", "svn", "xsltproc", "sed", "grep", "wget"]

apacheSVN="https://svn.apache.org/repos/asf/qpid/trunk/qpid/java"

svncmd = "svn log %s --xml -r %s:HEAD | tr '\\n\\r|' '  -' | xsltproc svnlog2wiki.xsl - | grep r | sed -e 's/^ *//' | sed -e 's/\\(QPID-[0-9]*\\)/\\[\\1 | https:\\/\\/issues.apache.org\\/jira\\/browse\\/\\1 \]/g'"


def get_commits(revision):
    (stdout, stdin) = popen2(svncmd % (options.repo,revision))
    return add_jira_status(stdout.read())

def add_jira_status(commits):
    commit_lines = commits.split("\n")
    new_commits = []
    for commit in commit_lines:
        if re.match(".*https://issues.apache.org/.*", commit):
            jira = re.findall("QPID-[0-9]*", commit)[0]
            jira_xml_url = "http://issues.apache.org/jira/si/jira.issueviews:issue-xml/%s/%s.xml" % (jira, jira)
            (stdout, stdin) = popen2("wget -q -O - %s" % jira_xml_url)

            jira_dom = parse(stdout)
            status = jira_dom.getElementsByTagName("status")[0]
            new_commits.append("%s %s | " % (commit, status.lastChild.data))
        else:
            new_commits.append(commit)

    return "\n".join(new_commits)


def main():
    global options
    parser = OptionParser()
    parser.add_option("-r", "--revision", dest="revision", action="store",
                      type="string",
                      help="The first revision to generate logs for")

    parser.add_option("-s", "--svn-repo", dest="repo", action="store",
                      default=apacheSVN,
		      type="string",
                      help="Provide a svn repository to process")


    (options, args) = parser.parse_args()

    # Check that we have what's necessary

    notfound = re.compile('^which')
    for cmd in prereqs:
        (stdout, stdin, stderr) = popen3('which %s' % cmd)
        if (notfound.match(stderr.read())):
            parser.error ("Could not find command %s, try [apt-get|yum] install %s" %
                          (cmd, cmd))
            
    if (options.revision == None):
        parser.error("svn revision must be specified")

    print(get_commits(options.revision))
    
if __name__ == "__main__":
    main()
