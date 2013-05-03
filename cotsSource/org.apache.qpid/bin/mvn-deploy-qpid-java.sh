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
# 
#

#!/bin/sh

qpid_version=$1
repo=$2

if [ -z "$qpid_version" -o -z "$repo" ]; then
    echo "Usage: mvn-deploy-qpid-java.sh <qpid-version> <mvn-repo>"
    exit 1
fi

set -xe


build_dir=build/lib

deploy_artifact() {
  mvn deploy:deploy-file -DuniqueVersion=false -Durl=$repo -Dfile=${build_dir}/$1-${qpid_version}.jar -DgroupId=org.apache.qpid -DartifactId=$1 -Dversion=${qpid_version} -Dpackaging=jar
}

deploy_artifact_with_classifier() {
  mvn deploy:deploy-file -DuniqueVersion=false -Durl=$repo -Dfile=${build_dir}/$1-$2-${qpid_version}.jar -DgroupId=org.apache.qpid -DartifactId=$1 -Dclassifier=$2 -Dversion=${qpid_version} -Dpackaging=jar
}

deploy_artifact qpid-broker
deploy_artifact_with_classifier qpid-broker-plugins tests
deploy_artifact_with_classifier qpid-broker tests
deploy_artifact qpid-client-example
deploy_artifact_with_classifier qpid-client-example tests
deploy_artifact qpid-client
deploy_artifact_with_classifier qpid-client tests
deploy_artifact qpid-common
deploy_artifact_with_classifier qpid-common tests
deploy_artifact qpid-integrationtests
deploy_artifact_with_classifier qpid-integrationtests tests
deploy_artifact qpid-junit-toolkit
deploy_artifact_with_classifier qpid-junit-toolkit tests
deploy_artifact qpid-management-eclipse-plugin
deploy_artifact_with_classifier qpid-management-eclipse-plugin tests
deploy_artifact qpid-perftests
deploy_artifact_with_classifier qpid-perftests tests
deploy_artifact qpid-systests
deploy_artifact_with_classifier qpid-systests tests
deploy_artifact qpid-testkit
deploy_artifact_with_classifier qpid-testkit tests
deploy_artifact qpid-tools
deploy_artifact_with_classifier qpid-tools tests

build_dir=build/lib/plugins

deploy_artifact qpid-broker-plugins
