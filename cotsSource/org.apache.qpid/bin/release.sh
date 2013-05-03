#!/bin/sh
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
# Script to pull together an Apache Release
#

usage()
{
    echo "Usage: release.sh <svn-path> <svn-revision> <version> [options]"
    echo
    echo "Options: Default : --prepare --svn --all --sign"
    echo "--help  |-h : Show this help"
    echo "--prepare   : Export specified tree from source control"
    echo "--svn       : Export from svn"
    echo "--git       : Export from git repository with svn metadata"
    echo "--clean-all : Remove build artefacts and downloaded svn tree"
    echo "--clean     : Remove built artefacts"
    echo "--all   |-a : Generate all artefacts"
    echo "--source|-e : Generate the source artefact"
    echo "--cpp   |-c : Generate the CPP artefacts"
    echo "--dotnet|-d : Generate the dotnet artefacts"
    echo "--java  |-j : Generate the java artefacts"
    echo "--ruby  |-r : Generate the ruby artefacts"
    echo "--python|-p : Generate the python artefacts"
    echo "--wcf   |-w : Generate the WCF artefacts"
    echo "--source|-e : Generate the source artefact"
    echo "--sign  |-s : Sign generated artefacts"
    echo "--upload|-u : Upload the artifacts directory to people.apache.org as qpid-\$VER"
    echo
}

REPO="SVN"
for arg in $* ; do 
 case $arg in
 --help|-h)
   HELP="HELP"
 ;;
 --prepare)
   PREPARE="PREPARE"
 ;;
 --svn)
   REPO="SVN"
 ;;
 --git)
   REPO="GIT"
 ;;
 --clean-all)
   CLEAN="CLEAN"
   CLEAN_ARTIFACTS="CLEAN_ARTIFACTS"
 ;;
 --clean)
   CLEAN_ARTIFACTS="CLEAN_ARTIFACTS"
 ;;
 --sign|-s)
   SIGN="SIGN"
 ;;
 --all|-a)
   CPP="CPP"
   DOTNET="DOTNET"
   JAVA="JAVA"
   RUBY="RUBY"
   PYTHON="PYTHON"
   WCF="WCF"
   SOURCE="SOURCE"
 ;;
 --cpp|-c)
   CPP="CPP"
 ;;
 --dotnet|-d)
   DOTNET="DOTNET"
 ;;
 --java|-j)
   JAVA="JAVA"
 ;;
 --ruby|-r)
   RUBY="RUBY"
 ;;
 --python|-p)
   PYTHON="PYTHON"
 ;;
 --wcf|-w)
   WCF="WCF"
 ;;
 --source|-e)
   SOURCE="SOURCE"
 ;;
 --upload|-u)
   UPLOAD="UPLOAD"
 ;;
 *)
  if [ -z "$SVN" ] ; then
   SVN=$arg
   continue
  fi
 
  if [ -z "$REV" ] ; then
   REV=$arg
   continue
  fi
  
  if [ -z "$VER" ] ; then
   VER=$arg
   continue
  fi 
 ;;
 esac
done

if [ -n "${HELP}" ] ; then
 usage
 exit 0
fi

if [ -z "$SVN" -o -z "$REV" -o -z "$VER" ]; then
    echo "Usage: release.sh <svn-path> <svn-revision> <version>"
    exit 1
fi

echo SVN:$SVN
echo REV:$REV
echo VER:$VER

# If nothing is specified then do it all
if [ -z "${CLEAN}${PREPARE}${CPP}${DOTNET}${JAVA}${RUBY}${PYTHON}${WCF}${SOURCE}${SIGN}${UPLOAD}" ] ; then
   PREPARE="PREPARE"
   CPP="CPP"
   DOTNET="DOTNET"
   JAVA="JAVA"
   RUBY="RUBY"
   PYTHON="PYTHON"
   WCF="WCF"
   SOURCE="SOURCE"

   SIGN="SIGN"
fi

set -xe

if [ "CLEAN" == "$CLEAN" ] ; then
  rm -rf qpid-${VER}
fi

if [ "CLEAN_ARTIFACTS" == "$CLEAN_ARTIFACTS" ] ; then
  rm -rf artifacts
fi

if [ "PREPARE" == "$PREPARE" ] ; then
  mkdir artifacts
  case ${REPO} in
  SVN)  
    URL=https://svn.apache.org/repos/asf/qpid/${SVN}
    svn export -r ${REV} ${URL} qpid-${VER}
    echo ${URL} ${REV} > artifacts/qpid-${VER}.svnversion
  ;;
  GIT)
    URL=${SVN}
    GITREV=$(GIT_DIR=${URL} git svn find-rev r${REV})
    git archive --remote=${URL}  ${GITREV} | tar xvf -
    mv qpid qpid-${VER}
    echo ${REV} > artifacts/qpid-${VER}.svnversion
  ;;
  esac
fi

if [ "SOURCE" == "$SOURCE" ] ; then
  tar -czf artifacts/qpid-${VER}.tar.gz qpid-${VER}
fi

if [ "RUBY" == "$RUBY" ] ; then
  tar -czf artifacts/qpid-ruby-${VER}.tar.gz qpid-${VER}/ruby qpid-${VER}/specs
fi

if [ "PYTHON" == "$PYTHON" ] ; then
  tar -czf artifacts/qpid-python-${VER}.tar.gz qpid-${VER}/python qpid-${VER}/specs
fi

if [ "WCF" == "$WCF" ] ; then
  zip -rq artifacts/qpid-wcf-${VER}.zip qpid-${VER}/wcf
fi

if [ "CPP" == "$CPP" ] ; then
  pushd qpid-${VER}/cpp
  ./bootstrap
  ./configure
  make dist -j2
  popd

  cp qpid-${VER}/cpp/*.tar.gz artifacts/qpid-cpp-${VER}.tar.gz
fi

if [ "JAVA" == "$JAVA" ] ; then
  pushd qpid-${VER}/java
  ant build release release-bin -Dsvnversion.output=${REV}
  popd

  cp qpid-${VER}/java/release/*.tar.gz  artifacts/qpid-java-${VER}.tar.gz
  cp qpid-${VER}/java/broker/release/*.tar.gz artifacts/qpid-java-broker-${VER}.tar.gz
  cp qpid-${VER}/java/client/release/*.tar.gz artifacts/qpid-java-client-${VER}.tar.gz
  #cp qpid-${VER}/java/client/example/release/*.tar.gz 
  cp qpid-${VER}/java/management/eclipse-plugin/release/*.tar.gz qpid-${VER}/java/management/eclipse-plugin/release/*.zip artifacts/
  cp qpid-${VER}/java/management/client/release/*.tar.gz artifacts/qpid-management-client-${VER}.tar.gz
fi

if [ "DOTNET" == "$DOTNET" ] ; then
  pushd qpid-${VER}/dotnet
  cd Qpid.Common
  ant
  cd ..
  ./build-nant-release mono-2.0

  cd client-010/gentool
  ant
  cd ..
  nant -t:mono-2.0 release-pkg
  popd

  cp qpid-${VER}/dotnet/bin/mono-2.0/release/*.zip artifacts/qpid-dotnet-0-8-${VER}.zip
  cp qpid-${VER}/dotnet/client-010/bin/mono-2.0/debug/*.zip artifacts/qpid-dotnet-0-10-${VER}.zip
fi

if [ "SIGN" == "$SIGN" ] ; then
  pushd artifacts
  sha1sum *.zip *.gz *.svnversion > SHA1SUM
  if [ ! -z $SIGNING_KEY ] ; then
    KEYOPTION="--default-key $SIGNING_KEY"
  fi
  for i in `find . | egrep 'jar$|pom$|gz$|zip$|svnversion$|SHA1SUM'`; do gpg --sign --armor --detach $KEYOPTION $i; done;
  popd
fi

if [ "UPLOAD" == "$UPLOAD" ] ; then
  scp -r artifacts people.apache.org:qpid-${VER}
fi
