#!/bin/sh

#Add local dir processing
PATH_TO_HERE=`dirname $0`
cd $PATH_TO_HERE

export TOP_DIR=`pwd`
echo $TOP_DIR
rpmbuild -ba --target=i386 --define "_topdir $TOP_DIR" SPECS/qpid-cpp-mrg.spec

