#!/bin/sh

# Set up default variable values if not supplied by the user.

# where the HDFView is installed
HDFVIEW_HOME=.
export HDFVIEW_HOME

# where Java is installed (requires jdk1.4.x)
JAVAPATH=/common/awips/jdk1.6.0_05/bin
export JAVAPATH

###############################################################################
#            DO NOT MODIFY BELOW THIS LINE
###############################################################################

rm TestH5MemoryLeak.class

CPATH=".:"$HDFVIEW_HOME"/lib/jhdf5.jar:"$HDFVIEW_HOME"/lib/jhdfobj.jar:"$HDFVIEW_HOME"/lib/jhdf5obj.jar"

TEST=/usr/bin/test
if [ ! -x /usr/bin/test ]
then
TEST=`which test`
fi

if $TEST -z "$CLASSPATH"; then
        CLASSPATH=""
fi
CLASSPATH=$CPATH":"$CLASSPATH
export CLASSPATH

if $TEST -n "$JAVAPATH" ; then
        PATH=$JAVAPATH":"$PATH
        export PATH
fi

if $TEST -e /bin/uname; then
   os_name=`/bin/uname -s`
elif $TEST -e /usr/bin/uname; then
   os_name=`/usr/bin/uname -s`
else
   os_name=unknown
fi

case  $os_name in
    SunOS)
        OS_NAME=solaris
        ;;
    Linux)
        OS_NAME=linux
        ;;
    IRIX*)
        OS_NAME=irix-6.5
        ;;
    OSF1)
        OS_NAME=alpha
        ;;
    AIX)
        OS_NAME=aix
        ;;
    Darwin)
        OS_NAME=macosx
        ;;
    FreeBSD)
        OS_NAME=freebsd
        ;;
    *)
        echo "Unknown Operating System:  HDF-Java may not work correctly"
        ;;
esac

$JAVAPATH/javac TestH5MemoryLeak".java"
(cd ../..; $JAVAPATH/java -Xmx1024M -Djava.library.path=$HDFVIEW_HOME"/lib/"$OS_NAME test.object.TestH5MemoryLeak $*)



