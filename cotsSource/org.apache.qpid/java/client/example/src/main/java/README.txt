In order to use the runSample script, you are required to set two environment
variables, QPID_HOME and QPID_SAMPLE. If not the default values will be used.

QPID_HOME
---------
This is the directory that contains the QPID distribution. If you are running the Qpid
Java broker on the same machine as the examples, you have already set QPID_HOME to this 
directory.

default: /usr/share/java/

QPID_SAMPLE
-----------
This is the parent directory of the 'java' directory in which you find 'runSample.sh'
(Ex:- $QPID_SRC_HOME/java/client/example/src/main)

default: $PWD

