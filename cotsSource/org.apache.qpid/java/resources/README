
Documentation
--------------
All of our user documentation for the Qpid Java components can be accessed on our wiki at:

http://cwiki.apache.org/confluence/display/qpid/Qpid+Java+Documentation

This includes a Getting Started Guide and FAQ as well as detailed developer documentation.
However, here's a VERY quick guide to running the installed Qpid broker, once you have installed it somewhere !


Running the Broker
------------------

To run the broker, set the QPID_HOME environment variable to
distribution directory and add $QPID_HOME/bin to your PATH. Then run
the qpid-server shell script or qpid-server.bat batch file to start
the broker. By default, the broker will use $QPID_HOME/etc to find
the configuration files. You can supply a custom configuration using
the -c argument.

For example:

qpid-server -c ~/etc/config.xml

You can get a list of all command line arguments by using the -h argument.


Developing
----------

In order to build Qpid you need Ant 1.6.5. Use ant -p to list the
available targets. The default ant target, build, creates a working
development-mode distribution in the build directory. To run the
scripts in build/bin set QPID_HOME to the build directory and put
${QPID_HOME}/bin on your PATH. The scripts in that directory include
the standard ones in the distribution and a number of testing scripts.



