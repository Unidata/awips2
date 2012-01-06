README
n======

INSTALL
=======
        source
        ======  
        1.Set the environment variable QPID_HOME to the root directory of the repository or the release.
      
        2.Run these command to build the source

        ant compile OR run the ant build in the parent directory(main ant script)

        3.To launch the CLI run the script in bin folder with appropriate parameters.

        ex:

        Linux   $QPID_HOME/bin/qpid-cli  -h 10.8.100.122 -p 8334
        Windows %QPID_HOME%/bin/qpid-cli.bat -h 10.8.100.122 -p 8334

        -h hostname (default localhost)
        -p broker port (default 8999)

        binary
        ======
        1.Set the environment variable QPID_HOME to the root directory of the repository or the release.

        2.To launch the CLI run the script in bin folder with appropriate parameters.

        ex:

        Linux   $QPID_HOME/bin/qpid-cli  -h 10.8.100.122 -p 8334
        Windows %QPID_HOME%/bin/qpid-cli.bat -h 10.8.100.122 -p 8334

        -h hostname (default localhost)
        -p broker port (default 8999)

        3. No test cases are included in the binary version.

TESTING
=======

1.Test source is located in the test directory.If you want to run the tests please start the
Qpid java broker and run following ant targets.
        
        ant compile-tests       This compile all the test sources
        ant test                This runs all the test cases
2.If you want to test with a remote broker please use the source release and change the constants in ConnectionConstants.java
class.(Default values are BROKER_HOSTNAME="localhost" BROKER_PORT="8999")

For more informations please visit the project home page.













