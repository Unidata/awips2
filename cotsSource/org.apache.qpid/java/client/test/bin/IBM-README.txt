The IBM JMS Performance Harness scripts have take the following additional parameters

-tx : Enable transactions
-pp : Enable persistent messaging

-ms 1000 : Set message size (default 1000 bytes)
-cc 1 : Number of messages to per commit (default 1) Only applies to Sender/Subscriber

The IBM JMS Performance Harness will need to be downloaded and the library added to client/test/lib.

The Library can be found here:

http://www.alphaworks.ibm.com/tech/perfharness

Before running the required test the IBM JNDI Setup script should be run.

This will create a filesystem based JNDI Context located at:

System.properties{java.io.tmpdir}/IBMPerfTestsJNDI/