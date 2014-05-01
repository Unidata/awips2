*** QMan WS-DM examples ***

1) DESCRIPTION
This set of examples shows QMan WS-DM interface capabilities.
Each example is articulated in the following way. 
First the name of the example class with a brief description about that is printed out. For example :

      GetWSDLMetadataExample 
-------------------------------------------------------------------

This example shows the usage of WS-DM 
GetResourcePropertyRequest / Response on a 
Group service.
The target resource is the WS-DM Adapter itself 
and the requested property is "ws-rp:Entry".
WS-DM Adapter is a special WS-Resource (is a Group)
that  acts as the main entry point for retrieving
all other managed resources.
So clients that want to deal with QMan WS-Resources
must first get resource identifiers sending
a GetResourcePropertyRequest to WS-DM Adapter 
with "ws-rp:Entry" as target target property.

-------------------------------------------------------------------

Type enter to proceed.

When you're ready type enter to proceed. Now the example runs and all the exchanged
SOAP messages are printed out on the screen.
If you want, we shipped (under sample_messages folder) several files containing those messages.

A general note concerning examples...they are all written using java language so what you see is the 
"java" usage of WS-DM client API. 
The most important thing that you should keep in mind is that what is expected (on QMan side) is a SOAP WS-DM 
compliant message so on top of that you don't need to use those java API but feel free to produce those messages
in your preferred way (by hand or using another programming language).

Another thing : the examples contain a lot of code duplication because each of them is took as independent as possible.
The general idea is that you open an example source file and in the executeExample(...) method you should have a quick
idea of how things are working.
Also, as mentioned before, we provided, under the sample_messages folder, the messages that are part of each example conversation. 
Remember : these messages are important, not the way / language you use to produce them.

2) HOW TO RUN

2.1) Java  
You need JDK 1.5 or higher in order to run and / or compile the examples.

2.2) Dependencies 
You need to set / update the CLASSPATH environment variable with libraries found under $QMAN_HOME/app/qman/WEB-INF/lib.
After that you should be able to run one the shipped examples:

> java org.apache.qpid.management.example.GetMultipleResourcePropertiesExample <qman_host> <qman_port>
> java org.apache.qpid.management.example.GetQManResourceMembersExample <qman_host> <qman_port>
> java org.apache.qpid.management.example.GetResourceMetadataDescriptorExample <qman_host> <qman_port>
> java org.apache.qpid.management.example.GetResourcePropertyDocumentExample <qman_host> <qman_port>
> java org.apache.qpid.management.example.GetResourcePropertyExample <qman_host> <qman_port>
> java org.apache.qpid.management.example.GetWSDLMetadataExample <qman_host> <qman_port>
> java org.apache.qpid.management.example.SetResourcePropertyExample <qman_host> <qman_port>

Where 
<qman_host> is the host (ip or hostname) where QMan is running;
<qman_port> is the port number where QMan is running;

2.3) Qpid
You must have a running C++ broker with management enabled.

2.4) QMan 
You must have QMan WS-DM up, running and connected with the broker above.