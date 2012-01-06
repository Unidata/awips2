QMan - Qpid JMX & WS-DM Management Bridge 
---------------------------------------------------------

Documentation
--------------
All of our user documentation for QMan module can be accessed on our wiki at:

http://cwiki.apache.org/qpid/qman-qpid-management-bridge.html

This includes a Getting Started and User Guide as well as detailed developer documentation.
However, here's a VERY quick guide to running QMan, once you have installed it somewhere !

Running
------------------

Once you installed QMan, under the root folder you should have the following structure

- bin (folder) : contains startup & shutdown scripts;
- app (folder) : contains the web application module;
- etc (folder) : contains configuration files;
- examples (folder) : contains examples (and a nested README as well)
- lib (folder) : contains dependency libraries;
- log (folder) : this is the default log folder.

To run QMan, 

1) edit the $QMAN_HOME/etc/qman-config.xml file and configure broker connection data (host,port, username, etc...)
2) under the $QMAN_HOME/bin directory run :

> ./qman-wsdm-start.sh

now, under $QMAN_HOME/log directory you should see two files :

1) server.log : contains web server log messages;
2) qman.log : contains qman log messages; 

Administration
-----------------------

After QMan has been started successfully you can browse its administration console pointing your browser to :

http://<host>:<port>/qman/admin.jsp