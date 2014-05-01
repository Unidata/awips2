Cluster test scripts.

A set of scripts to start and stop cluster and test clients on
multiple hosts using ssh.

Pre-requisites: You must be
 - set up for password-free ssh access to the test hosts.
 - a member of the ais group on all the test hosts.

Configuration:

Copy defaults.sh to config.sh and edit the values as necessary.
 
Test scripts:

Test scripts use the functions in functions.sh to start & monitor
cluster and clients.
A test script can collect other scripts.


