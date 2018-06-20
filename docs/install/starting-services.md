
All EDEX services are started and stopped with the commands `edex start` and `edex stop`, and individual services can be started in the following order

    service edex_postgres start
    service httpd-pypies start
    service qpidd start
    service edex_camel start
    service edex_ldm start
    
Services can be stopped in reverse order

    service edex_ldm stop
    service edex_camel stop
    service qpidd stop
    service httpd-pypies stop
    service edex_postgres stop

The service config files are located in `/etc/init.d/`:


    ls -la /etc/init.d/ |grep -e edex -e pypies -e qpid

    -rwxr--r--   1 root  root     6693 Nov  7 17:53 edex_camel
    -rwxr-xr-x   1 root  root     1422 Oct 29 15:28 edex_ldm
    -rwxr--r--   1 root  root     2416 Sep  7 15:48 edex_postgres
    -rwxr-xr-x   1 root  root     5510 Aug 26 13:05 httpd-pypies
    -rwxr-xr-x   1 root  root     3450 Aug 26 13:04 qpidd

