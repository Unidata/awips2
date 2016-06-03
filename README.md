# Install AWIPS II

These scripts should be run as *root*:

## CAVE Client

Download and run `installCAVE.sh`

    wget https://raw.githubusercontent.com/Unidata/awips2/unidata_16.1.4/installCAVE.sh
    chmod 755 ./installCAVE.sh
    ./installCAVE.sh

## EDEX Server

Download and run `installEDEX.sh`:

    wget https://raw.githubusercontent.com/Unidata/awips2/unidata_16.1.4/installEDEX.sh
    chmod 755 ./installEDEX.sh
    ./installEDEX.sh

## What do these scripts do?

1. Downloads [http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo](http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo) to `/etc/yum.repos.d/awips2.repo`
2. Runs `yum clean all`
3. Runs `yum groupinstall awips2-server` and/or `yum groupinstall awips2-cave`

## System Requirements

EDEX operations systems supported:

* CentOS 5 and 6
* Red Hat 5 and 6
* Fedora Core 12 to 14 
 
Not supported for EDEX:
 
* Debian, Ubuntu, SUSE, Solaris, OS X, Fedora 15+, CentOS/RHEL 7, Windows
   
It will probably work on Scientific Linux. 
 
selinux should be **disabled** [(read more about selinux at redhat.com)](https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Security-Enhanced_Linux/sect-Security-Enhanced_Linux-Enabling_and_Disabling_SELinux-Disabling_SELinux.html)

## CentOS/RHEL 6 Server Config

#### Download awips2.repo yum file

    wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo

####  create user `awips` and group `fxalpha` 
        
    groupadd fxalpha
    useradd -G fxalpha awips
        
#### create /awips2 and set owner to awips:fxalpha

    mkdir -p /awips2/data_store 
    chown -R awips:fxalpha /awips2

#### edit /etc/security/limits.conf
 
Qpid is known to crash on some systems which have not defined a higher security limit for user processes and files. To be sure that Qpid can handle the high number of messages from edexBridge, add the following two lines to `/etc/security/limits.conf`
    
	printf "awips soft nproc 65536\nawips soft nofile 65536\n" >> /etc/security/limits.conf

Or copy manually:

    awips soft nproc 65536
    awips soft nofile 65536
   
#### /etc/sysconfig/iptables

To serve data from an EDEX server, iptables must allow TCP connections on ports **5672**, **9581** and **9582**. The following lines added to `/etc/sysconfig/iptables`, followed by the command `service iptables restart`, will configure iptables for EDEX.
    
    -A INPUT -p tcp -m tcp --dport 5672 -j ACCEPT
    -A INPUT -p tcp -m tcp --dport 9581 -j ACCEPT
    -A INPUT -p tcp -m tcp --dport 9582 -j ACCEPT

## Install with Yum

#### yum groupinstall awips2-server

The EDEX Data Server packages will take roughly 20-30 minutes to install, so you may want to let this run and come back.

    yum groupinstall awips2-server -y 2>&1 | tee -a edex_install.log

#### Start EDEX

    edex start

If you see an error from the above command, check your EDEX configuration with the command

    edex setup

(this is done once during installation, but it's a good idea to check again)
Confirm that all EDEX services are running:

    [root@edex ~]# edex
    [edex status]
     postgres    :: running :: pid 7317
     pypies      :: running :: pid 7345
     qpid        :: running :: pid 7378
     EDEXingest  :: running :: pid 8271
     EDEXgrib    :: running :: pid 8317
     EDEXrequest :: running :: pid 8218
     ldmadmin    :: running :: pid 1337 
    
     edex (status|start|stop|setup|log|purge)


#### Watch Logs

    edex log
    [edex] EDEX Log Viewer

     :: No log specified - Defaulting to ingest log
     :: Viewing /awips2/edex/logs/edex-ingest-20140413.log. Press CTRL+C to exit
    
    
    INFO  2014-04-13 20:48:25,413 [genericThreadPool-59] Ingest: EDEX: Ingest - obs:: /awips2/data_store/text/13/20/SPXX61_KWBC_132048_25234695.20140413Apr processed in: 0.1600 (sec) Latency: 0.3280 (sec)
    INFO  2014-04-13 20:48:25,415 [genericThreadPool-91] Ingest: EDEX: Ingest - taf:: /awips2/data_store/text/13/20/FTXX60_KWBC_132048_25234702.20140413Apr processed in: 0.0060 (sec) Latency: 0.3300 (sec)
    INFO  2014-04-13 20:48:25,445 [genericThreadPool-62] Ingest: EDEX: Ingest - sfcobs:: /awips2/data_store/text/13/21/SXUS20_KWNB_132100_25234700.20140413Apr processed in: 0.1850 (sec) Latency: 0.3600 (sec)
    
* edex log grib - Grib Products
* edex log radar - Radar Products
* edex log satellite - Satellite Products
* edex log text - Text Products
* edex log ldm - LDM log

#### Confirm EDEX Hostname

Check the file `/awips2/edex/bin/setup.env` to confirm that the EDEX hostname is resolvable both from inside and outside your machine.
