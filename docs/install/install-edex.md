# <i class="fa fa-linux"></i> EDEX for Linux 

[installEDEX.sh <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/software/awips2/installEDEX.sh) 

Installs to subdirectories in **/awips2**

!!! note "System Requirements"

	* **64-bit** CentOS/RHEL 6 or 7
	* **8 CPU** cores (16 recommended) 
	* **16GB** RAM (32+GB recommended for full IDD processing)
	* **500GB** disk space, more if you build a data archive.

An **SSD is an especially good idea**, mounted to **/awips2/edex/data/hdf5**  to contain the decoded data files, or mounted to **/awips2** to contain the entire AWIPS software system.
 
EDEX **can scale to any system**, either by adjusting the incoming data feeds, or the resources allocated to each data type, but when selecting a server, **more is always better**.

**64-bit CentOS/RHEL 6 and 7** are the only supported Linux operating systems. You may have luck with Fedora Core 12 to 14 and Scientific Linux. EDEX is not developed, tested, or supported on Debian, Ubuntu, SUSE, Solaris, OS X, or Windows.

---

## Linux One-Time Setup

All of these command should be run as **root**!

### Users and Groups

Create user and group awips:fxalpha

	groupadd fxalpha && useradd -G fxalpha awips

or if the awips account already exists:

	groupadd fxalpha && usermod -G fxalpha awips

---

### iptables

Configure iptables to allow TCP connections on ports 5672, 9581 and 9582

- **To open ports to all connections**
    
		vi /etc/sysconfig/iptables
    
		*filter
		:INPUT ACCEPT [0:0]
		:FORWARD ACCEPT [0:0]
		:OUTPUT ACCEPT [0:0]
		-A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
		-A INPUT -p icmp -j ACCEPT
		-A INPUT -i lo -j ACCEPT
		-A INPUT -m state --state NEW -m tcp -p tcp --dport 22 -j ACCEPT
		-A INPUT -m state --state NEW -m tcp -p tcp --dport 5672 -j ACCEPT
		-A INPUT -m state --state NEW -m tcp -p tcp --dport 9581 -j ACCEPT
		-A INPUT -m state --state NEW -m tcp -p tcp --dport 9582 -j ACCEPT
		-A INPUT -j REJECT --reject-with icmp-host-prohibited
		-A FORWARD -j REJECT --reject-with icmp-host-prohibited
		COMMIT

- **To open ports to specific IP addresses**
    
		vi /etc/sysconfig/iptables
    
		*filter
		:INPUT DROP [0:0]
		:FORWARD DROP [0:0]
		:OUTPUT ACCEPT [0:0]
		:EXTERNAL - [0:0]
		:EDEX - [0:0]
		-A INPUT -i lo -j ACCEPT
		-A INPUT -p icmp --icmp-type any -j ACCEPT
		-A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
		-A INPUT -s 128.117.140.0/24 -j EDEX
		-A INPUT -s 128.117.156.0/24 -j EDEX
		-A INPUT -j EXTERNAL
		-A EXTERNAL -j REJECT
		-A EDEX -m state --state NEW -p tcp --dport 22 -j ACCEPT
		-A EDEX -m state --state NEW -p tcp --dport 5672 -j ACCEPT
		-A EDEX -m state --state NEW -p tcp --dport 9581 -j ACCEPT
		-A EDEX -m state --state NEW -p tcp --dport 9582 -j ACCEPT
		-A EDEX -j REJECT
		COMMIT
    
> In this example, the IP range `128.117.140.0/24` will match all 128.117.140.* addresses, while `128.117.156.0/24` will match 128.117.156.*.
 
**Restart iptables**

	service iptables restart

For CentOS 7 error *Redirecting to /bin/systemctl restart  iptables.service
Failed to restart iptables.service: Unit iptables.service failed to load: No such file or directory.*

The solution is:
	
	yum install iptables-services
	systemctl enable iptables
	service iptables restart

---

### Disable SELinux

	vi /etc/sysconfig/selinux

	# This file controls the state of SELinux on the system.
	# SELINUX= can take one of these three values:
	#     enforcing - SELinux security policy is enforced.
	#     permissive - SELinux prints warnings instead of enforcing.
	#     disabled - No SELinux policy is loaded.
	SELINUX=disabled
	# SELINUXTYPE= can take one of these two values:
	#     targeted - Targeted processes are protected,
	#     mls - Multi Level Security protection.
	SELINUXTYPE=targeted

!!! note "Read more about selinux at [redhat.com](https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Security-Enhanced_Linux/sect-Security-Enhanced_Linux-Enabling_and_Disabling_SELinux-Disabling_SELinux.html)"

 **reboot if necessary**, required if iptables was updated.

---

### SSD Mount

Though a Solid State Drive is not required, it is *strongly encouraged* in order to handle the amount of disk IO for real-time IDD feeds.  

The simplest configuration would be to mount an 500GB+ SSD to **/awips2** to contain both the installed software (approx. 20GB) and the real-time data (approx. 150GB per day).

The default [purge rules]() are configured such that **/awips2** does not exceed 450GB. **/awips2/data_store** is scoured every hour and should not exceed 50GB. 

If you want to increase EDEX data retention you should mount a large disk to **/awips2/edex/data/hdf5** since this will be where the archived processed data exists, and any case studies created.

        Filesystem      Size  Used Avail Use% Mounted on
        /dev/sda1        30G  2.5G   26G   9% /
        tmpfs            28G     0   28G   0% /dev/shm
        /dev/sdc1       788G   81G  667G  11% /awips2
        /dev/sdb1       788G   41G  708G  10% /awips2/edex/data/hdf5

---

### yum install

Download the script [installEDEX.sh](http://www.unidata.ucar.edu/software/awips2/installEDEX.sh) to setup and run yum to install AWIPS packages:

	wget http://www.unidata.ucar.edu/software/awips2/installEDEX.sh
	chmod 755 ./installEDEX.sh
	./installEDEX.sh

!!! info "What does [installEDEX.sh](http://www.unidata.ucar.edu/software/awips2/installEDEX.sh) do?"

	1. Downloads [awips2.repo](http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo) or [el7.repo](http://www.unidata.ucar.edu/software/awips2/doc/el7.repo) to **/etc/yum.repos.d/awips2.repo**
	2. Runs **yum clean all**
	3. Runs **yum groupinstall awips2-server**

---

## EDEX Setup

The command **edex setup** attempts to add the domain name of your server. 

- **/awips2/edex/bin/setup.env** should contain the fully-qualified domain name, externally resolved, localhost will not work. 

        export AW_SITE_IDENTIFIER=OAX
        export EDEX_SERVER=edex-cloud.unidata.ucar.edu

- **/awips2/ldm/etc/ldmd.conf** contains the upstream server (default *idd.unidata.ucar.edu*, which requires you connect form a .edu domain). This file also contains the **edexBridge** hostname (default *localhost*). 

        EXEC    "pqact -e"
        EXEC    "edexBridge -s localhost"

- **/etc/security/limits.conf** defines the number of user processes and files (this step is automatically performed by **installEDEX.sh**). Without these definitions, Qpid is known to crash during periods of high ingest.
    
        awips soft nproc 65536
        awips soft nofile 65536

---

## LDM 

EDEX installs its own version of the LDM to the directory **/awips2/ldm**.  As with a the default LDM configuration, two files are used to control what IDD feeds are ingested:

* **/awips2/ldm/etc/ldmd.conf** - specifies an upstream LDM server to request data from, and what feeds to request:

		REQUEST NEXRAD3 "./p(DHR|DPR|DSP|DTA|DAA|DVL|EET|HHC|N0Q|N0S|N0U|OHA|NVW|NTV|NST)." idd.unidata.ucar.edu
		REQUEST FNEXRAD|IDS|DDPLUS|UNIWISC ".*" idd.unidata.ucar.edu
		REQUEST NGRID ".*" idd.unidata.ucar.edu
		REQUEST NOTHER "^TIP... KNES.*" idd.unidata.ucar.edu
	 
	!!! note "[read more about ldmd.conf in the LDM User Manual](https://www.unidata.ucar.edu/software/ldm/ldm-current/basics/ldmd.conf.html)"

* **/awips2/ldm/etc/pqact.conf** - specifies the WMO headers and file pattern actions to request:

		# Redbook graphics
		ANY     ^([PQ][A-Z0-9]{3,5}) (....) (..)(..)(..) !redbook [^/]*/([^/]*)/([^/]*)/([^/]*)/([0-9]{8})
		        FILE    -overwrite -close -edex /awips2/data_store/redbook/\8/\4\5Z_\8_\7_\6-\1_\2_(seq).rb.%Y%m%d%H
		# NOAAPORT GINI images
		NIMAGE  ^(sat[^/]*)/ch[0-9]/([^/]*)/([^/]*)/([^ ]*) ([^/]*)/([^/]*)/([^/]*)/ (T[^ ]*) ([^ ]*) (..)(..)(..)
		        FILE    -overwrite -close -edex /awips2/data_store/sat/\(11)\(12)Z_\3_\7_\6-\8_\9_(seq).satz.%Y%m%d%H


	!!! note "[read more about pqact.conf in the LDM User Manual](https://www.unidata.ucar.edu/software/ldm/ldm-current/basics/pqact.conf.html)"
	!!! tip "[see available AWIPS LDM feeds](/edex/ldm/)"

---

## Start and Stop

to start all EDEX services, including the LDM:

    edex start
    
    Starting EDEX PostgreSQL:                                  [  OK  ]
    Starting httpd:                                            [  OK  ]
    Starting QPID                                              [  OK  ]
    Starting EDEX Camel (request): 
    Starting EDEX Camel (ingest): 
    Starting EDEX Camel (ingestGrib): 
    Starting AWIPS LDM:The product-queue is OK.

to stop:

    edex stop

    Stopping EDEX Camel (request): 
    Stopping EDEX Camel (ingest): 
    Stopping EDEX Camel (ingestGrib): 
    Stopping QPID                                              [  OK  ]
    Stopping httpd:                                            [  OK  ]
    Stopping EDEX PostgreSQL:                                  [  OK  ]
    Stopping AWIPS LDM:Stopping the LDM server...

To manually start, stop, and restart:

    service edex_postgres start
    service httpd-pypies start
    service qpidd start
    service edex_camel start

The fifth service, **edex_ldm**, does **not run at boot** to prevent filling up disk space if EDEX is not running. 

    ldmadmin start

To start *all services except the LDM* (good for troubleshooting):

    edex start base

To restart EDEX

    edex restart

---

## Directories to know

* `/awips2` - Contains all of the installed AWIPS software. 
* `/awips2/edex/logs` - EDEX logs.
* `/awips2/httpd_pypies/var/log/httpd` - httpd-pypies logs.
* `/awips2/data/pg_log` - PostgreSQL logs.
* `/awips2/qpid/log` - Qpid logs.
* `/awips2/edex/data/hdf5` - HDF5 data store. 
* `/awips2/edex/data/utility` - Localization store and configuration files. 
* `/awips2/ldm/etc` - Location of **ldmd.conf** and **pqact.conf**
* `/awips2/ldm/logs` - LDM logs.
* `/awips2/data_store` - Raw data store.
* `/awips2/data_store/ingest` - Manual data ingest endpoint.
