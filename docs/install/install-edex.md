# Install EDEX

EDEX is the **E**nvironmental **D**ata **Ex**change system that represents the backend server for AWIPS.  EDEX is only supported for Linux systems: CentOS and RHEL, and ideally, it should be on its own dedicated machine.  It requires administrator priviledges to make root-level changes. EDEX can run on a single machine or be spread across multiple machines.  To learn more about that please look at [Distributed EDEX, Installing Across Multiple Machines](../../edex/distributed-computing/)

---

## Latest Version

**20.3.2-1**

[**View release notes**](https://www.unidata.ucar.edu/blogs/news/tags/awips-release)

!!! note "Version 20.\* of CAVE is not compatible with Version 18.\* EDEX and vice versa, Version 18.\* of CAVE is not compatible with Version 20.\* EDEX."

---

## Functionality/Reporting

If you come across issues/bugs/missing functionality, we also encourage you to <a href="https://docs.google.com/forms/d/e/1FAIpQLSf6jyZtbh49g-GCBoAQYzTVwAIf_aKz0QOeAr7gDVFhPrjAmw/viewform?usp=sf_link">report it using this short form</a>.

---


## System requirements

- 64-bit CentOS/RHEL 7

!!! note "While CentOS8 has reach End of Life as of Dec. 31, 2021, CentOS7 End of Life isn't until June 30, 2024."  

- Bash shell environment
- 16+ CPU cores (each CPU core can run a decorder in parallel)
- 24GB RAM
- 700GB+ Disk Space
- gcc-c++ package
- A **Solid State Drive (SSD)** is recommended
    - A SSD should be mounted either to `/awips2` (to contain the entire EDEX system) or to `/awips2/edex/data/hdf5` (to contain the large files in the decoded data store). EDEX can scale to any system by adjusting the incoming LDM data feeds or adjusting the resources (CPU threads) allocated to each data type.

!!! note "EDEX is only supported for 64-bit CentOS and RHEL 7 Operating Systems."

!!! warning "EDEX is **not** supported in Debian, Ubuntu, SUSE, Solaris, macOS, or Windows. You may have luck with Fedora Core 12 to 14 and Scientific Linux, but we will not provide support."

---

## Download and Installation Instructions

The first 3 steps should all be run as **root**

### 1. Install EDEX

Download and run the installer: [**awips_install.sh** <i class="fa fa-download"></i>](https://downloads.unidata.ucar.edu/awips2/current/linux/awips_install.sh)

```
wget https://downloads.unidata.ucar.edu/awips2/current/linux/awips_install.sh
chmod 755 awips_install.sh
sudo ./awips_install.sh --edex
```


!!! note "**awips_install.sh --edex** will perform the following steps (it's always a good idea to review downloaded shell scripts):"

       1. Checks to see if EDEX is currently running, if so stops the processes with the `edex stop` command
       2. If EDEX is installed, asks the user if it can be removed and where to backup the data to and does a `yum groupremove awips2-server`
       3. If the user/group awips:fxalpha does not exist, it gets created
       4. Saves the appropriate yum repo file to `/etc/yum.repos.d/awips2.repo`
       5. Increases process and file limits for the the *awips* account in `/etc/security/limits.conf`
       6. Creates `/awips2/data_store` if it does not exist already
       7. Runs `yum groupinstall awips2-server`

!!! warning "If you receive an error relating to yum, then please run"

    ```
    sudo su - -c "[PATH_TO_INSTALL_FILE]/awips_install.sh --edex"
    ```


### 2. EDEX Setup 
The external and localhost addresses need to be specified in `/etc/hosts`

```
127.0.0.1         localhost    localhost.localdomain
XXX.XXX.XXX.XXX   edex-cloud   edex-cloud.unidata.ucar.edu

```

### 3. Configure iptables

This should be a one time configuration change. Configure iptables to allow TCP connections on ports 9581 and 9582 if you want to serve data publicly to CAVE clients and the Python API.

#### Open Port 9588

If you are running a Registry (Data Delivery) server, you will also want to open port **9588**.

##### To open ports to all connections

```
vi /etc/sysconfig/iptables

*filter
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
-A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
-A INPUT -p icmp -j ACCEPT
-A INPUT -i lo -j ACCEPT
-A INPUT -m state --state NEW -m tcp -p tcp --dport 22 -j ACCEPT
-A INPUT -m state --state NEW -m tcp -p tcp --dport 9581 -j ACCEPT
-A INPUT -m state --state NEW -m tcp -p tcp --dport 9582 -j ACCEPT
#-A INPUT -m state --state NEW -m tcp -p tcp --dport 9588 -j ACCEPT # for registry/dd
-A INPUT -j REJECT --reject-with icmp-host-prohibited
-A FORWARD -j REJECT --reject-with icmp-host-prohibited
COMMIT
```

##### To open ports to specific IP addresses

In this example, the IP range `128.117.140.0/24` will match all 128.117.140.\* addresses, while `128.117.156.0/24` will match 128.117.156.\*.

```
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
-A EDEX -m state --state NEW -p tcp --dport 9581 -j ACCEPT
-A EDEX -m state --state NEW -p tcp --dport 9582 -j ACCEPT
#-A EDEX -m state --state NEW -p tcp --dport 9588 -j ACCEPT # for registry/dd
-A EDEX -j REJECT
COMMIT
```

#### Restart iptables

```
service iptables restart
```

#### Troubleshooting 

For CentOS 7 error:

```
Redirecting to /bin/systemctl restart  iptables.service 
Failed to restart iptables.service: Unit iptables.service failed to load: No such file or directory.
```


The solution is:
```
yum install iptables-services
systemctl enable iptables
service iptables restart
```

### 4. Start EDEX

!!! note "These steps should be run as user *awips* with sudo.  Switch to the user by running `su - awips`."

```
edex start
```
To manually start, stop, and restart:
```
service edex_postgres start
service httpd-pypies start
service qpidd start
service edex_camel start
```
The fifth service, **edex_ldm**, does **not run at boot** to prevent filling up disk space if EDEX is not running. Start ldm manually:
```
service edex_ldm start
```
To restart EDEX
```
edex restart
```

---

## Additional Notes

### Ensure SELinux is Disabled

```
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
```

!!! note "Read more about selinux at [redhat.com](https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Security-Enhanced_Linux/sect-Security-Enhanced_Linux-Enabling_and_Disabling_SELinux-Disabling_SELinux.html)"

---

### SSD Mount

Though a Solid State Drive is not required, it is *strongly encouraged* in order to handle the amount of disk IO for real-time IDD feeds.

The simplest configuration would be to mount an 500GB+ SSD to **/awips2** to contain both the installed software (approx. 20GB) and the real-time data (approx. 150GB per day).

The default [purge rules](/edex/data-purge/) are configured such that the processed data in **/awips2** does not exceed 450GB. The raw data is located in **/awips2/data_store**, and is scoured every hour and should not exceed 50GB.

If you want to increase EDEX data retention you should mount a large disk to **/awips2/edex/data/hdf5** since this will be where the archived processed data exists, and any case studies created.

```
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda1        30G  2.5G   26G   9% /
tmpfs            28G     0   28G   0% /dev/shm
/dev/sdc1       788G   81G  667G  11% /awips2
/dev/sdb1       788G   41G  708G  10% /awips2/edex/data/hdf5
```

---

### Configure LDM Feeds

EDEX installs its own version of the LDM to the directory **/awips2/ldm**.  As with a the default LDM configuration, two files are used to control what IDD feeds are ingested:

#### Configuration file: /awips2/ldm/etc/ldmd.conf

This file specifies an upstream LDM server to request data from, and what feeds to request:

```
REQUEST NEXRAD3 "./p(DHR|DPR|DSP|DTA|DAA|DVL|EET|HHC|N0Q|N0S|N0U|OHA|NVW|NTV|NST)." idd.unidata.ucar.edu
REQUEST FNEXRAD|IDS|DDPLUS|UNIWISC ".*" idd.unidata.ucar.edu
REQUEST NGRID ".*" idd.unidata.ucar.edu
REQUEST NOTHER "^TIP... KNES.*" idd.unidata.ucar.edu
```

!!! note "[Read more about ldmd.conf in the LDM User Manual](https://www.unidata.ucar.edu/software/ldm/ldm-current/basics/ldmd.conf.html)"

#### Configuration File: /awips2/ldm/etc/pqact.conf

This file specifies the WMO headers and file pattern actions to request:

```
# Redbook graphics
ANY     ^([PQ][A-Z0-9]{3,5}) (....) (..)(..)(..) !redbook [^/]*/([^/]*)/([^/]*)/([^/]*)/([0-9]{8})
        FILE    -overwrite -close -edex /awips2/data_store/redbook/\8/\4\5Z_\8_\7_\6-\1_\2_(seq).rb.%Y%m%d%H
# NOAAPORT GINI images
NIMAGE  ^(sat[^/]*)/ch[0-9]/([^/]*)/([^/]*)/([^ ]*) ([^/]*)/([^/]*)/([^/]*)/ (T[^ ]*) ([^ ]*) (..)(..)(..)
        FILE    -overwrite -close -edex /awips2/data_store/sat/\(11)\(12)Z_\3_\7_\6-\8_\9_(seq).satz.%Y%m%d%H
```

!!! note "[Read more about pqact.conf in the LDM User Manual](https://www.unidata.ucar.edu/software/ldm/ldm-current/basics/pqact.conf.html)"
!!! tip "[See available AWIPS LDM feeds](/edex/ldm/)"

#### Configuration File: /awips2/ldm/etc/registry.xml

This file specifies configuration and runtime parameters. If you are pulling in a lot of data, you may want to consider increasing your LDM queue size:

```
  <queue>
    <path>/awips2/ldm/var/queues/ldm.pq</path>
    <size>24GB</size>
    <slots>default</slots>
  </queue>
```

!!! note "[Read more about registry.xml in the LDM User Manual](https://www.unidata.ucar.edu/software/ldm/ldm-current/basics/LDM-registry.html)"

---

### Directories to Know

* `/awips2` - Contains all of the installed AWIPS software.
* `/awips2/edex/logs` - EDEX logs.
* `/awips2/httpd_pypies/var/log/httpd` - httpd-pypies logs.
* `/awips2/database/data/pg_log` - PostgreSQL logs.
* `/awips2/qpid/log` - Qpid logs.
* `/awips2/edex/data/hdf5` - HDF5 data store.
* `/awips2/edex/data/utility` - Localization store and configuration files.
* `/awips2/ldm/etc` - Location of **ldmd.conf** and **pqact.conf**
* `/awips2/ldm/logs` - LDM logs.
* `/awips2/data_store` - Raw data store.
* `/awips2/data_store/ingest` - Manual data ingest endpoint.

---

### What Version is my EDEX?
```
rpm -qa | grep awips2-edex
```

---

## Uninstalling EDEX
These are instructions to manually uninstall EDEX. However, the [`awips_install.sh`](#1-install-edex) script will do all of these steps for you if you are installing a newer version of EDEX.

**1. Make sure all EDEX processes are stopped**
```
sudo edex stop
sudo edex status

[edex status]
 postgres    :: not running
 pypies      :: not running
 qpid        :: not running
 EDEXingest  :: not running
 EDEXgrib    :: not running
 EDEXrequest :: not running
 ldmadmin    :: not running
```

**2. Backup any important configuration files that you may want to reference**

Here are some possible important directories/files to backup:

```
/awips2/database/data/pg_hba.conf
/awips2/edex/data/utility/*
/awips2/edex/bin/*
/awips2/ldm/*
/awips2/dev/*
/awips2/edex/conf*
/awips2/edex/etc/*
/awips2/edex/logs/*
/usr/bin/edex/*
/etc/init.d/edexServiceList
```

**3. See what AWIPS yum groups are currently installed**

In this case the ```AWIPS EDEX Server``` group is installed

```
sudo yum grouplist

Available Environment Groups:
   Minimal Install
   Compute Node
   Infrastructure Server
   File and Print Server
   Cinnamon Desktop
   MATE Desktop
   Basic Web Server
   Virtualization Host
   Server with GUI
   GNOME Desktop
   KDE Plasma Workspaces
   Development and Creative Workstation
Installed Groups:
   AWIPS EDEX Server
   Development Tools
Available Groups:
   AWIPS ADE SERVER
   AWIPS CAVE
   AWIPS Development
   AWIPS EDEX DAT Server
   AWIPS EDEX Database/Request Server
   AWIPS EDEX Decode/Ingest Node (No Database, PyPIES, GFE)
   Cinnamon
   Compatibility Libraries
   Console Internet Tools
   Educational Software
   Electronic Lab
   Fedora Packager
   General Purpose Desktop
   Graphical Administration Tools
   Haskell
   LXQt Desktop
   Legacy UNIX Compatibility
   MATE
   Milkymist
   Scientific Support
   Security Tools
   Smart Card Support
   System Administration Tools
   System Management
   TurboGears application framework
   Xfce
```

**4. Remove any currently installed AWIPS yum groups**
```
sudo yum clean all
sudo yum groupremove "AWIPS EDEX Server"
```

!!! note "If you are having trouble removing a group, see the [troubleshooting](../appendix/common-problems.md#troubleshooting-uninstalling-edex) section."

**5. Check to make sure all awips rpms have been removed**
```
rpm -qa | grep awips2
```

If you still have rpms installed, remove them

```
sudo yum remove awips2-*
```

**6. Remove everything in the /awips2 directory**
```
rm -rf /awips2/*
```
