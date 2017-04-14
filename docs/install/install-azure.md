---
layout: default
type: guide
shortname: Docs
title: Install on Azure
subtitle: Install & Config
---

# Azure Portal

* create new virtual machine, CentOS 6.7
* network rules for ports
* disk drive mount
* iptables
* 



> All of these commands are issued as root user!

Create user **awips** and group **fxalpha** and create AWIPS directories.

    groupadd fxalpha
    useradd -G fxalpha awips

or add the existing user to the new group:

    groupadd fxalph
    usermod -a -G fxalpha awips
    
`/mnt/resource` is a temporary scratch disk on Azure Linux VMs, which makes it an ideal spot for the LDM Raw Data Store (since we don't care about losing the files which would be purged within one hour anyway.

    mkdir /awips2
    ln -s /mnt/resource /awips2/data_store

Mount an Azure SSD to `/awips2/edex/data`see `dmesg|grep sdc` to know if you have one configured):

    fdisk /dev/sdc
    mkfs -t ext4 /dev/sdc1
    mkdir -p /awips2/edex/data
    mount /dev/sdc1 /awips2/edex/data
   
and in fstab

    UUID=0ed45b61-1b93-4d5e-a03c-0adc5ffce62a   /awips2/edex/data   ext4   defaults,discard   1   2

where UUID is found with the command `ls -al /dev/disk/by-uuid`

Your system looks like this now

    Filesystem      Size  Used Avail Use% Mounted on
    /dev/sda1        30G  2.6G   26G  10% /
    tmpfs            28G     0   28G   0% /dev/shm
    /dev/sdb1       111G   60M  105G   1% /mnt/resource
    /dev/sdc1      1007G   72M  956G   1% /awips2/edex/data
    
and after install

    Filesystem      Size  Used Avail Use% Mounted on
    /dev/sda1        30G  8.2G   20G  30% /
    tmpfs            28G     0   28G   0% /dev/shm
    /dev/sdb1       111G   60M  105G   1% /mnt/resource
    /dev/sdc1      1007G  2.3G  954G   1% /awips2/edex/data


* /dev/sda1 will contain the /awips2 software installation
* /dev/sdb1 will contain the LDM raw data store (sym link from /awips2/data_store)
* /dev/sdc1 will contain the EDEX processed sata store (mounted on /awips2/edex/data 

2. **/etc/sysconfig/iptables**

    To serve data from an EDEX server, iptables must allow TCP connections on ports **5672**, **9581** and **9582**. The following lines added to `/etc/sysconfig/iptables`, followed by the command `service iptables restart`, will configure iptables for EDEX.
    
        -A INPUT -p tcp -m tcp --dport 5672 -j ACCEPT
        -A INPUT -p tcp -m tcp --dport 9581 -j ACCEPT
        -A INPUT -p tcp -m tcp --dport 9582 -j ACCEPT

# Linux Download

For 64-bit RHEL/CentOS 5 and 6, download and run the script [installEDEX.sh](http://www.unidata.ucar.edu/software/awips2/installEDEX.sh):

    wget http://www.unidata.ucar.edu/software/awips2/installEDEX.sh
    chmod 755 ./installEDEX.sh
    ./installEDEX.sh

This will install to `/awips2/edex`, `/awips2/data` and other directories.

> CentOS/RHEL 5 and 6 are the only supported operating systems for EDEX (Though you may have luck with Fedora Core 12 to 14 and Scientific Linux). Not supported for EDEX: Debian, Ubuntu, SUSE, Solaris, OS X, Fedora 15+, CentOS/RHEL 7, Windows

## Be Aware...

- selinux should be **disabled** [(read more about selinux at redhat.com)](https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Security-Enhanced_Linux/sect-Security-Enhanced_Linux-Enabling_and_Disabling_SELinux-Disabling_SELinux.html)
    
- Security Limits - **/etc/security/limits.conf**
 
    Qpid is known to crash on systems without a high security limit for user processes and files. The file `/etc/security/limits.conf` defines the number of each for the awips user (This is automatically configured by the `installEDEX.sh` script).
    
        awips soft nproc 65536
        awips soft nofile 65536
    
LDM config
    
    regutil /hostname -s edex-cloud.westus.cloudapp.azure.com
    regutil /queue/size -s 2500M
       
    127.0.0.1   localhost localhost.localdomain localhost4 localhost4.localdomain4 edex-cloud.westus.cloudapp.azure.com
    ::1         localhost localhost.localdomain localhost6 localhost6.localdomain6 edex-cloud.westus.cloudapp.azure.com


# What does installEDEX.sh do?

1. Downloads [http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo](http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo) to `/etc/yum.repos.d/awips2.repo`
2. Runs `yum clean all`
3. Runs `yum groupinstall awips2-server`
