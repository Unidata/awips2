# Install AWIPS II

These scripts should be run as *root*:

## CAVE Client

Download and run `installCAVE.sh`

        wget https://raw.githubusercontent.com/Unidata/awips2/unidata_15.1.1/installCAVE.sh
        chmod 755 ./installCAVE.sh
        ./installCAVE.sh

## EDEX Server

Download and run `installEDEX.sh`:

        wget https://raw.githubusercontent.com/Unidata/awips2/unidata_15.1.1/installEDEX.sh
        chmod 755 ./installEDEX.sh
        ./installEDEX.sh

## What do these scripts do?

1. Downloads [http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo](http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo) to `/etc/yum.repos.d/awips2.repo`
2. Runs `yum clean all`
3. Runs `yum groupinstall awips2-server` and/or `yum groupinstall awips2-cave`

## System Requirements

1. EDEX operations systems supported:

   * CentOS 5 and 6
   * Red Hat 5 and 6
   * Fedora Core 12 to 14 
 
    Not supported for EDEX:
 
   * Debian, Ubuntu, SUSE, Solaris, OS X, Fedora 15+, CentOS/RHEL 7, Windows
   
   It will probably work on Scientific Linux. 
 
2. selinux should be **disabled** [(read more about selinux at redhat.com)](https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Security-Enhanced_Linux/sect-Security-Enhanced_Linux-Enabling_and_Disabling_SELinux-Disabling_SELinux.html)

3. create user `awips` and group `fxalpha` and create AWIPS II directories

        groupadd fxalpha && useradd -G fxalpha awips
        mkdir -p /awips2/data_store

4. `/etc/security/limits.conf`
 
    Qpid is known to crash on some systems which have not defined a higher security limit for user processes and files. To be sure that Qpid can handle the high number of messages from edexBridge, add the following two lines to `/etc/security/limits.conf`
    
	printf "awips soft nproc 65536\nawips soft nofile 65536\n" >> /etc/security/limits.conf

Or copy manually:

        awips soft nproc 65536
        awips soft nofile 65536
   
5. `/etc/sysconfig/iptables`

    To serve data from an EDEX server, iptables must allow TCP connections on ports **5672**, **9581** and **9582**. The following lines added to `/etc/sysconfig/iptables`, followed by the command `service iptables restart`, will configure iptables for EDEX.
    
        -A INPUT -p tcp -m tcp --dport 5672 -j ACCEPT
        -A INPUT -p tcp -m tcp --dport 9581 -j ACCEPT
        -A INPUT -p tcp -m tcp --dport 9582 -j ACCEPT


