# Unidata AWIPS

AWIPS (formerly know as AWIPS II or AWIPS2) is a weather forecasting display and analysis package developed by the [National Weather Service](http://www.nws.noaa.gov/ost/SEC/AE/) and [Raytheon](http://www.raytheon.com/capabilities/products/awips/). AWIPS is a Java application consisting of a data-rendering client ([CAVE](#cave), which runs on Red Hat/CentOS Linux and Mac OS X) and a backend data server ([EDEX](#edex), which runs only on 64-bit CentOS or RedHat)

AWIPS takes a unified approach to data ingest, and most data types follow a standard path through the system, starting with an [LDM](#ldm) client requesting data from Unidata's [IDD](http://www.unidata.ucar.edu/projects/#idd), which are then decoded and stored as HDF5 and PostgreSQL/PostGIS metadata. Unidata supports two visualization frameworks for rendering AWIPS data: 1) [CAVE](#cave), 2) the [Python Data Access Framework (python-awips)](https://github.com/Unidata/python-awips).

# License

Unidata AWIPS source code and binaries (RPMs) are considered to be in the public domain, meaning there are no restrictions on any download, modification, or distribution in any form (original or modified).  Unidata AWIPS contains no proprietery content and is therefore not subject to export controls as stated in the [Master Rights](https://github.com/Unidata/awips2/blob/unidata_16.2.2/rpms/legal/Master_Rights_File.pdf) licensing file. 

# Install CAVE

# OS X client

Download [awips2-cave.dmg](http://www.unidata.ucar.edu/downloads/awips2/awips2-cave.dmg) (263 MB), click to open and drag to your Applications folder.  The application will write to a local data cache directory `~/Library/CAVE`.

<br>

# Windows client

32-bit [awips-cave.msi](http://www.unidata.ucar.edu/downloads/awips2/awips-cave.msi)

64-bit [awips-cave.amd64.msi](http://www.unidata.ucar.edu/downloads/awips2/awips-cave.amd64.msi)

<br>

# Linux client

Download and run the script [installCAVE.sh](http://www.unidata.ucar.edu/software/awips2/installCAVE.sh).

    wget http://www.unidata.ucar.edu/software/awips2/installCAVE.sh
    chmod 755 ./installCAVE.sh
    ./installCAVE.sh

This will install to `/awips2/cave` and `/awips2/alertviz` (as well as awips2 system directories like `/awips2/java` and `/awips2/python`).


## Requirements

* OpenGL 2.0
* [Latest NVIDIA driver](http://www.nvidia.com/Download/index.aspx?lang=en-us) for your graphics card.
* 1.5GB disk space (you should be prepared for more with caching datasets in `~/caveData`)
* All package dependencies should be resolved by yum.  Packages such as libXp, libXt, and openmotif will be picked up and installed along with CAVE. 

## How to run CAVE

    /awips2/cave/cave.sh

> AWIPS II was originally built for 32-bit Red Hat 5 (which is what the old AWIPS I system runs on).  As of 2016, 64-bit RHEL and CentOS 6 are supported, and Fedora Linux 9-12 should work as well.   **Unsupported distros** include CentOS 7, Ubuntu, Debian, and pretty much everthing else.

# AWIPS II Data in the Cloud

Unidata and Microsoft have partnered to offer a EDEX data server in the Azure cloud, open to the Unidata university community and the public.  Select the server in the Connectivity Preferences dialog, or enter **`edex-cloud.unidata.ucar.edu`** (without adding http:// before, or :9581/services after).

![EDEX in the cloud](../images/boEbFSf28t.gif)

## ~/caveData
You can always reset CAVE by manually removing the `~caveData` directory (on OS X remove `~/Library/CAVE`.  Then run `/awips2/cave/cave.sh` again and you will be prompted to connect to an EDEX server again.  Your local files have been removed, but if user and workstation-specific files exist on the EDEX server (meaning you are re-connecting to one you have used before), the remote files will sync again to `~/caveData` or `~/Library/CAVE` (custom colormaps, bundles, etc.). So even if you delete your local sync, you can retrieve any saved bundles from EDEX just by connecting again and selecting the files from the **File > Import** menu.

> If you are able to load wire-frame contours but not images, [update your video driver](http://www.nvidia.com/Download/index.aspx?lang=en-us). 

# Install EDEX

Download and run [installEDEX.sh](http://www.unidata.ucar.edu/software/awips2/installEDEX.sh) (64-bit CentOS or Red Hat)

    wget http://www.unidata.ucar.edu/software/awips2/installEDEX.sh
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
    
    edex log grib
    edex log radar
    edex log satellite
    edex log text
    edex log ldm 

#### Confirm EDEX Hostname

Check the file `/awips2/edex/bin/setup.env` to confirm that the EDEX hostname is resolvable both from inside and outside your machine.



![image](http://www.unidata.ucar.edu/software/awips2/images/awips2_coms.png)



# Software Components

The primary AWIPS II application for data ingest, processing, and storage is the Environmental Data EXchange (**EDEX**) server; the primary AWIPS II application for visualization/data manipulation is the Common AWIPS Visualization Environment (**CAVE**) client, which is typically installed on a workstation separate from other AWIPS II components.  

In addition to programs developed specifically for AWIPS, AWIPS II uses several commercial off-the-shelf (COTS) and Free or Open Source software (FOSS) products to assist in its operation. The following components, working together and communicating, compose the entire AWIPS II system.

### EDEX

The main server for AWIPS II.  Qpid sends alerts to EDEX when data stored by the LDM is ready for processing.  These Qpid messages include file header information which allows EDEX to determine the appropriate data decoder to use.  The default ingest server (simply named ingest) handles all data ingest other than grib messages, which are processed by a separate ingestGrib server.  After decoding, EDEX writes metadata to the database via Postgres and saves the processed data in HDF5 via PyPIES.   A third EDEX server, request, feeds requested data to CAVE clients. EDEX ingest and request servers are started and stopped with the commands `edex start` and `edex stop`, which runs the system script `/etc/rc.d/init.d/edex_camel`

### CAVE

Common AWIPS Visualization Environment. The data rendering and visualization tool for AWIPS II. CAVE contains of a number of different data display configurations called perspectives.  Perspectives used in operational forecasting environments include **D2D** (Display Two-Dimensional), **GFE** (Graphical Forecast Editor), and **NCP** (National Centers Perspective). CAVE is started with the command `/awips2/cave/cave.sh` or `cave.sh`

![CAVE](http://www.unidata.ucar.edu/software/awips2/images/Unidata_AWIPS2_CAVE.png)

### Alertviz

**Alertviz** is a modernized version of an AWIPS I application, designed to present various notifications, error messages, and alarms to the user (forecaster). AlertViz can be executed either independently or from CAVE itself.  In the Unidata CAVE client, Alertviz is run within CAVE and is not required to be run separately.  The toolbar is also **hidden from view** and is accessed by right-click on the desktop taskbar icon.

### LDM

[http://www.unidata.ucar.edu/software/ldm/](http://www.unidata.ucar.edu/software/ldm/)

The **LDM** (Local Data Manager), developed and supported by Unidata, is a suite of client and server programs designed for data distribution, and is the fundamental component comprising the Unidata Internet Data Distribution (IDD) system. In AWIPS II, the LDM provides data feeds for grids, surface observations, upper-air profiles, satellite and radar imagery and various other meteorological datasets.   The LDM writes data directly to file and alerts EDEX via Qpid when a file is available for processing.  The LDM is started and stopped with the commands `edex start` and `edex stop`, which runs the commands `service edex_ldm start` and `service edex_ldm stop`

### edexBridge

edexBridge, invoked in the LDM configuration file `/awips2/ldm/etc/ldmd.conf`, is used by the LDM to post "data available" messaged to Qpid, which alerts the EDEX Ingest server that a file is ready for processing.

### Qpid

[http://qpid.apache.org](http://qpid.apache.org)

**Apache Qpid**, the Queue Processor Interface Daemon, is the messaging system used by AWIPS II to facilitate communication between services.  When the LDM receives a data file to be processed, it employs **edexBridge** to send EDEX ingest servers a message via Qpid.  When EDEX has finished decoding the file, it sends CAVE a message via Qpid that data are available for display or further processing. Qpid is started and stopped by `edex start` and `edex stop`, and is controlled by the system script `/etc/rc.d/init.d/qpidd`

### PostgreSQL
[http://www.postgresql.org](http://www.postgresql.org)

**PostgreSQL**, known simply as Postgres, is a relational database management system (DBMS) which handles the storage and retrieval of metadata, database tables and some decoded data.  The storage and reading of EDEX metadata is handled by the Postgres DBMS.  Users may query the metadata tables by using the termainal-based front-end for Postgres called **psql**. Postgres is started and stopped by `edex start` and `edex stop`, and is controlled by the system script `/etc/rc.d/init.d/edex_postgres`

### HDF5

[http://www.hdfgroup.org/HDF5/](http://www.hdfgroup.org/HDF5/)

[**Hierarchical Data Format (v.5)**][hdf5] is the primary data storage format used by AWIPS II for processed grids, satellite and radar imagery and other products.   Similar to netCDF, developed and supported by Unidata, HDF5 supports multiple types of data within a single file.  For example, a single HDF5 file of radar data may contain multiple volume scans of base reflectivity and base velocity as well as derived products such as composite reflectivity.  The file may also contain data from multiple radars. HDF5 is stored in `/awips2/edex/data/hdf5/`

### PyPIES (httpd-pypies)

**PyPIES**, Python Process Isolated Enhanced Storage, was created for AWIPS II to isolate the management of HDF5 Processed Data Storage from the EDEX processes.  PyPIES manages access, i.e., reads and writes, of data in the HDF5 files.  In a sense, PyPIES provides functionality similar to a DBMS (i.e PostgreSQL for metadata); all data being written to an HDF5 file is sent to PyPIES, and requests for data stored in HDF5 are processed by PyPIES.

PyPIES is implemented in two parts: 1. The PyPIES manager is a Python application that runs as part of an Apache HTTP server, and handles requests to store and retrieve data. 2. The PyPIES logger is a Python process that coordinates logging. PyPIES is started and stopped by `edex start` and `edex stop`, and is controlled by the system script `/etc/rc.d/init.d/https-pypies` 


