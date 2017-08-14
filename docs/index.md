# Unidata AWIPS User Manual

---

[ldm]: http://www.unidata.ucar.edu/software/ldm/
[idd]: http://www.unidata.ucar.edu/projects/#idd
[gempak]: http://www.unidata.ucar.edu/software/gempak/
[awips2]: http://www.unidata.ucar.edu/software/awips2/
[ncep]: http://www.ncep.noaa.gov
[apache]: http://httpd.apache.org
[postgres]: www.postgresql.org
[hdf5]: http://www.hdfgroup.org/HDF5/
[eclipse]: http://www.eclipse.org
[camel]: http://camel.apache.org/ 
[spring]: http://www.springsource.org/ 
[hibernate]: http://www.hibernate.org/ 
[qpid]: http://qpid.apache.org 


[Unidata AWIPS](http://www.unidata.ucar.edu/software/awips2/) is a meteorological display and analysis package originally developed by the [National Weather Service](http://www.nws.noaa.gov/ost/SEC/AE/) and [Raytheon](http://www.raytheon.com/capabilities/products/awips/), repackaged by Unidata to support non-operational use in research and education by [UCAR member institutions](http://president.ucar.edu/governance/members/universities-representatives).

AWIPS takes a unified approach to data ingest, and most data types follow a path through the system starting with an [LDM](#ldm) client requesting data from the [Unidata IDD](http://www.unidata.ucar.edu/projects/#idd). These data files are then decoded and stored as HDF5 and Postgres metadata by [EDEX](install/install-edex). 

Unidata supports two visualization frameworks for rendering data: [CAVE](install/install-cave), and the Python Data Access Framework ([python-awips](http://python-awips.readthedocs.io)).

---

## Install CAVE 17.1.1

|                |                        |
|----------------|-----------------------:|
| Linux x86_64   | [installCAVE.sh <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/software/awips2/installCAVE.sh)         |
| macOS	         | Download and install both<br>[awips2-cave-17.1.1.dmg <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips2-cave-17.1.1.dmg)<br>[awips-python-jep.pkg <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips-python-jep.pkg)|
| 32-bit Windows | [awips-cave.msi <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips-cave.msi)        |
| 64-bit Windows | [awips-cave.amd64.msi <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips-cave.amd64.msi)  |

---

## EDEX Data Server 17.1.1

|                |                        |
|----------------|-----------------------:|
| Linux x86_64   | [installEDEX.sh <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/software/awips2/installEDEX.sh)         |

[Read full EDEX install instructions...](install/install-edex)

---

## License

Unidata AWIPS source code and binaries (RPMs) are considered to be in the public domain, meaning there are no restrictions on any download, modification, or distribution in any form (original or modified).  Unidata AWIPS contains no proprietery content and is therefore not subject to export controls as stated in the [Master Rights](https://github.com/Unidata/awips2/blob/unidata_16.2.2/rpms/legal/Master_Rights_File.pdf) licensing file. 

<img style="float:right;width:450px;" src="http://www.unidata.ucar.edu/software/awips2/images/awips2_coms.png">

---

## Software Components

* [EDEX](#edex)
* [CAVE](#cave)
* [LDM](#ldm)
* [edexBridge](#edexbridge)
* [Qpid](#qpid)
* [PostgreSQL](#postgresql)
* [HDF5](#hdf5)
* [PyPIES](#pypies)


The primary AWIPS application for data ingest, processing, and storage is the Environmental Data EXchange (**EDEX**) server; the primary AWIPS application for visualization/data manipulation is the Common AWIPS Visualization Environment (**CAVE**) client, which is typically installed on a workstation separate from other AWIPS components.  

In addition to programs developed specifically for AWIPS, AWIPS uses several commercial off-the-shelf (COTS) and Free or Open Source software (FOSS) products to assist in its operation. The following components, working together and communicating, compose the entire AWIPS system.

### EDEX

The main server for AWIPS.  Qpid sends alerts to EDEX when data stored by the LDM is ready for processing.  These Qpid messages include file header information which allows EDEX to determine the appropriate data decoder to use.  The default ingest server (simply named ingest) handles all data ingest other than grib messages, which are processed by a separate ingestGrib server.  After decoding, EDEX writes metadata to the database via Postgres and saves the processed data in HDF5 via PyPIES.   A third EDEX server, request, feeds requested data to CAVE clients. EDEX ingest and request servers are started and stopped with the commands `edex start` and `edex stop`, which runs the system script `/etc/rc.d/init.d/edex_camel`

* [Read More: How to Install EDEX](install/install-edex)

### CAVE

Common AWIPS Visualization Environment. The data rendering and visualization tool for AWIPS. CAVE contains of a number of different data display configurations called perspectives.  Perspectives used in operational forecasting environments include **D2D** (Display Two-Dimensional), **GFE** (Graphical Forecast Editor), and **NCP** (National Centers Perspective). CAVE is started with the command `/awips2/cave/cave.sh` or `cave.sh`

* [Read More: How to Install CAVE](install/install-cave)

![CAVE](http://www.unidata.ucar.edu/software/awips2/images/Unidata_AWIPS2_CAVE.png)

### LDM

[http://www.unidata.ucar.edu/software/ldm/](http://www.unidata.ucar.edu/software/ldm/)

The **LDM** (Local Data Manager), developed and supported by Unidata, is a suite of client and server programs designed for data distribution, and is the fundamental component comprising the Unidata Internet Data Distribution (IDD) system. In AWIPS, the LDM provides data feeds for grids, surface observations, upper-air profiles, satellite and radar imagery and various other meteorological datasets.   The LDM writes data directly to file and alerts EDEX via Qpid when a file is available for processing.  The LDM is started and stopped with the commands `edex start` and `edex stop`, which runs the commands `service edex_ldm start` and `service edex_ldm stop`

### edexBridge

edexBridge, invoked in the LDM configuration file `/awips2/ldm/etc/ldmd.conf`, is used by the LDM to post "data available" messaged to Qpid, which alerts the EDEX Ingest server that a file is ready for processing.

### Qpid

[http://qpid.apache.org](http://qpid.apache.org)

**Apache Qpid**, the Queue Processor Interface Daemon, is the messaging system used by AWIPS to facilitate communication between services.  When the LDM receives a data file to be processed, it employs **edexBridge** to send EDEX ingest servers a message via Qpid.  When EDEX has finished decoding the file, it sends CAVE a message via Qpid that data are available for display or further processing. Qpid is started and stopped by `edex start` and `edex stop`, and is controlled by the system script `/etc/rc.d/init.d/qpidd`

### PostgreSQL

[http://www.postgresql.org](http://www.postgresql.org)

**PostgreSQL**, known simply as Postgres, is a relational database management system (DBMS) which handles the storage and retrieval of metadata, database tables and some decoded data.  The storage and reading of EDEX metadata is handled by the Postgres DBMS.  Users may query the metadata tables by using the termainal-based front-end for Postgres called **psql**. Postgres is started and stopped by `edex start` and `edex stop`, and is controlled by the system script `/etc/rc.d/init.d/edex_postgres`

### HDF5

[http://www.hdfgroup.org/HDF5/](http://www.hdfgroup.org/HDF5/)

[**Hierarchical Data Format (v.5)**][hdf5] is the primary data storage format used by AWIPS for processed grids, satellite and radar imagery and other products.   Similar to netCDF, developed and supported by Unidata, HDF5 supports multiple types of data within a single file.  For example, a single HDF5 file of radar data may contain multiple volume scans of base reflectivity and base velocity as well as derived products such as composite reflectivity.  The file may also contain data from multiple radars. HDF5 is stored in `/awips2/edex/data/hdf5/`

### PyPIES (httpd-pypies)

**PyPIES**, Python Process Isolated Enhanced Storage, was created for AWIPS to isolate the management of HDF5 Processed Data Storage from the EDEX processes.  PyPIES manages access, i.e., reads and writes, of data in the HDF5 files.  In a sense, PyPIES provides functionality similar to a DBMS (i.e PostgreSQL for metadata); all data being written to an HDF5 file is sent to PyPIES, and requests for data stored in HDF5 are processed by PyPIES.

PyPIES is implemented in two parts: 1. The PyPIES manager is a Python application that runs as part of an Apache HTTP server, and handles requests to store and retrieve data. 2. The PyPIES logger is a Python process that coordinates logging. PyPIES is started and stopped by `edex start` and `edex stop`, and is controlled by the system script `/etc/rc.d/init.d/https-pypies` 

