# Unidata AWIPS User Manual

---

[ldm]: https://www.unidata.ucar.edu/software/ldm/
[idd]: https://www.unidata.ucar.edu/projects/#idd
[gempak]: https://www.unidata.ucar.edu/software/gempak/
[awips2]: https://www.unidata.ucar.edu/software/awips2/
[ncep]: http://www.ncep.noaa.gov
[apache]: http://httpd.apache.org
[postgres]: www.postgresql.org
[hdf5]: http://www.hdfgroup.org/HDF5/
[eclipse]: http://www.eclipse.org
[camel]: http://camel.apache.org/ 
[spring]: http://www.springsource.org/ 
[hibernate]: http://www.hibernate.org/ 
[qpid]: http://qpid.apache.org 


 [Unidata AWIPS](https://www.unidata.ucar.edu/software/awips2/) is a modified and extended version of AWIPS (originally developed by NWS and Raytheon) for non-operational use in research and education by [UCAR member institutions](http://president.ucar.edu/governance/members/universities-representatives).

AWIPS takes a unified approach to data ingest, and most data types follow a path through the system starting with an [LDM](#ldm) client requesting data from the [Unidata IDD](https://www.unidata.ucar.edu/projects/#idd). Various raw data and product files (grib, BUFR, text, gini, McIDAS, NetCDF, more) are  decoded and stored as HDF5 and Postgres metadata by [EDEX](install/install-edex), which then serves these data and products to  clients over http. 

Unidata supports two visualization frameworks for rendering data: [CAVE](install/install-cave), and the Python Data Access Framework ([python-awips](http://python-awips.readthedocs.io)).

---

## Download and Install CAVE

> [Release 17.1.1-6, June 13, 2018](https://www.unidata.ucar.edu/blogs/news/category/AWIPS)

|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-linux"></i> Linux  </h1> | <h4>[install.sh --cave <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/software/awips2/install.sh)  </h4>       <p>For CentOS/Red Hat 6 and 7. Installs to /awips2/cave and writes files to ~/caveData.</p> <tt><code>chmod 755 install.sh<br>sudo ./install.sh --cave</code></tt><p>Run CAVE from the Linux Desktop menu Applications > Internet > AWIPS CAVE, or from the command line as simply `cave`.</p> <p><img src="images/cave_linux_menu.png" style="width:300px;"></p><div class="admonition note"><p class="admonition-title">System Requirements</p><ul><li>x86_64 CentOS/RHEL 6 or 7</li><li>OpenGL 2.0 capable device</li><li>4GB RAM</li><li><a href="http://www.nvidia.com/Download/index.aspx?lang=en-us">Latest NVIDIA driver</a></li><li>approx. 2GB disk space for data caching (~/caveData)</li></ul></div><p>You can reset CAVE at any time by removing the **~/caveData** directory (on macOS **~/Library/caveData**) and reconnecting to an EDEX server. </p>  |


|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-apple"></i> macOS	      </h1>|<h4>    Download and install both<br>[awips2-cave-17.1.1-6.dmg <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips2-cave-17.1.1-6.dmg)<br>[awips-python.pkg <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-python.pkg)</h4> <p>**Supported Graphics Devices for macOS**<li>Intel HD Graphics</li><li>Intel Iris</li><li>NVIDIA GeForce</li></p><p>**Unsupported Graphics Devices for macOS**<li>AMD Radeon R9</li><li>AMD Radeon Pro</li><li>AMD FirePro D300</li></p> <p>Writes and syncs files to ~/Library/caveData.</p> <p>**awips-python.pkg** is not a prerequisite, and CAVE will still run and display data without it, but to use any derived parameter functions such as wind barbs/arrows and grid parameters on various vertical coordinates, jep must be installed in some way (it is assumed in /Library/Python/2.7/site-packages/jep/)</p>|



|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-windows"></i> 32-bit Windows </h1> | <h4> [awips-cave.msi <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-cave.msi)        </h4> <p>Windows clients are still based on the CAVE 16.2.2 code base and provided in lieu of no 17.1.1 client.</p> <p>Writes files to caveData in the user's home directory.</p>  |
| <h1><i class="fa fa-windows"></i> 64-bit Windows </h1> | <h4> [awips-cave.amd64.msi <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-cave.amd64.msi)  </h4>  <p>Windows clients are still based on the CAVE 16.2.2 code base and provided in lieu of no 17.1.1 client.</p> <p>Writes files to caveData in the user's home directory.</p> <div class="admonition warning"><p class="admonition-title">Beta status</p><p>Client-side Python scripts (including Derived Parameters) do not work on Windows</p></div>|


> [Read full CAVE install instructions](install/install-cave)

---

## Download and Install EDEX

> [Release 17.1.1-6, June 13, 2018](https://www.unidata.ucar.edu/blogs/news/category/AWIPS)

|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-linux"></i> Linux  </h1> | <h4>[install.sh --edex <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/software/awips2/install.sh)  </h4>       <p>Installs to /awips2/ directories.</p> <tt><code>chmod 755 install.sh<br>sudo ./install.sh --edex</code></tt><p>Start and Stop:</p><p><tt>edex start<br>edex stop</tt></p><div class="admonition note"><p class="admonition-title">System Requirements</p><ul><li>x86_64 CentOS/RHEL 6 or 7</li><li>16+ CPU cores (each CPU core is one more decoder which can run in parallel)</li><li>24GB RAM</li><li>700GB+ disk space</li>|

> [Read full EDEX install instructions](install/install-edex)

---

## License

Unidata AWIPS source code and binaries (RPMs) are considered to be in the public domain, meaning there are no restrictions on any download, modification, or distribution in any form (original or modified).  Unidata AWIPS contains no proprietery content and is therefore not subject to export controls as stated in the [Master Rights](https://github.com/Unidata/awips2/blob/unidata_16.2.2/rpms/legal/Master_Rights_File.pdf) licensing file. 

---

## AWIPS Data in the Cloud

Unidata and XSEDE Jetstream have partnered to offer a EDEX data server in the cloud, open to the Unidata university community.  Select the server in the Connectivity Preferences dialog, or enter **`edex-cloud.unidata.ucar.edu`** (without *http://* before, or *:9581/services* after).

![EDEX in the cloud](/images/boEbFSf28t.gif)

---

## Distributed Computing

AWIPS makes use of service-oriented architecture to request, process, and serve real-time meteorological data.  While originally developed for use on internal NWS forecast office networks, where operational installations of AWIPS can consist of a dozen servers or more, because the AWIPS source code was hard-coded with the NWS network configuration, the early Unidata releases were stripped of operation-specific configurations and plugins, and released specifically for standalone installation. This made sense given that a single EDEX instance with a Solid State Drive could handle most of the entire NOAAport data volume.  However, with GOES-R(16) coming online, and more gridded forecast models being created at finer temporal and spatial resolutions, there was now a need to distribute EDEX data decoding in order to handle this firehose of data.

* Read More: [Distributed EDEX](edex/distributed-computing)

---

<img style="float:right;width:450px;" src="https://www.unidata.ucar.edu/software/awips2/images/awips2_coms.png">
## Software Components

* [EDEX](#edex)
* [CAVE](#cave)
* [LDM](#ldm)
* [edexBridge](#edexbridge)
* [Qpid](#qpid)
* [PostgreSQL](#postgresql)
* [HDF5](#hdf5)
* [PyPIES](#pypies)

### EDEX

The main server for AWIPS.  Qpid sends alerts to EDEX when data stored by the LDM is ready for processing.  These Qpid messages include file header information which allows EDEX to determine the appropriate data decoder to use.  The default ingest server (simply named ingest) handles all data ingest other than grib messages, which are processed by a separate ingestGrib server.  After decoding, EDEX writes metadata to the database via Postgres and saves the processed data in HDF5 via PyPIES.   A third EDEX server, request, feeds requested data to CAVE clients. EDEX ingest and request servers are started and stopped with the commands `edex start` and `edex stop`, which runs the system script `/etc/rc.d/init.d/edex_camel`

* [Read More: How to Install EDEX](install/install-edex)

### CAVE

Common AWIPS Visualization Environment. The data rendering and visualization tool for AWIPS. CAVE contains of a number of different data display configurations called perspectives.  Perspectives used in operational forecasting environments include **D2D** (Display Two-Dimensional), **GFE** (Graphical Forecast Editor), and **NCP** (National Centers Perspective). CAVE is started with the command `/awips2/cave/cave.sh` or `cave.sh`

* [Read More: How to Install CAVE](install/install-cave)

![CAVE](https://www.unidata.ucar.edu/software/awips2/images/Unidata_AWIPS2_CAVE.png)

### LDM

[https://www.unidata.ucar.edu/software/ldm/](https://www.unidata.ucar.edu/software/ldm/)

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

PyPIES is implemented in two parts: 1. The PyPIES manager is a Python application that runs as part of an Apache HTTP server, and handles requests to store and retrieve data. 2. The PyPIES logger is a Python process that coordinates logging. PyPIES is started and stopped by `edex start` and `edex stop`, and is controlled by the system script `/etc/rc.d/init.d/httpd-pypies` 

