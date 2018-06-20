
The major file systems on the Linux-OS EDEX Data Server are as follows:

### Linux File Systems

* **root** ( / ), **/tmp**, **/usr**, **/var**. Linux mandates that these file systems exist. 
* **/boot**. This file system contains the Linux kernel and boot-up instructions. 
* **/home**. This file system contains all the user working areas. 
* **/dev/shm**. This file system is the Linux shared memory. 
* **/etc/init.d**. Location of startup services (*edex_postgres*, *httpd-pypies*, *qpidd*, *edex_camel*).

### AWIPS File Systems

* **/awips2**. This file system is used to store baselined AWIPS software. 
* **/awips2/data**.  Database files. 
* **/awips2/edex/data/hdf5**. Contains the HDF5 component of the data store and shared static data and hydro apps. 
* **/awips2/GFESuite**. Contains scripts and data relating to inter site coordination (ISC) and service backup.
* **/awips2/edex/data/utility**. Contains localization store and EDEX configuration files. 
* **/awips2/httpd_pypies/etc/https/conf**. Location of PyPIES Apache server configuration file *httpd.conf*.
* **/awips2/qpid/etc**. Location of Qpid configuration file *qpidd.conf*.
* **/awips2/qpid/sbin**. Location of *qpidd* executable and *queueCreator.sh*, which is called by */etc/init.d/qpidd*.
* **/awips2/ldm**. LDM account home directory.
* **/awips2/ldm/etc**. Location of *ldmd.conf* and *pqact.conf*.
* **/awips2/ldm/logs**. Location of LDM logs.

### Raw Data Store File System

Folders are usually laid out exactly like the sbn folders on the EDEX server with each plug-in having a folder on the data store. But some of them do not follow the same convention, for e.g., data sent to the 'metar' endpoint will be found in the **/data_store/text** folder.

Additionally, if ingest of a new format is being worked on, you will find these new data types not yet found on the development or integration systems, located in **/data_store/experimental**. 
