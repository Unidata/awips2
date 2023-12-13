
## Data Receipt

1. The LDM obtains a data product from an upstream LDM site on the IDD.
* The LDM writes the data to file in Raw Data Storage.
* The LDM uses edexBridge to post a “data available” message to the Qpid message broker.
* The EDEX Ingest process obtains the “data available” message from Qpid and removes the message from the message queue.
* The EDEX Ingest process obtains the data files from Raw Data Storage. 

This architecture provides separation between data sources and ingest processing.  Any data source, not just the LDM/IDD, can follow this architecture to provide data for EDEX to process.

## Data Decoding

Data decoding is defined as the process of converting data from a raw format into a decoded format that is usable by CAVE.  In AWIPS, data decoding is performed by the EDEX Ingest proessing (**ingest** and **ingestGrib**).

1. EDEX Ingest obtains the “data available” message from the Qpid message broker, and determines the appropriate data decoder based on the message contents. EDEX Ingest then forwards the message to the chosen decoder.  Finally, the message is removed from the message queue.
2. EDEX Ingest reads the data from Raw Data Storage.
3. EDEX Ingest decodes the data.
4. EDEX Ingest forwards the decoded data to Processed Data Storage.
5. EDEX Ingest sends a message to the CAVE client indicating that newly-decoded data is available.

It is important to note that in AWIPS all data types are processed by either the standard **ingest** process, or by the **ingestGrib** process, which handles all grib message ingestion. Once this data decoding process is complete, clients may obtain and perform additional processing on the data, including displaying data in CAVE.

### Processed Data Storage Architecture

Processed Data Storage refers to the persistence of decoded data and occurs in two separate forms: 1) metadata and some decoded data, which is stored in Postgres database tables; and 2) imagery data, gridded forecast data, and certain point data, which is stored in HDF5 files, and is managed by PyPIES. 


Writing to Processed Data Storage involves the following:

* 1) The **EDEX Process** sends serialized data, area data, and certain point data to PyPIES.
* 2) **PyPIES** writes the data to the HDF5 data store.
* 3) **EDEX** send the metadata to the Postgres DBMS
* 4) **Postgres** writes the metadata to the AWIPS database.

For data not stored in HDF5, Steps 1 and 2 are skipped.

For processed data retrieval, the process is revered:

* 3) **EDEX** requests the metadata from Postgres.
* 4) **Postgres** reads the AWIPS database and returns the requested metadata to EDEX.
* 1) **EDEX** sends a data request to PyPIES.
* 2) **PyPIES** reads the data from the HDF5 data store and returns it to EDEX.

In this case, if the data is not stored in HDF5, then Steps 3 and 4 are skipped.


### Data Retrieval Architecture

Data retrieval is the process by which the CAVE client obtains data using the EDEX Request server; the Request server obtains the data from processed data storage (Postgres and HDF5) and returns it to CAVE. 

1. **CAVE** sends a request via TCP to the EDEX Request server.
2. **EDEX Request** server obtains the requested metadata via Postgres and stored data via PyPIES.
3. **EDEX Request** forwards the requested data directly back to the CAVE client.

For clustered EDEX servers using IPVS, this architecture allows CAVE clients to access any available EDEX Request process, providing an improvement in system reliability and speed.  Data retrieval from processed data storage follows the same pattern as data storage: retrieval of HDF5 is handled by PyPIES; retrieval of database data is handled by Postgres.

## Data Purge Architecture

Raw data storage and processed data storage use two different purge mechanisms.  For processed data storage, AWIPS implements a plugin based purge strategy, where the user has the option to change the purge frequency for each plugin individually. 

### Raw Data Purge

Purging of Raw Data Storage is managed by the LDM user account cron, which executes the ldmadmin scour process, removing data files using an age-based strategy.  The directories and retention times for raw data storage are controlled by *scour.conf*, which is located in the ldm user's *~/etc/* directory.  Each entry in scour.conf contains the directory to manage, the retention time and an optional file name pattern for data files. 

1. An **ldm** user cron job executes ldmadmin.
2. **ldmadmin** executes the LDM scour program.
3. The **LDM scour program** deletes outdated data from AWIPS Raw Data Storage.


### Processed Data Purge

Rules for this version-based purge are contained in XML files located in **/awips2/edex/data/utility/**.  The purge is triggered by a quartz timer event that fires at 30 minutes after each hour.

1. A **Quartz** event is triggered in the EDEX Ingest process causing the Purge Service to obtain a purge lock.  If the lock is already taken, the Purge Service will exit, ensuring that only a single EDEX Ingest process performs the purge.
2. The **EDEX Purge Service** sends a delete message to Postgres.
3. **Postgres** deletes the specified data from the database.
4. If HDF5 data is to be purged, the **Purge Service** messages PyPIES.
5. **PyPIES** deletes the specified HDF5 files.
