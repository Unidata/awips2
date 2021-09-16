# Distributed EDEX

AWIPS makes use of service-oriented architecture to request, process, and serve real-time meteorological data. While originally developed for use on internal NWS forecast office networks, where operational installations of AWIPS could consist of a dozen servers or more, the early Unidata releases were stripped of operations-specific configurations and plugins, and released as a standalone server. This worked, since (at the time) a single EDEX instance with an attached SSD could handle most of NOAAport. However, with GOES-R(16) coming online in 2017, and more gridded forecast models being created at finer temporal and spatial resolutions, there is now a need to distribute the data decoding across multiple machines to handle this firehose of data.

---

## Unidata's Current EDEX Server

Currently, with our specific EDEX server we use a Database/Request instance that also decodes and ingests a good portion of the data.   It handles all data requests from CAVE users, as well as the majority of the decoding and ingesting for data feeds coming down on the LDM.  The **radar** data has been specifically exluded (from the decoding and ingest) and it has its own [**Ingest/Decode Server**](#ingestdecode-server) which is explained in more detail below.

For our EDEX we have designated an instance of the ingest/decoding server to be dedicated to handling the radar data.  Our *Radar-EDEX* recieves and decodes all radar down from the LDM and then stores it back on our main [**Database/Request EDEX**](#databaserequest-server) in the form of HDF5 data files and PostgreSQL metadata.

---

## Example Installation

This walkthrough will install different EDEX components on two machines in the XSEDE Jetstream Cloud, the first is used to **store and serve** while the second is used to **ingest and decode** data.

![](/images/awips2_distributed.png)

---

### Database/Request Server

For this example, this server will be referred to by the IP address **10.0.0.9**.

#### 1. Install

	wget https://downloads.unidata.ucar.edu/awips2/current/linux/awips_install.sh
	chmod 755 awips_install.sh
	sudo ./awips_install.sh --database


#### 2. IPtables Config

It is required that ports 5432 and 5672 be open for the specific IP addresses of outside EDEX ingest servers.  It is *not recommended* that you leave port 5432 open to all connections (since the default awips database password is known, and is not meant as a security measure).  Further, it *is recommended* that you change the default postgres awips user password (which then requires a reconfiguration of every remote EDEX ingest server in order to connect to this database/request server).

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
	-A INPUT -m state --state NEW -m tcp -p tcp --dport 22 -j ACCEPT
	-A INPUT -m state --state NEW -m tcp -p tcp --dport 9581 -j ACCEPT
	-A INPUT -m state --state NEW -m tcp -p tcp --dport 9582 -j ACCEPT
	-A INPUT -s 10.0.0.7 -j EDEX
	-A INPUT -j EXTERNAL
	-A EXTERNAL -j REJECT
	-A EDEX -m state --state NEW -p tcp --dport 5432 -j ACCEPT
	-A EDEX -m state --state NEW -p tcp --dport 5672 -j ACCEPT
	-A EDEX -j REJECT
	COMMIT

Note the line **`-A INPUT -s 10.0.0.7 -j EDEX`** as well as the following **`-A EDEX ...`** rules for ports 5432 (PostgreSQL) and 5672 (PyPIES/HDF5).  

!!! Note "The two ports left open to all connections (9581,9582) in addition to default port 22 are for outside CAVE client connections"

#### 3. Database Config

In the file `/awips2/database/data/pg_hba.conf` you define remote connections for all postgres tables with as `<IP address>/32`, after the block of IPv4 local connections and generic `<IP address/24>` for hostnossl:

	vi /awips2/database/data/pg_hba.conf
	
	# "local" is for Unix domain socket connections only
    local      all         all                               trust
    hostssl    all         all         162.0.0.0/8           cert clientcert=1
    hostssl    all         all         127.0.0.1/32          cert clientcert=1

	hostssl    all         all         10.0.0.7/32           cert clientcert=1
	hostnossl  postgres    all         10.0.0.0/24           md5
	hostnossl  fxatext     all         10.0.0.0/24           md5
	hostnossl  metadata    all         10.0.0.0/24           md5

    # IPv6 local connections:
    hostssl    all   all         ::1/128               cert clientcert=1
    hostnossl  all   all         ::1/128               md5

#### 4. Start EDEX

	edex start database

This will start PostgreSQL, httpd-pypies, Qpid, and the EDEX Request JVM (and will not start the LDM or the EDEX Ingest and IngestGrib JVMs)

#### 5. Monitor Services

The command `edex` will show which services are running, and for a Database/Request server, will not include the LDM, EDEXingest, or EDEXgrib:

	edex

	[edex status]
	postgres    :: running :: pid 571
	pypies      :: running :: pid 639
	qpid        :: running :: pid 674
	EDEXingest  :: not running
	EDEXgrib    :: not running
	EDEXrequest :: running :: pid 987 1029 23792


Since this Database/Request server is not running the main *edexIngest* JVM, we won't see anything from `edex log`, instead watch the Request Server with the command

	edex log request

!!! warning "Confirm that EDEX Request connects to PostgreSQL!"
    With the above `edex log request`, ensure that the log progresses **past this point**:

   		Spring-enabled Plugins:
		-----------------------
		acars-common, acars-common-dataaccess, acarssounding-common, activetable-common,
		activetable-request, airep-common, airep-common-dataaccess, airmet-common, 
		atcf-common, atcf-request, auth-request, awipstools-request, aww-common...
	
		JAXB context for PersistencePathKeySet inited in: 5ms
		INFO 20:21:09,134 5584 [EDEXMain] Reflections: Reflections took 436 ms to scan 258 urls, producing 31 keys and 3637 values
		Found 499 db classes in 720 ms

    If the log stops at the **Found db classes...** line, that means EDEX is not connecting to PostgreSQL - double-check `DB_ADDR` in `/awips2/edex/bin/setup.env`

---

### Ingest/Decode Server (Ancillary EDEX Server)

For this example, this server will be referred to by the IP address **10.0.0.7**.
The **Main EDEX** server will be referred to by the IP address **10.0.0.9**.

#### 1. Install

	wget https://downloads.unidata.ucar.edu/awips2/current/linux/awips_install.sh
	chmod 755 awips_install.sh
	sudo ./awips_install.sh --ingest

#### 2. EDEX Config

`vi /awips2/edex/bin/setup.env`

Here you should redefine `DB_ADDR` and `PYPIES_SERVER` to point to the **Main** or  **Database/Request** server (10.0.0.9) and the `EDEX_SERVER` to point to the current **Ingest** server (10.0.0.7)

	export EDEX_SERVER=10.0.0.7
	
	# postgres connection
	export DB_ADDR=10.0.0.9
	export DB_PORT=5432
	
	# pypies hdf5 connection
	export PYPIES_SERVER=http://10.0.0.9:9582

	# qpid connection
	export BROKER_ADDR=${EDEX_SERVER}

Notice that `EDEX_SERVER` and `BROKER_ADDR` (qpid) should remain defined as the *localhost* IP address (10.0.0.7)

#### 3. Modify the edexServiceList

  Most likely if you are running a distributed EDEX setup, you are only processing a subset of data. You can change your edexServiceList to only run the processes you need. You will need to update the `/etc/init.d/edexServiceList` file. For example replace the services with the associated right column based on the data you're processing:

	export SERVICES=('')

  | Data Processing: | edexServiceList |
  |---------------| ------------|
  | radar | ingestRadar |
  | satellite | ingestGoesR | 
  | model | ingestGrids, ingestGrib | 
   

#### 4. Configure your LDM


You'll want to modify your pqact.conf file to store only the data you want processed. There are example files in `/awips2/ldm/etc` that you can copy over to the main pqact.conf file. For example if you are wanting to process goesr data only, you can do the following steps:

	cd /awips2/ldm/etc
	mv pqact.conf pqact.conf.orig
	cp pqact.goesr pqact.conf

You will also want to edit the `pqact.conf` file on your **Main EDEX** and comment out any entries you're processing on this EDEX server. 

#### 5. Start EDEX

	edex start

This will start LDM, Qpid and the specified EDEX Ingest JVMs (and not start PostgreSQL, httpd-pypies, or the EDEX Request JVM)

#### 4. Monitor Services

Watch the edex JVM log with the command

	edex log

!!! warning "Confirm that EDEX connects to PostgreSQL!"
    With the above `edex log`, ensure that the log progresses **past this point**:

   		Spring-enabled Plugins:
		-----------------------
		acars-common, acars-common-dataaccess, acarssounding-common, activetable-common,
		activetable-ingest, airep-common, airep-common-dataaccess, airmet-common, 
		atcf-common, atcf-ingest, aww-common...
	
		JAXB context for PersistencePathKeySet inited in: 5ms
		INFO 20:21:09,134 5584 [EDEXMain] Reflections: Reflections took 436 ms to scan 258 urls, producing 31 keys and 3637 values
		Found 499 db classes in 720 ms

    If the log stops at the **Found db classes...** line, that means EDEX is not connecting to the *remote PostgreSQL instance* - double-check `DB_ADDR` in `/awips2/edex/bin/setup.env`

    You can **manually check remote PostgreSQL connectivity** on any EDEX Ingest server from the command line:

		su - awips
		psql -U awips -h <remote IP address> -p 5432 metadata

    Where the default passwd is *awips* and is defined in files in `/awips2/edex/conf/db/hibernateConfig/`

---

## Additional Notes

* Be mindful of what IP address and hostnames are used in `/awips2/edex/bin/setup.env` and `/awips2/database/data/pg_hba.conf`, and that they are resolvable from the command line.  Consult or edit `/etc/hosts` as needed.
* You can install multiple `awips2-ingest` servers, each decoding a different dataset or feed, all pointing to the same Database/Request server (`DB_ADDR` and `PYPIES_SERVER` in `/awips2/edex/bin/setup.env`):

* Every EDEX Ingest IP address must be allowed in both **iptables** and **pg_hba.conf** as [shown above](#2-iptables-config).

* Data processed on 