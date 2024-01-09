# Case Study Server Configuration

This document covers what is necessary to install and run AWIPS EDEX as an archive and case study server (no purging of processed data).

---

## Quick Install

Follow the [EDEX Install Instructions](../install/install-edex.md) including iptables config and an optional SSD mount (for large data volumes).

	groupadd fxalpha && useradd -G fxalpha awips
	mkdir -p /awips2/data_store
	wget -O /etc/yum.repos.d/awips2.repo https://downloads.unidata.ucar.edu/awips2/current/linux/awips2.repo
	yum clean all
	yum groupinstall awips2-server -y
	
---

## Disable Data Purging

The easiest way to disable data purging is to add an `<exclude>purge.*</exclude>` entry in `/awips2/edex/conf/modes/modes.xml` so that the purge plugin is not loaded when the EDEX ingest JVM is started:

	vi /awips2/edex/conf/modes/modes.xml 

	<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
	<edexModes>
	    <mode name="ingest">
				<exclude>.*request.*</exclude>
				<exclude>edex-security.xml</exclude>
				...
				<exclude>purge.*</exclude>
	    </mode>
		...
	</edexModes>
	
---

## Start EDEX 

Start EDEX without running the LDM, since we do not want current data.  Run the following command:

	edex start base

Double check everything is running, except the LDM:

	edex

	[edex status]
	 postgres    :: running :: pid 43644
	 pypies      :: running :: pid 3557
	 qpid        :: running :: pid 43742
	 EDEXingest  :: running :: pid 6564 44301 44597
	 EDEXgrib    :: running :: pid 6565 44302 44598
	 EDEXrequest :: running :: pid 6566 44303 44599
	 ldmadmin    :: not running
	
---

## Ingest Case Study Data

Raw data files of any type can be copied or moved into `/awips2/data_store/ingest/` to be picked up and decoded by EDEX.  Most data types are recognized by regular expression matching of the WMO Header or filename.  

Individual files can be ingested on the command line with the regex header/pattern supplied as the last argument:

	qpidNotify.py /full/path/to/data.file [regex match]

For example:

	qpidNotify.py /home/awips/uniwisc_U5_132GOES-15_IMG10.7um_4km_20171024_1830.area.png uniwisc

	qpidNotify.py /awips2/data_store/grid/NAM12/conduit/NAM_CONUS_12km_conduit_20171025_1200Z_F084_TMPK-7.000007.grib2 grib

	qpidNotify.py /awips2/data_store/radar/FTG_N0Q_20171015_1815 Level3
	
---

## Viewing Archive Data in CAVE

Because we are installing and configuring a standalone EDEX archive server without real-time LDM data ingest (and with purge disabled), any case study data that is ingested will be the "latest available" to CAVE, and you will see CAVE product menu time fill in with the latest of all data ingested.

However, to display specific time-based data (in case you ingest more than one case study), there are two options:

### Set Load Mode to Inventory

In the top-left toolbar change **Valid time seq** to **Inventory**.

![](/images/load_mode_inventory1.png)

Now any data product selected from the menus or the Product Browser should prompt you to select the exact time.

![](/images/load_mode_inventory2.png)

### Set Data Display Time in CAVE

At the bottom of the CAVE application, double-click the **Time:** entry to bring up a dialog window where you can set CAVE to a previous time, and choose the option of freezing CAVE at that time or allowing CAVE to "move forward in time" from that position as if it were real-time.

![](/images/cave_set_time.png)
