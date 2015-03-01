## Unidata AWIPS II Configuration

	yum groupinstall awips2-server -y
	
This single yum command installs all EDEX server components, database tables, the Qpid message broker, the LDM, a default site localization, and a Unidata EDEX configuration package `awips2-edex-upc`.  The Unidata configruation performs four functions:

1. Installs updated plugins for radar and grib decoding (increasing the number of simultaneous decoder threads for each).
2. Updates grib distribution file `grib.xml` for CONDUIT products.
3. Installs process manager script `edex`, and runs initial server address setup (`edex setup`)
4. Installs default OAX (Omaha) localization RPM.

**What this means:** One yum install command followed by `ldmadmin start` and `edex start` and your EDEX server is decoding live data.

## Preparing the Unidata AWIPS II Release

Adding a single RPM to the AWIPS II repository, rather than re-building multiple EDEX component RPMs, allows us to adapt to the still-changing development process.  Any changes made to the core installation files are added straight to the Unidata release, only will the configuration methods change as the baseline files change at Raytheon and the National Weather Service.

Updating on our end involves:

1. Receive new release from NWS (example: `awips2_OB13.4.1.tar`)
2. Update all core RPMs in the Unidata AWIPS II repository to 13.4.1
3. Update UPC-specific post-install configuration as needed.
4. Build latest `awips2-edex-upc` RPM and add to awips2upc repository.

## Example

#### version received: awips2_OB13.2.1 (319 RPMs, 5.0 GB)
* installed on 10+ servers
* unable to decode nationwide NEXRAD3 feed
* LDM 6.8.1
* 5 step installation

#### version released: awips2-upc-13.2.1 (185 RPMs, 3.7 GB)
* 1 standalone server
* full NEXRAD3 decoding
* faster CONDUIT grib2 decoding
* LDM 6.11.2
* 1 step installation


## Release Date?

End of 2013.

- Michael James, Unidata Program Center, April 2013
