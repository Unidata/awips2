# Unidata AWIPS

[https://www.unidata.ucar.edu/software/awips/](https://www.unidata.ucar.edu/software/awips/)

[![GitHub release](https://img.shields.io/github/release/Unidata/awips2/all.svg)]() [![Travis Badge](https://travis-ci.org/Unidata/awips2.svg?branch=unidata_18.2.1)](https://travis-ci.org/Unidata/awips2)

The Advanced Weather Interactive Processing System (AWIPS) is a meteorological software package.  It is used for decoding, displaying, and analyzing data, and was originally developed for the National Weather Service (NWS) by Raytheon. There is a division here at UCAR called the Unidata Program Center (UCP) which develops and supports a modified non-operational version of AWIPS for use in research and education by [UCAR member institutions](http://president.ucar.edu/governance/members/universities-representatives).  This is released as open source software, free to download and use by anyone.

AWIPS takes a unified approach to data ingest, where most data ingested into the system comes through the [LDM](#ldm) client pulling data feeds from the [Unidata IDD](https://www.unidata.ucar.edu/projects/#idd). Various raw data and product files (netCDF, grib, BUFR, ASCII text, gini, AREA) are decoded and stored as HDF5 files and Postgres metadata by [EDEX](install/install-edex), which serves products and data over http.

Unidata supports two data visualization frameworks: [CAVE](install/install-cave) (an Eclipse-built Java application which runs on Linux, Mac, and Windows), and [python-awips](python/overview) (a python package).

> **Note**: Our version of CAVE is a **non-operational** version.  It does not support some features of NWS AWIPS.  Warnings and alerts cannot be issued from Unidata's CAVE.  Additional functionality may not be available as well.


![CAVE](https://www.unidata.ucar.edu/software/awips2/images/Unidata_AWIPS2_CAVE.png)

---

## License

Unidata AWIPS source code and binaries (RPMs) are considered to be in the public domain, meaning there are no restrictions on any download, modification, or distribution in any form (original or modified).  Unidata AWIPS license information can be found [here](https://github.com/Unidata/awips2/blob/unidata_18.2.1/LICENSE).

---

## AWIPS Data in the Cloud

Unidata and XSEDE Jetstream have partnered to offer an EDEX data server in the cloud, open to the community.  Select the server in the Connectivity Preferences dialog, or enter **`edex-cloud.unidata.ucar.edu`** (without *http://* before, or *:9581/services* after).

![EDEX in the cloud](/images/boEbFSf28t.gif)


# Documentation - http://unidata.github.io/awips2/

* [Unidata AWIPS User Manual](http://unidata.github.io/awips2/)
* [How to Install CAVE](http://unidata.github.io/awips2/install/install-cave)
* [How to Install EDEX](http://unidata.github.io/awips2/install/install-edex)
* [Starting and Stopping EDEX](http://unidata.github.io/awips2/install/start-edex)
* [The D2D Perspective](http://unidata.github.io/awips2/cave/d2d-perspective)
* [The Localization Perspective](http://unidata.github.io/awips2/cave/localization-perspective)
* [AWIPS Development Environment (ADE)](http://unidata.github.io/awips2/dev/awips-development-environment)
* [python-awips Data Access Framework](http://unidata.github.io/python-awips/)
* [awips2-users Mailing List Archives](https://www.unidata.ucar.edu/mailing_lists/archives/awips2-users/)

	* [(click to subscribe)](mailto:awips2-users-join@unidata.ucar.edu)