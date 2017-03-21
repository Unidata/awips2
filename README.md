# Unidata AWIPS

AWIPS (formerly know as AWIPS II or AWIPS2) is a weather forecasting display and analysis package developed by the [National Weather Service](http://www.nws.noaa.gov/ost/SEC/AE/) and [Raytheon](http://www.raytheon.com/capabilities/products/awips/). AWIPS is a Java application consisting of a data-rendering client ([CAVE](http://unidata.github.io/awips2/docs/install/install-cave.html), which runs on Red Hat/CentOS Linux, macOS, and Windows, and a backend data server ([EDEX](http://unidata.github.io/awips2/docs/install/install-edex.html), which  64-bit CentOS or RedHat)

AWIPS takes a unified approach to data ingest, and most data types follow a standard path through the system, starting with an [LDM](http://www.unidata.ucar.edu/software/ldm/) client requesting data from Unidata's [IDD](http://www.unidata.ucar.edu/projects/#idd), which are then decoded and stored as HDF5 and PostgreSQL/PostGIS metadata. Unidata supports two visualization frameworks for rendering AWIPS data:

* [CAVE](http://unidata.github.io/awips2/docs/install/install-cave.html) - the **C**ommon **A**WIPS **V**isualization **E**nvironment
* [python-awips](https://github.com/Unidata/python-awips) - a Python data access framework for requesting Numpy data arrays and Shapely geometries.

# License

Unidata AWIPS source code and binaries (RPMs) are considered to be in the public domain, meaning there are no restrictions on any download, modification, or distribution in any form (original or modified).  Unidata AWIPS contains no proprietery content and is therefore not subject to export controls as stated in the Master Rights licensing file. 

# AWIPS Data in the Cloud

Through a grant provided by [Jetstream](https://jetstream-cloud.org/), Unidata is able to run a real-time EDEX data server in cloud, providing free AWIPS data to UCAR member institutions and other geoscience research and education organizations.  When prompted in the Connectivity Preferences dialog, enter **`edex-cloud.unidata.ucar.edu`** (without adding http:// before, or :9581/services after), or select it from the default dropdown list. 

![EDEX in the cloud](http://unidata.github.io/awips2/docs/images/boEbFSf28t.gif)

# Documentation

* [Unidata AWIPS User Manual](http://unidata.github.io/awips2/)
* [How to Install CAVE](http://unidata.github.io/awips2/docs/install/install-cave.html)
* [How to Install EDEX](http://unidata.github.io/awips2/docs/install/install-edex.html)
* [Starting and Stopping EDEX](http://unidata.github.io/awips2/docs/install/start-edex.html)
* [The D2D Perspective](http://unidata.github.io/awips2/docs/cave/d2d-intro.html)
* [The NCP Perspective](http://unidata.github.io/awips2/docs/cave/ncp.html)
* [The Localization Perspective](http://unidata.github.io/awips2/docs/cave/localization-perspective.html)
* [python-awips Data Access Framework](http://python-awips.readthedocs.io/)
* [awips2-users Mailing List Archives](http://www.unidata.ucar.edu/mailing_lists/archives/awips2-users/)

	* [(click to subscribe)](mailto:awips2-users-join@unidata.ucar.edu)



# AWIPS Source Code Respositories

* [awips2-builds](https://github.com/Unidata/awips2) (this repo)
* [awips2-core](https://github.com/Unidata/awips2-core)
* [awips2-core-foss](https://github.com/Unidata/awips2-core-foss)
* [awips2-foss](https://github.com/Unidata/awips2-foss)
* [awips2-ncep](https://github.com/Unidata/awips2-ncep)
* [awips2-goesr](https://github.com/Unidata/awips2-goesr)
* [awips2-rpm](https://github.com/Unidata/awips2-rpmbuild)
