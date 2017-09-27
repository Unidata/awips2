# Unidata AWIPS

AWIPS (formerly know as AWIPS II or AWIPS2) is a weather forecasting display and analysis package developed by the [National Weather Service](http://www.nws.noaa.gov/ost/SEC/AE/) and [Raytheon](http://www.raytheon.com/capabilities/products/awips/). AWIPS is a Java application consisting of a data-rendering client ([CAVE](http://unidata.github.io/awips2/install/install-cave/), which runs on Red Hat/CentOS Linux, macOS, and Windows), and a backend data server ([EDEX](http://unidata.github.io/awips2/install/install-edex), which runs on x86_64 Red Hat/CentOS 6 and 7).

AWIPS takes a unified approach to data ingest, and most data types follow a standard path through the system, starting with an [LDM](http://www.unidata.ucar.edu/software/ldm/) client requesting data from Unidata's [IDD](http://www.unidata.ucar.edu/projects/#idd), which are then decoded and stored as HDF5 and PostgreSQL/PostGIS metadata. Unidata supports two visualization frameworks for rendering AWIPS data:

* [CAVE](http://unidata.github.io/awips2/install/install-cave) - the **C**ommon **A**WIPS **V**isualization **E**nvironment
* [python-awips](https://github.com/Unidata/python-awips) - a Python data access framework for requesting Numpy data arrays and Shapely geometries.

# License

Unidata AWIPS source code and binaries (RPMs) are considered to be in the public domain, meaning there are no restrictions on any download, modification, or distribution in any form (original or modified).  Unidata AWIPS contains no proprietery content and is therefore not subject to export controls as stated in the Master Rights licensing file. 

# AWIPS Data in the Cloud

Through a grant provided by [Jetstream](https://jetstream-cloud.org/), Unidata is able to run a real-time EDEX data server in cloud, providing free AWIPS data to UCAR member institutions and other geoscience research and education organizations.  When prompted in the Connectivity Preferences dialog, enter **`edex-cloud.unidata.ucar.edu`** (without adding http:// before, or :9581/services after), or select it from the default dropdown list. 

![EDEX in the cloud](http://unidata.github.io/awips2/images/boEbFSf28t.gif)

# Documentation - http://unidata.github.io/awips2/

* [Unidata AWIPS User Manual](http://unidata.github.io/awips2/)
* [How to Install CAVE](http://unidata.github.io/awips2/install/install-cave)
* [How to Install EDEX](http://unidata.github.io/awips2/install/install-edex)
* [Starting and Stopping EDEX](http://unidata.github.io/awips2/install/start-edex)
* [The D2D Perspective](http://unidata.github.io/awips2/cave/d2d-perspective)
* [The NCP Perspective](http://unidata.github.io/awips2/cave/ncp-perspective)
* [The Localization Perspective](http://unidata.github.io/awips2/cave/localization-perspective)
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


# Setting up the AWIPS Development Environment (ADE)

Instructions on how to deploy CAVE from Eclipse.

1. Change `/etc/yum.repos.d/awips2.repo` to 

        [awips2repo]
        name=AWIPS II Repository
        baseurl=http://www.unidata.ucar.edu/repos/yum/awips2-dev/
        enabled=1
        protect=0
        gpgcheck=0
        proxy=_none_
        
2. `yum clean all && yum groupinstall awips2-ade`

    This will install Eclipse (4.5), Java (1.8), Ant, Maven, Python 2.7 and its modules (Numpy, Shapely, etc.) 

3. `git clone https://github.com/Unidata/awips2.git`

    The full list of repositories required as of release 17.1.1:
    
        git clone https://github.com/Unidata/awips2.git
        git clone https://github.com/Unidata/awips2-core.git
        git clone https://github.com/Unidata/awips2-core-foss.git
        git clone https://github.com/Unidata/awips2-foss.git
        git clone https://github.com/Unidata/awips2-ncep.git
        git clone https://github.com/Unidata/awips2-goesr.git
        git clone https://github.com/Unidata/awips2-rpm.git

    Optional repositories:

        git clone https://github.com/Unidata/awips2-nws.git
        git clone https://github.com/Unidata/awips2-gsd.git
        git clone https://github.com/Unidata/awips2-drawing.git
        git clone https://github.com/Unidata/awips2-cimss.git

4. Run `/awips2/eclipse/eclipse.sh`

    * Preferences > Java 
        
        Set to **/awips2/java**
    
    * Preferences > PyDev > Python Interpreter
    
        Set to **/awips2/python/bin/python** (should be resolved by Auto-Config)

    * File > Import > General > Existing Projects Into Workspace
    
        Import all of the git cloned project folders **EXCEPT** for the main (first) **github.com/Unidata/awips2.git** directory (which should be **~/awips2**).
         
        You'll want to import **~/awips2** in three parts to ensure a clean and error-free Eclipse build:
        
        1. Import **awips2/cave** > Select All Projects > Finish
        2. Import **awips2/edexOsgi** > Select All Projects > Finish
            
        Now import all other repositories fully: 
        
        Select **awips2-core**, **awips2-core-foss**, **awips2-foss**, **awips2-ncep**, etc. > Select All Projects > Finish 

    * Project > Clean
    
        Run a clean build and ensure no errors are reported.  
    
    
5. Run **com.raytheon.viz.product.awips/developer.product**

    Double-click the **developer.product** file to open the Product View in Eclipse.  Select **Overview** > **Synchronize** and then right-click the file in the left-side package explorer:
    
    Select **Run As** > **Eclipse Application** to launch CAVE in the development environment. 
    
    Select **Debug** > **Eclipse Application** to launch CAVE in in debug mode. 
