Quick instructions on how to deploy CAVE from Eclipse.


1. Change `/etc/yum.repos.d/awips2.repo` to 

        [awips2repo]
        name=AWIPS II Repository
        baseurl=http://www.unidata.ucar.edu/repos/yum/awips2-dev/
        enabled=1
        protect=0
        gpgcheck=0
        proxy=_none_
        
2. `yum clean all && yum groupinstall awips2-ade`

    This will install Eclipse (4.5), Java (1.8), Ant, Maven, Python 2.7 and its modules (Numpy, Matplotlib, Shapely, others). 


3. `git clone https://github.com/Unidata/awips2.git`

    The full list of repositories required as of release 17.1.1:
    
        git clone https://github.com/Unidata/awips2.git
        git clone https://github.com/Unidata/awips2-core.git
        git clone https://github.com/Unidata/awips2-core-foss.git
        git clone https://github.com/Unidata/awips2-foss.git
        git clone https://github.com/Unidata/awips2-ncep.git
        git clone https://github.com/Unidata/awips2-goesr.git
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
         
        You'll want to import **~/awips2** in two parts to ensure a clean and error-free Eclipse build:
        
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
