# AWIPS Development Environment (ADE)

Quick instructions on how to download the latest source code and run CAVE from Eclipse.

> **Note**: It is important to keep in mind these instructions are intended for a system that is specifically used for developing AWIPS.  It should not be used in conjunction with installed production versions of AWIPS.

## 1. Remove AWIPS Instances

First, make sure to remove any instances of AWIPS that are already installed, this can potentially cause problems when setting up the development environment.  Below is an example that had CAVE installed.

Uninstall with yum:

    yum clean all
    yum groupremove awips2-cave
          
Check to make sure all rpms have been removed:
      
    rpm -qa | grep awips2
            
Remove the awips2 directory:
          
    rm -rf /awips2
        
---        

## 2. Set Up AWIPS Repo

Create a repo file named `/etc/yum.repos.d/awips2.repo`, and set the contents to the following:
<pre>
[awips2repo]
name=AWIPS II Repository
baseurl=https://www.unidata.ucar.edu/repos/yum/<b>el7-dev</b>/
enabled=1
protect=0
gpgcheck=0
proxy=_none_
</pre>
>**Note**: This file may already exist if AWIPS had been previously installed on the machine, so make sure to edit the baseurl.
        
---

## 3. Install the ADE

Install the AWIPS Development Environment (ADE) using yum.  This will install Eclipse (4.6.1), Java (1.8), Ant (1.9.6), Python 2.7 and its modules (Numpy, Matplotlib, Shapely, Jep, and others). 

    yum clean all
    yum groupinstall awips2-ade

---

## 4. Download the Source Code

If it's not already installed, install git:
    
    yum install git
    
Next clone all of the required repositories for AWIPS:
    
    git clone https://github.com/Unidata/awips2.git
    git clone https://github.com/Unidata/awips2-core.git
    git clone https://github.com/Unidata/awips2-core-foss.git
    git clone https://github.com/Unidata/awips2-foss.git
    git clone https://github.com/Unidata/awips2-ncep.git
    git clone https://github.com/Unidata/awips2-nws.git
    git clone https://github.com/Unidata/awips2-gsd.git
    git clone https://github.com/Unidata/awips2-drawing.git

---

## 5. Set Up Eclipse

Open eclipse by running: `/awips2/eclipse/eclipse.sh`

Verify or make the following changes to set up eclipse for AWIPS development:

* Preferences > Java 
    
    Set to **/awips2/java**

* Preferences > PyDev > Python Interpreter

    Set to **/awips2/python/bin/python**
    
* There might be some unresolved errors.  These should be made to warnings instead.

    Preferences > Java > Compiler > Building > **Circular Dependencies** > Change to Warning
    Preferences > Plug-in Development > API Baselines > **Missing API Baseline** > Change to Warning
    
* **Turn off automatic building** (you will turn this back on after importing the repos)
    
    Project > Uncheck "Build Automatically"

* File > Import > General > Existing Projects Into Workspace

    Import all of the git cloned project folders **EXCEPT** for the main (first) **github.com/Unidata/awips2.git** directory (which should be **~/awips2**).  
    Select **awips2-core**, **awips2-core-foss**, **awips2-foss**, **awips2-ncep**, etc. > Select All Projects > Finish 
     
    You'll want to import **~/awips2** in two parts to ensure a clean and error-free Eclipse build:
    
    1. Import **awips2/cave** > Select All Projects > Finish
    2. Import **awips2/edexOsgi** > Select All Projects > Finish

* Project > Clean

    Clean the build and ensure no errors are reported.  
    
* Turn automatic building back on
    
    Project > Check "Build Automatically"
    
---

## 6. Run CAVE
    
Launch CAVE from eclipse using **com.raytheon.viz.product.awips/developer.product**.

Double-click the **developer.product** file to open the Product View in Eclipse.  Select **Overview** > **Synchronize** and then right-click the file in the left-side package explorer:

Select **Run As** > **Eclipse Application** to launch CAVE in the development environment. 

Select **Debug** > **Eclipse Application** to launch CAVE in in debug mode. 

---

## Troubleshooting

* If you are getting a lot of errors, try changing your Java Compiler to 1.7, build the project, then change back to 1.8 and rebuild.
