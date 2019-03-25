## Download and Install CAVE

> [Release 18.1.1-7, March 25, 2019](https://www.unidata.ucar.edu/blogs/news/category/AWIPS)

## Linux 
|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-linux"></i></h1> | <h4>[awips_install.sh --cave <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/software/awips2/awips_install.sh)  </h4>       <p>For CentOS/Red Hat 6 and 7. Installs to /awips2/cave and writes files to ~/caveData.</p> <tt><code>chmod 755 awips_install.sh<br>sudo ./awips_install.sh --cave</code></tt><p>Run CAVE from the Linux Desktop menu Applications > Internet > AWIPS CAVE, or from the command line as simply `cave`.</p> <div class="admonition note"><p class="admonition-title">System Requirements</p><ul><li>x86_64 CentOS/RHEL 6 or 7</li><li>OpenGL 2.0 capable device</li><li>4GB RAM</li><li><a href="http://www.nvidia.com/Download/index.aspx?lang=en-us">Latest NVIDIA driver</a></li><li>approx. 2GB disk space for data caching (~/caveData)</li></ul></div><p>You can reset CAVE at any time by removing the **~/caveData** directory (on macOS **~/Library/caveData**) and reconnecting to an EDEX server. </p>  |

## macOS
|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-apple"></i></h1>|<h4>    Download and install both<br>[awips-cave.dmg <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-cave.dmg)<br>[awips-python.pkg <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-python.pkg)</h4> <p>**Supported Graphics Devices for macOS**<li>Intel HD Graphics</li><li>Intel Iris</li><li>NVIDIA GeForce</li></p><p>**Unsupported Graphics Devices for macOS**<li>AMD Radeon R9</li><li>AMD Radeon Pro</li><li>AMD FirePro D300</li></p> <p>Writes and syncs files to ~/Library/caveData.</p> <p>**awips-python.pkg** is not a prerequisite, and CAVE will still run and display data without it, but to use any derived parameter functions such as wind barbs/arrows and grid parameters on various vertical coordinates, jep must be installed in some way (it is assumed in /Library/Python/2.7/site-packages/jep/)</p>|


## Windows 
|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-windows"></i> </h1> | <h4> [awips-cave.msi <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-cave.msi)</h4><p>Writes files to **~/caveData** (in your user home directory)</p> <p>Requires <b>Python 3</b>, <b>Numpy</b>, and <b>Jep</b> be installed</p> <p>Requires **PYTHONHOME** be defined</p> <p>In addition to the application directory, the MSI installer will attempt to copy the *[gridslice](https://github.com/mjames-upc/gridslice)* shared library to `$PYTHONHOME/Dlls`.  If the `$PYTHONHOME` environmental variable is not defined, *gridslice* will not be installed. You can always rerun the installer after defining `$PYTHONHOME` and then check that the file `gridslice.pyd` is installed in `$PYTHONHOME/Dlls`.</p> <p>CAVE will still run without gridslice, but certain bundles which use derived parameters, such as [isentropic analyses](../cave/d2d-grids/#isentopic-analysis-270k-320k), will not load. </p> |



<p><b>Windows-Specific Instructions</b></p>



<div class="admonition note">
<h4>1) Download and install <strong><a href="https://conda.io/miniconda.html">Miniconda Python 3.7 for Windows</a></strong><a class="headerlink" href="#1-download-and-install-miniconda-python-37-for-windows" title="Permanent link"></a></h4><ul><li>Allow Miniconda3 to set <strong>PATH</strong> and other environment variables.</li><li><p>Ensure that <strong>PYTHONHOME</strong> is set to the Miniconda3 location.</p><p><img alt="" src="../../images/windows_envvars.png" /></p><p>If <strong>PYTHONHOME</strong> is not set, the <strong>gridslice</strong> Python module will not be installed or available.</p></li></ul><h4>2) Install dependent Python packages</h4><ul><li><code>pip install numpy==1.15.1 jep==3.8.2</code></li></ul><h4>3) Run <a href="https://www.unidata.ucar.edu/downloads/awips2/awips-cave.msi">awips-cave.msi <i class="fa fa-download"></i></a><a class="headerlink" href="#3-run-awips-cave-1amd64msi" title="Permanent link"></a></h4>
</div>

---

## AWIPS Data in the Cloud

Unidata and XSEDE Jetstream have partnered to offer a EDEX data server in the cloud, open to the Unidata university community.  Select the server in the Connectivity Preferences dialog, or enter **`edex-cloud.unidata.ucar.edu`** (without *http://* before, or *:9581/services* after).

![EDEX in the cloud](../images/boEbFSf28t.gif)

---

## caveData Directory

After connecting to an EDEX server, you will have a local directory named **caveData** which contains files synced from EDEX as well as a client-side cache for data and map resources.

You can reset CAVE by removing the **caveData** directory and reconnecting to an EDEX server. Your local files have been removed, but if you are re-connecting to an EDEX server you have used before, the remote files will sync again to your local **~/caveData** (bundles, colormaps, etc.).

* Linux: `/home/<user>/caveData`
* macOS: `/Users/<user>/Library/caveData`
* Windows: `C:\Users\<user>\caveData`

