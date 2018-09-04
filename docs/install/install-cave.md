## Download and Install CAVE

[Latest Release 17.1.1-6 (June 13, 2018)](https://www.unidata.ucar.edu/blogs/news/category/AWIPS)

|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-linux"></i> Linux  </h1> | <h4>[install.sh --cave <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/software/awips2/install.sh)  </h4>       <p>For CentOS/Red Hat 6 and 7. Installs to /awips2/cave and writes files to ~/caveData.</p> <tt><code>chmod 755 install.sh<br>sudo ./install.sh --cave</code></tt><p>Run CAVE from the Linux Desktop menu Applications > Internet > AWIPS CAVE, or from the command line as simply `cave`.</p> <p><img src="../../images/cave_linux_menu.png" style="width:300px;"></p><div class="admonition note"><p class="admonition-title">System Requirements</p><ul><li>x86_64 CentOS/RHEL 6 or 7</li><li>OpenGL 2.0 capable device</li><li>4GB RAM</li><li><a href="http://www.nvidia.com/Download/index.aspx?lang=en-us">Latest NVIDIA driver</a></li><li>approx. 2GB disk space for data caching (~/caveData)</li></ul></div><p>You can reset CAVE at any time by removing the **~/caveData** directory (on macOS **~/Library/caveData**) and reconnecting to an EDEX server. </p> |


|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-apple"></i> macOS	      </h1>|<h4>    Download and install both<br>[awips2-cave-17.1.1-6.dmg <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips2-cave-17.1.1-6.dmg)<br>[awips-python.pkg <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-python.pkg)</h4> <p>**Supported Graphics Devices for macOS**<li>Intel HD Graphics</li><li>Intel Iris</li><li>NVIDIA GeForce</li></p><p>**Unsupported Graphics Devices for macOS**<li>AMD Radeon R9</li><li>AMD Radeon Pro</li><li>AMD FirePro D300</li></p> <p>Writes and syncs files to ~/Library/caveData.</p> <p>**awips-python.pkg** is not a prerequisite, and CAVE will still run and display data without it, but to use any derived parameter functions such as wind barbs/arrows and grid parameters on various vertical coordinates, jep must be installed in some way (it is assumed in /Library/Python/2.7/site-packages/jep/)</p>|





|                                          |   |
|:----------------------------------------:|:--|
| <h1><i class="fa fa-windows"></i> 32-bit Windows </h1> | <h4> [awips-cave.msi <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-cave.msi)        </h4> <p>Windows clients are still based on the CAVE 16.2.2 code base and provided in lieu of no 17.1.1 client.</p> <p>Writes files to caveData in the user's home directory.</p>  |
| <h1><i class="fa fa-windows"></i> 64-bit Windows </h1> | <h4> [awips-cave.amd64.msi <i class="fa fa-download"></i>](https://www.unidata.ucar.edu/downloads/awips2/awips-cave.amd64.msi)  </h4>  <p>Windows clients are still based on the CAVE 16.2.2 code base and provided in lieu of no 17.1.1 client.</p> <p>Writes files to caveData in the user's home directory.</p> <div class="admonition warning"><p class="admonition-title">Beta status</p><p>Client-side Python scripts (including Derived Parameters) do not work on Windows</p></div>|

---

## AWIPS Data in the Cloud

Unidata and XSEDE Jetstream have partnered to offer a EDEX data server in the cloud, open to the Unidata university community.  Select the server in the Connectivity Preferences dialog, or enter **`edex-cloud.unidata.ucar.edu`** (without *http://* before, or *:9581/services* after).

![EDEX in the cloud](../images/boEbFSf28t.gif)

---

## Troubleshooting

**Localization Preferences Error**

You can reset CAVE by removing the **~/caveData** directory (on macOS **~/Library/caveData**) and then run `cave` again to connect to an EDEX server.  Your local files have been removed, but if you are re-connecting to an EDEX server you have used before, the remote files will sync again to your local **~/caveData** (bundles, colormaps, etc.).

**No Images Displayed**

If you are able to load wire-frame contours but not images, [update your video driver](http://www.nvidia.com/Download/index.aspx?lang=en-us). 
