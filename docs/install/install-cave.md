# Download and Install CAVE

---

<img style="width:300px;float:right;" src="../../images/Unidata_AWIPS2_CAVE.png">

|                |                        |
|----------------|-----------------------:|
| Linux x86_64   | [installCAVE.sh <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/software/awips2/installCAVE.sh)         |
| macOS	         | [awips2-cave-16.2.2.dmg <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips2-cave-16.2.2.dmg)|
| 32-bit Windows | [awips-cave.msi <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips-cave.msi)        |
| 64-bit Windows | [awips-cave.amd64.msi <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips-cave.amd64.msi)  |

---

## <i class="fa fa-linux"></i> Linux

* [installCAVE.sh <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/software/awips2/installCAVE.sh)

For CentOS/Red Hat 6 and 7. Installs to **/awips2/cave** and writes files to **~/caveData**.

Install as root:

	chmod 755 ./installCAVE.sh
	./installCAVE.sh

All package dependencies should be resolved by yum. 


**Run CAVE from the command line**

	cave

or find CAVE in the GNOME menu **Applications** &gt; **Internet** &gt; **AWIPS CAVE**

!!! note "System Requirements"

	* x86_64 CentOS/RHEL 6 or 7
	* OpenGL 2.0
	* 4GB RAM
	* [Latest NVIDIA driver](http://www.nvidia.com/Download/index.aspx?lang=en-us) for your graphics card
	* 2GB disk space for caching datasets in **~/caveData**

---

## <i class="fa fa-apple"></i> macOS

* [awips2-cave-16.2.2.dmg <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips2-cave-16.2.2.dmg)

 
Writes config files to **~/Library/CAVE**

!!! warning "macOS 10.12 (Sierra) not fully supported, requires cave be run from the command line."

    New to 10.12 is Gatekeeper Path Randomization, which prevents applications delivered outside of the Mac App Store from running by normal means (taskbar icon or finder). 

    Until the next release, CAVE on Sierra should be run from the terminal with the command `/Applications/CAVE/cave.app/Contents/MacOS/cave`

---

## <i class="fa fa-windows"></i> Windows

* [awips-cave.msi <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips-cave.msi)
* [awips-cave.amd64.msi <i class="fa fa-download"></i>](http://www.unidata.ucar.edu/downloads/awips2/awips-cave.amd64.msi)

Writes files to **caveData** in the user's home directory.

!!! warning "Beta status"

	Client-side Python scripts (including Derived Parameters) do not work on Windows
---

## AWIPS Data in the Cloud

Unidata and XSEDE Jetstream have partnered to offer a EDEX data server in the cloud, open to the Unidata university community.  Select the server in the Connectivity Preferences dialog, or enter **`edex-cloud.unidata.ucar.edu`** (without *http://* before, or *:9581/services* after).

![EDEX in the cloud](../images/boEbFSf28t.gif)

---

## Troubleshooting

**Localization Preferences Error**

You can reset CAVE by removing the **~/caveData** directory (on macOS **~/Library/CAVE**) and then run `cave` again to connect to an EDEX server.  Your local files have been removed, but if you are re-connecting to an EDEX server you have used before, the remote files will sync again to your local **~/caveData** (bundles, colormaps, etc.). 

**No Images Displayed**

If you are able to load wire-frame contours but not images, [update your video driver](http://www.nvidia.com/Download/index.aspx?lang=en-us). 
