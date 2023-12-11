# Install CAVE

CAVE is the **C**ommon **A**WIPS **V**isualization **E**nvironment that is used for rendering and analyzing data for AWIPS.  The installer may require administrator priviledges to install and may require other system changes (environment variables, etc) as well.

---

## Latest CAVE Versions

- [**Linux: 20.3.2-1**](#linux)
- [**Windows: 20.3.2-1**](#windows)
- [**Mac: 20.3.2-1**](#macos)

[**View release notes**](https://www.unidata.ucar.edu/blogs/news/tags/awips-release)

!!! note "Version 20.\* of CAVE is not compatible with Version 18.\* EDEX and vice versa, Version 18.\* of CAVE is not compatible with Version 20.\* EDEX."

---

## Functionality/Reporting

If you come across issues/bugs/missing functionality, we also encourage you to <a href="https://docs.google.com/forms/d/e/1FAIpQLSf6jyZtbh49g-GCBoAQYzTVwAIf_aKz0QOeAr7gDVFhPrjAmw/viewform?usp=sf_link">report it using this short form</a>.

---

## General Requirements

Regardless of what Operating System CAVE is running on, these general requirements are recommended in order for CAVE to perform optimally:

- Local machine

    !!! error "Running CAVE via X11 forwarding or ssh tunneling is **not** supported. Using a [VNC connection is the only remote option](../appendix/common-problems.md#remotely-connecting-to-cave), and may result in worse performance than running locally."
  
- OpenGL 2.0 Compatible Devices
- At least 4GB RAM
- At least 2GB Disk Space for Caching
- NVIDIA Graphics Card
- [Latest NVIDIA Driver](http://www.nvidia.com/Download/index.aspx?lang=en-us)

    !!! warning "While other graphics cards *may* work, NVIDIA Quadro graphics card is recommended for full visualization capability"

---

## Linux <i class="fa fa-linux"></i> 

**Latest Version: 20.3.2-1**

### System Requirements

- 64 bit CentOS/Red Hat 7
- Bash shell environment

!!! note "While CentOS8 has reach End of Life as of Dec. 31, 2021, CentOS7 End of Life isn't until June 30, 2024."

### Download and Installation Instructions

1. Download the following installer: [**awips_install.sh** <i class="fa fa-download"></i>](https://downloads.unidata.ucar.edu/awips2/current/linux/awips_install.sh)
2. In a terminal, go to the download directory 
3. Make the installer an executable by running: `chmod 755 awips_install.sh`
4. Run the installer: `sudo ./awips_install.sh --cave`
     - This will install the application in `/awips2/cave/` and set the local cache to `~/caveData/`

### Run CAVE

To run CAVE either:

- Use the terminal and type the command `cave`
- Find the application in the Linux Desktop menu: Applications > Internet > AWIPS CAVE

Additionally users can choose to run a [virtual machine (VM)](#linux-virtual-machine) on Linux.

---

## Windows <i class="fa fa-windows"></i> 

**Latest Version: 20.3.2-1**

For Windows, Unidata offers two installation options: a [**Direct Windows Installation**](#method-1-direct-windows-install), or a  [**Linux Virtual Machine**](#method-2-linux-virtual-machine).

!!! note "The direct install is much easier/faster than v18*."

!!! warning "The virtual machine option won't render RGB composites of satellite imagery."

### Method 1: Direct Windows Install

#### Download and Installation Instructions

1. Download and install: [**awips-cave.msi** <i class="fa fa-download"></i>](https://downloads.unidata.ucar.edu/awips2/current/windows/awips-cave.msi)    

#### Run CAVE

To run CAVE, either:

- Double click on the CAVE icon on your desktop
- Type "cave" in the start bar and hit enter
- Find and run CAVE app in the file browser:  `C:\Users\%USER%\AppData\Roaming\UCAR Unidata\AWIPS CAVE\CAVE.bat`

### Method 2: Linux Virtual Machine

Please note, running CAVE in a Virtual Machine does have reduced functionality than running CAVE directly on hardware (ex: rendering RGB satellite images).

#### System Requirements

- [VMWare Workstation Player](https://www.vmware.com/products/workstation-player/workstation-player-evaluation.html) must be installed (free software):
- For high definition monitors (4k), you will want to enable the high DPI setting for VMWare Workstation Player
     1. Create a desktop shortcut for VMWare Workstation Player
     1. Right-click the shortcut and select Properties
     1. Open the Compatability Tab
     1. Select the "Change high DPI settings" button
     1. Check the "High DPI scaling ovveride" checkbox and choose "Application" in the enabled dropdown
     
      ![VMWare Workstation Player DPI Setting](../images/vmwareplayer-update-dpi.png)


#### Download and Installation Instructions

1. Download the zipped file containing the virtual machine: [**CentOS7-Unidata-CAVE-20.3.2-1** <i class="fa fa-download"></i>](https://downloads.unidata.ucar.edu/awips2/current/windows/unidata_cave.zip)
2. Unzip the folder.
3. Open VMWare Player and go to **Player** > **File...** > **Open** and locate the folder that was created from the downloaded zipped file.  Select the file called **"CentOS 7 - Unidata CAVE 20.3.2-1.vmx"**.
4. Run this new VM option.  If it asks if it's been moved or copied, select **"I Copied It"**.
     - There will be a user in the Linux machine named "awips" and the password is "awips"
     - The root password is "unidataAWIPS" if ever needed

#### Run CAVE 

Once inside the VM, to run CAVE either:

- Use the desktop icon 
- Use the terminal and type the command `cave`
- Find the application in the Linux Desktop menu: Applications > Internet > AWIPS CAVE

---

## macOS <i class="fa fa-apple"></i> 

**Latest Version: 20.3.2-1**

### System Requirements

- Nvidia Graphics Card (Some Intel Graphics cards seem to work as well)

### Download and Installation Instructions

1. [Download and install CAVE](https://downloads.unidata.ucar.edu/awips2/current/mac/awips-cave.dmg)
    ![Cave System Install](../images/mac-install.png)
     - You can click and drag the CAVE icon into the Applications Directory to install at the System Application level -- this may require Administrator Privileges
     - You can drag that icon to any other location (Desktop, local user's Applications directory, etc) to install CAVE at that location -- this will not require Administrator Privileges

### Run CAVE

To run CAVE either:

- Use the System Menu Go > Applications > CAVE
- Type &#8984; + Spacebar and then type "cave", the application should appear and you can hit **enter** to run it

!!! note "The first time CAVE is opened, it will ask you if you are sure you want to run it, because it was downloaded from the internet and not the Apple Store.  This is normal, and hit Open.  Your message my differ slightly but should look like the image below:"
![internet warning](../images/mac-cave-internet-download2.png)

---

## EDEX Connection

Unidata and Jetstream2 have partnered to offer a EDEX data server in the cloud, open to the public.  Select the server in the Connectivity Preferences dialog, or enter **edex-beta.unidata.ucar.edu**.

![EDEX in the cloud](../images/connectWindow.png)

---

## Local Cache

After connecting to an EDEX server, you will have a local directory named **caveData** which contains files synced from EDEX as well as a client-side cache for data and map resources.

You can reset CAVE by removing the **caveData** directory and reconnecting to an EDEX server. Your local files have been removed, but if you are re-connecting to an EDEX server you have used before, the remote files will sync again to your local **~/caveData** (bundles, colormaps, etc.).

- Linux: `/home/<user>/caveData/`
- macOS: `/Users/<user>/Library/caveData/`
- Windows: `C:\Users\<user>\caveData\`

---

## Uninstalling CAVE (Linux)
These are instructions to manually uninstall CAVE. However, the [`awips_install.sh`](#download-and-installation-instructions) script will do these steps for you if you are installing a newer version of CAVE.

**1. Make sure you have exited out of any CAVE sessions**

!!! note "Check to make sure your `/etc/yum.repos.d/awips2.repo` file has `enabled=1`."

**2. Remove currently installed CAVE**
```
sudo yum clean all
sudo yum groupremove "AWIPS CAVE"
```

!!! note "If you are having trouble removing a group, see the [troubleshooting](../appendix/common-problems.md#troubleshooting-uninstalling-edex) section."

**3. Check to make sure all awips rpms have been removed**
```
rpm -qa | grep awips2
```

If you still have rpms installed, remove them

```
sudo yum remove awips2-*
```

**4. Remove the cave directory in /awips2 and caveData from your home directory**
```
rm -rf /awips2/cave
rm -rf ~/caveData
```
