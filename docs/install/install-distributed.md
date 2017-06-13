
An example of a two-server configuration (LDM and EDEX seperately) using Microsoft Azure CentOS 7.2 virtual machines (Unidata EDEX is supported on CentOS/RHEL 7 since 16.2.2).

# cifs setup

Following the guide [https://docs.microsoft.com/en-us/azure/storage/storage-how-to-use-files-linux](https://docs.microsoft.com/en-us/azure/storage/storage-how-to-use-files-linux), our two Azure VMs will share a single file storage directory mounted via Samba **cifs**.   LDM will write to the file share, and EDEX will read from it to ingest and decode IDD products. 

In the [Azure portal](https://portal.azure.com):

* Create a new  **Standard** storage account (e.g. **edex7203**)
* Create a new **File service** within the storange account (e.g. **datastore**), 100GB minimum.  
* The file service will be located at **//edex7203.file.core.windows.net/datastore**
* Select the **Configuration** tab and confirm **Standard Performance** and **Locally-redundant storage (LRS)** for Replication (these should be defaults).
* Select the **Access keys** tab and copy one of the keys for `/etc/fstab`

`/etc/fstab` should look like this (**for both machines**):

	UUID=0177d0ac-2605-4bfb-9873-5bdefea12fe2 / xfs defaults 0 0
	//edex7203.file.core.windows.net/datastore /awips2/data_store cifs vers=3.0,password=YOUR_KEY_HERE,user=edex7203,dir_mode=0777,file_mode=0777

Note the `YOUR_KEY_HERE` placeholder above, that's where your key will go.  

Now run `mount -a` and confirm `/awips2/data_store` is mounted with the command `df -h`

	Filesystem                                  Size  Used Avail Use% Mounted on
	/dev/sda1                                    30G  7.4G   23G  25% /
	/dev/sdb1                                    14G   41M   13G   1% /mnt/resource
	//edex7203.file.core.windows.net/datastore  100G    1M  100G   1% /awips2/data_store

---


# EDEX server (10.0.0.1)

In the [Azure portal](https://portal.azure.com):

1. Create a new virtual machine with an *awips* user account
	* **CentOS 7.2**
	* **DS5_V2 Standard** (16 cores, 56 GB)
2. Ensure that this VM is on the same **Virtual Network** as the LDM machine (both on the 10.0.0.\* subnet).
3. Select the new vm, then select **Disks**, and modify the attached **OS Disk** to be 512GB or greater (vm must be stopped for this).
4. Start the VM, log in as root, and follow the steps in the guide [Step by Step: how to resize a Linux VM OS disk in Azure](https://blogs.msdn.microsoft.com/cloud_solution_architect/2016/05/24/step-by-step-how-to-resize-a-linux-vm-os-disk-in-azure-arm/) (with one dfference in step 5 below)
	* **fdisk /dev/sda**
	* type "**u**" to change the units to sectors.
	* type "**p**" to list current partition details.
	* type "**d**" to delete the current partition.
	* type "**n**" to create a new partition. Select defaults (p for primary partition, 1 for first part).
	* type "**w**" to write the partition.

5. Reboot the machine and log in again (as root).
6. Run `xfs_growfs /dev/sda1` and check that the OS disk mounts with the new  partition size with `df -h`

	> We use **xfs_growfs** here for XFS here ([read more...](http://ask.xmodulo.com/expand-xfs-file-system.html)) instead of **resize2fs** for EXT2/EXT3/EXT4.

7. `yum install iptables-services`

8. `vi /etc/sysconfig/iptables`
	
		*filter
		:INPUT ACCEPT [0:0]
		:FORWARD ACCEPT [0:0]
		:OUTPUT ACCEPT [0:0]
		-A INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT
		-A INPUT -p icmp -j ACCEPT
		-A INPUT -i lo -j ACCEPT
		-A INPUT -p tcp -m state --state NEW -m tcp --dport 22 -j ACCEPT
		-A INPUT -m state --state NEW -m tcp -p tcp --dport 5672 -j ACCEPT
		-A INPUT -m state --state NEW -m tcp -p tcp --dport 9581 -j ACCEPT
		-A INPUT -m state --state NEW -m tcp -p tcp --dport 9582 -j ACCEPT
		-A INPUT -j REJECT --reject-with icmp-host-prohibited
		-A FORWARD -j REJECT --reject-with icmp-host-prohibited
		COMMIT

9. `service iptables restart`

10. `vi /etc/sysconfig/selinux` ([read more about selinux at redhat.com](https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Security-Enhanced_Linux/sect-Security-Enhanced_Linux-Enabling_and_Disabling_SELinux-Disabling_SELinux.html))
    
		# This file controls the state of SELinux on the system.
		# SELINUX= can take one of these three values:
		#     enforcing - SELinux security policy is enforced.
		#     permissive - SELinux prints warnings instead of enforcing.
		#     disabled - No SELinux policy is loaded.
		SELINUX=disabled
		# SELINUXTYPE= can take one of these two values:
		#     targeted - Targeted processes are protected,
		#     mls - Multi Level Security protection.
		SELINUXTYPE=targeted

11. `reboot` for the selinux changes to take effect.

12. Create user and group **awips:fxalpha**

	`groupadd fxalpha && useradd -G fxalpha awips`

	or if the awips account already exists:

	`groupadd fxalpha && usermod -G fxalpha awips`

13. Finally, install the EDEX server 

	`wget http://www.unidata.ucar.edu/software/awips2/installEDEX.sh`

	`chmod 755 ./installEDEX.sh`

	`./installEDEX.sh`

---

# LDM server (10.0.0.2)

A small LDM server to write data files to the file share `/awips2/data_store` and send messages to the EDEX machine (10.0.0.1) via **edexBridge**.

In the [Azure portal](https://portal.azure.com):

1. Create a new virtual machine with an *awips* user account
	* **CentOS 7.2**
	* **DS2_V2 Standard** (2 cores, 7 GB)
2. Start the VM, log in and `sudo su -` to root, then run

	* `wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/el7.repo`

	* `yum clean all`

	* `yum groupinstall awips2-ldm-server`

	* `vi /awips2/ldm/etc/ldmd.conf` to define the **edexBridge** server nane

			EXEC    "edexBridge -s 10.0.0.1"

	* `service edex_ldm start`

	> Note: You *do not* need to configure iptables on an LDM-only machine (only for EDEX).

