#!/bin/bash
#
# 06/22/2016 - NeilG
#
# Script to migrate /awips2/rcm/data/config
# to NAS for DR18728

function defineVars {

RESET_TXT="\033[0m"
BOLD_TXT="\033[1m"
RED_TXT="\033[1;31m"
GREEN_TXT="\033[1;32m"
YELLOW_TXT="\033[1;33m"

}

function rootCheck {
if [[ "$( whoami )" != "root" ]]; then
    	echo -ne "\t\t${RED_TXT}ERROR: This script can only be run as root. ${RESET_TXT}\n"
    	echo -ne "\t\t${RED_TXT}Please change to the root user and re-run this script. ${RESET_TXT}\n\n"
    	exit 1
fi
}

function runHostCheck {
case `hostname | cut -d- -f1` in
	dx1|dx2 )       ;;
	*       )       echo -e "\t\t${RED_TXT}This script can only be run on dx1 or dx2. ${RESET_TXT}"
			echo -e "\t\t${RED_TXT}Please ssh to dx1 and re-run this script. ${RESET_TXT}\n"
                      	echo "END_SCRIPT"
                      	exit 1
                      	;;
esac
}

function hostCheck {
for dbserver in dx1 dx2; do
	if [[ "$( echo ${dbserver}-${SITE_IDENTIFIER} )" != "$( ssh -qn ${dbserver} hostname )" ]] ; then
		echo -e "\t\t${RED_TXT}ERROR ${dbserver} is inaccessible. Please re-run when both DX1 & DX2 are up. ${RESET_TXT}"
	fi
done
}

function rcmConfigBackup {
for dbserver in dx1 dx2; do
	echo -en "${BOLD_TXT}--> Backing up /awips2/rcm/data/config directory on $dbserver... ${RESET_TXT}"
	ssh -qn $dbserver "tar -czvf /data/fxa/awips2-rcm-data-config_${dbserver}.tgz /awips2/rcm/data/config" &> /tmp/${dbserver}_rcmbackup.txt
done
	
if [ -f /data/fxa/awips2-rcm-data-config_dx1.tgz && -f /data/fxa/awips2-rcm-data-config_dx2.tgz ]; then
	if [ -s /data/fxa/awips2-rcm-data-config_dx1.tgz && -s /data/fxa/awips2-rcm-data-config_dx2.tgz ]; then
		echo -e "${GREEN_TXT} SUCCESS. ${RESET_TXT}"
	else
		echo -e "${RED_TXT}FAILED\n\t\tBackup tarballs exist, but are zero byte. Please contact the NCF. Script will now exit. ${RESET_TXT}"
		echo -e "${RED_TXT}\t\tThe following output was given when trying the last operation:${RESET_TXT}"
		echo   
		cat /tmp/dx1_rcmbackup.txt;echo;cat /tmp/dx2_rcmbackup.txt
		echo  
		exit 1
	fi
else
	echo -e "${RED_TXT}FAILED\n\t\tBackup tarballs were not created. Please contact the NCF. Script will now exit. ${RESET_TXT}"		
	echo -e "${RED_TXT}\t\tThe following output was given when trying the last operation:${RESET_TXT}"
	echo
	cat /tmp/dx1_rcmbackup.txt;echo;cat /tmp/dx2_rcmbackup.txt
	echo
	exit 1
fi
}

function rcmQtreeCreate {
echo -en "${BOLD_TXT}--> Making sure auto-export is disabled on NAS first...${RESET_TXT}"
sleep 1
ssh -qn -o StrictHostKeyChecking=no nas1 "options nfs.export.auto-update off" &> /tmp/create_out1.txt

if [ ! "$(ssh -qn -o StrictHostKeyChecking=no nas1 \"options nfs.export.auto-update\" | awk '{ print $2 }')" != "off" ]; then
	echo -e "${RED_TXT}FAILED\n\t\tError while trying to disable auto-export on NAS. Please contact the NCF. Script will now exit. ${RESET_TXT}"
	echo -e "\t\t${BOLD_TXT}The following output was given when trying the last operation:${RESET_TXT}"
	echo
	cat /tmp/create_out1.txt
	echo
	exit 1
else
	echo -e "${GREEN_TXT} SUCCESS. ${RESET_TXT}"
fi
 
echo -en "${BOLD_TXT}--> Creating a new awips2RCM qtree...${RESET_TXT}"
sleep 1
ssh -qn -o StrictHostKeyChecking=no nas1 "qtree create /vol/vol_awipsNFS/awips2RCM" &> /tmp/create_out2.txt

if [[ "$(ssh -qn -o StrictHostKeyChecking=no nas1 "qtree status" | grep awips2RCM)" != "vol_awipsNFS awips2RCM unix  enabled  normal" ]]; then
	echo -e "\n\t\t${RED_TXT}ERROR trying to create awips2RCM qtree. Please contact the NCF. Exiting... ${RESET_TXT}"
	echo -e "\t\t${BOLD_TXT}The following output was given when trying the last operation:${RESET_TXT}"
	echo
	cat /tmp/create_out2.txt
	echo
	exit 1
else
	echo -e "${GREEN_TXT} SUCCESS. ${RESET_TXT}"
fi

echo -en "${BOLD_TXT}--> Setting size of awips2RCM qtree to 5G...${RESET_TXT}"
sleep 1
ssh -qn -o StrictHostKeyChecking=no nas1 "wrfile -a /etc/quotas \"/vol/vol_awipsNFS/awips2RCM tree 5G\"" &> /tmp/create_out3.txt

if [[ "$(ssh -qn -o StrictHostKeyChecking=no nas1 \"rdfile /etc/quotas\" | grep awips2RCM | awk '{ print $3 }')" != "5G" ]]; then
	echo -e "\n\t\t${RED_TXT}ERROR trying to set size of awips2RCM. Please contact the NCF. Exiting... ${RESET_TXT}"
	echo -e "\t\t${BOLD_TXT}The following output was given when trying the last operation:${RESET_TXT}"
	echo
	cat /tmp/create_out3.txt
	echo
	exit 1
else
	echo -e "${GREEN_TXT} SUCCESS. ${RESET_TXT}"
fi

echo -en "${BOLD_TXT}--> Initializing quota...${RESET_TXT}"
sleep 1
ssh -qn -o StrictHostKeyChecking=no nas1 "quota off /vol/vol_awipsNFS; quota on /vol/vol_awipsNFS" &> /tmp/create_out4.txt

if [[ "$(ssh -qn -o StrictHostKeyChecking=no nas1 \"quota status vol_awipsNFS\" | awk '{ print $4 }')" != "on." ]]; then
	echo -e"\n\t\t${RED_TXT}ERROR trying to initialize quota. Please contact the NCF. Exiting... ${RESET_TXT}"
	echo -e "\t\t${BOLD_TXT}The following output was given when trying the last operation:${RESET_TXT}"
        echo
	cat /tmp/create_out4.txt
	echo
	exit 1
else
	echo -e "${GREEN_TXT} SUCCESS. ${RESET_TXT}"
fi

echo -en "${BOLD_TXT}--> Exporting awips2RCM qtree and making it permanent...${RESET_TXT}"
sleep 1
ssh -qn -o StrictHostKeyChecking=no nas1 "exportfs -io actual=/vol/vol_awipsNFS/awips2RCM,sec=sys,root=@DX_SERVERS,nosuid /awips2RCM" &> /tmp/create_out5.txt
sleep 1
ssh -qn -o StrictHostKeyChecking=no nas1 "exportfs -w /etc/exports" &> /tmp/create_out6.txt

if [[ "$(ssh -qn nas1 \"rdfile /etc/exports\" | grep awips2RCM)" != "/awips2RCM	-actual=/vol/vol_awipsNFS/awips2RCM,sec=sys,rw,root=@DX_SERVERS,nosuid" ]]; then
	echo -e "\t\t${RED_TXT}ERROR creating and making permanent awips2RCM export. Please contact the NCF. Exiting... ${RESET_TXT}"
        echo -e "\t\t${BOLD_TXT}The following output was given when trying the last operation:${RESET_TXT}"
        echo 
        cat /tmp/create_out5.txt;echo;cat /tmp/create_out6.txt
        echo
	exit 1
else
	echo -e "${GREEN_TXT} SUCCESS. ${RESET_TXT}"
fi

echo -en "${BOLD_TXT}--> Adding new awips2RCM NAS export to DX1 & DX2...${RESET_TXT}"
sleep 1
rcmTAB="nas1:/awips2RCM            /awips2/edex/rcm/data/config          tcp,timeo=600,defaults"
for dbserver in dx1 dx2; do
	if ! ssh -qn -o ConnectTimeout=10 -o StrictHostKeyChecking=no $dbserver "echo -e \"${rcmTAB}\" >> /etc/fstab; ;mkdir -p /awips2/edex/rcm/data/config; chmod -R 000 /awips2/edex/rcm/data/config" &> /tmp/create_out7_${dbserver}.txt; then
		echo -e "${RED_TXT}FAILED\n\t\tError importing /awipsRCM to $dbserver. Please contact the NCF. Will keep going though.${RESET_TXT}"
		echo -e "\t\t${BOLD_TXT}The following output was given when trying the last operation:${RESET_TXT}"
        	echo 
        	cat /tmp/create_out_${dbserver}.txt
        	echo
	else
		echo -e "${GREEN_TXT} SUCCESS. ${RESET_TXT}"
	fi
done
	
}

function restoreRcmConfig {
echo -e "${BOLD_TXT}--> Restoring /awips2/rcm/data/config. This script will only restore DX1's config directory...${RESET_TXT}"
echo -e "${BOLD_TXT}    If site's config directories differed between DX1 & DX2 prior to backup, then site may need ${RESET_TXT}"
echo -e "${BOLD_TXT}    to access both DX1 & DX2's backup tarballs and obtain whatever they need. These are located ${RESET_TXT}"
echo -e "${BOLD_TXT}    within /data/fxa/: awips2-rcm-data-config_dx[1|2].tgz ${RESET_TXT}"
echo
echo -en "${BOLD_TXT}---> Restoring....."
sleep 1
cd /
if ! tar -xzvf /data/fxa/awips2-rcm-data-config_dx1.tgz &> /tmp/rcm_restore.out; then
	echo -e "${RED_TXT}FAILED\n\t\tError restoring /awips2/rcm/data/config from the dx1 tarball. Please contact the NCF. ${RESET_TXT}"
	echo -e "\t\t${BOLD_TXT}The following output was given when trying the last operation:${RESET_TXT}"
	echo
	cat /tmp/rcm_restore.out
	echo
else
	echo -e "${GREEN_TXT} SUCCESS. ${RESET_TXT}"
fi
	


# MAIN
defineVars
{
echo -e "${GREEN_TXT}$(date +%x_%R)\tStarting Migration of /awips2/rcm/data/config to NAS\n${RESET_TXT}"
rootCheck
runHostCheck
hostCheck
rcmConfigBackup
rcmQtreeCreate
restoreRcmConfig
echo -e "${GREEN_TXT}$(date +%x_%R)\tScript Finished${RESET_TXT}"
} 2>&1 | tee -a >(sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g' > ${logDir}/migrateRcmDataConfigToNAS.log

exit 0
