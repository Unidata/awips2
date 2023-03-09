#!/bin/bash
# about:  AWIPS install manager
# devorg: Unidata Program Center
# author: Michael James, Tiffany Meyer
# maintainer: <support-awips@unidata.ucar.edu>
# Date Updated: 2/17/2023
# use: ./awips_install-v20.sh (--cave|--help)

dir="$( cd "$(dirname "$0")" ; pwd -P )"

usage="$(basename "$0") [-h] (--cave) #script to install Unidata AWIPS CAVE release.\n
    -h, --help           show this help text\n
    --cave               install CAVE for x86_64 Linux\n"

function check_yumfile {
  if [[ $(grep "release 7" /etc/redhat-release) ]]; then
    repofile=awips2.repo
  else
    echo "You need to be running CentOS7 or RedHat7"
    exit
  fi
  if [ -f /etc/yum.repos.d/awips2.repo ]; then
    date=$(date +%Y%m%d-%H:%M:%S)
    cp /etc/yum.repos.d/awips2.repo /etc/yum.repos.d/awips2.repo-${date}
  fi

  wget_url="https://downloads.unidata.ucar.edu/awips2/20.3.2/linux/${repofile}"
  echo "wget -O /etc/yum.repos.d/awips2.repo ${wget_url}"
  wget -O /etc/yum.repos.d/awips2.repo ${wget_url}

  sed -i 's/enabled=0/enabled=1/' /etc/yum.repos.d/awips2.repo

  yum clean all --enablerepo=awips2repo --disablerepo="*" 1>> /dev/null 2>&1
  yum --enablerepo=awips2repo clean metadata
}

function check_limits {
  if [[ ! $(grep awips /etc/security/limits.conf) ]]; then
    echo "Checking /etc/security/limits.conf for awips: Not found. Adding..."
    printf "awips soft nproc 65536\nawips soft nofile 65536\n" >> /etc/security/limits.conf
  fi
}

function check_epel {
  if [[ ! $(rpm -qa | grep epel-release) ]]; then
    yum install epel-release -y
    yum clean all
  fi
}

function check_wget {
  if ! [[ $(rpm -qa | grep ^wget) ]]; then
    # install wget if not installed
    yum install wget -y
  fi
}

function check_rsync {
  if ! [[ $(rpm -qa | grep ^rsync) ]]; then
    # install rsync if not installed
    yum install rsync -y
  fi
}


function check_netcdf {
  if [[ $(rpm -qa | grep netcdf-AWIPS) ]]; then
    # replaced by epel netcdf(-devel) pkgs in 17.1.1-5 so force remove
    yum remove netcdf-AWIPS netcdf netcdf-devel -y
  fi
}

function check_cave {
  if [[ $(rpm -qa | grep awips2-cave-20) ]]; then
    echo $'\n'CAVE is currently installed and needs to be removed before installing.
    pkill cave.sh
    pkill -f 'cave/cave.sh'
    remove_cave
  fi

  check_edex
  if [[ $(rpm -qa | grep awips2-cave-18) ]]; then
  while true; do
    read -p "Version 18.* of CAVE is currently installed and needs to be removed before installing the Beta Version 20.* of CAVE. Do you wish to remove CAVE? (Please type yes or no) `echo $'\n> '`" yn
    case $yn in
      [Yy]* ) remove_cave; break;;
      [Nn]* ) echo "Exiting..."; exit;;
      * ) echo "Please answer yes or no"
    esac
  done
  fi
}

function remove_cave {
  yum groupremove awips2-cave -y

  if [[ $(rpm -qa | grep awips2-cave) ]]; then
    echo "
    =================== FAILED ===========================
    Something went wrong with the un-install of CAVE 
    and packages are still installed. Once the CAVE
    group has been successfully uninstalled, you can try
    running this script again.
     Try running a \"yum grouplist\" to see if the AWIPS 
     CAVE group is still installed and then do a 
     \"yum groupremove [GROUP NAME]\". 
       ex. yum groupremove 'AWIPS EDEX Server' 
     
     You may also need to run \"yum groups mark 
     remove [GROUP NAME]\"
       ex. yum groups mark remove 'AWIPS CAVE'"
     exit
  else
    dir=cave
    echo "Removing /awips2/$dir"
    rm -rf /awips2/$dir
    rm -rf /home/awips/caveData
  fi
}

function check_edex {
  if [[ $(rpm -qa | grep awips2-edex) ]]; then
    echo "Found EDEX RPMs installed. The current EDEX needs to be removed before installing version 20.* of CAVE (beta release)."
    check_remove_edex
  else
    if [ -d /awips2/database/data/ ]; then
      echo "cleaning up /awips2/database/data/ for new install..."
      rm -rf /awips2/database/data/
    fi
  fi
  for dir in /awips2/tmp /awips2/data_store ; do
    if [ ! -d $dir ]; then
      echo "creating $dir"
      mkdir -p $dir
      chown awips:fxalpha $dir
    fi
  done
  if getent passwd awips &>/dev/null; then
    echo -n ''
  else
    echo
    echo "--- user awips does not exist"
    echo "--- installation will continue but EDEX services may not run as intended"
  fi
}

function check_remove_edex {
  while true; do
    read -p "Do you wish to remove EDEX? (Please type yes or no) `echo $'\n> '`" yn
    case $yn in
      [Yy]* ) remove_edex; break;;
      [Nn]* ) echo "Exiting..."; exit;;
      * ) echo "Please answer yes or no"
    esac
  done
}

function calcLogSpace {
  a=("$@")
  logDiskspace=0
  for path in "${a[@]}" ; do
    if [ -d $path ] || [ -f $path ]; then
      out=`du -sk $path | cut -f1`
      logDiskspace=$((logDiskspace + $out))
    fi
  done
  logDiskspace=$(echo "scale=8;$logDiskspace*.000000953674316" | bc)
}

function calcConfigSpace {
  a=("$@")
  configDiskspace=0
  for path in "${a[@]}" ; do
    if [ -d $path ] || [ -f $path ]; then
      out=`du -sk $path | cut -f1`
      configDiskspace=$((configDiskspace + $out))
    fi
  done
  configDiskspace=$(echo "scale=8;$configDiskspace*.000000953674316" | bc)
}

function backupLogs {
  a=("$@")
  log_backup_dir=${backup_dir}/awips2_backup_${ver}_${date}/logs

  if [[ ! -d ${log_backup_dir} ]]; then
    mkdir -p ${log_backup_dir}
  fi
  echo "Backing up to $log_backup_dir"
  for path in "${a[@]}" ; do
    if [ -d $path ] || [ -f $path ]; then
      rsync -apR $path $log_backup_dir
    fi
  done
}

function backupConfigs {

  a=("$@")
  config_backup_dir=${backup_dir}/awips2_backup_${ver}_${date}/configs

  if [[ ! -d $config_backup_dir ]]; then
        mkdir -p $config_backup_dir
      fi
  echo "Backing up to $config_backup_dir"
  for path in "${a[@]}" ; do
    if [ -d $path ] || [ -f $path ]; then
      rsync -apR $path $config_backup_dir
    fi
  done
}

function remove_edex {
  logPaths=("/awips2/edex/logs" "/awips2/httpd_pypies/var/log/httpd/" "/awips2/database/data/pg_log/" "/awips2/qpid/log/" "/awips2/ldm/logs/")
  configPaths=("/awips2/database/data/pg_hba*conf" "/awips2/edex/data/utility" "/awips2/edex/bin" "/awips2/ldm/etc" "/awips2/ldm/dev" "/awips2/edex/conf" "/awips2/edex/etc" "/usr/bin/edex" "/etc/init*d/edexServiceList" "/var/spool/cron/awips")

  while true; do
    read -p "`echo $'\n'`Please make a selction for what you would like backed up. If you choose not to back up files you will lose all your configurations:
1. logs
2. configs
3. both logs and configs
4. none
`echo $'\n> '`" backup_ans

#User chooses to back of files
    if [[ $backup_ans =~ [1-3] ]]; then
      echo "ANSWER: $backup_ans"
      while true; do 
        read -p "`echo $'\n'`What location do you want your files backed up to? `echo $'\n> '`" backup_dir

        if [ ! -d $backup_dir ]; then
          echo "$backup_dir does not exist, enter a path that exists"
        else
          #Check to see if user has enough space to backup
          backupspace=`df -k --output=avail "$backup_dir" | tail -n1`
          backupspace=$(echo "scale=8;$backupspace*.000000953674316" | bc)
          date=$(date +'%Y%m%d-%H:%M:%S')
       
          echo "Checking to see which version of AWIPS is installed..."   
          rpm=`rpm -qa | grep awips2-[12]`
          IFS='-' str=(${rpm})
          IFS=. str2=(${str[2]})
          vers="${str[1]}-${str2[0]}"
          ver="${vers//[.]/-}"

          if [ $backup_ans = 1 ]; then
            calcLogSpace "${logPaths[@]}"
            #Don't let user backup data if there isn't enough space
            if (( $(echo "$logDiskspace > $backupspace" | bc ) )); then
              printf "You do not have enough disk space to backup this data to $backup_dir. You only have %.2f GB free and need %.2f GB.\n" $backupspace $logDiskspace
            #Backup logs
            else 
              backupLogs "${logPaths[@]}"
              printf "%.2f GB of logs were backed up to $backup_dir \n" "$logDiskspace"
            fi
          elif [ $backup_ans = 2 ]; then
            calcConfigSpace "${configPaths[@]}"
            #Don't let user backup data if there isn't enough space
            if (( $(echo "$configDiskspace > $backupspace" | bc ) )); then
              printf "You do not have enough disk space to backup this data to $backup_dir. You only have %.2f GB free and need %.2f GB.\n" $backupspace $configDiskspace
            #Backup logs
            else
              backupConfigs "${configPaths[@]}"
              printf "%.2f GB of configs were backed up to $backup_dir \n" "$configDiskspace"
            fi
          elif [ $backup_ans = 3 ]; then
            calcLogSpace "${logPaths[@]}"
            calcConfigSpace "${configPaths[@]}"
            configLogDiskspace=$( echo "$logDiskspace+$configDiskspace" | bc)
            #Don't let user backup data if there isn't enough space
            if (( $(echo "$configLogDiskspace > $backupspace" | bc ) )); then
               printf "You do not have enough disk space to backup this data to $backup_dir . You only have %.2f GB free and need %.2f GB.\n" $backupspace $configLogDiskspace
            #Backup logs
            else
              backupLogs "${logPaths[@]}"
              backupConfigs "${configPaths[@]}"
              printf "%.2f GB of logs and configs were backed up to $backup_dir \n" "$configLogDiskspace"
            fi
          fi 
          break
        fi
      done
      break
#User chooses not to back up any files
    elif [ $backup_ans = 4 ]; then
        while true; do
          read -p "`echo $'\n'`Are you sure you don't want to back up any AWIPS configuration or log files? Type \"yes\" to confirm, \"no\" to select a different backup option, or \"quit\" to exit` echo $'\n> '`" answer
          answer=$(echo $answer | tr '[:upper:]' '[:lower:]')
          if [ $answer = yes ] || [ $answer = y ]; then
            break 2 ;
          elif [ $answer = quit ] || [ $answer = q ]; then
            exit;
          elif [ $answer = no ] || [ $answer = n ]; then
            break
          fi
        done
#User did not make a valid selection
    else 
      echo "Please make a valid selection (1, 2, 3, or 4)"
    fi
  done

  FILE="/opt/bin/logarchival/edex_upgrade.pl"

  if test -f "$FILE"; then
    echo "Running /opt/bin/logarchival/edex_upgrade.pl and logging to /home/awips/crons/logarchival/general"
    /opt/bin/logarchival/edex_upgrade.pl >> /home/awips/crons/logarchival/general
  fi

  if [[ $(rpm -qa | grep awips2-cave) ]]; then
    echo "CAVE is also installed, now removing EDEX and CAVE"
    pkill cave.sh
    pkill -f 'cave/run.sh'
    rm -rf /home/awips/caveData
  else
    echo "Now removing EDEX"
  fi

  yum groupremove awips2-server awips2-database awips2-ingest awips2-cave -y
  yum remove awips2-* -y
  if [[ $(rpm -qa | grep awips2 | grep -v cave) ]]; then
    echo "
    =================== FAILED ===========================
    Something went wrong with the un-install of EDEX 
    and packages are still installed. Once the EDEX
    groups have been successfully uninstalled, you can try
    running this script again.
     Try running a \"yum grouplist\" to see which AWIPS 
     group is still installed and then do a 
     \"yum groupremove [GROUP NAME]\". 
       ex. yum groupremove 'AWIPS EDEX Server' 
     
     You may also need to run \"yum groups mark 
     remove [GROUP NAME]\"
       ex. yum groups mark remove 'AWIPS EDEX Server'"
     exit
  else
    awips2_dirs=("data" "database" "data_store" "edex" "hdf5" "httpd_pypies" "java" "ldm" "postgres" "psql" "pypies" "python" "qpid" "tmp" "tools" "yajsw")
    for dir in ${awips2_dirs[@]}; do
      if [ $dir != dev ] && [ $dir != cave ] ; then
        echo "Removing /awips2/$dir"
        rm -rf /awips2/$dir
      fi
    done
  fi
}

function check_users {
  if ! id "awips" >/dev/null 2>&1; then
    groupadd fxalpha && useradd -G fxalpha awips
  fi
}

function server_prep {
  check_users
  check_yumfile
  stop_edex_services
  check_limits
  check_netcdf
  check_wget
  check_rsync
  check_edex
  check_git
  check_epel
}

function disable_ndm_update {
  crontab -u awips -l >cron_backup
  crontab -u awips -r
  sed -i -e 's/30 3 \* \* \* \/bin\/perl \/awips2\/dev\/updateNDM.pl/#30 3 \* \* \* \/bin\/perl \/awips2\/dev\/updateNDM.pl/' cron_backup
  crontab -u awips cron_backup
  rm cron_backup
}

function cave_prep {
  check_users
  check_yumfile
  check_cave
  check_netcdf
  check_wget
  check_epel
 rm -rf /home/awips/caveData 
}

if [ $# -eq 0 ]; then
  key="-h"
else
  key="$1"
fi
case $key in
    --cave)
        cave_prep
        yum groupinstall awips2-cave -y 2>&1 | tee -a /tmp/awips-install.log
        sed -i 's/enabled=1/enabled=0/' /etc/yum.repos.d/awips2.repo
        echo "CAVE has finished installing, the install log can be found in /tmp/awips-install.log"
        ;;
    --server|--edex)
        echo "EDEX is not available to install for AWIPS Version 20.* (beta release). To install CAVE use the --cave flag\n"
        exit
        ;;
    -h|--help)
        echo -e $usage
        exit
        ;;
esac

PATH=$PATH:/awips2/edex/bin/
exit

