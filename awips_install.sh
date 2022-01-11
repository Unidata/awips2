#!/bin/bash -f
# about:  AWIPS install manager
# devorg: Unidata Program Center
# author: Michael James
# maintainer: <tiffanym@ucar.edu>
# use: ./awips_install.sh (--cave|--edex|--database|--ingest|--help)

dir="$( cd "$(dirname "$0")" ; pwd -P )"

usage="$(basename "$0") [-h] (--cave|--edex|--database|--ingest) #script to install Unidata AWIPS components.\n
    -h, --help           show this help text\n
    --cave               install CAVE for x86_64 Linux\n
    --edex, --server     install EDEX Standaone Server x86_64 Linux\n
    --database           install EDEX Request/Database x86_64 Linux\n
    --ingest             install EDEX Ingest Node Server x86_64 Linux\n"

function stop_edex_services {
  for srvc in edex_ldm edex_camel qpidd httpd-pypies edex_postgres ; do
    if [ -f /etc/init.d/$srvc ]; then
      service $srvc stop
    fi
  done
}

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

  wget_url="https://downloads.unidata.ucar.edu/awips2/current/linux/${repofile}"
  echo "wget -O /etc/yum.repos.d/awips2.repo ${wget_url}"
  wget -O /etc/yum.repos.d/awips2.repo ${wget_url}

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

function check_git {
  if ! [[ $(rpm -qa | grep ^git-[12]) ]]; then
    # install git if not installed
    yum install git -y

  fi
}

function check_cave {
  if [[ $(rpm -qa | grep awips2-cave) ]]; then
    echo $'\n'CAVE is currently installed and needs to be removed before installing.
    pkill cave.sh
    pkill -f 'cave/run.sh'
    remove_cave
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
    echo "found EDEX RPMs installed. The current EDEX needs to be removed before installing."
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

function remove_edex {
  while true; do
    read -p "`echo $'\n'`We want to back up some configuration files. What location do you want your files backed up to?
        If you choose not to back up files (you will lose all your configurations) type \"no\"`echo $'\n> '`" backup_dir

    backup_dir=$(echo $backup_dir | tr '[:upper:]' '[:lower:]')
    if [ $backup_dir = "no" ] || [ $backup_dir = "n" ]; then
        while true; do
          read -p "`echo $'\n'`Are you sure you don't want to back up any AWIPS configuraiton files? type \"yes\" to confirm.` echo $'\n> '`" answer
          answer=$(echo $answer | tr '[:upper:]' '[:lower:]')
          if [ $answer = yes ] || [ $answer = y ]; then
            break 2 ;
          else
            echo "Please answer \"yes\" to confirm you don't want to back up any AWIPS configuraiton files?"
          fi
        done
    elif [ ! -d $backup_dir ]; then
      echo "$backup_dir does not exist, enter a path that exists"

    else
      date=$(date +'%Y%m%d-%H:%M:%S')
      backup_dir=${backup_dir}/awips2_backup_${date}
      echo "Backing up to $backup_dir"

      if [ ! -d $backup_dir ]; then 
        mkdir -p $backup_dir
      fi    
      rsync -aP /awips2/database/data/pg_hba.conf $backup_dir/
      rsync -aP /awips2/edex/data/utility $backup_dir/
      rsync -aP /awips2/edex/bin $backup_dir/
      if [ ! -d $backup_dir/ldm ]; then
        mkdir -p $backup_dir/ldm
      fi
      rsync -aP /awips2/ldm/etc $backup_dir/ldm/
      rsync -aP /awips2/ldm/dev $backup_dir/ldm/
      rsync -aP /awips2/dev $backup_dir/
      rsync -aP /awips2/edex/conf $backup_dir/
      rsync -aP /awips2/edex/etc $backup_dir/
      rsync -aP /awips2/edex/logs $backup_dir/
      rsync -aP /usr/bin/edex $backup_dir/
      rsync -aP /etc/init.d/edexServiceList $backup_dir/init.d/
      rsync -aP /var/spool/cron/awips $backup_dir/
      break;
    fi
  done

  if [[ $(rpm -qa | grep awips2-cave) ]]; then
    echo "CAVE is also installed, now removing EDEX and CAVE"
    pkill cave.sh
    pkill -f 'cave/run.sh'
    rm -rf /home/awips/caveData
  else
    echo "Now removing EDEX"
  fi

  yum groupremove awips2-server awips2-database awips2-ingest awips2-cave awips2-qpid-lib -y

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
    for dir in $(ls /awips2/); do
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
  check_cave
  check_users
  check_yumfile
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
        echo "CAVE has finished installing, the install log can be found in /tmp/awips-install.log"
        ;;
    --server|--edex)
        server_prep
        yum groupinstall awips2-server -y 2>&1 | tee -a /tmp/awips-install.log
        sed -i 's/@LDM_PORT@/388/' /awips2/ldm/etc/registry.xml 
        echo "EDEX server has finished installing, the install log can be found in /tmp/awips-install.log"
        ;;
    --database)
        server_prep
        yum groupinstall awips2-database -y 2>&1 | tee -a /tmp/awips-install.log
        disable_ndm_update
        sed -i 's/@LDM_PORT@/388/' /awips2/ldm/etc/registry.xml 
        echo "EDEX database has finished installing, the install log can be found in /tmp/awips-install.log"
        ;;
    --ingest)
        server_prep
        yum groupinstall awips2-ingest -y 2>&1 | tee -a /tmp/awips-install.log
        disable_ndm_update
        sed -i 's/@LDM_PORT@/388/' /awips2/ldm/etc/registry.xml 
        echo "EDEX ingest has finished installing, the install log can be found in /tmp/awips-install.log"
        ;;
    -h|--help)
        echo -e $usage
        exit
        ;;
esac

PATH=$PATH:/awips2/edex/bin/
exit

