#!/bin/bash

# This script sets specified unix permissions on NFS mounts.
# It logs in to a few different boxes via SSH and changes permissions on
# specified NFS mounts.
#
# This script should be run during AWIPS II post-install, and can also be run
# periodically to enforce NFS mount permissions.
#
# Author: tgurney

now=$(date +%Y%m%d_%H%M%S)

ssh dv1 << EOF & disown
bash
set -x

set_nfs_perms () {
echo Starting $(date +%Y%m%d_%H%M%S)
set -x
# /awips2/rcm/data/config
find /awips2/rcm/data/config -xdev -print0 | xargs -0r chown awips:fxalpha
find '/awips2/rcm/data/config' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/rcm/data/config' -xdev -type d -print0 | xargs -0r chmod 770

# /archive
find /archive -xdev -print0 | xargs -0r chown awips:fxalpha
find '/archive' -xdev -type f -print0 | xargs -0r chmod 640
find '/archive' -xdev -type d -print0 | xargs -0r chmod 750

# /data_store
find /data_store -xdev -not -path '/data_store/manual*' -print0 | xargs -0r chown ldm:fxalpha
find /data_store/manual -xdev -print0 | xargs -0r chown awips:fxalpha
find '/data_store' -xdev -type f -print0 | xargs -0r chmod 660
find '/data_store' -xdev -type d -print0 | xargs -0r chmod 770
echo Finished $(date +%Y%m%d_%H%M%S)
}

export -f set_nfs_perms
nohup bash -c set_nfs_perms > /data/fxa/INSTALL/a2logs/set_nfs_perms_dv1_${now}.log 2>&1 & disown
exit
EOF


ssh dv3 << 'EOF' & disown
bash
set -x

set_nfs_perms () {
echo Starting $(date +%Y%m%d_%H%M%S)
set -x
# /awips2/edex/data and subdirectories
find /awips2/edex/data -xdev -print0 | xargs -0r chown awips:fxalpha

# files excluding bin
find '/awips2/edex/data' -xdev -regextype posix-egrep \
    -not -path "/awips2/edex/data/manual*" \
    -not -path "/awips2/edex/data/utility*" \
    -not -path "/awips2/edex/data/share*" \
    -not -path "/awips2/edex/data/fxa/trigger*" \
    -not -path "*/bin/*" \
    -not -regex '.*(sh|ksh|csh|py)$' \
    -type f -print0 | xargs -0r chmod 660

# bin
find '/awips2/edex/data' -xdev -regextype posix-egrep \
    -not -path "/awips2/edex/data/manual*" \
    -not -path "/awips2/edex/data/utility*" \
    -not -path "/awips2/edex/data/share*" \
    -not -path "/awips2/edex/data/fxa/trigger*" \
    \( -path "*/bin/*" \
    -o -regex '.*(sh|ksh|csh|py)$' \) \
    -type f -print0 | xargs -0r chmod 770

# directories
find '/awips2/edex/data' -xdev \
    -not -path "/awips2/edex/data/manual*" \
    -not -path "/awips2/edex/data/utility*" \
    -not -path "/awips2/edex/data/share*" \
    -not -path "/awips2/edex/data/fxa/trigger*" \
    -type d -print0 | xargs -0r chmod 770

# /awips2/edex/data/share, excluding bin
find '/awips2/edex/data/share' -xdev -regextype posix-egrep \
    -not -path "/awips2/edex/data/share/hydroapps*" \
    -not -path "*/bin/*" \
    -not -regex '.*(sh|ksh|csh|py)$' \
    -type f -print0 | xargs -0r chmod 660

# /awips2/edex/data/share, bin only
find '/awips2/edex/data/share' -xdev -regextype posix-egrep \
    -not -path "/awips2/edex/data/share/hydroapps*" \
    \( -path "*/bin/*" \
    -o -regex '.*(sh|ksh|csh|py)$' \) \
    -type f -print0 | xargs -0r chmod 760

# /awips2/edex/data/share, directories
find '/awips2/edex/data/share' -xdev -type d -print0 | xargs -0r chmod 770

# hydroapps all files get u+x,g+x
find '/awips2/edex/data/share/hydroapps' -xdev -type f -print0 | xargs -0r chmod 770

find '/awips2/edex/data/manual' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/edex/data/manual' -xdev -type d -print0 | xargs -0r chmod 770
find '/awips2/edex/data/utility' -xdev -type f -print0 | xargs -0r chmod 640
find '/awips2/edex/data/utility' -xdev -type d -print0 | xargs -0r chmod 750
find '/awips2/edex/data/fxa/trigger' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/edex/data/fxa/trigger' -xdev -type d -print0 | xargs -0r chmod 770
echo Finished $(date +%Y%m%d_%H%M%S)
}

export -f set_nfs_perms
nohup bash -c set_nfs_perms >>/data/fxa/INSTALL/a2logs/set_nfs_perms_dv3_${now}.log 2>&1 & disown
exit
EOF


ssh pv1 << EOF & disown
bash
set -x

set_nfs_perms () {
echo Starting $(date +%Y%m%d_%H%M%S)
set -x
# /awips2/bmh/conf
find /awips2/bmh/conf -xdev -print0 | xargs -0r chown awips:fxalpha
find '/awips2/bmh/conf' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/bmh/conf' -xdev -type d -print0 | xargs -0r chmod 770

# /awips2/bmh/data
find /awips2/bmh/data -xdev -print0 | xargs -0r chown awips:fxalpha
find '/awips2/bmh/data' -xdev -type f -print0 | xargs -0r chmod 664
find '/awips2/bmh/data' -xdev -type d -print0 | xargs -0r chmod 775

# /awips2/bmh/neospeech/result
find /awips2/bmh/neospeech/result -xdev -print0 | xargs -0r chown awips:fxalpha
find '/awips2/bmh/neospeech/result' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/bmh/neospeech/result' -xdev -type d -print0 | xargs -0r chmod 770

# /awips2/GFESuite and subdirectories
find /awips2/GFESuite -xdev -print0 | xargs -0r chown awips:fxalpha
find '/awips2/GFESuite' -xdev \
    -not -path "/awips2/GFESuite/bin*" \
    -not -path "/awips2/GFESuite/logs*" \
    -not -path "/awips2/GFESuite/products/ISC*" \
    -not -path "/awips2/GFESuite/ServiceBackup/scripts*" \
    -not -path "/awips2/GFESuite/ServiceBackup/logs*" \
    -not -path "/awips2/GFESuite/ServiceBackup/svcbu*" \
    -not -path "/awips2/GFESuite/hti/data*" \
    -not -path "/awips2/GFESuite/hti/logs*" \
    -not -path "/awips2/GFESuite/nwps*" \
    -type f -print0 | xargs -0r chmod 750
find '/awips2/GFESuite' -xdev \
    -not -path "/awips2/GFESuite/exportgrids*" \
    -not -path "/awips2/GFESuite/logs*" \
    -not -path "/awips2/GFESuite/products/ISC*" \
    -not -path "/awips2/GFESuite/ServiceBackup/logs*" \
    -not -path "/awips2/GFESuite/ServiceBackup/svcbu*" \
    -not -path "/awips2/GFESuite/hti/data*" \
    -not -path "/awips2/GFESuite/hti/logs*" \
    -not -path "/awips2/GFESuite/nwps*" \
        -type d -print0 | xargs -0r chmod 750
find '/awips2/GFESuite/bin' -xdev -maxdepth 1 -type f -print0 | xargs -0r chmod 755
find '/awips2/GFESuite/exportgrids' -xdev -type d -print0 | xargs -0r chmod 755
find '/awips2/GFESuite/logs' -xdev -type f -print0 | xargs -0r chmod 666
find '/awips2/GFESuite/logs' -xdev -type d -print0 | xargs -0r chmod 777
find '/awips2/GFESuite/products/ISC' -xdev -type f -print0 | xargs -0r chmod 666
find '/awips2/GFESuite/products/ISC' -xdev -type d -print0 | xargs -0r chmod 777
find '/awips2/GFESuite/ServiceBackup/logs' -xdev -type f -print0 | xargs -0r chmod 666
find '/awips2/GFESuite/ServiceBackup/logs' -xdev -type d -print0 | xargs -0r chmod 777
find '/awips2/GFESuite/ServiceBackup/scripts' -xdev -maxdepth 1 -type f -print0 | xargs -0r chmod 755
find '/awips2/GFESuite/ServiceBackup/svcbu' -xdev -type f -print0 | xargs -0r chmod 666
find '/awips2/GFESuite/ServiceBackup/svcbu' -xdev -type d -print0 | xargs -0r chmod 777
find '/awips2/GFESuite/hti/data' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/GFESuite/hti/data' -xdev -type d -print0 | xargs -0r chmod 770
find '/awips2/GFESuite/hti/logs' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/GFESuite/hti/logs' -xdev -type d -print0 | xargs -0r chmod 770
find '/awips2/GFESuite/nwps/bin' -xdev -print0 | xargs -0r chmod 770
find '/awips2/GFESuite/nwps' -xdev \
    -not -path '/awips2/GFESuite/nwps/bin*' \
    -type f -print0 | xargs -0r chmod 660
find '/awips2/GFESuite/nwps' -xdev \
    -not -path '/awips2/GFESuite/nwps/bin*' \
    -type d -print0 | xargs -0r chmod 770

# /nsbn_store
if df -t nfs /nsbn_store >/dev/null 2>&1; then
    find /nsbn_store -xdev -print0 | xargs -0r chown awpdbnet:fxalpha
    find '/nsbn_store' -xdev -type f -print0 | xargs -0r chmod 660
    find '/nsbn_store' -xdev -type d -print0 | xargs -0r chmod 770
fi

# qpid message store - pv1 edge case
if df -t nfs /awips2/qpid/edexMessageStore >/dev/null 2>&1; then
    find /awips2/qpid/edexMessageStore -xdev -print0 | xargs -0r chown awips:fxalpha
    find '/awips2/qpid/edexMessageStore' -xdev -type f -print0 | xargs -0r chmod 600
    find '/awips2/qpid/edexMessageStore' -xdev -type d -print0 | xargs -0r chmod 700
fi
if df -t nfs /awips2/qpid/messageStore >/dev/null 2>&1; then
    find /awips2/qpid/messageStore -xdev -print0 | xargs -0r chown awips:fxalpha
    find '/awips2/qpid/messageStore' -xdev -type f -print0 | xargs -0r chmod 600
    find '/awips2/qpid/messageStore' -xdev -type d -print0 | xargs -0r chmod 700
fi

# /home
find '/home' -xdev -not -path /home -type f -print0 | xargs -0r chmod g-wx,o-rwx
find '/home' -xdev -not -path /home -type d -print0 | xargs -0r chmod g-w,o-rwx
chmod 755 /home

#############################
#############################

# The following is a copy of dv3 commands, covering /awips2/edex/data tree;
# these are also run on pv1 because of weird setup between sites ALR/SJU.

# /awips2/edex/data and subdirectories
find /awips2/edex/data -xdev -print0 | xargs -0r chown awips:fxalpha

# files excluding bin
find '/awips2/edex/data' -xdev -regextype posix-egrep \
    -not -path "/awips2/edex/data/manual*" \
    -not -path "/awips2/edex/data/utility*" \
    -not -path "/awips2/edex/data/share*" \
    -not -path "/awips2/edex/data/fxa/trigger*" \
    -not -path "*/bin/*" \
    -not -regex '.*(sh|ksh|csh|py)$' \
    -type f -print0 | xargs -0r chmod 660

# bin
find '/awips2/edex/data' -xdev -regextype posix-egrep \
    -not -path "/awips2/edex/data/manual*" \
    -not -path "/awips2/edex/data/utility*" \
    -not -path "/awips2/edex/data/share*" \
    -not -path "/awips2/edex/data/fxa/trigger*" \
    \( -path "*/bin/*" \
    -o -regex '.*(sh|ksh|csh|py)$' \) \
    -type f -print0 | xargs -0r chmod 770

# directories
find '/awips2/edex/data' -xdev \
    -not -path "/awips2/edex/data/manual*" \
    -not -path "/awips2/edex/data/utility*" \
    -not -path "/awips2/edex/data/share*" \
    -not -path "/awips2/edex/data/fxa/trigger*" \
    -type d -print0 | xargs -0r chmod 770

# /awips2/edex/data/share, excluding bin
find '/awips2/edex/data/share' -xdev -regextype posix-egrep \
    -not -path "/awips2/edex/data/share/hydroapps*" \
    -not -path "*/bin/*" \
    -not -regex '.*(sh|ksh|csh|py)$' \
    -type f -print0 | xargs -0r chmod 660

# /awips2/edex/data/share, bin only
find '/awips2/edex/data/share' -xdev -regextype posix-egrep \
    -not -path "/awips2/edex/data/share/hydroapps*" \
    \( -path "*/bin/*" \
    -o -regex '.*(sh|ksh|csh|py)$' \) \
    -type f -print0 | xargs -0r chmod 760

# /awips2/edex/data/share, directories
find '/awips2/edex/data/share' -xdev -type d -print0 | xargs -0r chmod 770

# hydroapps all files get u+x,g+x
find '/awips2/edex/data/share/hydroapps' -xdev -type f -print0 | xargs -0r chmod 770

find '/awips2/edex/data/manual' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/edex/data/manual' -xdev -type d -print0 | xargs -0r chmod 770
find '/awips2/edex/data/utility' -xdev -type f -print0 | xargs -0r chmod 640
find '/awips2/edex/data/utility' -xdev -type d -print0 | xargs -0r chmod 750
find '/awips2/edex/data/fxa/trigger' -xdev -type f -print0 | xargs -0r chmod 660
find '/awips2/edex/data/fxa/trigger' -xdev -type d -print0 | xargs -0r chmod 770
echo Finished $(date +%Y%m%d_%H%M%S)
}

export -f set_nfs_perms
nohup bash -c set_nfs_perms > /data/fxa/INSTALL/a2logs/set_nfs_perms_pv1_${now}.log 2>&1 & disown
exit
EOF


ssh cpv1 << EOF & disown
bash
set -x

set_nfs_perms () {
echo Starting $(date +%Y%m%d_%H%M%S)
set -x
# qpid message store
if df -t nfs /awips2/qpid/edexMessageStore >/dev/null 2>&1; then
    find /awips2/qpid/edexMessageStore -xdev -print0 | xargs -0r chown awips:fxalpha
    find '/awips2/qpid/edexMessageStore' -xdev -type f -print0 | xargs -0r chmod 600
    find '/awips2/qpid/edexMessageStore' -xdev -type d -print0 | xargs -0r chmod 700
fi
if df -t nfs /awips2/qpid/messageStore >/dev/null 2>&1; then
    find /awips2/qpid/messageStore -xdev -print0 | xargs -0r chown awips:fxalpha
    find '/awips2/qpid/messageStore' -xdev -type f -print0 | xargs -0r chmod 600
    find '/awips2/qpid/messageStore' -xdev -type d -print0 | xargs -0r chmod 700
fi
find '/awips2/qpid' -xdev -type d -print0 | xargs -0r chmod 755
echo Finished $(date +%Y%m%d_%H%M%S)
}

export -f set_nfs_perms
nohup bash -c set_nfs_perms > /data/fxa/INSTALL/a2logs/set_nfs_perms_cpv1_${now}.log 2>&1 & disown
exit
EOF
