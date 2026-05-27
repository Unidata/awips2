#!/bin/bash

# This script updates nfs mounts in /etc/fstab to set noexec, nodev,
# nosuid options as necessary, then remounts all nfs mounts in
# /etc/fstab.
#
# Author: tgurney

if [[ $(id -u) -ne 0 ]]; then
    echo $0: Need to be root.
    exit 1
fi

fstab_location=/etc/fstab
update_fstab=$(mktemp || exit 1)

cat > $update_fstab << 'EOF'
#!/usr/bin/env python2

import re
import sys
import os.path

FSTAB_PATTERN = r'([^#]\S*)\s+(\S+)\s+(\S+)\s+(\S+)(\s+[0-9]+)?(\s+[0-9]+)?'
MOUNTS = {
    '/awips2/edex/data': ['nodev', 'nosuid'],
    '/archive': ['nodev', 'noexec', 'nosuid'],
    '/awips2/edex/data': ['nodev', 'nosuid'],
    '/awips2/edex/data/fxa/trigger': ['nodev', 'noexec', 'nosuid'],
    '/awips2/edex/data/manual': ['nodev', 'noexec', 'nosuid'],
    '/awips2/edex/data/share': ['nodev', 'nosuid'],
    '/awips2/edex/data/utility': ['nodev', 'noexec', 'nosuid'],
    '/awips2/rcm/data/config': ['nodev', 'noexec', 'nosuid'],
    '/data/fxa/INSTALL/awips2': ['nodev', 'nosuid'],
    '/home': ['nodev', 'nosuid'],
    '/awips2/bmh/conf': ['nodev', 'noexec', 'nosuid'],
    '/awips2/bmh/data': ['nodev', 'noexec', 'nosuid'],
    '/awips2/bmh/neospeech/result': ['nodev', 'noexec', 'nosuid'],
    '/nsbn_store': ['nodev', 'noexec', 'nosuid'],
    '/data_store': ['nodev', 'noexec', 'nosuid'],
    '/awips2/GFESuite': ['nodev', 'nosuid'],
    '/awips2/qpid/edexMessageStore': ['nodev', 'noexec', 'nosuid'],
    '/awips2/qpid/messageStore': ['nodev', 'noexec', 'nosuid'],
    '/tmp/awips2/edex/data': ['nodev', 'noexec', 'nosuid'],
    '/tmp/awips2/GFESuite': ['nodev', 'noexec', 'nosuid'],
    '/tmp/home': ['nodev', 'noexec', 'nosuid']
}

for line in sys.stdin:
    line = line.strip()
    m = re.match(FSTAB_PATTERN, line)
    if not m:
        print line
        continue

    fstab = {'vol': m.group(1),  'mount': m.group(2),
             'fs': m.group(3),   'opts': m.group(4).split(','),
             'dump': m.group(5) or '0', 'pass': m.group(6) or '0'
             }

    fstab['mount'] = os.path.abspath(fstab['mount'])
    if fstab['fs'] == 'nfs' and fstab['mount'] in MOUNTS:
        if 'defaults' in fstab['opts']:
            fstab['opts'].remove('defaults')
        for opt in MOUNTS[fstab['mount']]:
            if opt not in fstab['opts']:
                fstab['opts'].append(opt)

    fields = (fstab['vol'],
            fstab['mount'],
            fstab['fs'],
            ','.join(fstab['opts']),
            fstab['dump'],
            fstab['pass']
            )
    print "%s\t%s\t%s\t%s\t%s %s" % fields
EOF

tmp_fstab=$(mktemp || exit 1)

cleanup_exit() {
    rm -f $tmp_fstab $update_fstab
    exit $1
}

echo INFO: Updating "${fstab_location}"
cat "${fstab_location}" | python2 $update_fstab > $tmp_fstab || cleanup_exit 1
fstab_backup="${fstab_location}.$(date +%Y%m%d.%H%M%S)"
cp "${fstab_location}" $fstab_backup || cleanup_exit 1
echo INFO: Old fstab was saved to $fstab_backup
mv $tmp_fstab "${fstab_location}" || cleanup_exit 1
chmod 644 "${fstab_location}"

for item in $(awk '$3 == "nfs" {print $2}' /etc/mtab); do
    for fstab_item in $(grep -Ev '(^#|^\s*$)' "${fstab_location}" | awk '$3 == "nfs" {print $2}'); do
        if [[ "$item" == "$fstab_item" ]]; then
            if [[ "$item" == /awips2/bmh/neospeech/result* ]]; then
                # This particular mount may fail to "mount -o remount" due to strange mount options.
                # So we have to unmount and then mount
                echo INFO: Unmounting $item
                umount $item
                echo INFO: Mounting $item
                mount $item
            else
                echo INFO: Remounting $item
                mount -o remount $item
            fi
        fi
    done
done

errors=$(mount -fav 2>&1 | grep -v ' : ')

if [[ $? -eq 0 ]]; then
    failed_location=/tmp/fstab.$(date +%Y%m%d.%H%M%S).failed
    cp "${fstab_location}" $failed_location
    echo
    echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    echo
    echo ERROR: fstab failed validation! See below errors.
    echo Original "${fstab_location}" has been restored from backup.
    echo Failed fstab has been saved to $failed_location
    echo
    echo $errors
    echo
    echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    cp -v $fstab_backup "${fstab_location}"
    cleanup_exit 1
fi

echo INFO: Done.
cleanup_exit 0
