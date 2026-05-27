#!/bin/bash
# This script sets the correct rwx permissions for all files on specified NFS
# mounts. See the file /awips2/fxa/bin/set-nfs-perms.sh for details.

# Run this script on dx1 only, as root. This script will ssh from dx1 into
# other boxes as necessary to do work.

bash /awips2/fxa/bin/set-nfs-perms.sh
