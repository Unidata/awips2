#!/bin/sh

##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Feb 12, 2008            jelkins     - Initial creation

# remotely execute remote_program on local files
remote_program=$(basename $0)

# USAGE
# Create a link to this script.  The link name should be the name of the remote
# program to execute on the remote_machine.  Modify the workspace_dir and 
# remote_machine variables below as needed.

# The first time a program is remotely executed, this script will automatically
# generate the keys needed to perform password-less connections between the
# local machine and the remote machine and vice versa.  The remote machine
# password will only need to be entered once.

# remote_machine is the network address of the machine on which the 
# remote_program is setup. remote_user is the username to log into the
# remote_machine as.
remote_machine="awips-nis"
remote_user="$USER"

# workspace_dir is the location on the local machine which should be mounted 
# remotely.  The default value of "$HOME" assumes the files required by the 
# remote_program are someplace in the $HOME directory.  If the files reside
# outside of the $HOME directory change $HOME to reflect the correct location.
workspace_dir=$(readlink -f $HOME)

# local_mahchine is the local machine's ip address. local_machine should not 
# need changing unless the network interface is not eth0 or the line below fails
# to determine the correct ip address.
local_machine=$(/sbin/ifconfig eth0 | grep "inet"  | sed 's/.*addr:\([^ ]*\).*/\1/')

# local_id is the pattern that will uniquely identify the local_machine and
# user.  The mount pattern and root should not need to be changed.
local_id="$USER@$local_machine"
mount_root=".remote_execute"

# remote_mount_point is the location on the remote machine where the workspace_dir
# will be mounted.  There should be no need to change this variable.
remote_mount_point="$mount_root/$local_id"

if [ "$remote_machine" = "$local_mahchine" ]
then
    $remote_program $@
else

    remote_id="$remote_user@$remote_machine"
    local_key_file="$HOME/$mount_root/keys/$remote_id"
    remote_key_file="$mount_root/keys/$local_id"
    
    if [ ! -e "$local_key_file" ]
    then
        echo "Setting up execution of $remote_program on $remote_machine as $remote_user"
        
        # --- local key setup ---
        mkdir -p $(dirname $local_key_file)
        ssh-keygen -t rsa -f $local_key_file -N ""
        chmod 600 $local_key_file.pub
        ssh-copy-id -i $local_key_file.pub $remote_id
        
        # --- remote key setup ---
        ssh -i $local_key_file $remote_id << TO_HERE
        mkdir -p \$(dirname $remote_key_file)
        ssh-keygen -t rsa -f \$HOME/$remote_key_file -N ""
        chmod 600 \$HOME/$remote_key_file.pub
TO_HERE
        ssh -t -i $local_key_file $remote_id ssh-copy-id -i \$HOME/$remote_key_file.pub $local_id

    fi

    # --- mount workspace_dir and execute remote_program ---
    ssh -i $local_key_file $remote_id sh << TO_HERE

    mkdir -p ~/$remote_mount_point
    sshfs -o IdentityFile="~/$remote_key_file" $USER@$local_machine:$workspace_dir ~/$remote_mount_point 2> /dev/null
    cd ~/$remote_mount_point$(readlink -f $(pwd) | sed "s|$workspace_dir||")
    $remote_program $@

TO_HERE
fi


