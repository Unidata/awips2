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
#
#    Name:
#       avnkill.py
#       GFS1-NHD:A8264.0000-SCRIPT;3
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 3 (DELIVERED)
#         Created:  07-MAY-2005 11:41:19      OBERFIEL
#           Added Item Header Block
#       
#       Revision 2 (DELIVERED)
#         Created:  04-APR-2005 13:48:51      TROJAN
#           spr 6779
#       
#       Revision 1 (DELIVERED)
#         Created:  23-JAN-2005 18:50:15      TROJAN
#           spr 6604
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6832
#       	Action Date:       07-JUN-2005 13:13:53
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Add PVCS doc blocks
#       
#
# avnkill.py
# kills avnpython processes
# George Trojan, SAIC/MDL, September 2004 
# last update: 03/30/05

import os, signal, sys, time
import ConfigParser
TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend([os.path.join(TopDir, dir) for dir in \
    ['sitepy', 'py', 'toolpy']])
import AvnUtils

def getPPid(pid):
    path = '/proc/%d/status' % pid
    for line in file(path):
        if line.startswith('PPid'):
            ppid = line.split()[-1]
            return int(ppid)
    return 0

def kill(sig=None):
    for d in os.listdir('/proc'):
        try:
            pid = int(d)
            path = '/proc/%d/cmdline' % pid
            line = file(path).read().replace('\0', ' ')
            if line.startswith('avnpython'):
                ppid = getPPid(pid)
                if ppid == 1:
                    if sig is not None:
                        print 'Killing', line
                        os.kill(pid, sig)
                    else:
                        print line
        except (IOError, ValueError):
            pass

def killAll():
    print 'Killing all avnpython processes'
    print 'Using SIGTERM'
    kill(signal.SIGTERM)
    time.sleep(30.0)
    print 'Using SIGKILL'
    kill(signal.SIGKILL)
    print 'Remaining processes'
    kill()

def killServers(host):
    print 'Killing server processes on host', host
    args = ['avninit.py', host]
    for pid in AvnUtils.isRunning(args):
        print 'Using SIGTERM', pid
        os.kill(pid, signal.SIGTERM)
    time.sleep(5)
    for pid in AvnUtils.isRunning(args):
        print 'Using SIGKILL', pid
        os.kill(pid, signal.SIGKILL)
    children = AvnUtils.getInitCfg(host)
    AvnUtils.killOrphans(children, host)

def main():
    os.chdir(TopDir)
    if len(sys.argv) == 1:
        killAll()
    else:
        killServers(sys.argv[1])

if __name__ == '__main__':
    main()
