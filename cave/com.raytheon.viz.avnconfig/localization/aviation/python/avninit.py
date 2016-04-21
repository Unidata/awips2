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
#       avninit.py
#       GFS1-NHD:A8265.0000-SCRIPT;5
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 5 (DELIVERED)
#         Created:  06-JUL-2005 20:11:32      TROJAN
#           spr 6885
#       
#       Revision 4 (DELIVERED)
#         Created:  07-MAY-2005 11:41:10      OBERFIEL
#           Added Item Header Block
#       
#       Revision 3 (DELIVERED)
#         Created:  11-MAR-2005 15:55:29      TROJAN
#           spr 6717
#       
#       Revision 2 (DELIVERED)
#         Created:  15-FEB-2005 13:58:10      TROJAN
#           spr 6604
#       
#       Revision 1 (APPROVED)
#         Created:  23-JAN-2005 18:52:00      TROJAN
#           spr 6604
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6885
#       	Action Date:       09-AUG-2005 14:09:30
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS:  Add port selection feature in avnenv.sh to allow concurrent Pyro instances
#       
#
# avninit.py
# An init-like utility to start avnfps servers
# Author: George Trojan, SAIC/MDL, January 2005
# last update: 07/06/05

import getopt, os, signal, sys, time
import ConfigParser
TopDir = os.environ['TOP_DIR']
Python = os.environ['PYTHON']
import AvnUtils

MaxTries = 10   # 10 times before giving up
_Logger = None
Children = []

def handler(signum, frame):
    AvnUtils.killChildren(Children, _Logger)
    raise SystemExit

def make_child(child, host):
    now = time.time()
    if child['num'] >= MaxTries:
        return
    dt = now - child['time']
    if dt < 10.0:
        time.sleep(10.0-dt)
    if dt > 3600.0:
        child['num'] = 0
    name = child['name']
    _Logger.info('Starting process %s' % name)
    args = ('-d', '-n', host)
    pid = os.fork()
    if pid == 0: 
        os.execl(Python, 'avn'+os.path.basename(Python),
            os.path.join(TopDir, 'py', name+'.py'), *args)
    elif pid < 0:
        _Logger.error('Fork failed for %s' % name)
    else:
        child['pid'] = pid
        child['time'] = now
        child['num'] += 1
        if 'wait' in child:
            time.sleep(int(child['wait']))

def main():
    me = os.path.basename(sys.argv[0])
    try:
        opts, pargs = getopt.getopt(sys.argv[1:], 'd')
        kwds = dict(opts)
        args = [me] + sys.argv[1:]
        host = pargs[0]
    except IndexError:
        print 'Usage: %s [-d] host' % os.path.basename(sys.argv[0])
        raise SystemExit
    pids = AvnUtils.isRunning(args)
    if pids:
        print '%s is running, pids=%s. Terminating' % (args[0], str(pids))
        raise SystemExit
    if '-d' not in kwds:
        AvnUtils.daemonize()
    os.chdir(TopDir)
    try:
        import logging
        import Startup
        global _Logger
        global Children
        _Logger = logging.getLogger(__name__)
        Children = AvnUtils.getInitCfg(host)
        AvnUtils.killOrphans(Children, host, _Logger)
	signal.signal(signal.SIGTERM, handler)
        time.sleep(10)
        for child in Children:
            args = [child['name']+'.py', '-d', '-n', host]
            pids = AvnUtils.isRunning(args)
            if pids:
                _Logger.error('Failed to kill %s, pids=%s' % \
                    (child['name'], str(pids)))
                raise SystemExit
        for child in Children:
            make_child(child, host)
        while True:
            signal.signal(signal.SIGINT, signal.SIG_IGN)
            pid, status = os.wait()
            if pid <= 0:
                continue
            for child in Children:
                if pid == child['pid']:
                    _Logger.info('Process %s, pid=%d, signal=%d has died' % \
                        (child['name'], pid, os.WTERMSIG(status)))
                    # due to a buggy Linux implementation of threads
                    AvnUtils.killOrphans([child], host, _Logger)
                    make_child(child, host)
    except SystemExit:
        raise
    except Exception:
        logging.getLogger(__name__).exception('Uncaught exception')

if  __name__ == '__main__':
    main()
