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
#       AvnUtils.py
#       GFS1-NHD:A7788.0000-SCRIPT;1.10
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.10 (DELIVERED)
#         Created:  06-JUL-2005 18:16:35      TROJAN
#           spr 6548
#       
#       Revision 1.9 (DELIVERED)
#         Created:  01-JUN-2005 17:41:03      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 1.8 (DELIVERED)
#         Created:  07-MAY-2005 11:30:44      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.7 (DELIVERED)
#         Created:  18-APR-2005 17:31:39      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.6 (DELIVERED)
#         Created:  11-MAR-2005 15:55:29      TROJAN
#           spr 6717
#       
#       Revision 1.5 (DELIVERED)
#         Created:  15-FEB-2005 18:18:49      TROJAN
#           spr 6604
#       
#       Revision 1.4 (APPROVED)
#         Created:  23-JAN-2005 18:42:22      TROJAN
#           spr 6604
#       
#       Revision 1.3 (APPROVED)
#         Created:  30-SEP-2004 20:22:09      TROJAN
#           stdr 873
#       
#       Revision 1.2 (APPROVED)
#         Created:  19-AUG-2004 20:27:58      OBERFIEL
#           Code change
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:38:07      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6548
#       	Action Date:       09-AUG-2005 14:09:33
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS:  Data acquistion change in OB6
#       
#
# AvnUtils.py
# Utilities for AvnFPS 
# Author: George Trojan, SAIC/MDL, May 2004
# last update: 07/05/05

import os, signal, sys
import ConfigParser

#############################################################################
# Python Cookbook receipe
'''This module is used to fork the current process into a daemon.
Almost none of this is necessary (or advisable) if your daemon 
is being started by inetd. In that case, stdin, stdout and stderr are 
all set up for you to refer to the network connection, and the fork()s 
and session manipulation should not be done (to avoid confusing inetd). 
Only the chdir() and umask() steps remain as useful.
References:
UNIX Programming FAQ
1.7 How do I get my program to act like a daemon?
http://www.erlenstar.demon.co.uk/unix/faq_2.html#SEC16
    
Advanced Programming in the Unix Environment
W. Richard Stevens, 1992, Addison-Wesley, ISBN 0-201-56317-7.
'''

def daemonize(stdin='/dev/null', stdout='/dev/null', stderr='/dev/null'):
    '''This forks the current process into a daemon.
    The stdin, stdout, and stderr arguments are file names that
    will be opened and be used to replace the standard file descriptors
    in sys.stdin, sys.stdout, and sys.stderr.
    These arguments are optional and default to /dev/null.
    Note that stderr is opened unbuffered, so
    if it shares a file with stdout then interleaved output
    may not appear in the order that you expect.
    '''
    # Do first fork.
    try: 
        pid = os.fork()
        if pid > 0:
            sys.exit(0) # Exit first parent.
    except OSError, e: 
        sys.stderr.write('fork #1 failed: (%d) %s\n' % (e.errno, e.strerror))
        sys.exit(1)
        
    # Decouple from parent environment.
#   os.chdir("/") 
#   os.umask(0) 
    os.setsid()
    
    # Do second fork.
    try: 
        pid = os.fork() 
        if pid > 0:
            sys.exit(0) # Exit second parent.
    except OSError, e: 
        sys.stderr.write('fork #2 failed: (%d) %s\n' % (e.errno, e.strerror))
        sys.exit(1)
        
    # Now I am a daemon!
    
    # Redirect standard file descriptors.
    si = file(stdin, 'r')
    so = file(stdout, 'a+')
    se = file(stderr, 'a+', 0)
    os.dup2(si.fileno(), sys.stdin.fileno())
    os.dup2(so.fileno(), sys.stdout.fileno())
    os.dup2(se.fileno(), sys.stderr.fileno())

##############################################################################
def isRunning(args):
    '''Checks whether there is another instance of a program running.
    Returns list of pids of such instances.
    '''
    mypid = os.getpid()
    if type(args) == type(''):
        targs = [args]
    else:
        targs = args
    pids = []
    for d in os.listdir('/proc'):
        try:
            pid = int(d)
            if pid == mypid:
                continue
            pargs = filter(None, 
                file('/proc/%s/cmdline' % d).read().split('\0')[1:])
            pargs[0] = os.path.basename(pargs[0])
            if targs == pargs:
                pids.append(pid)
        except Exception:
            pass
    return pids

##############################################################################
# Functions used by avninit and avnkill
def getInitCfg(host):
    '''Returns list of child processes to be startet by avninit'''
    path = os.path.join('etc', host+'init.cfg')
    cp = ConfigParser.SafeConfigParser()
    cp.read(path)
    children = [dict(cp.items(section)) for section in cp.sections()]
    # sort with respect to 'order'
    tmp = [(int(ch['order']), ch) for ch in children]
    tmp.sort()
    children = [t[1] for t in tmp]
    for child in children:
        child.update({'pid': 0, 'time': 0, 'num': 0})
    return children

def killChildren(children, logger=None):
    # reverse order to avoid error messages
    tmp = children[:]
    tmp.reverse()
    for child in tmp:
        if logger:
            logger.info('Killing process %s, pid=%s' % \
                (child['name'], child['pid']))
        try:
            os.kill(child['pid'], signal.SIGTERM)
        except OSError:
            pass

def killOrphans(children, host, logger=None):
    # reverse order to avoid error messages
    tmp = children[:]
    tmp.reverse()
    for child in tmp:
        args = [child['name']+'.py', '-d', '-n', host]
        if 'tag' in child:
            args.append(child['tag'])
        pids = isRunning(args)
        if logger and pids:
            logger.info('Killing process %s, pids=%s' % \
                (child['name'], str(pids)))
        for pid in pids:
            os.kill(pid, signal.SIGKILL)
