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
#       avnserver.py
#       GFS1-NHD:A7964.0000-SCRIPT;1.6
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.6 (DELIVERED)
#         Created:  29-NOV-2007 09:54:12      OBERFIEL
#           Removed obsolete directory search path
#       
#       Revision 1.5 (DELIVERED)
#         Created:  11-JUL-2005 18:14:03      TROJAN
#           spr 6885
#       
#       Revision 1.4 (DELIVERED)
#         Created:  07-MAY-2005 11:41:45      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.3 (DELIVERED)
#         Created:  23-JAN-2005 18:42:21      TROJAN
#           spr 6604
#       
#       Revision 1.2 (APPROVED)
#         Created:  30-SEP-2004 20:22:08      TROJAN
#           stdr 873
#       
#       Revision 1.1 (APPROVED)
#         Created:  19-AUG-2004 21:07:38      OBERFIEL
#           date and time created 08/19/04 21:07:37 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7351
#       	Action Date:       19-MAR-2008 08:14:54
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Remove dependency on shared library, climmodule.so
#       
#
#    Purpose:
#	A driver for starting Pyro name server

import getopt, os, sys, time

TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend([os.path.join(TopDir, dir) for dir in \
    ['sitepy', 'py', 'toolpy']])
import AvnUtils

def main():
    me = os.path.basename(sys.argv[0])
    args = [me] + sys.argv[1:]
    pids = AvnUtils.isRunning(args)
    if pids:
        print '%s is running, pids=%s. Terminating' % (args[0], str(pids))
        raise SystemExit
    try:
        opts, pargs = getopt.getopt(sys.argv[1:], 'dn:')
        kwds = dict(opts)
    except Exception, e:
        print 'python avnserver.py [-d] -n host'
        raise SystemExit
    if '-d' in kwds:
        del kwds['-d']
    else:
        AvnUtils.daemonize()
    os.chdir(TopDir)
    try:
        import Startup
        import AvnServer
        nss = AvnServer.NameServer(**kwds)
        nss.start()
        time.sleep(2)
        if not nss.waitUntilStarted(10):
            raise SystemExit
        ess = AvnServer.EventServer(**kwds)
        ess.start()
        ess.waitUntilStarted()
        time.sleep(1000000000.0)
    except SystemExit:
        raise
    except Exception:
        import logging
        logging.getLogger(__name__).exception('Uncaught exception')

if __name__ == '__main__':
    main()
