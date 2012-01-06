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
#       avndis.py
#       GFS1-NHD:A7828.0000-SCRIPT;1.13
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.13 (DELIVERED)
#         Created:  29-NOV-2007 09:54:10      OBERFIEL
#           Removed obsolete directory search path
#       
#       Revision 1.12 (DELIVERED)
#         Created:  10-JUL-2005 13:18:54      TROJAN
#           correstions to previous spr
#       
#       Revision 1.11 (UNDER WORK)
#         Created:  06-JUL-2005 20:11:31      TROJAN
#           spr 6885
#       
#       Revision 1.10 (DELIVERED)
#         Created:  01-JUN-2005 17:40:51      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 1.9 (DELIVERED)
#         Created:  07-MAY-2005 11:40:52      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.8 (DELIVERED)
#         Created:  18-APR-2005 17:31:28      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.7 (DELIVERED)
#         Created:  11-MAR-2005 15:55:28      TROJAN
#           spr 6717
#       
#       Revision 1.6 (DELIVERED)
#         Created:  23-JAN-2005 18:42:21      TROJAN
#           spr 6604
#       
#       Revision 1.5 (APPROVED)
#         Created:  07-DEC-2004 18:13:18      TROJAN
#           spr 6510
#       
#       Revision 1.4 (APPROVED)
#         Created:  30-SEP-2004 20:22:08      TROJAN
#           stdr 873
#       
#       Revision 1.3 (APPROVED)
#         Created:  19-AUG-2004 20:27:58      OBERFIEL
#           Code change
#       
#       Revision 1.2 (APPROVED)
#         Created:  09-JUL-2004 19:45:05      OBERFIEL
#           Fixed syntax error that stopped DIS from starting
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:46:19      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
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
#	A driver for Data Ingest Server

import getopt, os, socket, sys
TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend([os.path.join(TopDir, dir) for dir in \
    ['sitepy', 'py', 'toolpy']])
import AvnUtils

MaxSize = 1<<28     # Server will exit if virtual memory exceeds 256MB

def main():
    me = os.path.basename(sys.argv[0])
    try:
        opts, pargs = getopt.getopt(sys.argv[1:], 'dn:')
        kwds = dict(opts)
        host = kwds.get('-n', socket.gethostname())
        args = [me] + sys.argv[1:]
    except IndexError:
        print 'Usage: %s [-d] [-n host] modules' % os.path.basename(sys.argv[0])
        raise SystemExit
    pids = AvnUtils.isRunning(args)
    if pids:
        print '%s is running, pids=%s. Terminating' % (args[0], str(pids))
        raise SystemExit
    if '-d' not in kwds:
        AvnUtils.daemonize()
    os.chdir(TopDir)
    try:
        import logging, resource
        import Startup
        import DataIngestServ
        resource.setrlimit(resource.RLIMIT_AS, (MaxSize, MaxSize))
        dis = DataIngestServ.Server(host, pargs)
        dis.run()
    except SystemExit:
        raise
    except MemoryError:
        logging.getLogger(__name__).exception('Exceeded allowable memory')
        raise
    except Exception:
        logging.getLogger(__name__).exception('Uncaught exception')

if  __name__ == '__main__':
    main()
