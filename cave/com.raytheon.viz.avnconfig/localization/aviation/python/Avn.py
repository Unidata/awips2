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
#       Avn.py
#       GFS1-NHD:A4695.0000-SCRIPT;46
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 46 (DELIVERED)
#         Created:  17-APR-2009 12:03:01      OBERFIEL
#           Added new exception when unknown precipitation (UP)
#           reported in observation.
#       
#       Revision 45 (REVIEW)
#         Created:  20-MAR-2009 18:22:14      OBERFIEL
#           Removed code cruft. ETA changed to NAM. NGMMOS removed.
#       
#       Revision 44 (DELIVERED)
#         Created:  28-JUL-2008 13:56:48      OBERFIEL
#           ICAO new TAF format switch now moved up 18 hours. 
#           DTG format will begin 00Z 05 November 2008
#       
#       Revision 43 (DELIVERED)
#         Created:  22-APR-2008 12:08:20      OBERFIEL
#           Aviation Services Branch has provided a specific date and
#           time for the TAF format change.
#       
#       Revision 42 (DELIVERED)
#         Created:  07-MAR-2008 10:48:55      OBERFIEL
#           Changed assignment of global variables to have fallback
#           values in case there aren't environment variables set.
#       
#       Revision 41 (DELIVERED)
#         Created:  14-MAY-2007 10:04:48      OBERFIEL
#           Removed references to the obsolete prototype XTF product.
#           Allow decoder and encoder to format TAF in two different
#           ways.  New format will be triggered by day and time to be
#           specified at a later date.
#       
#       Revision 40 (REVIEW)
#         Created:  04-MAY-2007 15:38:13      OBERFIEL
#           Added new function DTGImplementationSwitch()
#       
#       Revision 39 (DELIVERED)
#         Created:  24-AUG-2006 15:42:36      OBERFIEL
#           Reworded contact information
#       
#       Revision 38 (DELIVERED)
#         Created:  16-AUG-2006 14:38:07      BLI
#           Removed GT contact info from the Help->about panel
#       
#       Revision 37 (DELIVERED)
#         Created:  06-JUN-2006 13:45:32      OBERFIEL
#           Updated code to allow for wildcard expansion
#       
#       Revision 36 (DELIVERED)
#         Created:  20-MAY-2006 09:33:54      OBERFIEL
#           Added place holders in documentation
#       
#       Revision 35 (DELIVERED)
#         Created:  04-APR-2006 07:57:36      OBERFIEL
#           Corrected title bar
#       
#       Revision 34 (DELIVERED)
#         Created:  23-MAR-2006 15:14:13      TROJAN
#           spr 7109 - modified method to determine work PIL
#       
#       Revision 33 (DELIVERED)
#         Created:  22-MAR-2006 13:05:15      TROJAN
#           spr 7110. Revised TAF handling for consistency
#       
#       Revision 32 (DELIVERED)
#         Created:  16-FEB-2006 14:15:25      TROJAN
#           modified Version date
#       
#       Revision 31 (APPROVED)
#         Created:  29-JAN-2006 14:23:42      TROJAN
#           Added method to Bunch class
#       
#       Revision 30 (APPROVED)
#         Created:  23-JAN-2006 08:23:11      TROJAN
#           stdr 956
#       
#       Revision 29 (DELIVERED)
#         Created:  07-SEP-2005 13:17:13      TROJAN
#           spr 7010
#       
#       Revision 28 (DELIVERED)
#         Created:  16-AUG-2005 13:03:16      TROJAN
#           spr 6989
#       
#       Revision 27 (DELIVERED)
#         Created:  08-AUG-2005 13:13:55      TROJAN
#           spr 6971
#       
#       Revision 26 (DELIVERED)
#         Created:  06-JUL-2005 18:16:34      TROJAN
#           spr 6548
#       
#       Revision 25 (DELIVERED)
#         Created:  07-MAY-2005 11:29:23      OBERFIEL
#           Added Item Header Block
#       
#       Revision 24 (DELIVERED)
#         Created:  04-APR-2005 15:51:03      TROJAN
#           spr 6775
#       
#       Revision 23 (DELIVERED)
#         Created:  14-FEB-2005 20:54:47      TROJAN
#           spr 6649
#       
#       Revision 22 (APPROVED)
#         Created:  24-JAN-2005 21:44:03      TROJAN
#           stdr 855
#       
#       Revision 21 (APPROVED)
#         Created:  16-NOV-2004 19:56:58      PCMS
#           Restoring history
#       
#       Revision 20 (DELIVERED)
#         Created:  08-JAN-2004 21:39:46      PCMS
#           Updating for code cleanup
#       
#       Revision 19 (APPROVED)
#         Created:  05-NOV-2003 19:04:33      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 18 (DELIVERED)
#         Created:  24-APR-2003 14:54:42      TROJAN
#           sprs 5055, 5056, 5057, 5070
#       
#       Revision 17 (DELIVERED)
#         Updated:  10-APR-2003 15:26:53      TROJAN
#           spr 4997
#         Created:  16-MAR-2003 17:48:09      TROJAN
#           spr 4931
#       
#       Revision 16 (BUILD_RELEASE)
#         Created:  10-MAR-2003 13:38:59      TROJAN
#           sprs 4904 - 4908
#       
#       Revision 15 (BUILD_RELEASE)
#         Created:  28-FEB-2003 12:31:30      TROJAN
#           spr 4821 4822
#       
#       Revision 14 (DELIVERED)
#         Created:  14-NOV-2002 14:14:47      PCMS
#           Fixing improper display of LDAD metars
#       
#       Revision 13 (DELIVERED)
#         Created:  05-NOV-2002 17:58:17      PCMS
#           Added definition for 'unique' attribute
#       
#       Revision 12 (DELIVERED)
#         Created:  21-OCT-2002 21:52:52      PCMS
#           Updating of rnew NWSI 10-813 migration
#       
#       Revision 11 (DELIVERED)
#         Created:  10-SEP-2002 20:14:07      PCMS
#           Fixing problem with one line TAF reported as NIL.
#       
#       Revision 10 (DELIVERED)
#         Created:  06-AUG-2002 18:04:12      PCMS
#           Fixed problem when loading from template/merge.
#       
#       Revision 9 (DELIVERED)
#         Created:  17-JUL-2002 13:23:37      PCMS
#           Fixed problem with monitoring old TAF.
#       
#       Revision 8 (DELIVERED)
#         Created:  16-JUL-2002 20:41:20      PCMS
#           Fixed monitoring of old TAF. (Failed 2nd time at NGIT)
#       
#       Revision 7 (DELIVERED)
#         Created:  09-JUL-2002 21:04:29      PCMS
#           Fixing problem monitoring old TAF
#       
#       Revision 6 (DELIVERED)
#         Created:  25-JUN-2002 19:47:12      PCMS
#           Fixed monitoring of old TAF.
#       
#       Revision 5 (DELIVERED)
#         Created:  18-JUN-2002 19:28:32      PCMS
#           Fixed Forecat Editor Display problem for TWBs.
#       
#       Revision 4 (BUILD_RELEASE)
#         Created:  14-JUN-2002 15:11:04      PCMS
#           Fixed time problems which affected which TAF is monitored.
#       
#       Revision 3 (DELIVERED)
#         Created:  11-JUN-2002 18:25:52      PCMS
#           Fixed Forecast Editor Text Display problems
#       
#       Revision 2 (DELIVERED)
#         Created:  29-MAY-2002 22:27:50      PCMS
#           Added capability to mark default product group to monitor
#       
#       Revision 1 (DELIVERED)
#         Created:  13-MAY-2002 21:39:40      PCMS
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7418
#       	Action Date:       06-OCT-2009 09:42:01
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: AvnFPS regression based lightning forecast to use LAMP
#       
#
import exceptions, itertools, math, os, time, logging
#
# Placement of special characters so that updates of AvnFPS version and
# release dates are automated
#
Name = 'AvnFPS %V'
Version = '%D'
Contact = """
For more information about this application consult the AvnFPS User's Guide
under AWIPS "One Stop Resource" Page under "Software" or from the World Wide Web at
http://www.nws.noaa.gov/mdl/pgb/AvnFPS/%V/AvnFPS%V.html"""

# relative to top level directory
WatchBitmap = '@etc/avnwatch.xbm'
SetupBitmap = '@etc/avnsetup.xbm'

# the next two values are set by the decoders
UNLIMITED = 99999   # SKC
CLEAR = 99998       # CLR, must be < UNLIMITED
VARIABLE = 999

# flight categories
LIFR = 'lifr'
IFR = 'ifr'
MVFR = 'mvfr'
VFR = 'vfr'

_TextDB = os.environ.get('FXA_HOME','/awips/fxa') + '/bin/textdb'
TAFWorkPIL = os.environ.get('DEFAULT_CCC','XXX') +'WRKTAF'

PLUGIN_NAME = 'com.raytheon.viz.aviation'
CATEGORY = 'AVNFPS'

_Logger = logging.getLogger(CATEGORY)
try:
    import UFStatusHandler
    _Logger.addHandler(UFStatusHandler.UFStatusHandler(PLUGIN_NAME, CATEGORY, level=logging.INFO))
except:    
    logging.basicConfig(filename='/tmp/avnfps.log',level=logging.INFO)
    _Logger = logging.getLogger()


PATH_MGR = None
try:
    from com.raytheon.uf.common.localization import PathManagerFactory    
    PATH_MGR = PathManagerFactory.getPathManager()    
    ConfigDir = os.path.join('aviation', 'config')               
except:
    #_Logger.exception("Error determining AvnFPS config directory")
    pass

##############################################################################
# Determination of TAF format based on beginning valid time.
##############################################################################
def DTGImplementationSwitch(t=None):
    if not t:
        t = time.time()
    dtg = mkgmtime(time.strptime('05 Nov 2008 00:00:00 GMT',
                                    '%d %b %Y %H:%M:%S %Z'))
    return t >= dtg
##############################################################################
# exceptions
##############################################################################
class AvnError(exceptions.Exception): 
    pass

class AvnMissing(AvnError): 
    pass

class AvnUnknwnPcp(AvnError):
    pass

##############################################################################
# stolen code
##############################################################################
# Python documentation
def any(seq, pred=bool):
    "Returns True if pred(x) is True at least one element in the iterable"
    return True in itertools.imap(pred, seq)

def all(seq, pred=bool):
    "Returns True if pred(x) is True for every element in the iterable"
    return False not in itertools.imap(pred, seq)

def flatten(listOfLists):
    return list(itertools.chain(*listOfLists))

def pairs(seq):
   is1 = itertools.islice(iter(seq), 0, None, 2)
   is2 = itertools.islice(iter(seq), 1, None, 2)
   return list(itertools.izip(is1, is2))

def window(seq, n=2):
    "Returns a sliding window (of width n) over data from the iterable"
    "   s -> (s0,s1,...s[n-1]), (s1,s2,...,sn), ...                   "
    it = iter(seq)
    result = tuple(itertools.islice(it, n))
    if len(result) == n:
        yield result    
    for elem in it:
        result = result[1:] + (elem,)
        yield result

def category(v, iterable):
    "Returns smallest index n such that v is less than iterable[n]."
    return sum(itertools.imap(lambda x: x<=v, iterable))

def accumulate(iterable):
    "'Integrates' iterable."
    s = 0
    for v in iterable:
        s += v
        yield s

def nth(iterable, n, m=1):
    "Returns list of length m starting at position n"
    return list(itertools.islice(iterable, n, n+m))

def wind_category(dd, ff, dd_threshold, ff_threshold):
    if dd == 'VRB' or dd == 0:
        return 0, 0
    else:
        numDDcat = len(dd_threshold)
        dd_cat = (category(dd, dd_threshold)%numDDcat) + 1
    ff_cat = category(ff, ff_threshold)
    if ff_cat == 0:
        dd_cat = 0
    return dd_cat, ff_cat

def dd_avg(count, num_wind_dir=8):
    delta = 360.0/num_wind_dir
    x, y = 0.0, 0.0
    for n in range(num_wind_dir):
        r3 = math.pow(count[n+1], 3)
        alpha = math.radians(delta*n)
        x += r3*math.cos(alpha)
        y += r3*math.sin(alpha)
    return math.degrees(math.atan2(y, x))

# Python Cookbook
# Chapter 1.7
class Bunch(object):
    def __init__(self, **kwds):
        self.__dict__ = kwds

    def values(self):
        return self.__dict__.values()

# Chapter 1.15
def frange(start, end=None, inc=1.0):
    if end == None:
        end = start + 0.0
        start = 0.0
    assert inc 
    i = 0
    while True:
        next = start + i*inc
        if inc > 0 and next >= end:
            break
        elif inc < 0 and next <= end:
            break
        yield next
        i += 1

# Chapter 15.7
def curry(*args, **kwds):
    def callit(*moreargs, **morekwds):
        kw = kwds.copy()
        kw.update(morekwds) 
        return args[0](*(args[1:]+moreargs), **kw)
    return callit

# Chapter 14.4
def printExcPlus():
    '''Print the usual traceback information, followed by a listing of all
the local variables in each frame'''
    import sys, traceback
    tb = sys.exc_info()[2]
    while 1:
        if not tb.tb_next:
            break
        tb = tb.tb_next
    stack = []
    f = tb.tb_frame
    while f:
        stack.append(f)
        f = f.f_back
    stack.reverse()
    traceback.print_exc()
    print 'Locals by frame, innermost last'
    for frame in stack:
        print '\nFrame %s in %s at line %s' % (frame.f_code.co_name, \
            frame.f_code.co_filename, frame.f_lineno)
        for key, value in frame.f_locals.items():
            print '\t%20s = ' % key,
            try:
                print value
            except:
                print '<ERROR WHILE PRINTING VALUE>'

###############################################################################
# utility functions
##############################################################################
def playCommand(audiofile):
    if not audiofile:
        return None
    return '/usr/bin/play %s &' % audiofile

def string2time(s):
    # Converts string %y%m%d%H%M to Unix time
    return mkgmtime((int(s[:2])+2000, int(s[2:4]), int(s[4:6]), \
        int(s[6:8]), int(s[8:10]), 0, 0, 0, 0)) 

def time2string(t=None):
    # Converts string Unix time to string %y%m%d%H%M
    if t:
        return time.strftime('%y%m%d%H%M', time.gmtime(t))
    else:
        return time.strftime('%y%m%d%H%M')

def tagToBBB(tag):
    # used by several GUIs
    return {'Amd': 'AAX', 'Rtd': 'RRX', 'Cor': 'CCX'}.get(tag, '')

def storeInTextDB(filename):
    # stores content of a filename in text database
    # returns output of 'textdb -w' command
    cmd = '%s -w %s' % (_TextDB, os.path.basename(filename).split('.')[0])
    (chldin, chldout) = os.popen4(cmd, -1)
    chldin.write(file(filename).read())
    chldin.close()
    return chldout.read()
def _getIds(taf):
    n = taf.find('\n')+1
    return taf[n:n+4]

def getTafPath(ident, name):
    """This searches for a file name first in the taf/ident local and base
     directory then in the default tafs/XXXX directory.
     Raises an AvnError when unable to find the file.
     ident - site specific directory name
     name - name of file to search for       
     returns fully qualified file name.
    """
    relPath = os.path.join(ConfigDir, 'tafs', ident, name)
    fname = ''
    f = PATH_MGR.getStaticFile(relPath)
    if f:
        fname = f.getPath()
    if len(fname) == 0 or not os.path.isfile(fname):
        if len(fname) == 0 :
            idErrName = relPath
        else:
            idErrName = fname
        relPath = os.path.join(ConfigDir, 'tafs', 'XXXX', name)
        f = PATH_MGR.getStaticFile(relPath)
        if f:
            fname = f.getPath()
        if len(fname) == 0 or not os.path.isfile(fname):
            if len(fname) == 0:
                defErrName = relPath
            else:
                defErrName = fname
            fname = None
            raise AvnError('Unable to find file "%s" or file "%s"' % (idErrName, defErrName))
    return fname

def mkgmtime(t):
    """This is the inverse function of time.gmtime(). It should be the same
    as the calendar.timegm(); but no need to import calendar. This takes a
    struct_time or 9-tuple which expresses the time in UTC not local time.
    It returns a floating point number, for compatibility with time().
    If the input value cannot be represented as a valid time, either OverflowError
    or ValueError will be raised (which depends on whether the invalid value is
    caught by Python or the underlying C libraries). The earliest date for which
    it can generate a time is platform-dependent.
    """
    return time.mktime(t) - time.timezone