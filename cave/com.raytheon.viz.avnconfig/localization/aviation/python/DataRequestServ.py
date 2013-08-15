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
#       DataRequestServ.py
#       GFS1-NHD:A7792.0000-SCRIPT;1.39
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.39 (DELIVERED)
#         Created:  18-SEP-2008 13:35:18      OBERFIEL
#       
#       Revision 1.38 (DELIVERED)
#         Created:  17-SEP-2008 20:34:48      OBERFIEL
#           Modified start time of searching in transmission log for
#           BBB
#       
#       Revision 1.37 (DELIVERED)
#         Created:  17-SEP-2008 13:02:05      OBERFIEL
#           Updated _getBBB() routine to not use the WMO time header
#           information but the actual issuance times
#       
#       Revision 1.36 (REVIEW)
#         Created:  23-MAY-2008 08:29:18      GILMOREDM
#           Fixed code that was missed during prior checkout
#       
#       Revision 1.35 (REVIEW)
#         Created:  22-MAY-2008 09:56:00      GILMOREDM
#           Changed climQC so that it passes QC check items to
#           TafQC.climate() method.
#       
#       Revision 1.34 (DELIVERED)
#         Created:  20-NOV-2007 07:45:21      OBERFIEL
#           Added ProbReader module for import.
#       
#       Revision 1.33 (DELIVERED)
#         Created:  19-NOV-2007 20:31:27      OBERFIEL
#           Removed carriage return characters in files
#       
#       Revision 1.32 (REVIEW)
#         Created:  19-NOV-2007 12:54:50      GILMOREDM
#           added code to return LAMP Probability Thresholds
#       
#       Revision 1.31 (DELIVERED)
#         Created:  04-MAY-2007 15:52:14      OBERFIEL
#           Changes to reflect OB8.2/AvnFPS3.5 values and clean up in
#           installation staging scripts. cvt3.py
#           updated to obsolete fields and add new one, tafduration.
#           AvnParser and DataRequestServ code cleaned up 
#           to remove references to avnmos, xtfs attributes and removed
#           obsolete modules.
#       
#       Revision 1.30 (DELIVERED)
#         Created:  08-FEB-2007 13:56:02      OBERFIEL
#           Implement fixes to correct 'All' Metar obs viewing and the
#           scrambled ordering of obs and TAFs in the Tweb
#           Viewer.
#       
#       Revision 1.29 (DELIVERED)
#         Created:  05-JAN-2007 13:20:25      OBERFIEL
#           Removed unnecessary sort of metars
#       
#       Revision 1.28 (DELIVERED)
#         Created:  14-APR-2006 13:17:48      TROJAN
#           spr 7118
#       
#       Revision 1.27 (DELIVERED)
#         Created:  14-APR-2006 08:42:21      TROJAN
#           spr 7117
#       
#       Revision 1.26 (DELIVERED)
#         Created:  21-FEB-2006 12:34:21      TROJAN
#           spr 7093
#       
#       Revision 1.25 (APPROVED)
#         Created:  01-FEB-2006 15:11:54      TROJAN
#           get???Files() method did not handle list od site ids
#       
#       Revision 1.24 (APPROVED)
#         Created:  31-JAN-2006 08:35:24      TROJAN
#           Change in naming convention for MOS/LAMP data, added LAMP
#           to plotting module
#       
#       Revision 1.23 (APPROVED)
#         Created:  19-JAN-2006 11:08:20      OBERFIEL
#           Numerous changes to sync up platforms
#       
#       Revision 1.22 (DELIVERED)
#         Created:  19-SEP-2005 13:47:39      TROJAN
#           spr 7011
#       
#       Revision 1.21 (APPROVED)
#         Created:  07-SEP-2005 12:47:28      TROJAN
#           spr 7011
#       
#       Revision 1.20 (DELIVERED)
#         Created:  19-AUG-2005 13:49:03      TROJAN
#           spr 6998
#       
#       Revision 1.19 (APPROVED)
#         Created:  16-AUG-2005 20:51:27      TROJAN
#           fix to spr 6964
#       
#       Revision 1.18 (APPROVED)
#         Created:  09-AUG-2005 14:18:50      TROJAN
#           fixed regular expression: spr 6964
#       
#       Revision 1.17 (DELIVERED)
#         Created:  04-AUG-2005 18:47:41      TROJAN
#           spr 6964
#       
#       Revision 1.16 (DELIVERED)
#         Created:  10-JUL-2005 13:18:54      TROJAN
#           correstions to previous spr
#       
#       Revision 1.15 (UNDER WORK)
#         Created:  07-JUL-2005 12:21:34      TROJAN
#           spr 6548, 6886
#       
#       Revision 1.14 (DELIVERED)
#         Created:  07-MAY-2005 11:32:23      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.13 (DELIVERED)
#         Created:  04-APR-2005 15:51:04      TROJAN
#           spr 6775
#       
#       Revision 1.12 (DELIVERED)
#         Created:  11-MAR-2005 15:55:30      TROJAN
#           spr 6717
#       
#       Revision 1.11 (DELIVERED)
#         Created:  04-MAR-2005 15:31:34      TROJAN
#           spr 6700
#       
#       Revision 1.10 (APPROVED)
#         Created:  28-FEB-2005 21:37:46      TROJAN
#           spr 6686
#       
#       Revision 1.9 (DELIVERED)
#         Created:  23-JAN-2005 18:42:23      TROJAN
#           spr 6604
#       
#       Revision 1.8 (APPROVED)
#         Created:  08-NOV-2004 19:01:19      OBERFIEL
#           Changes to support LLWS
#       
#       Revision 1.7 (APPROVED)
#         Created:  21-OCT-2004 19:36:11      TROJAN
#           spr 6420
#       
#       Revision 1.6 (APPROVED)
#         Created:  30-SEP-2004 20:22:09      TROJAN
#           stdr 873
#       
#       Revision 1.5 (APPROVED)
#         Created:  19-AUG-2004 20:38:56      OBERFIEL
#           Code change
#       
#       Revision 1.4 (APPROVED)
#         Created:  15-JUL-2004 18:17:46      OBERFIEL
#           Fixed statistical output
#       
#       Revision 1.3 (APPROVED)
#         Created:  09-JUL-2004 20:20:17      OBERFIEL
#           Worked around memory leak caused by COTS
#       
#       Revision 1.2 (UNDER WORK)
#         Created:  09-JUL-2004 19:28:34      OBERFIEL
#           Added climate QC
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:39:00      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7415
#       	Action Date:       11-OCT-2008 12:55:40
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: OB9 'AMD' and 'COR' WMO header counter broken (Ref SPR_7414)
#       
#

import atexit, cPickle, logging, os, re, threading, time
import Pyro.core, Pyro.naming
from Pyro.errors import *
import Avn, AvnPyro, AvnThread, MetarDecoder, ProbReader, TafDecoder, TafQC
import MosData, EtaData, LLWSData

_Logger = logging.getLogger(__name__)

##############################################################################
def _invalidPath(path):
    return path.startswith('/') or '../' in path

##############################################################################
# functions to split report into WMO header and the rest
##############################################################################
_MetarPat = re.compile(r'(METAR|SPECI) [A-Z]\w{3} \d+Z ')
def splitMetar(report):
    # Splits report into header and METAR
    if type(report) == type(''):
        report = report.rstrip().split('\n')
    # skip header
    for n, line in enumerate(report):
	if _MetarPat.match(line):
            return '\n'.join(report[:n])+'\n', '\n'.join(report[n:])+'\n'
    else:
        raise Avn.AvnError('Cannot find METAR')

_startTaf = re.compile(r'[A-Z][A-Z0-9]{3}\s+\d{6}Z|TAF(\s+AMD)?\s*$')
def splitTaf(report):
    # Splits report into header and TAF
    if type(report) == type(''):
        report = report.rstrip().split('\n')
    # skip header
    for n, line in enumerate(report):
        if _startTaf.match(line):
            return '\n'.join(report[:n])+'\n', '\n'.join(report[n:])+'\n'
    else:
        raise Avn.AvnError('Cannot find start of forecast')

_startTwb = re.compile(r'[A-Z]{3} SYNS\s+|\d{3}\s+')
def splitTwb(report):
    # Splits report into header and TWEB
    if type(report) == type(''):
        report = report.rstrip().split('\n')
    # skip header
    for n, line in enumerate(report):
        if _startTwb.match(line):
            return '\n'.join(report[:n])+'\n', '\n'.join(report[n:])+'\n'
    else:
        raise Avn.AvnError('Cannot find start of forecast')

##############################################################################
# used to write forecast file for transmission
def _getXmitActivity(ident3, bbb, stime, dow):
    """Get all of the activity on ident3 since time 't'"""
    def parseFile(filename):
        num, awips, tttt, cccc, tstamp, bbb, t = filename.split('-')
        return (awips[7:], bbb, t)
    #
    activity = []
    fname = os.path.join('xmit', dow)
    try:
        xmitlist = map(parseFile, [x.split()[-1] for x in file(fname) \
                                   if x.startswith('SUCCESS')])
        activity = [x[1][2] for x in xmitlist if x[0] == ident3 and \
                    (x[1][0] in [bbb[0],'_']) and int(x[2]) >= stime]
    except (ValueError, IOError):
        pass
    
    return activity

def _getBBB(ident3, bbb):
    """Returns bbb based on prior activity for ident3"""
    #
    allActivity = []
    #
    # Determine current log in use
    now=time.time()
    secsInDay = now % 86400
    tms = list(time.gmtime(now))
    logs = [time.strftime('%a', tms)]
    #
    # Determine the time to begin the search
    if secsInDay < 19200:
        #
        # If we're in a situation where log rollover occurred
        tms[3:6] = 23, 20, 0
        tms[2] -= 1
        logs.insert(0,time.strftime('%a', time.gmtime(time.mktime(tms))))
    elif secsInDay < 40800:
        tms[3:6] = 5, 20, 0
    elif secsInDay < 62400:
        tms[3:6] = 11, 20, 0
    elif secsInDay < 84000:
        tms[3:6] = 17, 20, 0

    beginSearch = time.mktime(tms)
    for log in logs:
        allActivity.extend(_getXmitActivity(ident3, bbb, beginSearch, log))
    #
    # Get the latest bbb after the last initial issuance and increment by one
    try:
        allActivity.reverse()
        try:
            ver = chr(ord(allActivity[:allActivity.index('_')][0])+1)
        except ValueError:
            ver = chr(ord(allActivity[0])+1)
    except IndexError:
        ver = 'A'
        
    return bbb[:2]+ver

def _makeTimeStamp(tstamp):
    # Returns time stamp used in output file name
    now = time.time()
    tmptstamp = '%s%s' % (Avn.time2string(now)[:4], tstamp)
    # possible correction for next month/year
    t = Avn.string2time(tmptstamp)
    if t > now + 86400:
        mm = int(tmptstamp[2:4]) - 1
        yy = int(tmptstamp[:2])
        if mm < 1:
            mm = 12
            yy -= 1
        return '%02d%02d%s' % (yy, mm, tmptstamp[4:])
    else:
        return tmptstamp

##############################################################################
def _getFiles(path, length, ftime=0, num=99):
    """Returns sorted list of file names of length "length", newer than
ftime, up to "num" long"""
    cutoff = time.time() + 3600.0
    filelist = [f for f in os.listdir(path) if len(f) == length \
        and cutoff > Avn.string2time(f) > ftime]
    if not filelist:
        raise OSError, 'Empty list'
    filelist.sort()
    filelist.reverse()
    return filelist[:num]

##############################################################################
class TEXTServer(Pyro.core.ObjBase):
    def getMetars(self, ids, ftime=0, num=99):
        # Retrieves up to 'num' METARs newer than ftime, 
        # most recent first
        if type(ids) == type(''):
            ids = [ids]
        flist = []
        for i in ids:
            path = os.path.join('data', 'mtrs', i)
            try:
                flist.extend([(f, path) for f in \
                    _getFiles(path, 10, ftime, num)])
            except OSError:
                pass
        if not flist:
            return []

        metars = []
        for f, path in flist[:num]:
            try:
                data = file(os.path.join(path, f)).read()
                header, text = splitMetar(data)
                metars.append(Avn.Bunch(file=f, header=header, text=text))
            except (OSError, Avn.AvnError):
                pass
        return metars

    def getTafs(self, ids, ftime=0, num=99):
        # Retrieves 'num' recent TAFs newer than ftime, 
        # most recent first
        if type(ids) == type(''):
            ids = [ids]
        flist = []
        for i in ids:
            path = os.path.join('data', 'tafs', i)
            try:
                flist.extend([(f, path) for f in \
                    _getFiles(path, 11, ftime, num)])
            except OSError:
                pass
        if not flist:
            return []
        if len(ids) > 1:
            flist.sort()        # need to resort for multiple sites
            flist.reverse()
        tafs = []
        for f, path in flist[:num]:
            try:
                data = file(os.path.join(path, f)).read()
                header, text = splitTaf(data)
                tafs.append(Avn.Bunch(file=f, header=header, text=text))
            except (OSError, Avn.AvnError):
                pass
        return tafs

    def getTwbs(self, ids, ftime=0, num=99):
        # Retrieves 'num' recent TWBs newer than ftime, 
        # most recent first
        if type(ids) == type(''):
            ids = [ids]
        flist = []
        for i in ids:
            path = os.path.join('data', 'twbs', i)
            try:
                flist.extend([(f, path) for f in \
                    _getFiles(path, 11, ftime, num)])
            except OSError:
                pass
        if not flist:
            return []
        if len(ids) > 1:
            flist.sort()        # need to resort for multiple sites
            flist.reverse()
        twbs = []
        for f, path in flist[:num]:
            try:
                data = file(os.path.join(path, f)).read()
                header, text = splitTwb(data)
                twbs.append(Avn.Bunch(file=f, header=header, text=text))
            except (OSError, Avn.AvnError):
                _Logger.exception(data)
        return twbs

    def getCCFP(self, id_, ftime=0):
        # Retrieves recent CCFP newer than ftime
        path = os.path.join('data', 'ccfp')
        ccfps = []
        try:
            # get up to 3 most recent forecasts
            # if valid times equal, select one with newer issue time
            flist = _getFiles(path, 13, ftime, 3)
            vtimes = [Avn.string2time(x)+3600.0*int(x[-2:]) for x in flist]
            tmp = [(vtimes[0], flist[0])]
            for n, t in enumerate(vtimes[1:]):
                if t not in vtimes[:n+1]:
                    tmp.append((vtimes[n+1], flist[n+1]))
            tmp.sort()
            for t, f in tmp:
                try:
                    data = file(os.path.join(path, f)).read()
                    lines = data.split('\n')
                    for line in lines[1:]:
                        if line.startswith(id_):
                            ccfps.append(Avn.Bunch(file=f, header=lines[0], 
                                text=line))
                            break
                    else:
                        ccfps.append(Avn.Bunch(file=f, header=lines[0], 
                            text=''))
                except OSError:
                    pass
            return ccfps
        except (OSError, IndexError):
            return ccfps

##############################################################################
class MOSServer(Pyro.core.ObjBase):
    def getMosFiles(self, ids, model):
        # Returns list of MOS data files, most recent first
        if type(ids) == type(''):
            ids = [ids]
        flist = []
        for i in ids:
            path = os.path.join('data', model.lower(), i)
            try:
                flist.extend(_getFiles(path, 15))
            except (IndexError, OSError):
                pass
        if flist and len(ids) > 1:
            flist.sort()        # need to resort for multiple sites
            flist.reverse()
        return flist

    def getMosTable(self, ids, model, issuetime=None):
        # Retrieves most recent tabulated MOS data for model 
        if type(ids) == type(''):
            idslist = [ids]
        else:
            idslist = ids
        tables = []
        for i in idslist:
            path = os.path.join('data', model.lower(), i)
            try:
                if issuetime:
                    f = Avn.time2string(issuetime) + '.table'
                else:
                    f = _getFiles(path, 16, time.time()-86400.0, 1)[0]
                tables.append(Avn.Bunch(file=f, 
                    table=file(os.path.join(path, f)).read()))
            except (IndexError, OSError):
                pass
        if type(ids) == type(''):
            try:
                return tables[0]
            except IndexError:
                return None
        else:
            return tables

    def getMosData(self, ids, model, issuetime=None):
        # Returns most recent MOS data for model 
        if type(ids) == type(''):
            idslist = [ids]
        else:
            idslist = ids
        data = []
        for i in idslist:
            path = os.path.join('data', model.lower(), i)
            try:
                if issuetime:
                    f = Avn.time2string(issuetime) + '.data'
                else:   
                    f = _getFiles(path, 15, time.time()-86400.0, 1)[0]
                data.append(Avn.Bunch(file=f, 
                    data=cPickle.load(file(os.path.join(path, f)))))
            except (IndexError, OSError):
                pass
        if type(ids) == type(''):
            try:
                return data[0]
            except IndexError:
                return None
        else:
            return data

##############################################################################
class MODELServer(Pyro.core.ObjBase):
    def getEtaFiles(self, ids):
        # Returns list of ETA model data files
        if type(ids) == type(''):
            ids = [ids]
        flist = []
        for i in ids:
            path = os.path.join('data', 'eta', i)
            try:
                flist.extend(_getFiles(path, 16))
            except (IndexError, OSError):
                pass
        if flist and len(ids) > 1:
            flist.sort()        # need to resort for multiple sites
            flist.reverse()
        return flist

    def getEtaTable(self, ids, issuetime=None):
        # Returns most recent tabulated ETA model data
        if type(ids) == type(''):
            idslist = [ids]
        else:
            idslist = ids
        tables = []
        for i in idslist:
            path = os.path.join('data', 'eta', i)
            try:
                if issuetime:
                    f = Avn.time2string(issuetime) + '.table'
                else:
                    f = _getFiles(path, 16, time.time()-86400.0, 1)[0]
                tables.append(Avn.Bunch(file=f, 
                    table=file(os.path.join(path, f)).read()))
            except (IndexError, OSError):
                pass
        if type(ids) == type(''):
            try:
                return tables[0]
            except IndexError:
                return None
        else:
            return tables

    def getEtaData(self, ids, issuetime=None):
        # Returns most recent Eta model data
        if type(ids) == type(''):
            idslist = [ids]
        else:
            idslist = ids
        data = []
        for i in idslist:
            path = os.path.join('data', 'eta', i)
            try:
                if issuetime:
                    f = Avn.time2string(issuetime) + '.data'
                else:
                    f = _getFiles(path, 15, time.time()-86400.0, 1)[0]
                data.append(Avn.Bunch(file=f, 
                    data=cPickle.load(file(os.path.join(path, f)))))
            except (IndexError, OSError):
                pass
        if type(ids) == type(''):
            try:
                return data[0]
            except IndexError:
                return None
        else:
            return data

##############################################################################
class XMITServer(Pyro.core.ObjBase):
    def putForecast(self, fcst, awipsid, wmoid, tstamp, bbb, xmittime, fcstid):
        # Writes forecast to a file in 'xmit/pending'
        if not bbb:
            bbb = '___'
        elif bbb[2] in 'xX':
            bbb = _getBBB(awipsid[7:], bbb)
            
        tt, cc = wmoid.split()
        # make yymmddHHMM timestamp
        path = os.path.join('xmit', 'pending', '%03d-%s-%s-%s-%s-%s-%-10d' % \
            (fcstid, awipsid, tt, cc, _makeTimeStamp(tstamp), bbb, xmittime))
        file(path, 'w').write(fcst)
        return path

    def getForecast(self, pattern):
        # returns forecast from the queue
        path = os.path.join('xmit', 'pending')
        awips = pattern.split('-')[1]
        reg = re.compile(pattern)
        try:
            files = [f for f in os.listdir(path) if reg.match(f)]
            if not files:
                return None
            fname = os.path.join(path, files[-1])
            data =  file(fname).read()
            # remove forecast file from the queue
            os.unlink(fname)
            return data
        except (OSError, IOError, IndexError):
            msg = 'Cannot access queued forecast for %s' % awips
            _Logger.exception(msg)
            return None

    def getFile(self, path):
        try:
            if _invalidPath(path):
                raise ValueError('Invalid path')
            return file(path).read()
        except Exception:
            _Logger.exception('Cannot access %s', path)
            return None

    def putFile(self, path, data):
        try:
            if _invalidPath(path):
                raise ValueError('Invalid path')
            file(path, 'w').write(data)
        except Exception:
            _Logger.exception('Cannot write %s', path)

    def getDir(self, path):
        try:
            if _invalidPath(path):
                raise ValueError('Invalid path')
            return os.listdir(path)
        except Exception:
            _Logger.exception('Cannot access %s', path)
            return []

    def rmFile(self, path):
        try:
            if _invalidPath(path):
                raise ValueError('Invalid path')
            os.unlink(path)
        except Exception:
            _Logger.exception('Cannot remove %s', path)

    def mvFile(self, oldfile, newfile):
        try:
            if _invalidPath(oldfile) or _invalidPath(newfile):
                raise ValueError('Invalid path')
            os.rename(oldfile, newfile)
            os.utime(newfile, None) # update timestamp for avnxs
        except Exception:
            _Logger.exception('Cannot rename %s', oldfile)

##############################################################################
class LTGServer(Pyro.core.ObjBase):
    def getLightning(self, id_, ftime=0):
        # Returns number of lightnings newer than ftime
        try:
            times = [float(t) for t in file(os.path.join('data', 'ltg', id_))]
            times.sort()
            times.reverse()
            for n, t in enumerate(times):
                if t < ftime:
                    return n
            return len(times)
        except IOError:
            return 0

##############################################################################
class RLTGServer(Pyro.core.ObjBase):
    def getFcstLightning(self, id_):
        # Returns lightning probability forecast
        try:
            for line in file(os.path.join('data', 'rltg', 'data')):
                tok = line.split()
                key = tok[0]
                if key == 'Time':   # first line
                    vtime = float(tok[1])
                elif key == id_:
                    return {'from': vtime-10800.0, 'to': vtime, \
                        'prob': min(int(tok[1]), 100)}
            return None
        except (IOError, IndexError):
            return None

##############################################################################
class GRIDServer(Pyro.core.ObjBase):
    def getGrids(self, ids):
        if type(ids) == type(''):
            idslist = [ids]
        else:
            idslist = ids
        grids = []
        for i in idslist:
            try:
                grids.append(
                    (i, file(os.path.join('data', 'grids', i)).read()))
            except Exception:
                pass
        if type(ids) == type(''):
            try:
                return grids[0][1]
            except IndexError:
                return None
        else:
            return grids

##############################################################################
class LLWSServer(Pyro.core.ObjBase):
    def getLLWS(self, id_):
        return LLWSData.readLLWS(id_)

##############################################################################
class CLIMServer(Pyro.core.ObjBase):
    def climQC(self, qcdata, items):
        return TafQC.climate(qcdata, items)

###############################################################################
class ProbServer(Pyro.core.ObjBase):
    def getProbs(self, id_, fDate):
	return ProbReader.prob_reader(id_, fDate)

###############################################################################

ModuleDict = { \
        'clim': CLIMServer, \
        'grid': GRIDServer, \
        'ltg': LTGServer, \
        'llws': LLWSServer, \
        'model': MODELServer, \
        'mos': MOSServer, \
        'rltg': RLTGServer, \
        'text': TEXTServer, \
        'xmit': XMITServer, \
	'prob': ProbServer, \
}

class Server(object):
    CleanupAge = 60
    def __init__(self, host, modulenames):
        if modulenames:
            self._names = [x for x in modulenames if x in ModuleDict]
        else:
            self._names = ModuleDict.keys()
        self._host = host
        atexit.register(self.__exitfun)

    def __exitfun(self):
        if hasattr(self, '_daemon'):
            self._daemon.shutdown()

    def __notifier(self):
        try:
            self._publisher = AvnThread.Publisher()
        except Pyro.errors.NamingError:
            _Logger.error('Cannot connect to Event Server')
            raise SystemExit
        while 1:
            try:
                msg = Avn.Bunch(src='DATA-'+self._host, value='ALIVE')
                self._publisher.publish(msg)
            except Exception, e:
                _Logger.exception(str(e))
            time.sleep(30.0)

    def run(self):
        try:
            Pyro.core.initServer()
            try:
                port = int(os.environ['AVN_NS_PORT'])
            except KeyError:
                port = None
            ns = Pyro.naming.NameServerLocator().getNS(port=port)
            try:
                ns.createGroup(Pyro.config.PYRO_NS_DEFAULTGROUP)
            except NamingError:
                # hoping that another instance of the server created this group
                pass
            self._daemon = Pyro.core.Daemon(host=self._host)
            self._daemon.setNewConnectionValidator(AvnPyro.newConnValidator())
            self._daemon.setTransientsCleanupAge(self.CleanupAge)
            self._daemon.useNameServer(ns)
            for name in self._names:
                self._daemon.connectPersistent(ModuleDict[name](), name)
            t = threading.Thread(target=self.__notifier)
            t.setDaemon(1)
            t.start()
            _Logger.info('Servers ready: %s', ', '.join(self._names))
            ns._release()
            self._daemon.requestLoop()
        except NamingError:
            _Logger.error('Cannot connect to Name Server')
            return
        except SystemExit:
            return
        except Exception, e:
            _Logger.exception(str(e))
            return

###############################################################################
class Client(object):
    def __init__(self, ns=None):
        self._lock = threading.RLock()
        self._servernames = []
        self._proxy = {}
        if ns is None:
            try:
                port = int(os.environ['AVN_NS_PORT'])
            except KeyError:
                port = None
            ns = Pyro.naming.NameServerLocator().getNS(port=port)
            self._servernames = [x for (x, y) in \
                ns.list(Pyro.config.PYRO_NS_DEFAULTGROUP) \
                if x in ModuleDict and y == 1]
        for name in self._servernames:
            try:
                URI = ns.resolve( \
                    '.'.join([Pyro.config.PYRO_NS_DEFAULTGROUP, name]))
                self._proxy[name] = Pyro.core.getProxyForURI(URI)
            except PyroError, e:
                _Logger.error('Cannot get proxy: %s', str(e))
        ns._release()

    def __generic(self, name, method, *args):
        # obj._release() reduces number of connections/threads, overhead
        # of reconnecting each time is unknown
        self._lock.acquire()    # cannot share proxy between threads
        data = None
        try:
            obj = self._proxy.get(name)
            data = getattr(obj, method)(*args)
#           obj._release()
        except (ProtocolError, ConnectionClosedError), e:
            _Logger.exception('%s. Rebinding...', e)
            try:
                # 5 times, 2 second wait time
                obj.adapter.rebindURI(3, 1)
                data = getattr(obj, method)(*args)
#               obj._release()
            except Exception, e:
                _Logger.exception('%s failed: %s', method, str(e))
        except AttributeError:
            _Logger.error('%s object is not on the server', name)
        except Exception, e:
            _Logger.exception('%s failed: %s', method, str(e))
        self._lock.release()
        return data

    def release(self):
        for key in self._proxy:
            self._proxy[key]._release()

    def getMetars(self, ids, decoded, ftime=0, num=99):
        data = self.__generic('text', 'getMetars', ids, ftime, num)
        if data is None:
            data = []
        if decoded:
            decoder = MetarDecoder.Decoder()
            for rpt in data:
                rpt.dcd = decoder(rpt.text)
        return data

    def getTafs(self, ids, decoded, ftime=0, num=99):
        data = self.__generic('text', 'getTafs', ids, ftime, num)
        if data is None:
            data = []
        if decoded:
            decoder = TafDecoder.Decoder()
            for rpt in data:
                try:
                    wmo = rpt.header.split('\n', 1)[0]
                    bbb = wmo.split()[3]
                except IndexError:
                    bbb = '   '
                rpt.dcd = decoder(rpt.text, bbb)
        return data

    def getTwbs(self, ids, ftime=0, num=99):
        data = self.__generic('text', 'getTwbs', ids, ftime, num)
        if data is None:
            data = []
        return data

    def getCCFP(self, id_, ftime=0):
        return self.__generic('text', 'getCCFP', id_, ftime)

    def getMosFiles(self, id_, model):
        return self.__generic('mos', 'getMosFiles', id_, model)

    def getMosTable(self, ids, model, itime=None):
        return self.__generic('mos', 'getMosTable', ids, model, itime)

    def getMosData(self, ids, model, itime=None):
        return self.__generic('mos', 'getMosData', ids, model, itime)

    def getEtaFiles(self, id_):
        return self.__generic('model', 'getEtaFiles', id_)

    def getEtaTable(self, ids, itime=None):
        return self.__generic('model', 'getEtaTable', ids, itime)

    def getEtaData(self, ids, itime=None):
        return self.__generic('model', 'getEtaData', ids, itime)

    def putForecast(self, fcst, awipsid, wmoid, tstamp, bbb, xmittime, fcstid):
        return self.__generic('xmit', 'putForecast', fcst, awipsid, wmoid, 
            tstamp, bbb, xmittime, fcstid)

    def getForecast(self, pattern):
        return self.__generic('xmit', 'getForecast', pattern) 

    def getFile(self, path):
        return self.__generic('xmit', 'getFile', path)

    def putFile(self, path, data):
        return self.__generic('xmit', 'getFile', path, data)

    def getDir(self, path):
        return self.__generic('xmit', 'getDir', path)

    def rmFile(self, path):
        return self.__generic('xmit', 'rmFile', path)

    def mvFile(self, oldpath, newpath):
        return self.__generic('xmit', 'mvFile', oldpath, newpath)

    def getLightning(self, id_, ftime):
        return self.__generic('ltg', 'getLightning', id_, ftime)

    def getFcstLightning(self, id_):
        return self.__generic('rltg', 'getFcstLightning', id_)

    def getGrids(self, ids):
        return self.__generic('grid', 'getGrids', ids)

    def getLLWS(self, id_):
        return self.__generic('llws', 'getLLWS', id_)

    def climQC(self, qcdata, items):
        return self.__generic('clim', 'climQC', qcdata, items)

    def getProbs(self, id_, fDate):
	return self.__generic('prob', 'getProbs', id_, fDate)
