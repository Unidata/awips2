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
#       LtgThread.py
#       GFS1-NHD:A6837.0000-SCRIPT;11
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 11 (DELIVERED)
#         Created:  06-JUL-2005 18:16:39      TROJAN
#           spr 6548
#       
#       Revision 10 (DELIVERED)
#         Created:  07-MAY-2005 11:35:15      OBERFIEL
#           Added Item Header Block
#       
#       Revision 9 (DELIVERED)
#         Created:  28-APR-2005 19:30:43      TROJAN
#           spr 6816
#       
#       Revision 8 (DELIVERED)
#         Created:  18-APR-2005 17:32:03      OBERFIEL
#           Changes to support gamin
#       
#       Revision 7 (DELIVERED)
#         Created:  11-MAR-2005 15:55:31      TROJAN
#           spr 6717
#       
#       Revision 6 (DELIVERED)
#         Created:  15-FEB-2005 13:47:37      TROJAN
#           spr 6650
#       
#       Revision 5 (APPROVED)
#         Created:  21-OCT-2004 19:27:19      TROJAN
#           spr 6417
#       
#       Revision 4 (APPROVED)
#         Created:  30-SEP-2004 20:22:10      TROJAN
#           stdr 873
#       
#       Revision 3 (APPROVED)
#         Created:  01-JUL-2004 14:59:39      OBERFIEL
#           Update
#       
#       Revision 2 (DELIVERED)
#         Created:  09-JAN-2004 15:27:50      PCMS
#           Updating for code cleanup
#       
#       Revision 1 (REVIEW)
#         Created:  08-JAN-2004 21:31:17      PCMS
#           Initial version
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
# LtgThread.py
# acesses lightning data
# Author: George Trojan, SAIC/MDL, November 2003
# last update: 05/29/05

import logging, math, os, Queue, re, time
import Numeric
import Scientific.IO.NetCDF
import Avn, AvnParser

_Pattern = re.compile(r'[0-9]{8}_[0-9]{4}')

_Logger = logging.getLogger(__name__)

def _old(fname):
    try:
        tms = time.strptime(fname[:13], '%Y%m%d_%H%M')
    except ValueError:
        return True
    return time.mktime(tms) < time.time() - 8000.0

###############################################################################
class Server(object):
    WaitTime = 60.0  # time between reads (seconds)

    def __init__(self, inqueue, outqueue, info):
        self.inqueue = inqueue
        self.outqueue = outqueue
        self.name = info['name']
        self.datapath = info['source']
        self.distance = float(info['distance'])
        self.age = int(info['age'])*60
        self.lastStrikeNum = 0

    def __getSites(self):
        ids = AvnParser.getAllSiteIds()
        dlat = math.degrees(self.distance/3959.0)
        self.sitelist = []
        self.sitedata = {}
        for ident in ids['taf']:
            info = AvnParser.getTafSiteCfg(ident)
            if info is None:
                continue
            lat = float(info['geography']['lat'])
            lon = float(info['geography']['lon'])
            dlon = dlat/math.cos(math.radians(lat))
            d = {'id': ident, 'lat': (lat-dlat, lat+dlat) , \
                'lon': (lon-dlon, lon+dlon)}
            self.sitelist.append(d)
            self.sitedata[ident] = []   # list of lightnings
        if not self.sitelist:
            raise SystemExit

    def __matchSite(self, lat, lon):
        def _match(s):
            slat, slon = s['lat'], s['lon']
            return slat[0] < lat < slat[1] and slon[0] < lon < slon[1]
        return [s['id'] for s in self.sitelist if _match(s)]

    def __readNetCDF(self, path):
        changed = {}    # a set indicating new data for a site
        now = time.time()
        try:
            fh = Scientific.IO.NetCDF.NetCDFFile(path)
            _Logger.info('Processing file %s', path)
            cutoff = now - self.age
            # remove data older that self.age
            for id in self.sitedata:
                d = self.sitedata[id]
                k = 0
                for k in range(len(d)):
                    if d[k] >= cutoff:
                        break
                if k > 0:
                    self.sitedata[id] = d[k:]
                    changed[id] = 0
            timevar = fh.variables['time']
            lastStrikeNum = len(timevar)
            for n in range(self.lastStrikeNum, lastStrikeNum):
                if timevar[n] < cutoff:
                    continue
                lat = fh.variables['lat'][n]
                lon = fh.variables['lon'][n]
                idlist = self.__matchSite(lat, lon)
                for id in idlist:
                    self.sitedata[id].append(timevar[n])
                    changed[id] = 0
            fh.close()
            self.lastStrikeNum = lastStrikeNum
        except IOError:
            # bad or nonexistent file
            _Logger.error('Error accessing %s', path)
        for ident in self.sitedata:
            if ident not in changed:
                continue
            path = os.path.join('data', 'ltg', ident)
            data = '\n'.join([str(t) for t in self.sitedata[ident]])
            file(path, 'w').write(data)
            self.outqueue.put(Avn.Bunch(src=self.name, ident=ident))

    def paths(self):
        return [self.datapath]

    def run(self):
        self.__getSites()
        while True:
            lasttime = time.time()
            flist = []
            # optimization
            while True:
                try:
                    code, fname, direct = self.inqueue.get(True, 6)
                    if code == 0:   # end thread
                        _Logger.info('Got exit request')
                        raise SystemExit
                    if _Pattern.match(fname) and (direct, fname) not in flist:
                        flist.append((direct, fname))
                except Queue.Empty:
                    pass
                if time.time() > lasttime + self.WaitTime:
                    break
            try:
                for direct, fname in flist:
                    if not _old(fname):
                        self.__readNetCDF(os.path.join(direct, fname))
            except Exception:
                _Logger.exception('Unexpected error')
                break
