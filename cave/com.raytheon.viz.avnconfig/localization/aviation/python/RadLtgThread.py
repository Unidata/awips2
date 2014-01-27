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
#       RadLtgThread.py
#       GFS1-NHD:A7818.0000-SCRIPT;1.8
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.8 (DELIVERED)
#         Created:  06-JUL-2005 18:16:41      TROJAN
#           spr 6548
#       
#       Revision 1.7 (DELIVERED)
#         Created:  07-MAY-2005 11:37:26      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.6 (DELIVERED)
#         Created:  28-APR-2005 19:30:44      TROJAN
#           spr 6816
#       
#       Revision 1.5 (DELIVERED)
#         Created:  18-APR-2005 17:32:21      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.4 (DELIVERED)
#         Created:  11-MAR-2005 15:55:31      TROJAN
#           spr 6717
#       
#       Revision 1.3 (DELIVERED)
#         Created:  15-FEB-2005 13:47:37      TROJAN
#           spr 6650
#       
#       Revision 1.2 (APPROVED)
#         Created:  30-SEP-2004 20:22:11      TROJAN
#           stdr 873
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:44:14      OBERFIEL
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
# RadLtgThread.py
# server for NCEP radar based lightning probability forecast
# Author: George Trojan, SAIC/MDL, October 2003
# last update: 05/27/05

import logging, math, os, Queue, re, time
import Numeric
import Scientific.IO.NetCDF
import Avn, AvnParser

_Pattern = re.compile(r'[0-9]{8}_[0-9]{4}.multi')

_Logger = logging.getLogger(__name__)

###############################################################################
class Server(object):
    WaitTime = 60 # time between reads (seconds)
    _Dest = os.path.join('data', 'rltg', 'data')

    def __init__(self, inqueue, outqueue, info):
        self.inqueue = inqueue
        self.outqueue = outqueue
        self.name = info['name']
        self.datapath = info['source']
        self.map = {}

    def __getSites(self):
        ids = AvnParser.getAllSiteIds()
        self.sitelist = []
        for ident in ids['taf']:
            info = AvnParser.getTafSiteCfg(ident)
            if info is None:
                continue
            d = {'id': ident, \
                'lat': float(info['geography']['lat']), \
                'lon': float(info['geography']['lon'])}
            self.sitelist.append(d)
        if not self.sitelist:
            _Logger.debug('Cannot get site list')
            raise SystemExit

    def __getMapParms(self, fh):
        if fh.projName != 'LAMBERT_CONFORMAL':
            raise ValueError, 'Unsupported projection %s' % fh.projName
        latC = math.radians(fh.centralLat[0])
        lat1 = math.radians(fh.rotation[0])
        if latC != lat1:
            raise ValueError('Unsupported projection %s with 2 parallels' \
                % fh.projName)
        self.map = {'xmin': fh.xMin[0], 'xmax': fh.xMax[0], \
            'ymin': fh.yMin[0], 'ymax': fh.yMax[0]}
        self.map['n'] = math.cos(math.pi/2.0-latC)
        self.map['lonC'] = math.radians(fh.centralLon[0])
        self.map['nx'] = fh.dimensions['x']
        self.map['ny'] = fh.dimensions['y']

    def __getPoint(self, lat, lon):
        r = math.tan((math.pi/2.0-math.radians(lat))/2.0)**self.map['n']
        tau = (math.radians(lon)-self.map['lonC'])*self.map['n']
        nx = (self.map['nx']-1)/(self.map['xmax']-self.map['xmin'])* \
            (math.sin(tau)*r-self.map['xmin'])
        ny = (self.map['ny']-1)/(self.map['ymin']-self.map['ymax'])* \
            (-math.cos(tau)*r-self.map['ymax'])
        return int(nx+1.0), int(ny+1.0)

    def __readNetCDF(self, filename):
        try:
            fh = Scientific.IO.NetCDF.NetCDFFile(filename)
        except IOError:
            # bad or nonexistent file
            _Logger.error('Error accessing %s', path)
            return
        _Logger.info('Processing file %s', filename)
        self.__getMapParms(fh)
        validtime = fh.variables['validTime'][0]
        llvar = fh.variables['image']
        for site in self.sitelist:
            nx, ny = self.__getPoint(site['lat'], site['lon'])
            if nx < 0 or nx >= self.map['nx'] or \
                ny < 0 or ny >= self.map['ny']:
                site['prob'] = -1
            else:
                v = llvar[0][ny][nx]
                if v >= 0:
                    site['prob'] = v/2.0
                else:
                    site['prob'] = (256.0+v)/2.0
        fh.close()
        data = ['Time\t%.0f\n' % validtime]
        for site in self.sitelist:
            p = site['prob']
            if p >= 0:
                line = '%s\t%.0f\n' % (site['id'], p)
                data.append(line)
                _Logger.debug(line[:-1])
        file(self._Dest, 'w').writelines(data)
        self.outqueue.put(Avn.Bunch(src=self.name, ident='ALL'))

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
            if flist:
                try:
                    flist.sort()
                    self.__readNetCDF(os.path.join(*flist[-1]))
                except Exception:
                    _Logger.exception('Unexpected error')
                    break
