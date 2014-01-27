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
#       MosData.py
#       GFS1-NHD:A6638.0000-SCRIPT;1.69
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision AWIPS II                           NJENSEN
#          Updated to work with AWIPS II pointdata interface
#
#       Revision 1.69 (DELIVERED)
#         Created:  19-NOV-2008 09:28:23      OBERFIEL
#           Update to include NAM-MOS guidance
#       
#       Revision 1.68 (DELIVERED)
#         Created:  29-OCT-2008 12:47:57      OBERFIEL
#           Eta-MOS guidance retired.  Now NAM-MOS.  Labelling to
#           reflect this.
#       
#       Revision 1.67 (DELIVERED)
#         Created:  01-AUG-2008 15:44:46      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.66 (DELIVERED)
#         Created:  11-JUN-2008 14:53:09      OBERFIEL
#           Restored use of configuration setting in file.
#       
#       Revision 1.65 (DELIVERED)
#         Created:  16-NOV-2007 12:51:40      OBERFIEL
#           Extracted additional probabilistic information out of the
#           netCDF file and into the .data files.
#       
#       Revision 1.64 (DELIVERED)
#         Created:  03-OCT-2007 13:47:16      OBERFIEL
#           Fixed inadvertant conversion of 9999K to F.  Empty string
#           returned instead.
#       
#       Revision 1.63 (DELIVERED)
#         Created:  26-SEP-2007 13:40:23      OBERFIEL
#           Fixed rounding problem when display MOS/LAMP guidance.
#           Updated Issuance and Valid time update code to call
#           time.time() just once.
#       
#       Revision 1.62 (DELIVERED)
#         Created:  14-MAY-2007 10:04:49      OBERFIEL
#           Removed references to the obsolete prototype XTF product.
#           Allow decoder and encoder to format TAF in two different
#           ways.  New format will be triggered by day and time to be
#           specified at a later date.
#       
#       Revision 1.61 (INITIALIZE)
#         Updated:  18-APR-2007 12:47:21      SOLSON
#           Removed CR characters from this item.
#         Created:  18-APR-2007 12:46:44      SOLSON
#           Removed CR characters from the previous rev of this item.
#       
#       Revision 1.60 (DELIVERED)
#         Created:  11-DEC-2006 09:26:12      BLI
#           Modified to include conditional probability for cig and vis
#           in tabulated gfsmos display
#       
#       Revision 1.59 (DELIVERED)
#         Created:  31-AUG-2006 10:29:18      BLI
#           Fixed an incorrect looking-for-drizzle number for LAMP
#       
#       Revision 1.58 (DELIVERED)
#         Created:  16-AUG-2006 11:22:54      BLI
#           Removed space from empty string
#       
#       Revision 1.57 (DELIVERED)
#         Created:  15-JUN-2006 14:55:56      BLI
#           More fix on tstm time shift
#       
#       Revision 1.56 (DELIVERED)
#         Created:  14-JUN-2006 11:43:40      BLI
#           Fixed misaligned tstm best cat and probability forecasts
#       
#       Revision 1.55 (DELIVERED)
#         Created:  13-JUN-2006 13:44:51      BLI
#           Fixed for wrong wind spd units for LAMP
#       
#       Revision 1.54 (DELIVERED)
#         Created:  08-JUN-2006 14:14:26      BLI
#           Fixed yet another index outbound problem
#       
#       Revision 1.53 (APPROVED)
#         Created:  08-JUN-2006 13:46:25      BLI
#           Fixed wind speed conversion and windGust name error
#       
#       Revision 1.52 (APPROVED)
#         Created:  06-JUN-2006 11:50:28      BLI
#           Fixed a index outbound problem
#       
#       Revision 1.51 (DELIVERED)
#         Created:  06-JUN-2006 11:45:02      BLI
#           Fixed a index outbound problem
#       
#       Revision 1.50 (APPROVED)
#         Created:  02-JUN-2006 14:24:15      BLI
#           Fixed vis cates and value mismatch
#       
#       Revision 1.49 (DELIVERED)
#         Created:  02-JUN-2006 14:09:59      BLI
#           Fixed vis category mismatch
#       
#       Revision 1.48 (DELIVERED)
#         Created:  01-JUN-2006 14:55:10      BLI
#           Fixed for 2 hour tstorm
#       
#       Revision 1.47 (APPROVED)
#         Created:  01-JUN-2006 14:39:40      BLI
#           Fixed for 2-hr tstorm
#       
#       Revision 1.46 (APPROVED)
#         Created:  01-JUN-2006 13:04:40      BLI
#           Fixed for pop value range and ptype missing
#       
#       Revision 1.45 (DELIVERED)
#         Created:  01-JUN-2006 12:50:31      BLI
#           Fixed for missing ptype and pop being in 1~100.
#       
#       Revision 1.44 (DELIVERED)
#         Created:  17-MAY-2006 11:23:58      BLI
#           Fixed wind speed units
#       
#       Revision 1.43 (APPROVED)
#         Created:  16-MAY-2006 15:24:23      BLI
#           Fixed for new LAMP netcdf files
#       
#       Revision 1.42 (DELIVERED)
#         Created:  04-MAY-2006 09:50:29      BLI
#           Assigned unlimited ceiling for 'FEW' and 'SCT'
#       
#       Revision 1.41 (DELIVERED)
#         Created:  20-APR-2006 13:14:41      BLI
#           Fixed a missing '=' for TafGen and a 'TP2' error for
#           MosData
#       
#       Revision 1.40 (DELIVERED)
#         Created:  20-APR-2006 13:02:59      BLI
#           Fixed  missing TCP2
#       
#       Revision 1.39 (DELIVERED)
#         Created:  19-APR-2006 15:43:46      BLI
#           Merged from 3.3.
#       
#       Revision 1.38 (DELIVERED)
#         Created:  27-MAR-2006 13:39:12      BLI
#           Modifed __name__ section
#       
#       Revision 1.37 (DELIVERED)
#         Created:  27-MAR-2006 13:26:28      BLI
#           Modfied the __name__ section for command line testing
#       
#       Revision 1.36 (DELIVERED)
#         Created:  03-MAR-2006 15:24:22      BLI
#           Fixed precip intensity for both MOS andLAMP
#       
#       Revision 1.35 (DELIVERED)
#         Created:  03-MAR-2006 14:08:01      BLI
#           Added PCO,TC2,TP2 in the tabuluated LAMP
#       
#       Revision 1.34 (DELIVERED)
#         Created:  15-FEB-2006 13:49:15      BLI
#           Changed 'SZ' to 'SN'
#       
#       Revision 1.33 (APPROVED)
#         Created:  15-FEB-2006 13:38:59      BLI
#           Corrected Frz vs SN
#       
#       Revision 1.32 (APPROVED)
#         Created:  14-FEB-2006 14:38:15      BLI
#           Changed a limit for heavy snow
#       
#       Revision 1.31 (APPROVED)
#         Created:  14-FEB-2006 13:50:46      BLI
#           Changed vis limits for '-' and '+' sn and drizzle
#       
#       Revision 1.30 (APPROVED)
#         Created:  02-FEB-2006 08:27:40      BLI
#           Changed a few notation
#       
#       Revision 1.29 (REVIEW)
#         Created:  31-JAN-2006 08:35:25      TROJAN
#           Change in naming convention for MOS/LAMP data, added LAMP
#           to plotting module
#       
#       Revision 1.28 (APPROVED)
#         Created:  30-JAN-2006 11:06:04      BLI
#           Added a missing c for 'OVC'
#       
#       Revision 1.27 (APPROVED)
#         Created:  27-JAN-2006 14:54:01      BLI
#           Modified to toggle on/off prob. of LAMP
#       
#       Revision 1.26 (APPROVED)
#         Created:  25-JAN-2006 15:44:38      BLI
#           Corrected a few spacing issue for LAMP report
#       
#       Revision 1.25 (APPROVED)
#         Created:  23-JAN-2006 14:23:27      BLI
#           Changed dest dir for gfslamp
#       
#       Revision 1.24 (APPROVED)
#         Created:  23-JAN-2006 11:14:09      BLI
#           One error correction
#       
#       Revision 1.23 (APPROVED)
#         Created:  15-DEC-2005 16:30:54      BLI
#           Removed an extra space to line up with the hours
#       
#       Revision 1.22 (APPROVED)
#         Created:  13-DEC-2005 15:18:30      BLI
#           Some modifications for implementing LAMP into avnfps
#       
#       Revision 1.21 (DELIVERED)
#         Created:  06-SEP-2005 20:17:40      TROJAN
#           spr 7014
#       
#       Revision 1.20 (DELIVERED)
#         Created:  06-SEP-2005 19:09:59      TROJAN
#           spr 7009
#       
#       Revision 1.19 (DELIVERED)
#         Created:  25-JUL-2005 19:36:26      BLI
#           Fix visBestCat
#       
#       Revision 1.18 (APPROVED)
#         Created:  25-JUL-2005 14:02:26      BLI
#           Corrected two typos
#       
#       Revision 1.17 (APPROVED)
#         Created:  22-JUL-2005 19:59:52      BLI
#           Corrected an error in cigBestcat and CigCat
#       
#       Revision 1.16 (APPROVED)
#         Created:  22-JUL-2005 15:34:25      BLI
#           fixed a couple typo
#       
#       Revision 1.15 (APPROVED)
#         Created:  14-JUL-2005 16:27:09      BLI
#           merged changes
#       
#       Revision 1.14 (APPROVED)
#         Created:  12-JUL-2005 19:45:32      BLI
#           correct syntax error
#       
#       Revision 1.13 (APPROVED)
#         Created:  11-JUL-2005 20:01:07      BLI
#           map cig cats to new ones for gfs/eta
#       
#       Revision 1.12 (REVIEW)
#         Created:  07-JUL-2005 12:36:35      TROJAN
#           spr 6912
#       
#       Revision 1.11 (DELIVERED)
#         Created:  07-MAY-2005 11:36:25      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.10 (DELIVERED)
#         Created:  21-APR-2005 18:49:45      BLI
#           added check for the presence of the variable in netcdf file
#           before trying to fetch them
#       
#       Revision 1.9 (DELIVERED)
#         Created:  18-APR-2005 18:55:49      BLI
#           Added handler for clouds_CL etc
#       
#       Revision 1.8 (APPROVED)
#         Created:  18-APR-2005 18:12:19      TROJAN
#           stdr 917
#       
#       Revision 1.7 (DELIVERED)
#         Created:  04-APR-2005 15:51:08      TROJAN
#           spr 6775
#       
#       Revision 1.6 (APPROVED)
#         Created:  21-MAR-2005 15:32:31      TROJAN
#           spr 6733
#       
#       Revision 1.5 (DELIVERED)
#         Created:  15-FEB-2005 18:12:21      TROJAN
#           spr 6561
#       
#       Revision 1.4 (DELIVERED)
#         Created:  30-SEP-2004 20:22:10      TROJAN
#           stdr 873
#       
#       Revision 1.3 (APPROVED)
#         Created:  01-JUL-2004 14:59:45      OBERFIEL
#           Update
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:40:13      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:46:01      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7407
#       	Action Date:       03-JAN-2009 09:26:51
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: Allow WFOs to update HDF5 climate files
#       
#
import cPickle, logging, os, time
import numpy
import pupynere
import Avn, AvnLib

ToKt = 3600.0/1852.0

import NoDataException
_Logger = logging.getLogger(Avn.CATEGORY)


# note: POP6hr and SEVERE6hr are added in by the individual MOS types
PARAMETERS = ["stationId", "refTime", "forecastHr",
            "ceiling_bestCat", "windSpeedInflated", "MaxWindSpeed",
            "precipFreezing", "clouds_OV", "ceiling_cat1", "ceiling_cat2", "ceiling_cat3",
            "ceiling_cat4", "ceiling_cat5", "ceiling_cat6", "ceiling_cat7",
            "vis_cat5", "vis_cat4", "vis_cat6", "vis_cat1",
            "vis_cat3", "vis_cat2", "QPF6hr_bestCat", "tstorm6hr",
            "POP_bestCat", "dewpoint", "vis_bestCat",
            "precipSnow", "clouds_SC", "temperature", "clouds_BK",            
            "clouds_CL", "precipType", "obVis_bestCat",
            "clouds_bestCat", "POP6hr", "severe6hr", "csevere6hr", "POP_hour",
            "windDir", 'cvis_bestCat', 'POP_hour_bestCat', 'tstorm2hr', 'tstorm_bestCat',
            'PQPF_6hr', 'ceiling_cat8',
            'c_ceiling_cat1', 'c_ceiling_cat2', 'c_ceiling_cat3', 'c_ceiling_cat4',
            'c_ceiling_cat5', 'c_ceiling_cat6', 'c_ceiling_cat7', 'c_ceiling_cat8',
            'c_ceiling_bestCat',
            'cvis_cat1', 'cvis_cat2', 'cvis_cat3', 'cvis_cat4', 'cvis_cat5', 'cvis_cat6'] 

###############################################################################
# converters used to create tables
def _textKtoF(t):
    if t < 330:
        return '%2.0f' % ((t-273.15)*1.8 + 32.0)
    return ''

def _textDD(t):
    """t is a tuple (d, f)"""
    if 0 <= t[0] <= 360:
        dd = t[0]
        if 0.0 <= t[1] < 0.5:
            dd = 0.0
        return '%02d0' % int((dd+5)/10.0)
    else:
        return ''

def _textFF(t):
    if 0.0 <= t < 70.0:
        return '%02d' % int(t*ToKt+0.5)
    else:
        return ''

def _textGG(t):
    if 0.0 <= t < 80.0:
        return '%02d' % int(t*ToKt+0.5)
    else:
        return ''

def _textProb(t):
    if 0 <= t <= 100:
        return '%3.0f' % t
    else:
        return ''

def _textPcpType(t):
    return {1: 'FZ', 2: 'SN', 3: 'RA'}.get(t, '')

def _textLampPcpType(t):
    return {1: 'FZ', 2: 'SN', 3: 'RA'}.get(t, '')

def _textObv(t):
    return {0: 'BL', 2: 'HZ', 3: 'FG', 4: 'N', 5: 'BR'}.get(t, '')

def _textLampObv(t):
    return {4: '', 2: 'HZ', 5: 'BR', 3: 'FG', 1: 'BL'}.get(t, '')

def _textCld(t):
    return {0: 'SKC', 8: 'OVC', 11: 'SCT', 12: 'BKN', 13:'FEW'}.get(t, '')

def _textLampCld(t):
    return {0: 'SKC', 13: 'FEW', 11: 'SCT', 12: 'BKN', 8:'OVC'}.get(t, '')

def _getCigProbs(v,element,numCategories):
    try:
        probs = [v[element+str(x)] for x in xrange(1,numCategories+1)]
        return [min(x,100) for x in Avn.accumulate(probs)]
    except KeyError:
        return [0]*(numCategories-1)+[100]
    
def _getVisProbs(v,element,numCategories):
    try:
        return [v[element+str(x)] for x in xrange(1,numCategories+1)]+[100]
    except KeyError:
        return [0]*(numCategories-1)+[100]

###############################################################################
class _NetCDFFile:

    def __init__(self):
        pass

    def makeWind(self, v, noToKt):
        d = {}
	gg=0
        dd = int(v['windDir'])
        if dd != v.getFillValue('windDir'):
            dd = 10*((dd+5)//10)
            if dd == 0:
                dd = 360
            d['dd'] = dd
	if 'windSpeedInflated' in v:    
            ff = float(v['windSpeedInflated'])
	    FValue=v.getFillValue('windSpeedInflated')
	else:
	    ff = float(v['windSpeed'])
	    FValue = v.getFillValue('windSpeed')
        if ff != FValue:
            if noToKt:
                d['ff'] = int(ff+0.5)
            else:
                d['ff'] = int(ff*ToKt+0.5)
            if d['ff'] == 0:
                d['dd'] = 0
        if 'MaxWindSpeed' in v: gg = int(v['MaxWindSpeed']*ToKt+0.5)
        if 'dd' in d and 'ff' in d and 9998 > gg > 0:
	    d['gg'] = int(gg)
            d['str'] = '%03d%02dG%02dKT' % (d['dd'], d['ff'],d['gg'])
        else:    
            if 'dd' in d and 'ff' in d:
                d['str'] = '%03d%02dKT' % (d['dd'], d['ff'])
            else:
                d['str'] = '??????KT'
        return d


    def makeObv(self, v):
        t = int(v['obVis_bestCat'])
        s = {0: 'BL', 2: 'HZ', 3: 'FG', 4: '', 5: 'BR'}.get(t, '')
        if s:
            return {'str': s}
        else:
            return None

    def makeVsby(klass, var):
        # returns mid point of category range
        tmp = klass.VsbyValues.get(int(var), None)
        if tmp:
            return AvnLib.fixTafVsby(tmp)
        else:
            return None
    makeVsby = classmethod(makeVsby)

    def makeSky(klass, ceiling_bestCat, clouds_bestCat):
        Cld = ['SKC', 'FEW', 'SCT', 'BKN', 'OVC']
        Cover = {0: 0, 8: 4, 11: 2, 12: 3, 13: 1}
        cig = klass.CigValues.get(int(ceiling_bestCat), None)
        cover = Cover.get(int(clouds_bestCat), None)
        if cover < 3: cig = Avn.UNLIMITED 
        if cig is not None:
            if 1 <= cover <= 4:
                if cig != Avn.UNLIMITED:
                    d = {'str': '%s%03d' % (Cld[cover], cig/100), \
                        'cover': cover, 'cig': cig}
                else:
                    d = {'str': '%s%03d' % (Cld[cover], 250), \
                        'cover': cover, 'cig': cig}
            elif cover == 0:
                d = {'str': 'SKC', 'cover': 0, 'cig': Avn.UNLIMITED}
	    else:
	    	return None
            return d
        else:
            return None
    makeSky = classmethod(makeSky)

    def makePcp(self, v, vsby, pdc, n, fcstHrList):
        PType =  {11: 'FZDZ', 12: 'FZRA', 13: 'SHPL', \
            21: 'DZSN', 22: 'SN',   23: 'SHSN', \
            31: 'DZ',   32: 'RA',   33: 'SHRA'}
        d = {}
        # shift
        if n & 0x01:    # 3 hours
            m = 1
        else:           # 6 hours
            m = 2
        #p = int(v[self.POP6hr][recno,n+m])
        p = int(pdc[fcstHrList[n+m]][self.POP6hr])
        if p !=v.getFillValue(self.POP6hr):
            d['pop'] = p
        #p = int(v['tstorm6hr'][recno,n+m])
        p = int(pdc[fcstHrList[n+m]]['tstorm6hr'])
        if p != v.getFillValue('tstorm6hr'):
            d['pot'] = p
        ptype = int(v['precipType'])
        if not 1 <= ptype <= 3:
            ptype = 3   # rain 
        pop = int(v['POP_bestCat'])
        if not 1 <= pop <= 3:
            pop = 2     # continuous 
        intensity = ''
        if ptype == 2 or pop == 1:  # SN or DZ
            if vsby:
                if vsby > 0.50:
                    intensity = '-'
                elif vsby < 0.245:
                    intensity = '+'
        else:
            intensity = '-' 
        pcp = PType[10*ptype+pop]
        d.update({'str': intensity+pcp, 'int': intensity})
        return d 

    def getRecord(self, ident):
        if not self._fh:
            return None
        try:
            return self._sitedict[ident]
        except KeyError:
            raise Avn.AvnError('%s not in %s' % (ident, self._path))
            return None

    def cigBestCat(self,t):
        if 0 < t < 100:
	    return '%3.0f' % t
	else:
	    return ''

    def visBestCat(self,t):
        if 0 < t < 100:
	    return '%3.0f' % t
	else:
	    return ''

    def makePeriod(self, pdc, n, fcstHrList):
        #v = self._fh.variables
        v = pdc[fcstHrList[n]]
        f, t = self._validTimeList[n:n+2]
        g = {'time': {'from': f, 'to': t}}
        d = self.makeWind(v, 0)
        if d:
            g['wind'] = d
        d = self.makeVsby(v['vis_bestCat'])
        if d:
            g['vsby'] = d
            vsby = d['value']
        else:
            vsby = None
        d = self.makePcp(v, vsby, pdc, n, fcstHrList)
        if d:
            g['pcp'] = d
        d = self.makeObv(v)
        if d:
            g['obv'] = d
        d = self.makeSky(v['ceiling_bestCat'], v['clouds_bestCat'])
        if d:
            g['sky'] = d
        # fix visibility and obstruction to vision
        if 'vsby' in g and 'obv' in g and g['obv']['str'] in ['BR', 'FG']:
            vsby = g['vsby']['value']
            if vsby > 6.1:
                g['vsby'] = {'str': '6SM', 'value': 6.0}
            if vsby < 0.6:
                g['obv']['str'] = 'FG'
            elif vsby <= 6.1:
                g['obv']['str'] = 'BR'
        return g

    def close(self):
        try:
            self._fh.close()
        except:
            _Logger.error('Failed to close data file')

    def getFile(self, path):
        try:
            self._path = path
            self._fh = pupynere.NetCDFFile(self._path, 'r')
            self.issuetime = self._fh.variables['time'].getValue()
            self._validtimes = \
                self._fh.variables['validTimeList'][:self.NumData].tolist()
            var = self._fh.variables['stationName']
            self._sitedict = {}
            for n in xrange(var.shape[0]):
                ident = var[n,:].tostring().split('\x00')[0].rstrip()
                self._sitedict[ident] = n
            return True
        except IOError:
            _Logger.error('Error accessing %s', path)
            return False

    def makeData(self, ident, refTime=None):
#        recno = self.getRecord(ident)
#        if recno is None:
#            return None
        
        import ForecastPointDataRetrieve
#        print 'makeData: ident (%s), selfModel(%s) refTime(%s):' % (ident, self.Model, refTime)
        pdc = ForecastPointDataRetrieve.retrieve('bufrmos' + self.Model, ident, PARAMETERS, refTime=refTime)
        self.NumData = min(self.NumData, len(pdc.keys()))
        self.issuetime = pdc.refTime.getTime() / 1000
        fcstHrList = pdc.keys()
        fcstHrList.sort()
        self._validTimeList = []
        for f in fcstHrList:
            self._validTimeList.append(self.issuetime + (f * 3600))
        
        #
        # NGMMOS.  The internal ID should be four characters long.
        if len(ident) == 3:
            ident = 'K'+ident
            
        d = {'itime': {'value': self.issuetime, \
            'str': time.strftime('%d%H%MZ', time.gmtime(self.issuetime))}, \
            'ident': {'str': ident}}        
        d['group'] = [self.makePeriod(pdc,n, fcstHrList) for n in range(self.NumData)]
        return d
    
    def loopAll(self, pdc, fcstHrList, key):
        result = []
        for f in fcstHrList:
            result.append(pdc[f][key])
            if len(result) == self.NumData:
                break
        return result

    def makeReport(self, ident):    
        import ForecastPointDataRetrieve
        pdc = ForecastPointDataRetrieve.retrieve('bufrmos' + self.Model, ident, PARAMETERS)
        self.NumData = min(self.NumData, len(pdc.keys()))
        self.issuetime = pdc.refTime.getTime() / 1000
        fcstHrList = pdc.keys()
        fcstHrList.sort()
        self._validTimeList = []
        count = 0;
        for f in fcstHrList:
            self._validTimeList.append(self.issuetime + (f * 3600))
            count += 1
            if count >= self.NumData:
                break                    
            
        rpt = ['%s    %s   %s' % (ident, self.Header, \
            time.strftime('%x  %H%M UTC', time.gmtime(self.issuetime)))]
        #v = self._fh.variables
        rpt.append('hour     ' + ' '.join([time.strftime(' %H',
            time.gmtime(t)) for t in self._validTimeList]))
        if pdc.hasParam('temperature'):                
            t = map(_textKtoF, self.loopAll(pdc, fcstHrList, 'temperature'))            
            rpt.append('TMP       ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('dewpoint'):
            t = map(_textKtoF, self.loopAll(pdc, fcstHrList, 'dewpoint'))            
            rpt.append('DPT       ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('windDir'):
            #t = map(_textDD, zip(v['windDir'][recno,:self.NumData],
            #    v['windSpeedInflated'][recno,:self.NumData]))
            t = map(_textDD, zip(self.loopAll(pdc, fcstHrList, 'windDir'),
                                 self.loopAll(pdc, fcstHrList, 'windDir')))
            rpt.append('WDR      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('windSpeedInflated'):
            t = map(_textFF, self.loopAll(pdc, fcstHrList, 'windSpeedInflated'))
            rpt.append('WSP       ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('MaxWindSpeed'):
            t = map(_textGG, self.loopAll(pdc, fcstHrList, 'MaxWindSpeed'))
            rpt.append('WGST      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('vis_bestCat'):
            t = map(self.visBestCat, self.loopAll(pdc, fcstHrList, 'vis_bestCat'))
            rpt.append('VIS     ' + '%+4s' * self.NumData % tuple(t))

        for cat in ['vis_cat%d' % k for k in range(1, self.NumVsbyCat+1)]:
            if pdc.hasParam(cat):
                t = map(_textProb, self.loopAll(pdc, fcstHrList, cat))
                rpt.append(('%s     ' + '%-4s' * self.NumData) % \
                    tuple([cat[-4:]] + t))
	        
        if pdc.hasParam('obVis_bestCat'):
            t = map(_textObv, self.loopAll(pdc, fcstHrList, 'obVis_bestCat'))
            rpt.append('OBVIS   ' + '%+4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_bestCat'):
            t = map(_textCld, self.loopAll(pdc, fcstHrList, 'clouds_bestCat'))
            rpt.append('CLD      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_CL'): 
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_CL'))
            rpt.append('PSKC     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_FW'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_FW'))
            rpt.append('PFEW     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_SC'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_SC'))
            rpt.append('PSCT     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_BK'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_BK'))
            rpt.append('PBKN     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_OV'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_OV'))
            rpt.append('POVC     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('ceiling_bestCat'):
            t = map(self.cigBestCat, self.loopAll(pdc, fcstHrList, 'ceiling_bestCat'))
            rpt.append('CIG      ' + '%-4s' * self.NumData % tuple(t))
        for cat in ['ceiling_cat%d' % k for k in \
            range(1, self.NumCigCat+1)]:
            if pdc.hasParam(cat):
                t = map(_textProb, self.loopAll(pdc, fcstHrList, cat))
                rpt.append(('%s     ' + '%-4s' * self.NumData) % \
                    tuple([cat[-4:]] + t))
        if pdc.hasParam('precipType'):
            t = map(_textPcpType, self.loopAll(pdc, fcstHrList, 'precipType'))
            rpt.append('PTYPE     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam(self.POP6hr):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, self.POP6hr))
            rpt.append('POP06    ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('QPF6hr_bestCat'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'QPF6hr_bestCat'))
            rpt.append('QPF06    ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('tstorm6hr'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'tstorm6hr'))
            rpt.append('TS06     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam(self.SEVERE6hr):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, self.SEVERE6hr))
            rpt.append('STS06    ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('precipFreezing'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'precipFreezing'))
            rpt.append('POZ      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('precipSnow'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'precipSnow'))
            rpt.append('POS      ' + '%-4s' * self.NumData % tuple(t))
        rpt.append('')
        return rpt

###############################################################################
class _AvnNetCDFFile(_NetCDFFile):
    """AVN MOS NetCDF file"""
    NumData = 15    # 42 hours
    NumVsbyCat = 7
    NumCigCat = 7
    POP6hr = 'PQPF_6hr' # to avoid repeating code
    SEVERE6hr='severe6hr'
    VsbyValues = {1: 0.25, 2: 0.5, 3: 0.75, 4: 2.0, 5: 4.0, 6: 6.0, 7: 99.0}
    CigValues = {1: 100, 2: 400, 3: 700, 4: 2000, 5: 5000, 6: 10000, 7: 25000}
    Header = 'AVN MOS Guidance'
    Model = 'AVN'    

###############################################################################
class _NgmNetCDFFile(_NetCDFFile):
    """NGM MOS NetCDF file"""
    NumData = 15    # 42 hours
    NumVsbyCat = 5
    NumCigCat = 7
    POP6hr = 'POP6hr'   # to avoid repeating code
    SEVERE6hr='severe6hr'
    VsbyValues = {1: 0.25, 2: 0.75, 3: 2.0, 4: 4.0, 5: 99.0}
    CigValues = {1: 100, 2: 400, 3: 700, 4: 2000, 5: 5000, 6: 10000, 7: 25000}
    Header = 'NGM MOS Guidance'
    Model = 'NGM'    

###############################################################################
class _GfsNetCDFFile(_NetCDFFile):
    """GFS MOS NetCDF file"""
    NumData = 15    # 42 hours
    NumVsbyCat = 7 
    NumCigCat = 8
    POP6hr = 'PQPF_6hr' # to avoid repeating code
    SEVERE6hr='csevere6hr'
    VsbyValues = {8: 0.25, 9: 0.5, 10: 1.5, 11: 2.5, 5: 4.0, 6: 5.5, 7: 10.0}
    CigValues = {1:100, 2:400, 3:700, 8:1500, 9:2500, 5:5000, 6:10000, 7:25000}
    Header = 'GFS MOS Guidance'
    Model = 'GFS'
    
    def cigBestCat(self,t):
        covt={1:1, 2:2, 3:3, 8:4, 9:5, 5:6, 6:7, 7:8}
	try:
	    return '%3.0f' % covt[int(t+0.1)]
	except:
	    return ''

    def visBestCat(self,t):
        covt={8:1, 9:2, 10:3, 11:4, 5:5, 6:6, 7:7}
	try:
	    return '%3.0f' % covt[int(t+0.1)]
	except:
	    return ''

###############################################################################
class _EtaNetCDFFile(_NetCDFFile):
    """NAM MOS NetCDF file"""
    NumData = 15    # 42 hours
    NumVsbyCat = 7
    NumCigCat = 8
    POP6hr = 'PQPF_6hr' # to avoid repeating code
    SEVERE6hr='csevere6hr'
    VsbyValues = {8: 0.25, 9: 0.5, 10: 1.5, 11: 2.5, 5: 4.0, 6: 5.5, 7: 10.0}
    CigValues = {1:100, 2:400, 3:700, 8:1500, 9:2500, 5:5000, 6:10000, 7:25000}
    Header = 'NAM MOS Guidance'
    Model = 'ETA'
    
    def cigBestCat(self,t):
        covt={1:1, 2:2, 3:3, 8:4, 9:5, 5:6, 6:7, 7:8}
	try:
	    return '%3.0f' % covt[int(t+0.1)]
	except:
	    return ''

    def visBestCat(self,t):
        covt={8:1, 9:2, 10:3, 11:4, 5:5, 6:6, 7:7}
	try:
	    return '%3.0f' % covt[int(t+0.1)]
	except:
	    return ''

###############################################################################
class _GfsLampNetCDFFile(_NetCDFFile):
    """GFS LAMP NetCDF file"""
    NumData = 25    # 25 forecast hours
    NumVsbyCat = 6
    NumCigCat = 8
    POP6hr = 'POP_hour' # to avoid repeating code
    SEVERE6hr = ''
    VsbyValues = {8: 0.25, 9: 0.5, 10: 1.5, 11: 2.5, 5: 4.0, 6: 6, 7: 10.0}
    CigValues = {1:100, 2:300, 3:700, 8:1500, 9:2500, 5:5000, 6:10000, 7:25000}
    Header = 'GFS LAMP Guidance'
    Model = 'LAMP'    

    def cigBestCat(self,t):
        covt={1:1, 2:2, 3:3, 8:4, 9:5, 5:6, 6:7, 7:8}
	try:
	    return '%3.0f' % covt[int(t+0.1)]
	except:
	    return ''

    def visBestCat(self,t):
        covt={8:1, 9:2, 10:3, 11:4, 5:5, 6:6, 7:7}
	try:
	    return '%3.0f' % covt[int(t+0.1)]
	except:
	    return ''

    def makeObv(self, v):
        t = int(v['obVis_bestCat'])
        s = {4: '', 2: 'HZ', 5: 'BR', 3: 'FG', 1: 'BL'}.get(t, '')
        if s:
            return {'str': s}
        else:
            return None

    def makePcp(self, v, vsby, pdc, n, fcstHrList):
        PType =  {13: 'SHPL', 11: 'FZDZ', 12: 'FZRA', \
                  23: 'SHSN', 21: 'DZSN', 22: 'SN', \
                  33: 'SHRA', 31: 'DZ',   32: 'RA'}
        d = {}
        p = v['POP_hour']
        if p != v.getFillValue('POP_hour'):
            d['pop'] = int(p)
        p = int(v['POP_hour_bestCat'])
        if p != v.getFillValue('POP_hour_bestCat'):
            d['pcat'] = p

        #tstm has overlapped 2-hour forecasts in the first five hours,then 2 hour 
        if n < self.NumData-1:            
            #p = v['tstorm2hr'][recno,n+1]
            p = pdc[fcstHrList[n+1]]['tstorm2hr']
            if p == v.getFillValue('tstorm2hr') and n < self.NumData-2: 
                try:
                    #p = v['tstorm2hr'][recno,n+2]
                    p = pdc[fcstHrList[n+2]]['tstorm2hr']
                except:
                    p = v.getFillValue('tstorm2hr')
        else:
            p = v.getFillValue('tstorm2hr')
        if p != v.getFillValue('tstorm2hr'):
            d['pot'] = int(p)

        if n < self.NumData-1:
            #p = int(v['tstorm_bestCat'][recno,n+1])
            p = int(pdc[fcstHrList[n+1]]['tstorm_bestCat'])
            if p == v.getFillValue('tstorm_bestCat') and n < self.NumData-2:
                try:
                    #p = int(v['tstorm_bestCat'][recno,n+2])
                    p = int(pdc[fcstHrList[n+2]]['tstorm_bestCat'])
                except:
                    p = v.getFillValue('tstorm_bestCat')
        else:
            p = v.getFillValue('tstorm_bestCat')

        if p != v.getFillValue('tstorm_bestCat'):
            d['tcat'] = p
        ptype = int(v['precipType'])
        #if ptype is missing, it's rain
	if ptype == v.getFillValue('precipType'):
            ptype = 3   # rain 
        pchar = int(v['POP_bestCat'])
        if pchar == v.getFillValue('POP_bestCat'):
            pchar = 2
        intensity = ''
        if ptype == 2 or pchar == 1:  # SN or DZ
            if vsby:
                if vsby < 0.245:
                    intensity = '+'
                elif vsby > 0.50:
                    intensity = '-'
        else:
            intensity = '-'
        pcp = PType[ptype*10+pchar]
        d.update({'str': intensity+pcp, 'int': intensity})
        return d

    def makeSky(klass, ceiling_bestCat, clouds_bestCat):
        Cld = {0:'SKC', 13:'FEW', 11:'SCT', 12:'BKN', 8:'OVC'}
        cig = klass.CigValues.get(int(ceiling_bestCat), None)
        cover = int(clouds_bestCat)
        #if sky cover is not BKN or OVC, set cig to unlimited.
        if cover in [0, 13, 11] : cig = Avn.UNLIMITED
  
        if cig is not None:
            if cover == 0:
                d = {'str': 'SKC', 'cover': 0, 'cig': Avn.UNLIMITED}
            elif cover in [0,13,11,12,8]:
                if cig != Avn.UNLIMITED:
                    d = {'str': '%s%03d' % (Cld[cover], cig/100), \
                        'cover': cover, 'cig': cig}
                else:
                    d = {'str': '%s%03d' % (Cld[cover], 250), \
                        'cover': cover, 'cig': cig}
	    else:
	    	return None
            return d
        else:
            return None
    makeSky = classmethod(makeSky)

    def makePeriod(self, pdc, n, fcstHrList):
        #v = self._fh.variables
        v = pdc[fcstHrList[n]]
        try:
           #f, t = v['validTimeList'][n:n+2]
           f, t = self._validTimeList[n:n+2]
        except ValueError: #LAMP only has 25 projections, so need to consider running out of pairs
           #f, t = v['validTimeList'][n],v['validTimeList'][n]+3600
           f,t = self._validTimeList[n],self._validTimeList[n]+3600
           
        g = {'time': {'from': f, 'to': t}}
        d = self.makeWind(v, 0)
        if d:
            g['wind'] = d

        d = self.makeVsby(v['vis_bestCat'])
        if d:
            g['vsby'] = d
            vsby = d['value']
        else:
            vsby = None
        
        d = self.makeVsby(v['cvis_bestCat'])
        if d:
            g['cvsby'] = d
            cvsby = d['value']
        else:
            cvsby = None

        if v['POP_hour']*100 > 40:
	    vsby=cvsby

        d = self.makePcp(v, vsby, pdc, n, fcstHrList)
        if d:
            g['pcp'] = d

        d = self.makeObv(v)
        if d:
            g['obv'] = d
	    #cobv is the same as obv until 'FG' and 'BR' is switched based vis
            g['cobv'] = d

        d = self.makeSky(v['ceiling_bestCat'], v['clouds_bestCat'])
        if d:
            g['sky'] = d

        try:
            d = self.makeSky(v['c_ceiling_bestCat'], v['clouds_bestCat'])
            if d:
                g['csky'] = d
        except:
	    pass

        # fix visibility and obstruction to vision
        if 'vsby' in g and 'obv' in g and g['obv']['str'] in ['BR', 'FG']:
            vsby = g['vsby']['value']
            if vsby > 6.1:
                g['vsby'] = {'str': '6SM', 'value': 6.0}
            if vsby < 0.6:
                g['obv']['str'] = 'FG'
            elif vsby <= 6.1:
                g['obv']['str'] = 'BR'

        # fix conditional visibility and obstruction to vision
        if 'cvsby' in g and 'obv' in g and g['obv']['str'] in ['BR', 'FG']:
            vsby = g['cvsby']['value']
            if vsby > 6.1:
                g['cvsby'] = {'str': '6SM', 'value': 6.0}
            if vsby < 0.6:
                g['cobv']['str'] = 'FG'
            elif vsby <= 6.1:
                g['cobv']['str'] = 'BR'
        #
        # include the probabilities
        # Look ahead for the 6hr QPF POP
        #
        g['pop6hr'] = -1
        try:
            for i in xrange(n,25):
                #if v['PQPF_6hr'][recno,i] < 100:
                if pdc[fcstHrList[i]]['PQPF_6hr'] < 100:
                    #g['pop6hr'] = v['PQPF_6hr'][recno,i]
                    g['pop6hr'] = pdc[fcstHrList[i]]['PQPF_6hr'] 
                    break
        except KeyError:
            pass
        #
        # Probability of ceiling categories including best category
        g['cprob'] = _getCigProbs(v,'ceiling_cat',8)
        g['ccprob'] = _getCigProbs(v,'c_ceiling_cat',8)
        try:
            g['cig_bestCat'] = int(self.cigBestCat(v['ceiling_bestCat']))
            g['ccig_bestCat'] =int(self.cigBestCat(v['c_ceiling_bestCat']))
        except ValueError:
            pass
        #
        # Probability of visibility categories including best category
        g['vprob'] = _getVisProbs(v,'vis_cat',6)
        g['cvprob'] = _getVisProbs(v,'cvis_cat',6)
        try:
            g['vis_bestCat'] = int(self.visBestCat(v['vis_bestCat']))
            g['cvis_bestCat'] = int(self.visBestCat(v['cvis_bestCat']))
        except ValueError:
            pass

        return g

    def makeReport(self, ident):
        import ForecastPointDataRetrieve
        pdc = ForecastPointDataRetrieve.retrieve('bufrmos' + self.Model, ident, PARAMETERS)
        self.NumData = min(self.NumData, len(pdc.keys()))
        self.issuetime = pdc.refTime.getTime() / 1000
        fcstHrList = pdc.keys()
        fcstHrList.sort()
        self._validTimeList = []
        count = 0
        for f in fcstHrList:
            self._validTimeList.append(self.issuetime + (f * 3600))
            count += 1
            if count >= self.NumData:
                break

        rpt = ['%s    %s   %s' % (ident, self.Header, \
            time.strftime('%x  %H%M UTC', time.gmtime(self.issuetime)))]
        rpt.append('hour     ' + ' '.join([time.strftime(' %H',
            time.gmtime(t)) for t in self._validTimeList]))        
        if pdc.hasParam('temperature'):
            t = map(_textKtoF, self.loopAll(pdc, fcstHrList, 'temperature'))
            rpt.append('TMP       ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('dewpoint'):
            t = map(_textKtoF, self.loopAll(pdc, fcstHrList, 'dewpoint'))
            rpt.append('DPT       ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('windDir'):
        #if 'windDir' in v:
            #t = map(_textDD, zip(v['windDir'][recno,:self.NumData],
            #    v['windDir'][recno,:self.NumData]))
            t = map(_textDD, zip(self.loopAll(pdc, fcstHrList, 'windDir'),
                                 self.loopAll(pdc, fcstHrList, 'windDir')))
            rpt.append('WDR      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('windSpeedInflated'):
            t = map(_textFF, self.loopAll(pdc, fcstHrList, 'windSpeedInflated'))
            rpt.append('WSP       ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('MaxWindSpeed'):
            t = map(_textGG, self.loopAll(pdc, fcstHrList, 'MaxWindSpeed'))
            rpt.append('WGST      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('vis_bestCat'):
            t = map(self.visBestCat, self.loopAll(pdc, fcstHrList, 'vis_bestCat'))
            rpt.append('VIS     ' + '%+4s' * self.NumData % tuple(t))
        for cat in ['vis_cat%d' % k for k in range(1, self.NumVsbyCat+1)]:
            if pdc.hasParam(cat):
                t = map(_textProb, self.loopAll(pdc, fcstHrList, cat))
                rpt.append(('%s     ' + '%-4s' * self.NumData) % \
                    tuple([cat[-4:]] + t))
        if pdc.hasParam('cvis_bestCat'):
            t = map(self.visBestCat, self.loopAll(pdc, fcstHrList, 'cvis_bestCat'))
            rpt.append('CVIS    ' + '%+4s' * self.NumData % tuple(t))
        for cat in ['cvis_cat%d' % k for k in range(1, self.NumVsbyCat+1)]:
            if pdc.hasParam(cat):
                t = map(_textProb, self.loopAll(pdc, fcstHrList, cat))
                rpt.append(('%s     ' + '%-4s' * self.NumData) % \
                    tuple([cat[-4:]] + t))
        if pdc.hasParam('obVis_bestCat'):
            t = map(_textLampObv, self.loopAll(pdc, fcstHrList, 'obVis_bestCat'))
            rpt.append('OBVIS   ' + '%+4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_bestCat'):
            t = map(_textLampCld, self.loopAll(pdc, fcstHrList, 'clouds_bestCat'))
            rpt.append('CLD      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_CL'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_CL'))
            rpt.append('PSKC     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_FW'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_FW'))
            rpt.append('PFEW     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_SC'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_SC'))
            rpt.append('PSCT     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_BK'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_BK'))
            rpt.append('PBKN     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('clouds_OV'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'clouds_OV'))
            rpt.append('POVC     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('ceiling_bestCat'):
            t = map(self.cigBestCat, self.loopAll(pdc, fcstHrList, 'ceiling_bestCat'))
            rpt.append('CIG      ' + '%-4s' * self.NumData % tuple(t))
        for cat in ['ceiling_cat%d' % k for k in \
            range(1, self.NumCigCat+1)]:
            if pdc.hasParam(cat):
                t = map(_textProb, self.loopAll(pdc, fcstHrList, cat))
                rpt.append(('%s     ' + '%-4s' * self.NumData) % \
                    tuple([cat[-4:]] + t))
        if pdc.hasParam('c_ceiling_bestCat'):
            t = map(self.cigBestCat, self.loopAll(pdc, fcstHrList, 'c_ceiling_bestCat'))
            rpt.append('CCIG     ' + '%-4s' * self.NumData % tuple(t))
        for cat in ['c_ceiling_cat%d' % k for k in \
            range(1, self.NumCigCat+1)]:
            if pdc.hasParam(cat):
                t = map(_textProb, self.loopAll(pdc, fcstHrList, cat))
                rpt.append(('%s     ' + '%-4s' * self.NumData) % \
                    tuple([cat[-4:]] + t))
        if pdc.hasParam('precipType'):
            t = map(_textLampPcpType, self.loopAll(pdc, fcstHrList, 'precipType'))
            rpt.append('PTYPE     ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('precipSnow'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'precipSnow'))
            rpt.append('POS      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('precipFreezing'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'precipFreezing'))
            rpt.append('POZ      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('POP_hour'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'POP_hour'))
            rpt.append('PPO      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('POP_hour_bestCat'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'POP_hour_bestCat'))
            rpt.append('PCO      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('tstorm2hr'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'tstorm2hr'))
            rpt.append('TP2      ' + '%-4s' * self.NumData % tuple(t))
        if pdc.hasParam('tstorm_bestCat'):
            t = map(_textProb, self.loopAll(pdc, fcstHrList, 'tstorm_bestCat'))
            rpt.append('TC2      ' + '%-4s' * self.NumData % tuple(t))
        rpt.append('')
        return rpt


###############################################################################
def _cleanup(path, nhours):
    tstamp = Avn.time2string(time.time()-nhours*3600.0)
    for f in os.listdir(path):
        if f[:10] < tstamp:
            fname = os.path.join(path, f)
            try:
                os.unlink(fname)
            except OSError, e:
                _Logger.exception('Cannot remove %s', fname)

###############################################################################
def retrieve(model, idlist, includeReport, refTime=None):
    if model == 'avnmos':
        nc = _AvnNetCDFFile()
    elif model == 'ngmmos':
        nc = _NgmNetCDFFile()
    elif model == 'gfsmos':
        nc = _GfsNetCDFFile()
    elif model == 'etamos':
        nc = _EtaNetCDFFile()
    elif model == 'nammos':
        nc = _EtaNetCDFFile()
    elif model == 'gfslamp':
        nc = _GfsLampNetCDFFile()
    else:
        raise Avn.AvnError('Invalid model %s' % model)
    ids = []
    for ident in idlist:
        try:            
            rpt = None
            data = None
            if includeReport:
                rpt = nc.makeReport(ident) # rpt is table view
            data = nc.makeData(ident, refTime=refTime)
            ids.append(Avn.Bunch(data=data, rpt=rpt))
#            print 'Retrieved %s data for %s at refTime %s' % (model, ident, refTime)
            _Logger.info('Retrieved %s data for %s', model, ident)
        except Avn.AvnError, e:
            _Logger.error(str(e))
        except NoDataException.NoDataException, e:
            msg = [str(e)]
            ids.append(Avn.Bunch(data=msg, rpt=msg))
        except Exception, e:
            _Logger.exception(str(e))
    return ids

   ############################################################################
if __name__ == '__main__':
    import pprint, os,sys
    
    path = '/data/fxa/point/gfslamp/netcdf/%s' % sys.argv[1]    
    nc = _GfsLampNetCDFFile()
    if nc.getFile(path):
        pp=pprint.PrettyPrinter(indent=2)
        pp.pprint(nc.makeData(sys.argv[2]))
    else:
        print 'File not found'
    
