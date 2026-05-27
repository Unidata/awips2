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
#       GridData.py
#       GFS1-NHD:A7800.0000-SCRIPT;1.25
#
#    Status:
#       DELIVERED
#
#    History:
#
#       Revision AWIPS II                           NJENSEN
#          Updated to work with AWIPS II and directly retrieve GFE data
#
#       Revision 1.25 (DELIVERED)
#         Created:  29-OCT-2008 12:51:49      OBERFIEL
#           Non-operationally important weather now ignored, e.g. FR ==
#           frost
#
#       Revision 1.24 (DELIVERED)
#         Created:  18-APR-2008 14:53:34      OBERFIEL
#           Updated header in TAF viewer
#
#       Revision 1.23 (DELIVERED)
#         Created:  13-JUN-2006 14:33:00      BLI
#           Fixed for unable to handle missing cigHt
#
#       Revision 1.22 (DELIVERED)
#         Created:  12-JUN-2006 16:00:51      BLI
#           Fixed for missing CigHt
#
#       Revision 1.21 (DELIVERED)
#         Created:  16-MAY-2006 12:06:55      BLI
#           multiplied cig with 100
#
#       Revision 1.20 (DELIVERED)
#         Created:  04-MAY-2006 09:54:40      BLI
#           Grids are now displaying 'CigHt' instead of 'CigCat'.
#
#       Revision 1.19 (DELIVERED)
#         Created:  29-JAN-2006 14:26:50      TROJAN
#           Fixed fog key in the symbol translation dictionary
#
#       Revision 1.18 (APPROVED)
#         Created:  29-JAN-2006 12:16:16      TROJAN
#           stdr 958
#
#       Revision 1.17 (DELIVERED)
#         Created:  06-JUL-2005 20:50:38      TROJAN
#           spr 6910
#
#       Revision 1.16 (DELIVERED)
#         Created:  07-MAY-2005 11:33:53      OBERFIEL
#           Added Item Header Block
#
#       Revision 1.15 (DELIVERED)
#         Created:  04-APR-2005 15:51:06      TROJAN
#           spr 6775
#
#       Revision 1.14 (APPROVED)
#         Created:  21-MAR-2005 15:32:31      TROJAN
#           spr 6733
#
#       Revision 1.13 (DELIVERED)
#         Created:  07-MAR-2005 22:43:02      TROJAN
#           spr 6710
#
#       Revision 1.12 (APPROVED)
#         Created:  04-MAR-2005 15:22:45      TROJAN
#           spr 6699
#
#       Revision 1.11 (APPROVED)
#         Created:  15-FEB-2005 18:12:21      TROJAN
#           spr 6561
#
#       Revision 1.10 (DELIVERED)
#         Created:  04-FEB-2005 19:12:38      BLI
#           Added variable wind
#
#       Revision 1.9 (APPROVED)
#         Created:  01-FEB-2005 20:30:15      BLI
#           Fixed to include intensity
#
#       Revision 1.8 (APPROVED)
#         Created:  01-FEB-2005 18:59:02      BLI
#           Fixed it to include prob30 group
#
#       Revision 1.7 (APPROVED)
#         Created:  01-FEB-2005 18:45:18      BLI
#           Make GridViewer call a new taf formater
#
#       Revision 1.6 (APPROVED)
#         Created:  24-JAN-2005 15:51:13      TROJAN
#           spr 6259
#
#       Revision 1.5 (APPROVED)
#         Created:  24-NOV-2004 19:15:00      OBERFIEL
#           Fixed logic problem while processing new format IFPS grid
#           files
#
#       Revision 1.4 (APPROVED)
#         Created:  21-OCT-2004 19:36:11      TROJAN
#           spr 6420
#
#       Revision 1.3 (APPROVED)
#         Created:  30-SEP-2004 20:22:10      TROJAN
#           stdr 873
#
#       Revision 1.2 (APPROVED)
#         Created:  19-AUG-2004 20:42:56      OBERFIEL
#           Code change
#
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:40:40      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#           Change Document:   GFS1-NHD_SPR_7383
#           Action Date:       06-NOV-2008 15:25:22
#           Relationship Type: In Response to
#           Status:           TEST
#           Title:             AvnFPS: Lack of customization in QC check
#
# <pre>
#
# Retrieves data at airport locations as points from GFE and builds a data structure
# from that data.  Partially ported from AWIPS 1 GridData.py.
#
##
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
#                                  Initial creation.
# Mar 07, 2013  1735     rferrel   Changes to obtain grid data for a list of
#                                  sites.
# Apr 23, 2014  3006     randerso  Fix Wx parsing, handling of missing pdcs
# Feb 23, 2018  7227     njensen   Request Vsby, PredHgt, CigHgt to get variable
#                                  visibility and ceiling in NDFD TAFs
# Mar 10, 2022  8808     randerso  Update ConfigParser to better work with
#                                  Java commons.configuration
#
##
# This is a base file that is not intended to be overridden.
##

import logging
import time

import Avn
import AvnConfigParser
import AvnLib
import AvnParser
import GfeValues
import JUtil
import PointDataView

# These variables are unchanged from AWIPS 1 AvnFPS
_Missing = ''

_WxCode = {'L': 'DZ',
           'R': 'RA',
           'RW': 'SHRA',
           'ZL': 'FZDZ',
           'ZR': 'FZRA',
           'IP': 'PL',
           'IPW': 'SHPL',
           'S': 'SN',
           'SP': 'SG',
           'SW': 'SHSN',
           'K': 'FU',
           'IC': 'IC',
           'BR': 'BR',
           'FG': 'FG',
           'H': 'HZ',
           'BS': 'BLSN',
           'BD': 'BLDU'
           }

_IntCode = {'--': '-', '-': '-', 'm': '', '+': '+'}

_Keys = ['Temp', 'DwptT', 'WDir', 'WSpd', 'WGust', 'Obvis', 'Vsby',
         'Sky', 'PrdHt', 'CigHt', 'Tstm', 'PTyp1', 'Ints1', 'Prob1', 'PTyp2',
         'Ints2', 'Prob2', 'PTyp3', 'Ints3', 'Prob3'
         ]

_NumHours = 36

# Parameters to request from GFE.  These were determined by the parameters
# requested in the AWIPS 1 AvnFPS IFPS2AvnFPS.py's _makeProduct().
Parameters = ['Sky', 'T', 'Td', 'Wind', 'WindGust', 'PoP', 'Wx',
              'Vsby', 'PredHgt', 'CigHgt'
              ]

# Translations of AvnFPS parameter names to GFE parameter names
Translate = {'Sky': 'Sky',
             'Temp': 'T',
             'DwptT': 'Td',
             'WDir': 'WindDir',
             'WSpd': 'WindSpd',
             'WGust': 'WindGust',
             'PoP1h': 'PoP',
             'Obvis': 'Wx',
             'PoP': 'PoP',
             'Tstm': 'Wx',
             'Tint': 'Wx',
             'PTyp1': 'Wx',
             'Prob1': 'Wx',
             'Ints1': 'Wx',
             'PTyp2': 'Wx',
             'Prob2': 'Wx',
             'Ints2': 'Wx',
             'PTyp3': 'Wx',
             'Prob3': 'Wx',
             'Ints3': 'Wx',
             'Vsby': 'Vsby',
             'PrdHt': 'PredHgt',
             'CigHt': 'CigHgt'
             }

_Logger = logging.getLogger(Avn.CATEGORY)

##############################################################################


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _wxcode(c):
    return _WxCode.get(c, _Missing)


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _intcode(c):
    return _IntCode.get(c, _Missing)


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _vis(c):
    try:
        v = int(c)
        if v < 999:
            return v / 100.0
    except ValueError:
        pass
    return 999


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _cldHgt(c):
    try:
        v = int(c)
        if v < 999:
            return AvnLib.fixCldBase(v)
    except ValueError:
        pass
    return 999


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _cig(c):
    try:
        if c != '999':
            v = int(c) // 100
        else:
            v = int(c)
        if v < 0:
            v = 250
        if v < 999:
            return AvnLib.fixCldBase(v)
    except ValueError:
        pass
    return 999


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _skycode(c):
    try:
        ic = int(c)
        if ic == 0:
            return 'SKC'
        elif ic < 3:
            return 'FEW'
        elif ic < 6:
            return 'SCT'
        elif ic < 10:
            return 'BKN'
        elif ic == 10:
            return 'OVC'
        else:
            return ''
    except ValueError:
        return ''


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _stripmsng(c):
    if c == '999':
        return _Missing
    else:
        return c


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _winddir(c):
    try:
        v = int(c)
        if 0 <= v <= 36:
            return 10 * v
    except ValueError:
        pass
    return None


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _windspd(c):
    try:
        v = int(c)
        if 0 <= v <= 200:
            return v
    except ValueError:
        pass
    return None


# This function is ported from AWIPS 1 AvnFPS to return the same
# data structure as AWIPS 1 AvnFPS returned.
def _getData(pdc, firstTime):
    organizedData = {}
    data = []
    if pdc is not None:
        for i in range(pdc.getCurrentSz()):
            jpdv = pdc.readRandom(i)
            jpdv.setContainer(pdc)
            pdv = PointDataView.PointDataView(jpdv)
            fcstHr = round(((pdv['time'] - firstTime) / 1000) / 3600)
            organizedData[fcstHr] = pdv
        for n in range(_NumHours):
            dd = {'time': 3600.0 * n + (firstTime // 1000)}
            dd = _createRecord(dd, organizedData[n])
            data.append(dd)
    else:
        return None
    return {'issuetime': (firstTime // 1000), 'record': data}


# This function is a port of AWIPS 1 AvnFPS' function _cvt(itime, d).
def _createRecord(dd, pdv):
    for k in Translate:
        try:
            arg = pdv[Translate[k]]
        except:
            v = '999'
        if k in ['Temp', 'DwptT']:
            v = int(GfeValues.scalarValue(arg))
        elif k == 'Obvis':
            v = _wxcode(GfeValues.obvisValue(arg))
        elif k == 'Sky':
            v = _skycode(GfeValues.skyValue(arg))
        elif k == 'WDir':
            v = _winddir(GfeValues.windDirValue(arg))
        elif k in ['WSpd', 'WGust']:
            if k == 'WSpd':
                v = GfeValues.windMagValue(arg)
            elif k == 'WGust':
                v = GfeValues.scalarValue(arg)
            v = _windspd(v)
        elif k == 'PrdHt':
            v = _cldHgt(GfeValues.scalarValue(arg))
        elif k == 'CigHt':
            v = _cig(GfeValues.scalarValue(arg))
        elif k == 'Vsby':
            v = _vis(GfeValues.vsbyValue(arg))
        elif k == 'Tstm':
            v = _stripmsng(GfeValues.wxTstm(arg))
        elif k == 'Tint':
            v = _stripmsng(GfeValues.wxTstmInt(arg))
        elif k[:4] == 'Prob':
            v = _stripmsng(GfeValues.wxValCov(arg, int(k[4])))
        elif k[:4] == 'PTyp':
            v = _wxcode(GfeValues.wxVal(arg, int(k[4])))
        elif k[:4] == 'Ints':
            v = _intcode(GfeValues.wxValInst(arg, int(k[4])))
        else:
            v = _stripmsng(arg)
        dd[k] = v

        for ptype, pints, pprob in [('PTyp1', 'Ints1', 'Prob1'),
                                   ('PTyp2', 'Ints2', 'Prob2'),
                                   ('PTyp3', 'Ints3', 'Prob3')]:
            try:
                if dd[ptype] == _Missing:
                    dd[pints] = dd[pprob] = _Missing
            except KeyError:
                pass

    return dd


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _setPop(config, prbStr, dt):
    if dt <= 9 * 3600:
        return config['before9hr'][prbStr]
    else:
        return config['after9hr'][prbStr]


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _makePeriod(prbConfig, rec, itime):
    grp = {'time': {'from': rec['time'], 'to': rec['time'] + 3600.0}}
    dd = rec.get('WDir')
    ff = rec.get('WSpd')
    if not None in (dd, ff):
        gg = rec.get('WGust')
        if gg is None or gg <= ff:
            gg = None
        if gg is not None and gg > ff:
            grp['wind'] = {'dd': dd, 'ff': ff, 'gg': gg,
                'str': '%03d%02dG%02dKT' % (dd, ff, gg)}
        else:
            grp['wind'] = {'dd': dd, 'ff': ff, 'str': '%03d%02dKT' % (dd, ff)}
    pot = 0
    if 'Tstm' in rec and rec['Tstm']:
        pot = _setPop(prbConfig, rec['Tstm'], rec['time'] - itime)
    max_pop = 0
    p_str = ""
    for i in range(3):
        pstr = 'Prob%d' % i
        istr = 'Ints%d' % i
        tstr = 'PTyp%d' % i
        if pstr in rec and istr in rec and tstr in rec:
            if rec[pstr]:
                pop = _setPop(prbConfig, rec[pstr], rec['time'] - itime)
                if pop > max_pop:
                    max_pop = pop
                    p_str = rec[istr] + rec[tstr]

    grp['pcp'] = {'str': p_str, 'pop': max_pop, 'pot': pot}

    h = rec.get('PrdHt')
    cig = rec.get('CigHt')
    if cig:
        cig = cig * 100
    else:
        cig = 3000
    if cig < 0:
        cig = Avn.UNLIMITED
    if h is None or h == 999:
        h = 300
    else:
        h = AvnLib.fixCldBase(h)
    s = rec.get('Sky', None)
    if s:
        cld = s
        cover = {'SKC': 0, 'FEW': 1, 'SCT': 2, 'BKN': 3, 'OVC': 4}
        if cld == 'SKC':
            grp['sky'] = {'str': 'SKC', 'cig': Avn.UNLIMITED,
                          'cover': cover[cld]}
        else:
            grp['sky'] = {'str': '%s%03d' % (cld, h), 'cig': cig,
                          'cover': cover[cld]}
    obv = rec.get('Obvis')
    v = rec.get('Vsby')
    if v is None:
        if obv:
            grp['vsby'] = {'str': '6SM', 'value': 6.0}
        else:
            grp['vsby'] = {'str': 'P6SM', 'value': 99.0}
    elif v >= 6.5:
        grp['vsby'] = {'str': 'P6SM', 'value': 99.0}
    else:
        grp['vsby'] = AvnLib.fixTafVsby(v)
        if not obv and grp['pcp']['str'] == '':
            if v <= 0.6:
                obv = 'FG'
            else:
                obv = 'BR'
            grp['obv'] = {'str': obv}
    return grp


# This function is unchanged from the AWIPS 1 AvnFPS function.
def _readPrbConf():
    conf = AvnConfigParser.AvnConfigParser()
    conf.read(Avn.getTafPath('XXXX', 'grid_prob.cfg'))
    prb_conf = {'before9hr': {}, 'after9hr': {}}
    wxkeys = ['S', 'IS', 'WS', 'SC', 'NM', 'O', 'C', 'D', 'WP', 'L']
    for wx in wxkeys:
        prb_conf['before9hr'][wx] = conf.getint('before9hr', wx)
        prb_conf['after9hr'][wx] = conf.getint('after9hr', wx)
    return prb_conf


# This function is ported from AWIPS 1 AvnFPS to return the same
# data structure as AWIPS 1 AvnFPS returned.
def makeData(siteID, timeSeconds):
    data = retrieveData(siteID, timeSeconds)
    return formatData(siteID, timeSeconds, data)


# This function was extracted from AWIPS 1 AvnFPS' makeData(ident, text).
def formatData(siteID, timeSeconds, data):
    if data is None or data['issuetime'] < time.time() - 86400:
        msg = 'Grid data is not available'
        _Logger.info(msg)
        raise Avn.AvnError(msg)
    d = __formatData(data, siteID)
    return d


# This function was extracted from AWIPS 1 AvnFPS' makeData(ident, text).
def __formatData(data, siteID):
    if data is not None:
        prbConfig = _readPrbConf()
        itime = data['issuetime']
        d = {'itime': {'value': itime, 'str': time.strftime('%d%H%MZ',
             time.gmtime(itime))}, 'ident': {'str': siteID}}
        d['group'] = [_makePeriod(prbConfig, data['record'][n], itime)
                      for n in range(_NumHours)
                      ]
    return d


# This function is unchanged from the AWIPS 1 AvnFPS function.
def makeTable(siteID, timeSeconds):

    def _tostr(x):
        s = str(x)
        if s in ['-1', '999']:
            s = ''
        return s

    data = retrieveData(siteID, timeSeconds)
    if data is None or data.get('issuetime', 0.0) < time.time() - 86400:
        msg = 'Grid data for %s is not available', siteID
        _Logger.info(msg)
        raise Avn.AvnError(msg)
    rpt = {'header': '', 'hours': [], 'element': [], 'data': []}
    itime = data['issuetime']
    rpt = ['%s NDFD Guidance  %s' % (siteID,
        time.strftime('%x  %H%M UTC', time.gmtime(itime)))]
    rpt.append('hour    ' + '   '.join([time.strftime('%H',
        time.gmtime(itime + 3600.0 * n)) for n in range(_NumHours)]))
    try:
        for k in _Keys:
            if not k in data['record'][0]:
                continue
            tok = [_tostr(data['record'][n][k]) for n in range(_NumHours)]
            rpt.append('%-5s' % k + '%+5s' * _NumHours % tuple(tok))
        return Avn.Bunch(data=__formatData(data, siteID), rpt=rpt)
    except KeyError:
        _Logger.info('Grid data for %s is not available', siteID)
        msg = 'Grid data for %s is not available', siteID
        raise Avn.AvnError(msg)


def retrieveData(siteID, timeSeconds, parameters=Parameters):
    results = _retrieveMapData([siteID], timeSeconds, parameters)
    return results[siteID]


def retrieveMapData(siteIDs, timeSeconds, parameters=Parameters):
    r = _retrieveMapData(siteIDs, timeSeconds, parameters=Parameters)
    results = {}
    for siteID in siteIDs:
        results[siteID] = JUtil.javaPickle(r[siteID])

    return JUtil.pyDictToJavaMap(results)


# New function in AWIPS 2.  Determine the latitude and longitude of each site
# in siteIDs and then makes a request to send to EDEX for GFE data.
def _retrieveMapData(siteIDs, timeSeconds, parameters=Parameters):
    from com.raytheon.uf.common.dataplugin.gfe.request import GetPointDataRequest
    from org.locationtech.jts.geom import Coordinate
    from com.raytheon.viz.aviation.guidance import GuidanceUtil
    from com.raytheon.uf.viz.core.localization import LocalizationManager
    gfeSiteId = LocalizationManager.getInstance().getCurrentSite()
    task = GetPointDataRequest()
    task.setSiteID(gfeSiteId)
    db = gfeSiteId + '_GRID__Official_00000000_0000'
    task.setDatabaseID(db)
    for siteID in siteIDs:
        config = AvnParser.getTafSiteCfg(siteID)
        lat = config['geography']['lat']
        lon = config['geography']['lon']
        c = Coordinate(float(lon), float(lat))
        task.addCoordinate(c)
    task.setNumberHours(_NumHours)
    task.setStartTime(int(timeSeconds * 1000))
    for p in parameters:
        task.addParameter(p)
    pdcs = GuidanceUtil.getGFEPointsData(task)
    results = {}
    if pdcs is None:
        for siteId in siteIDs:
            _Logger.info('Data not available for %s', siteID)
            results[siteID] = None
        return results

    for i, siteID in enumerate(siteIDs):
        data = None
        if i < pdcs.getSize():
            pdc = pdcs.getContainer(i)
            data = _getData(pdc, timeSeconds * 1000)

        if data is None:
            _Logger.info('Data not available for %s', siteID)
        results[siteID] = data
    return results

###############################################################################
