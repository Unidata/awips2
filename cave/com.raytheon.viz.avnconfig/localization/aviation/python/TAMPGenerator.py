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
#       TAMPGenerator.py
#       GFS1-NHD:A10032.0000-SCRIPT;10
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 10 (DELIVERED)
#         Created:  07-AUG-2009 10:22:17      OBERFIEL
#           Use of CB is now constrained LTE a configurable height used
#           by TAFGen
#           when creating guidance TAFs.
#       
#       Revision 9 (DELIVERED)
#         Created:  17-JUL-2009 16:40:07      OBERFIEL
#           Viewers now use a resource to determine the Routine button
#           setting.
#       
#       Revision 8 (REVIEW)
#         Created:  05-MAY-2009 14:54:52      OBERFIEL
#           Additional filter for cloud layers
#       
#       Revision 7 (DELIVERED)
#         Created:  01-MAY-2009 13:57:47      OBERFIEL
#           Added exception handling when VRB is encountered during
#           wind averaging.
#       
#       Revision 6 (DELIVERED)
#         Created:  01-AUG-2008 15:44:45      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 5 (DELIVERED)
#         Created:  19-JUN-2008 14:20:42      OBERFIEL
#           Allowed variable length of TAF -- not just 24h.
#       
#       Revision 4 (DELIVERED)
#         Created:  18-APR-2008 14:19:09      OBERFIEL
#           Numerous enhancements more robust error checking added.
#       
#       Revision 3 (DELIVERED)
#         Created:  18-MAR-2008 14:40:28      OBERFIEL
#           Fixed numerous formatting errors when TAFs are combined.
#       
#       Revision 2 (DELIVERED)
#         Created:  14-MAR-2008 14:56:22      OBERFIEL
#           Fixed some wind and cloud bugs
#       
#       Revision 1 (DELIVERED)
#         Created:  20-NOV-2007 16:36:36      OBERFIEL
#           New module to create TAF combined with GFSLAMP guidance.
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7417
#       	Action Date:       06-OCT-2009 09:42:01
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: TUG code does not handle transition from warm to cold seasons
#       
#
import copy, math, re, time
from itertools import groupby
import Avn, AvnLib, TafGen
#
# LAMP categories for ceiling and visibility
#
_LAMPCeilings=[0,200,500,1000,2000,3100,6600,12100]
_LAMPVisibilities=[0.,.5,1.0,2.0,3.0,5.1,6.1]
#
# Upper and lower limits within LAMP categories, allowed for insertion into TAF
#
_LAMPNewCeilings=[(),(0,1),(2,4),(5,9),(10,19),(20,30),(35,60),(70,120),(250,250)]
_LAMPNewVisibilities=[(),(0.1,0.25),(0.25,0.75),(1.0,1.5),(2.0,2.0),(3.0,5.0),(6.0,6.0),(12.0,12.0)]

re_TS = re.compile('TS\s?')
#
#  Custom exception class
class NoEntries(Exception):
    "Used when empty list is found"

class OcnlTimeLimit(Exception):
    "Used when conditional group reaches its maximum hours in length"

def _findBestCategory(probabilities,thresholds):
    cat=len(probabilities)
    for cat,probability,threshold in enumerate(probabilities,thresholds):
        if probability >= threshold:
            break 
    return cat

def _findCategory(probability,thresholds):
    cat=len(thresholds)
    for cat,threshold in enumerate(thresholds):
        if probability >= threshold:
            break 
    return cat
    
def _inCategory(cat,thresholds,probabilities,delta):
    try:
        t=thresholds[cat]
    except IndexError:
        t=thresholds[-1]
        
    return abs(t-probabilities[cat]) < t*delta

def _getCldHt(cldlyr):
    try:
        if cldlyr.endswith('CB'):
            return int(cldlyr[-5:-2])*100
        else:
            return int(cldlyr[-3:])*100
    except ValueError:
        return 25000

def _nearesthr(t=0):
    """Top of the hour nearest within 30 minutes"""
    hr=3600
    d=t%hr
    if d<1800:
        hr=0
    return t+hr-d

def _checkOcnlGrp(TAFLines):
    """Check the TEMPO/PROB30 group for elimination"""
    #
    # Check for last two strings in list
    try:
        if (TAFLines[-1].startswith('TEMPO') or TAFLines[-1].startswith('PROB30')):
            pass
        else:
            return
    except IndexError:
        return
    #
    # Parse the last two lines into seperate tokens
    ocnltokens = TAFLines[-1].split(' ')[2:]
    prvlngtokens = TAFLines[-2].split(' ')[1:]
    #
    # If differences are found return, otherwise remove occasional group.
    for token in ocnltokens:
        if token not in prvlngtokens:
            break
    else:
        TAFLines.pop(-1)
        
    return
        
def _prjAverage(prjs,tafDuration=24):
    """Average elements together"""
    #
    # Initialization
    TAFStrings = []
    ocnlgrp = []
    spos = n = 0
    #
    for n,prj in enumerate(prjs):
        if prj.tocnl and prj.tocnl['type']:
            #
            # Make sure its of the same type and continuous in time
            try:
                if ocnlgrp[2] != prj.tocnl['type'] or \
                       ocnlgrp[0] + ocnlgrp[1]  != n:
                    raise IndexError
                
                if ocnlgrp[1:] == [4,'TEMPO'] or ocnlgrp[1:] == [6,'PROB']:
                    raise OcnlTimeLimit
                        
                ocnlgrp[1] += 1
                #
                # If occasional group is discontinuous in time or exceeds four
                # hours in length
                #
            except (IndexError, OcnlTimeLimit):
                if ocnlgrp:
                    # start position of occasional group and its duration
                    ospos,duration = ocnlgrp[:2]
                    
                    TAFStrings.append(' '.join(_buildString([proj.tprev for proj in prjs[spos:n]],
                                                            prjs[spos].lampdata,tafDuration)))
                    TAFStrings.append(' '.join(_buildString([prjs[ospos+x].tocnl
                                                             for x in xrange(duration)])))
                    _checkOcnlGrp(TAFStrings)
                    spos = n
                    
                ocnlgrp = [n,1,prj.tocnl['type']]
    #
    prjs[spos].tprev['type']='FM'
    if spos == n:
        _tafstring = ' '.join(_buildString([prjs[spos].tprev],prjs[spos].lampdata,tafDuration))
    else:
        _tafstring = ' '.join(_buildString([prj.tprev for prj in prjs[spos:n]],
                                           prjs[spos].lampdata,tafDuration))
    TAFStrings.append(_tafstring)
    
    try:
        ospos,duration = ocnlgrp[:2]
        _tafstring = ' '.join(_buildString([prjs[ospos+n].tocnl for n in xrange(duration)]))
        TAFStrings.append(_tafstring)
        _checkOcnlGrp(TAFStrings)
        
    except (ValueError,TypeError):
        pass
    
    return TAFStrings

def _avgNSW(alist):
    """Just return NSW if present"""
    if alist:
        return 'NSW'    
    raise NoEntries

def _avgLLWS(alist):
    """Just return the first one, if present"""
    if alist:
        return alist[0]['str']
    raise NoEntries

def _mostFrequent(alist):
    """Returns most frequent string found in list"""

    strings = [element.get('str','') for element in alist]
    return _freqList(strings)[-1]
                
def _freqList(alist):
    """Count unique items in list"""
    if alist:
        freqs = [(len(list(g)),k) for k, g in groupby(sorted(alist))]
        return [b for a, b in sorted(freqs)]
    
    raise NoEntries

def _avgWind(winds):
    """Average winds"""

    n = len(winds)
    if n == 0:
        raise NoEntries
    elif n == 1:
        return winds[0]['str']
    
    wspd = [x['ff'] for x in winds]
    wdir = [x['dd'] for x in winds]
    gsts = []
    for x in winds:
        try:
            gsts.append(x['gg'])
        except KeyError:
            continue

    ff = int(float(sum(wspd))/len(wspd)+0.5)
    try:
        gg = sum(gsts)/len(gsts)
    except ZeroDivisionError:
        gg = 0
        pass

    dd = 'VRB'
    if ff > 3:
        try:
            u = sum([math.cos(math.radians(270 - dir))*spd \
                     for dir,spd in zip(wdir,wspd)])
            v = sum([math.sin(math.radians(270 - dir))*spd \
                     for dir,spd in zip(wdir,wspd)])
            dd = math.degrees(math.atan2(v,u))
            if dd >= -90:
                dd = 270-dd
            else:
                dd = -90-dd
            
            dd = '%03d' % (10*((dd+5)//10))
            if dd == '000':
                dd = '360'
                
        except TypeError:
            pass
            
    elif ff < 3:
        dd = '000'
        ff = 0
        
    if gg - ff > 5:
        return '%s%02dG%02dKT' % (dd,ff,gg)
    else:
        return '%s%02dKT' % (dd,ff)

def _avgSky(clouds):
    """Returns most frequently occurring layers"""
    #
    cldlayers=[]
    newstring=[]
    #
    allrpts=Avn.flatten([y.split() for y in [x['str'] for x in clouds]])
    toofew = len(allrpts)/4
    cldlayers=[rpt for num, rpt in [(len(list(g)),k) for k, g in groupby(sorted(allrpts))] if num > toofew]
    if len(cldlayers) == 0:
        mostfreqlyrs=_freqList(allrpts)
    else:
        mostfreqlyrs=_freqList(cldlayers)
        
    if mostfreqlyrs[-1] == 'SKC':
        return 'SKC'
    #
    # Remove all occurrences of SKC
    while True:
        try:
            ignored = mostfreqlyrs.pop(mostfreqlyrs.index('SKC'))
        except:
            break
    #
    # Remove layers at the same height
    uniqclddict=dict([(_getCldHt(x),x) for x in mostfreqlyrs])
    #
    # Prepare to order the most frequent layers
    lyrs = dict([(x,0) for x in ['OVC','BKN','SCT','FEW','VV']])
    sortedlayers=sorted([_getCldHt(x) for x in mostfreqlyrs[-4:]])
    lastheight = 0
    
    for height in sortedlayers:
        try:
            strng = uniqclddict[height]
        except KeyError:
            continue
        
        if height <= lastheight:
            continue
        
        lastheight = height
        coverage=strng[:3]            
        if strng.startswith('V'):
            coverage='VV'
        #
        # Don't allow VV, SCT or FEW if a BKN is present
        if lyrs['BKN']:
            if coverage in ['FEW','SCT','VV']:
                continue
        #
        # Don't allow FEW or VV above a SCT layer
        if lyrs['SCT']:
            if coverage in ['FEW','VV']:
                continue
        try:
            lyrs[coverage] += 1
            newstring.append(strng)
        except KeyError:
            continue
        #
        # First overcast or VV stops the loop.
        if lyrs['OVC'] or lyrs['VV']:
            break
        #
        # Or two BKN layers
        if lyrs['BKN'] == 2:
            break
        #
        # Three cloud layers results in breakout
        if lyrs['FEW'] + lyrs['SCT'] + lyrs['BKN'] > 2:
            break
        
    return ' '.join(newstring)
        
def _buildString(elements,lampdata=None,tafDuration=24):
    """Examine each element in TAF and come up with a string"""
    #
    # The presence of lampdata indicates a FM group, bleh.
    rec = {}
    rec['time'] = { 'from':elements[0]['time']['from'] }
    rec['type'] = elements[0]['type']
    
    if not lampdata:
        rec['time']['to']=elements[-1]['time']['to']
    
    for _function,_element in [(_avgWind,'wind'),(_mostFrequent,'vsby'),
                               (_mostFrequent,'pcp'),(_mostFrequent,'obv'),
                               (_avgSky,'sky'),(_avgNSW,'nsw'),
                               (_mostFrequent,'vcnty'),(_avgLLWS,'llws')]:
        try:
            rec[_element]={'str':_function([v[_element] for v in elements if v.has_key(_element)])}
        except NoEntries:
            pass
    #
    # Sanity checks
    try:
        if rec['vsby']['str'] == 'P6SM':
            del rec['obv']
    except KeyError:
        pass
    #
    # 
    try:
        if rec['nsw']:
            if rec['type'] in ['FM','PROB']:
                del rec['nsw']
            elif rec['pcp']:
                del rec['nsw']
    except KeyError:
        pass
    
    try:
        if re_TS.match(rec['pcp']['str']):
            for lyr in rec['sky']['str'].split():
                if lyr.endswith('CB'):
                    break
            else:
                rec['sky']['str'] += 'CB'
    except KeyError:
        pass

    if lampdata and lampdata.has_key('ident'):
        return AvnLib.formatRecForMAIN(rec,lampdata['ident'],lampdata['amd'],
                                       tafDuration=tafDuration)
    else:
        if rec['type'] == 'FM':
            return AvnLib.formatRecForFM(rec)
        elif len(rec.keys()) > 2:
            return AvnLib.formatRecForOCNL(rec)
        
def _addPrj(grpnum,pos,currentFltCat,nextFltCat,prevFltCat):
    """Almost always insures that that two or more of the same flight category exists"""
    #
    # Beginning hour of TAF is always added, regardless of any impending flight category
    # change
    #
    if grpnum == 0 and pos == 0:
        return True
    elif pos:
        return currentFltCat in [nextFltCat,prevFltCat]
    else:
        return currentFltCat == nextFltCat
            
def _summarizeTAFPrjs(TAFPrjs,TAFData,tafDuration=24):
    """Attempt to group based on previous TAF"""
    #
    # Initialization
    TAFText = []
    #
    # Save Station Identifier and whether its an amendment
    try:
        ident=TAFPrjs[0].lampdata['ident']
        amd=TAFPrjs[0].lampdata['amd']
    except (KeyError, IndexError):
        return
    #
    # Start with breakpoints in the official TAF
    for grpnum,grp in enumerate(TAFData):
        try:
            shr = grp['prev']['time']['from']
            ehr = grp['prev']['time']['to']
        except KeyError:
            continue
        #
        # Identify those projections and the flight category
        # they correspond to.
        #
        prjs = [(x.flightCategory(),x) for x in TAFPrjs if shr <= x.vtime < ehr]
        #
        for n,cat in enumerate(prjs):
            if n == 0:
                numPrjs = len(prjs)-1
                prjs2avg = []
                
            crntPrj = cat[0]
            nextPrj = prjs[min(n+1,numPrjs)][0]
            prevPrj = prjs[max(0,n-1)][0]
            
            if _addPrj(grpnum,n,crntPrj,nextPrj,prevPrj):
                #
                prjs2avg.append(cat[1])
                #
                # If there's a change in flight category ahead,
                # average the projections gathered so far
                #
                if crntPrj != nextPrj:
                    if TAFText == []:
                        prjs2avg[0].lampdata['ident']=ident
                        prjs2avg[0].lampdata['amd']=amd
                        
                    TAFText.extend(_prjAverage(prjs2avg,tafDuration))
                    prjs2avg = []
        #
        if prjs and prjs2avg:            
            if TAFText == []:
                prjs2avg[0].lampdata['ident']=ident
                prjs2avg[0].lampdata['amd']=amd
                
            TAFText.extend(_prjAverage(prjs2avg,tafDuration))
            
    return TAFText

class TUGPrj:
    def __init__(self,**kwds):
        self.__dict__.update(kwds)
        try:
            self.tprev['time']['from']=self.vtime
            self.tprev['time']['to']=self.vtime+3600.0
        except KeyError:
            pass
        
        try:
            self.tocnl['time']['from']=self.vtime
            self.tocnl['time']['to']=self.vtime+3600.0
        except KeyError:
            pass
        
        self.wet = self._isWet()
        self.pcpn_changed = self.changed = False

    def checkSky(self,tafGrpInstructions,dthresholds={'up':.1,'down':.1},
                 wthresholds={'up':.1,'down':.1}):
        """Make changes to ceiling when guidance strongly differs"""
        
        maxcbhgt=tafGrpInstructions.get('cbhight',50)
        #
        # For prevailing and occasional groups, adjust if necessary
        try:
            for group in [self.tprev,self.tocnl]:
                if self._isGroupWet(group):
                    self._checkCeiling(group['sky'],wthresholds,maxcbhgt,True)
                else:
                    self._checkCeiling(group['sky'],dthresholds,maxcbhgt,False)
            #
            # Determine if the sky condition is duplicated.
            new_OcnlSky = []
            for layer in self.tocnl['sky']['str'].split():
                if not (layer.endswith('CB') or layer.startswith('VV')) and \
                       layer in self.tprev['sky']['str']:
                    continue
                
                new_OcnlSky.append(layer)

            if len(new_OcnlSky) == 0:
                del self.tocnl['sky']
            
        except KeyError:
            pass

    def _checkCeiling(self,taf,deltas,maxcbhgt,wet=False):
        """Adjust ceilings if necessary"""
        #
        if wet:
            lamp=self.lampdata['csky']
            lampBestCat=self.lampdata['ccig_bestCat']
            probabilities=self.lampdata['ccprob']
            thresholds=self.ccigthr
        else:
            lamp=self.lampdata['sky']
            lampBestCat=self.lampdata['cig_bestCat']
            probabilities=self.lampdata['cprob']
            thresholds=self.cigthr
            
        tcat = Avn.category(taf['cig'],_LAMPCeilings)
        if tcat == lampBestCat:
            return
        #
        # If LAMP and TAF both do not have a ceiling, return early
        if lamp['cig'] == taf['cig'] == 99999:
            return
        #
        # Adjust thresholds, determine if we can hit taf's category.
        if tcat > lampBestCat and _inCategory(lampBestCat,thresholds,probabilities,deltas['up']):
            return
        if tcat < lampBestCat and _inCategory(tcat,thresholds,probabilities,deltas['down']):
            return
        #
        # Otherwise, the guidance 'strongly' disagrees with TAF
        self.cig_changed = self.changed = True
        newsky = []
        newceiling = []
        #
        # Preserve CB in sky condition, cb_skyamt serves as a flag as well
        cb_skyamt = None
        for lyr in taf['str'].split():
            if lyr.endswith('CB'):
                cb_skyamt = lyr[:3]
        #
        # Find layers at or below LAMP ceiling category
        if lampBestCat < tcat:
            # They have to be FEW or SCT layers
            for layer in [x for x in taf['str'].split() if Avn.category(_getCldHt(x),_LAMPCeilings) <= lampBestCat]:
                # SCT layers that match LAMP category, change to BKN
                if layer[:3] == 'SCT' and Avn.category(_getCldHt(layer),_LAMPCeilings) == lampBestCat:
                    newceiling.append('BKN%03d' % int(_getCldHt(layer)*0.01))
                else:
                    newsky.append(layer)
            #
            # If no ceiling found in LAMP category add one
            if not newceiling:
                maxCeiling = _LAMPNewCeilings[lampBestCat][1]
                if lamp['str'] != 'SKC':
                    newceiling.append(lamp['str'][:3]+'%03d'%maxCeiling)
                else:
                    newceiling.append(lamp['str'])
                    cb_skyamt = None
                    newsky = []
            #
            newsky.extend(newceiling)
        else:
            # Remove ceilings below lamp category, leave FEW and SCT alone
            newsky = [x for x in taf['str'].split() 
                      if x[:3] in ['FEW','SCT'] and \
                      Avn.category(_getCldHt(x),_LAMPCeilings) < lampBestCat]
            newceiling = [x for x in taf['str'].split()
                          if Avn.category(_getCldHt(x),_LAMPCeilings) == lampBestCat]
            #
            if not newceiling:
                if lamp['str']=='SKC':
                    newsky=['SKC']
                else:
                    newsky.extend([lamp['str'][:3]+'%03d'%(_LAMPNewCeilings[lampBestCat][0])])
            else:
                newsky.extend(newceiling)
                
        if cb_skyamt:
            #
            # If there's already a CB present, break
            for i, lyr in enumerate(newsky):
                if lyr.endswith('CB'):
                    break
            else:
                #
                # If there's a cloud amount that matches the original TAF CB amount and its
                # below a configurable max CB height
                #
                for i, lyr in enumerate(newsky):
                    try:
                        if cb_skyamt == lyr[:3] and int(lyr[3:6]) <= maxcbhgt:
                            newsky[i]+='CB'
                            break
                    except (ValueError,IndexError):
                        pass
                else:
                    #
                    # Otherwise, use the first acceptable layer found below a configurable
                    # max CB height
                    #
                    for i, lyr in enumerate(newsky):
                        try:
                            if lyr[:3] in ['SCT','BKN','OVC'] and int(lyr[3:6]) <= maxcbhgt:
                                newsky[i]+='CB'
                                break
                        except (ValueError,IndexError):
                            pass
                        
        taf['str'],taf['cig'] = ' '.join(newsky), _getCldHt(newsky[-1])
        
    def checkVsby(self,dthresholds={'up':.1,'down':.1},wthresholds={'up':.1,'down':.1}):
        """Make changes to visibility when guidance disagrees"""
        # For prevailing and occasional groups, adjust if necessary
        try:
            for group in [self.tprev,self.tocnl]:
                if self._isGroupWet(group):
                    self._checkVisibility(group,wthresholds,True)
                else:
                    self._checkVisibility(group,dthresholds,False)

        except KeyError:
            pass
            
    def _adjustSNDZIntensity(self,pcpn_str,intensity=None):
        """Based on visibility, the intensity of snow or drizzle may need to be adjusted"""
        
        newPcpnStr = []
        for pcp in pcpn_str.split():
            result = re.compile('(?P<Pint>[+-])?[A-Z]{,6}(DZ|SN)').match(pcp)
            #
            # If SN and/or drizzle present
            if result:
                oldintensity = None
                try:
                    oldintensity = result.group('Pint')
                except AttributeError:
                    pass
            
                if intensity == oldintensity:
                    return pcpn_str
                elif intensity and not oldintensity:
                    newPcpnStr.append('%c%s' % (intensity,pcp))
                elif oldintensity and not intensity:
                    newPcpnStr.append(pcp[1:])
                else:
                    newPcpnStr.append('%c%s' % (intensity,pcp[1:]))                    
            else:
                newPcpnStr.append(pcp)

        return ' '.join(newPcpnStr)

    def _checkVisibility(self,taf,deltas,wet=False):
        """Adjust ceilings if necessary"""
        #
        if wet:
            lamp=self.lampdata['cvsby']
            lampBestCat=self.lampdata['cvis_bestCat']
            probabilities=self.lampdata['cvprob']
            thresholds=self.cvisthr
        else:
            lamp=self.lampdata['vsby']
            lampBestCat=self.lampdata['vis_bestCat']
            probabilities=self.lampdata['vprob']
            thresholds=self.visthr

        tcat = Avn.category(taf['vsby']['value'],_LAMPVisibilities)
        if tcat == lampBestCat:
            try:
                if taf['obv']['str'] in ['BR','FG']:
                    if taf['vsby']['value'] <= 0.5:
                        taf['obv']['str'] = 'FG'
                    else:
                        taf['obv']['str'] = 'BR'
            except KeyError:
                pass
            
            return
        #
        # Determine if we can hit taf's category by seeing how much its off
        if tcat > lampBestCat and _inCategory(lampBestCat,thresholds,probabilities,deltas['up']):
            return
        if tcat < lampBestCat and _inCategory(tcat,thresholds,probabilities,deltas['down']):
            return
        #
        # Check precip/obvis in the VFR/VLIFR cases, all other cases, TAF obvis will be accepted.
        if lampBestCat < tcat:
            
            taf['vsby'] = AvnLib.fixTafVsby(_LAMPNewVisibilities[lampBestCat][1])
            #
            # If LAMP forecasting VLIFR and TAF obvis is BR, change that
            if lampBestCat == 1:
                try:
                    if taf['obv'] and taf['obv']['str'] == 'BR':
                        taf['obv']['str'] = 'FG'
                except KeyError:
                    pass
            #
            # Tedious for precipitation
            try:
                if lampBestCat == 1:
                    taf['pcp']['str'] = self._adjustSNDZIntensity(taf['pcp']['str'],'+')
                else:
                    taf['pcp']['str'] = self._adjustSNDZIntensity(taf['pcp']['str'],'-')
                
            except KeyError:
                pass

            if not taf.has_key('pcp') and not taf.has_key('obv'):
                taf['obv'] = copy.copy(self.lampdata['obv'])
                if taf['obv']['str'] == 'FG' and lampBestCat > 1:
                    taf['obv']['str'] = 'BR'
        else:
            #
            # If there's obstruction to vision or precipitation, and LAMP indicates VFR
            # better to accept forecaster's value in this case.
            #
            if lampBestCat > 5 and ('obv' in taf.keys() or self._isGroupWet(taf)):
                return
            #
            # Otherwise, adjust according.
            taf['vsby'] = AvnLib.fixTafVsby(_LAMPNewVisibilities[lampBestCat][0])
            #
            # Change occurrence of FG to BR
            try:
                if lampBestCat > 2 and taf['obv'] and taf['obv']['str'] == 'FG':
                    taf['obv']['str'] = 'BR'
            except KeyError:
                pass
            #
            # Tedious for precipitation
            try:
                if lampBestCat == 2:
                    taf['pcp']['str'] = self._adjustSNDZIntensity(taf['pcp']['str'],'+')
                else:
                    taf['pcp']['str'] = self._adjustSNDZIntensity(taf['pcp']['str'],'-')
            except KeyError:
                pass
            
            if lampBestCat < 7 and not taf.has_key('pcp') and not taf.has_key('obv'):
                taf['obv'] = copy.copy(self.lampdata['obv'])
                if taf['obv']['str'] == 'FG' and lampBestCat > 1:
                    taf['obv']['str'] = 'BR'
                    
    def checkWind(self):
        """Simply copies LAMP winds into TAF"""
        #
        # Provide LAMP winds aren't missing!
        if not self.lampdata['wind']['str'].startswith('?'):
            self.tprev['wind']=copy.copy(self.lampdata['wind'])
        
    def _genOcnlPcp(self,otype):
        """Add precipitation to occasional group"""
        if hasattr(self,'tocnl') and self.tocnl.has_key('pcp'):
            return
        
        if not hasattr(self,'tocnl'):
            self.tocnl = { 'time': { 'from':self.vtime,'to':self.vtime+3600.0 }}
        else:
            self.tocnl['time'] = { 'from':self.vtime,'to':self.vtime+3600.0 }
            
        self.tocnl['type']=otype
        self.tocnl['pcp'] = self.lampdata['pcp']
        self.tocnl['vsby'] = self.lampdata['cvsby']
        self.tocnl['sky'] = self.lampdata['csky']
        try:
            self.tocnl['obv'] = self.lampdata['obv']
        except KeyError:
            pass
        
    def _genPrevailingPcp(self):
        """Add precipitation to prevailing group"""
        self.tprev['pcp'] = self.lampdata['pcp']
        self.tprev['vsby'] = self.lampdata['cvsby']
        try:
            if not self.tprev.has_key('obv'):
                self.tprev['obv'] = self.lampdata['obv']
        except KeyError:
            pass
        
    def checkPrecip(self,bbound=-.1,tbound=.1):
        """Compare guidance and official TAF to see if they agree w.r.t precipitation"""
        #
        # Probability 'score' combines 6-h POP and relative probability over
        # climatology 0.17 ~= 1/6, bleh.
        #
        score = self.lampdata.get('pop6hr',0)*.17+self.lampdata['pcp']['pop']
        #
        # A dry TAF
        if not self.wet:
            # Look at the 'score' value to determine if precip is warranted
            if score <= 30.0:
                return
            elif 30 < score <= 50.0:
                if self.lampprj > 9:
                    self._genOcnlPcp('PROB')
            elif 50 < score <= 70.0:
                self._genOcnlPcp('TEMPO')            
            else:
                self._genPrevailingPcp()
            return
        #
        # TAF is wet, but LAMP indicates dry
        elif self.lampdata['pcp']['pcat'] == 0:
            #
            # if prevailing group of TAF is wet...
            if self._isGroupWet(self.tprev):
                #
                # Use the freezing precipitation that LAMP suggests
                if 'FZ' in self.lampdata['pcp']['str'] and \
                       not 'FZ' in self.tprev['pcp']['str']:
                    self.tprev['pcp']=self.lampdata['pcp']
                #
                # but if probablity is low, demote or remove
                if score < 40.0:
                    if self._tsCheck(self.tprev):
                        self.tprev['pcp']['str'] = 'TS'
                    else:
                        del self.tprev['pcp']
                #
                # Add the appropriate group
                if 30 > score >= 40.0:
                    self._genOcnlPcp('TEMPO')
                elif score <= 30.0 and self.lampprj > 9:
                    self._genOcnlPcp('PROB')
            #
            # If in TEMPO or PROB30 don't remove unless really low.
            else:
                if score <= 20.0:
                    #
                    # PROB30 is used only for precipiation and/or thunderstorms,
                    # so if its too low for PROB30, remove it entirely.
                    #
                    if self.tocnl['type'] == 'PROB':
                        self.tocnl = {}
                    else:
                        del self.tocnl['pcp']
        #
        # Both TAF and LAMP indicate precipitation
        elif not self._isGroupWet(self.tprev):
            #
            # Promote PROB30 precip group as appropriate
            if self.lampprj <= 9 and self.tocnl['type'] == 'PROB':
                if score <= 70:
                    self.tocnl['type'] = 'TEMPO'
                else:
                    self.tprev['pcp'] = copy.copy(self.tocnl['pcp'])
                    self.tocnl = {}
                    
                        
    def _tsCheck(self,g):
        """See if TS is present"""
        return 'pcp' in g and re_TS.match(g['pcp']['str'])

    def _rmvTS(self,g):
        """Remove TS from weather string"""
        new_wx=re_TS.sub('',g['str'])
        if len(new_wx) < 2:
            return ''
        return new_wx
        
    def checkTstms(self,bbound=-.1,tbound=.1):
        """Check for thunderstorms"""
        #
        if not 'tcat' in self.lampdata['pcp']:
            return
        #
        # If LAMP suggests no thunderstorms, remove them if pot is low enough            
        if self.lampdata['pcp']['tcat'] == 0:
            for group,threshold in [(self.tprev,0.9),(self.tocnl,0.5)]:
                try:
                    score = self.lampdata['pcp']['pot']/self.poptthr
                    if self._tsCheck(group) and score <= threshold:
                        new_wx = self._rmvTS(group)
                        if new_wx:
                            group['pcp']['str']=new_wx
                        else:
                            del group
                        
                        self.pcpn_changed=True
                except KeyError:
                    pass
        #
        # Otherwise add them if threshold high enough
        else:
            for group,threshold in [(self.tprev,2.0),(self.tocnl,1.25)]:
                try:
                    score = self.lampdata['pcp']['pot']/self.poptthr
                    if not self._tsCheck(group) and score >= threshold:
                        try:
                            plist = group['pcp'].get('str').split()
                            plist.insert(0,'TS')
                        except KeyError:
                            plist =['TS']
                            
                        self.pcpn_changed=True
                        group['pcp']['str'] = TafGen.fixPcp(plist)
                        break
                    
                except KeyError:
                    pass

    def _isGroupWet(self,g):
        try:
            return len(self._rmvTS(g['pcp'])) > 1
        except KeyError:
            return False
        
    def _isWet(self):
        return self._isGroupWet(self.tprev) or self._isGroupWet(self.tocnl)

    def checkSyntax(self):
        """Checks for inconsistency in forecast"""
        #
        # Check the occasional group for duplication in the prevailing
        items =self.tocnl.keys()

        for item in items:
            if item in ['time','type']: continue
            try:
                if self.tprev[item]['str'] != self.tocnl[item]['str']:
                    break
            except KeyError:
                break
        else:
            for item in items:
                if item in ['time','type']: continue
                del self.tocnl[item]
        
    def printOfficalTAFPrj(self,tafDuration):
        """Print hourly TAF groups"""
        taf=[]
        try:
            try:
                taf=AvnLib.formatRecForMAIN(self.tprev,self.lampdata['ident'],
                                            self.lampdata['amd'],
                                            tafDuration=tafDuration)
            except KeyError:
                taf=AvnLib.formatRecForFM(self.tprev)

            if len(self.tocnl.keys()) > 2: 
                taf.extend(AvnLib.formatRecForOCNL(self.tocnl))
                
        except KeyError:
            pass
        
        return ' '.join(taf)

    def flightCategory(self):
        return AvnLib.flightCategory(self.tprev)
        
def _rmvBestCategoryBounces(key,startIdx,LAMPData):
    index = 1
    for i in xrange(startIdx,len(LAMPData)):
        try:
            p1,p2,p3 = LAMPData[startIdx+index-1][key],LAMPData[startIdx+index][key], \
                       LAMPData[startIdx+index+1][key]
            if p1 == p3 and p1 != p2:
                LAMPData[startIdx+index][key] = p1            
            index=index+1
        except IndexError:
            pass
    
def TAMPGenerator(LAMP,TAFData,thresholdsDict,amdmt=' ',cvOnly=True,longFmt=True,
                  tafDuration=24):
    """Combine latest TAF with LAMP guidance"""
    LAF = None
    #
    # Find first LAMP projection for forecast
    LAMPData = LAMP.data['group']
    now = AvnLib.getValidTime('taf',amdmt)
    #
    startIdx=0
    for startIdx,prj in enumerate(LAMPData):
        if now <= prj['time']['from']:
            break
    else:
        raise Avn.AvnError('No guidance available')
    #
    if len(thresholdsDict) == 0:
        raise Avn.AvnError('No thresholds available')
    #
    # Create lists of proper thresholds and valid times for the LAMP data we're going to
    # use
    #
    thresholds = [thresholdsDict[x] for x in xrange(startIdx+1,len(thresholdsDict)+1)]
    ValidTimes = [prj['time']['from'] for prj in LAMPData[startIdx:]]
    #
    # Remove 1hr bounces
    index = 1
    for i in xrange(startIdx,len(LAMPData)):
        try:
            p1,p2,p3 = LAMPData[startIdx+index-1]['pcp']['pcat'],LAMPData[startIdx+index]['pcp']['pcat'], \
                       LAMPData[startIdx+index+1]['pcp']['pcat']
            if p1 == p3 and p1 != p2:
                LAMPData[startIdx+index]['pcp']['pcat'] = p1            
            index=index+1
        except IndexError:
            pass
    
    _rmvBestCategoryBounces('ccig_bestCat',startIdx,LAMPData)
    _rmvBestCategoryBounces('cvis_bestCat',startIdx,LAMPData)
    _rmvBestCategoryBounces('cig_bestCat',startIdx,LAMPData)
    _rmvBestCategoryBounces('vis_bestCat',startIdx,LAMPData)
    #
    # Generate LAMP TAF based on 'smoothed' data and append to
    # original TAF when creating the next regular issued TAF
    #
    if amdmt[0] == ' ':
        tafGen = TafGen.TafGen('gfslamp',LAMP.data,amdmt,now)
        tafCfgDict = tafGen.grpTaf.copy()
        LAF = tafGen.formNewDic(False)
        pos = len(TAFData)
        #
        # LAMP may not be able to add anything here.
        try:
            TAFData.extend([copy.deepcopy(group) for group in LAF \
                        if group['prev']['time']['to'] >= TAFData[-1]['prev']['time']['to']])
            if TAFData[pos]['prev']['time']['from'] < TAFData[pos-1]['prev']['time']['to']:
                TAFData[pos]['prev']['time']['from'] = TAFData[pos-1]['prev']['time']['to']
        except (KeyError, IndexError):
            pass
    else:
        tafCfgDict = TafGen.Config(LAMP.data['ident']['str'],'gfslamp').grpTaf().copy()
    #
    # Map the TAF to each LAMP forecast hour
    TAFIndex=[]
    o = len(TAFData)
    for i,grp in enumerate(TAFData):
        for vtime in ValidTimes:
            o = len(TAFData)           
            if grp.has_key('ocnl'):
               shr = _nearesthr(grp['ocnl']['time']['from']) 
               if shr <= vtime < grp['ocnl']['time']['to']:
                   o = i

            shr = _nearesthr(grp['prev']['time']['from']) 
            if shr <= vtime < grp['prev']['time']['to']:
                TAFIndex.append((i,o))

            if vtime >= grp['prev']['time']['to']:
                break
    #
    # Fill out the rest of sequence
    for x in xrange(len(ValidTimes)-len(TAFIndex)):
        TAFIndex.append((o,o))
    TAFData.append({'prev':{},'ocnl':{}})
    #
    # First projection object needs additional information to be formatted correctly.
    LAMPData[startIdx]['amd']=amdmt
    LAMPData[startIdx]['ident']=LAMP.data['ident']['str']
    #
    # Construct the objects
    TAFPrjs = [TUGPrj(lampprj=p,vtime=v,lampdata=l,tprev=tp,tocnl=to,visthr=th['vis'],cvisthr=th['cvis'],
                      cigthr=th['cig'],ccigthr=th['ccig'],popothr=th.get('popt',[0])[0],
                      poptthr=th.get('pott',[0])[0])
               for p,v,l,tp,to,th in zip(xrange(1,len(LAMPData[startIdx:])+1),ValidTimes,LAMPData[startIdx:],
                                      [copy.deepcopy(TAFData[n[0]]['prev']) for n in TAFIndex],
                                      [copy.deepcopy(TAFData[n[1]]['ocnl']) for n in TAFIndex],
                                      thresholds)]
    newTAF=[]
    for prj in TAFPrjs:
        if not cvOnly:
            prj.checkPrecip()
            prj.checkTstms()
            
        prj.checkSky(tafCfgDict)
        prj.checkVsby()
        prj.checkWind()
        prj.checkSyntax()
        
        if longFmt:
            newTAF.append(prj.printOfficalTAFPrj(tafDuration).rstrip())

    if not longFmt:
        newTAF = _summarizeTAFPrjs(TAFPrjs,TAFData,tafDuration)
        
    newTAF = AvnLib.indentTaf(newTAF)
    if amdmt.startswith('A'):
        newTAF.insert(0,'TAF AMD')
    else:
        newTAF.insert(0,'TAF')
        
    return newTAF

if __name__ == '__main__':
    import os,sys,pprint
    
    pp = pprint.PrettyPrinter(indent=1)

    #os.environ['PYRO_NS_DEFAULTGROUP']=':AvnFPSOB92'
    #os.environ['PYRO_NS_PORT']='9190'
    
    cdir = os.environ['PWD']
    sys.path.insert(1,os.path.join(os.path.split(cdir)[0],'sitepy'))
    os.chdir(os.path.split(cdir)[0])

    import DataRequestServ,AvnLib,TafDecoder
    
    DRC = DataRequestServ.Client()
    
    longFmt=(sys.argv[1]=='long')
    amend=sys.argv[2]
    cvOnly=False
    #
    # Get latest TAF and guidance
    for station in sys.argv[3:]:
        try:
            taf = DRC.getTafs(station,True,time.time()-22800,1)[0]                
            print '\n============\nOfficial TAF'
            print taf.text
        except IndexError:
            print 'Cannot retrieve TAF for %s' % station
            continue

        try:    
            lamp = DRC.getMosData(station,'gfslamp')
            tmtuple = time.gmtime(lamp.data['itime']['value'])
            thresholds = DRC.getProbs(station,[tmtuple[7],('%02d'%tmtuple[3])])
            if not thresholds:
                continue
            
            TAFList = TAMPGenerator(lamp,taf.dcd['group'],thresholds,amend,cvOnly,longFmt)
            print 'TAF updated with Guidance'
            print '\n'.join(TAFList)
            if len(TAFList):
                print '============\nDecoding Report'
                TafDecoder.main(['FTUS43 KAAA 000000' + amend]+TAFList)

        except (KeyError,TypeError,AttributeError):
            continue
        
        except Exception, e:
            raise

