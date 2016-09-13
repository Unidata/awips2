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
#       %PM%
#       %PID%
#
#    Status:
#       %PS%
#    
#    History:
#       %PL%
#
#    Change Document History:
#       %PIRC%
#
#
#    Date          Ticket#       Engineer       Description
#    ----------    ----------    -----------    --------------------------
#    10/28/2015    15464         zhao           Modified mkTempo & mkProb30 to handle case of "TS"+"SKC"
#
import sys,os,copy,cPickle,math,ConfigParser,time,logging
import AvnLib, AvnParser, Avn

_Logger = logging.getLogger(Avn.CATEGORY)

#moved here from GridData.py
def fixPcp(plist):
    if not plist or plist == [' ']:
        return ''
    #skip the first one if it is an empty string 
    if plist[0] == ' ':
       plist = plist[1:]

    # TS is always first, set in makeRec()
    if plist[0] == 'TS':
        plist = plist[1:]
        ts = 'TS'
    else:
        ts = None
    # push FZ to the top of the list
    for p in plist:
        if 'FZ' in p:
            fz = p
            plist.remove(p)
            break
    else:
        fz = ''
    if ts:
        if fz:
            pcp = 'TS %s' % fz
        elif plist:
            if plist[0][0] in ['+', '-']:
                i, p = plist[0][0], plist[0][1:]
            else:
                i, p = '', plist[0]
            # replace SH by TS
            if p[:2] == 'SH':
                pcp = '%sTS%s' % (i, p[2:])
            else:
                pcp = '%sTS%s' % (i, p)
            plist[0] = pcp
        else:
            return 'TS'
    elif fz:
        pcp = fz
    else:
        pcp = plist[0]
        plist.pop(0)
    for p in plist:
        if p[0] in ('+', '-'):
            p = p[1:]
        if pcp.find(p) == -1:
            pcp = pcp + p
    return pcp

class Config:
    def __init__(self,siteIdent,model):            
        self.grpTafFile = Avn.getTafPath(siteIdent, 'grp_taf.cfg')
        self.fltCatFile = Avn.getTafPath(siteIdent, 'flt_cat.cfg')
        self.model = model
 
    def grpTaf(self):
        config = ConfigParser.RawConfigParser()
        prdCfg={}
        try:
            config.read(self.grpTafFile)
            if config.has_section(self.model):
                prdCfg['prev_pop'] = config.getint(self.model,'prev_precip')
                prdCfg['prev_tstm'] = config.getint(self.model,'prev_tstm')
                prdCfg['tempo_tstm'] = config.getint(self.model,'tempo_tstm')
                prdCfg['tempo_pop'] = config.getint(self.model,'tempo_precip')
                prdCfg['prob30_tstm'] = config.getint(self.model,'prob30_tstm')
                prdCfg['prob30_pop'] = config.getint(self.model,'prob30_precip')
            else: #use mos configuration as a default
                prdCfg['prev_pop'] = config.getint('mos','prev_precip')
                prdCfg['prev_tstm'] = config.getint('mos','prev_tstm')
                prdCfg['tempo_tstm'] = config.getint('mos','tempo_tstm')
                prdCfg['tempo_pop'] = config.getint('mos','tempo_precip')
                prdCfg['prob30_tstm'] = config.getint('mos','prob30_tstm')
                prdCfg['prob30_pop'] = config.getint('mos','prob30_precip')
                
            prdCfg['prob30']=config.getboolean('prob30','value')
            prdCfg['short_dt']=config.getint('def_singleton','short_dt')
            prdCfg['long_dt']=config.getint('def_singleton','long_dt')
            prdCfg['low_p']=config.getint('def_singleton','low_p')
            prdCfg['vis_cat']={'dc':config.getint('vis_diff','dc_vis')}
            prdCfg['vis_cat']['low_c']=config.getint('vis_diff','low_c_vis')
            prdCfg['vis_cat']['high_c']=config.getint('vis_diff','high_c_vis')
            prdCfg['cig_cat']={'dc':config.getint('cig_diff','dc_cig')}
            prdCfg['cig_cat']['low_c']=config.getint('cig_diff','low_c_cig')
            prdCfg['cig_cat']['high_c']=config.getint('cig_diff','high_c_cig')
            prdCfg['cbhight']=config.getint('cb_hi','value')
            prdCfg['calmwd']=config.getint('wind','calm_wind')
            prdCfg['wdthrsh']=config.getint('wind','high_wind')
            prdCfg['wddt']=config.getint('wind','wind_chg')
            return prdCfg
        except:
            _Logger.exception('Cannot read taf config file,%s.',self.grpTafFile)

    def fltCat(self):
        cat_cfg=ConfigParser.ConfigParser()
        vc,cc=[],[]
        try:
            cat_cfg.read(self.fltCatFile)
            vc=[float(v) for v in cat_cfg.get('vis','thresholds').split(',')]
            cc=[int(v) for v in cat_cfg.get('cig','thresholds').split(',')]
            return {'vis':vc, 'cig':cc} 
        except:
            _Logger.exception('Cannot read flight category config file,%s.',self.fltCatFile)

class Projection:
    def __init__(self,siteId,grpTaf,fltCat,fcstData,itime):
        self.siteId = siteId
        self.startTime = fcstData.get('time').get('from')
        self.endTime = fcstData.get('time').get('to')
        self.itime = itime
        self.wind = fcstData.get('wind')
        self.vis = fcstData.get('vsby')
        self.sky = fcstData.get('sky')
        self.obv = fcstData.get('obv')
        self.grpTaf = grpTaf
        self.fltCat = fltCat
        self.visCat = self.mkVisCat()
        self.cigCat = self.mkCigCat()
        self.pcp = self.mkPcp(fcstData.get('pcp'))
        self.tstm = self.mkTstm(fcstData.get('pcp'))
        self.tempo = self.mkTempo(fcstData.get('pcp'))
        #mkProb30 needs to be called after mkTempo since some
        #consolidation is done in mkProb30
        self.prob30 = self.mkProb30(fcstData.get('pcp'))

    def getCat(self,val,thresholds):
        if thresholds[-1] == 5:#MVFR limit
            thresholds[-1] = 5.1 #so that 5 SM will be MVFR
        nlen = len(thresholds)
        if val < 0 : return 0
        #VLIFR 
        elif val < thresholds[0]:
            return 1
        #VFR or the last category
        elif val >= thresholds[-1]:
            return nlen+1
        #LIFR, IFR , MVFR, VFR(when more than one VFR categories exist)
        else:
            for i in range(nlen-1):
                if thresholds[i] <= val < thresholds[i+1]:
                    return i+2
    
    def mkVisCat(self):
        if self.vis:
            return self.getCat(self.vis['value'],self.fltCat['vis'])
        else:
            return 0

    def mkCigCat(self):
        if self.sky and self.sky.get('cig'):
            return self.getCat(self.sky['cig'],self.fltCat['cig'])
        else:
            return 0

    def mkPcp(self,pcpn):
        if pcpn and pcpn.get('pop') >= self.grpTaf['prev_pop']:
            return pcpn['str']

    def mkTstm(self,pcpn):
        if pcpn and pcpn.get('pot') >= self.grpTaf['prev_tstm']:
            self.sky['str'] += 'CB'
            return 'TS'

    def mkTempo(self,pcpn):
        tmpStr=[]
        if pcpn:
            if self.grpTaf['prev_tstm'] > pcpn.get('pot') >= \
                self.grpTaf['tempo_tstm']:
                tmpStr.append('TS')
            if self.grpTaf['prev_pop'] > pcpn.get('pop') >= \
                self.grpTaf['tempo_pop']:
                tmpStr.append(pcpn['str'])
            if tmpStr == ['TS'] and self.pcp:
                    tmpStr.append(self.pcp)
            if tmpStr:
                skyStr = self.sky['str']
                if 'TS' in tmpStr:
                #make sure cig is below threshold
		    try:
		       cig = int(self.sky['str'][3:])
		    except ValueError:
		       cig = 250
                    if cig > self.grpTaf['cbhight']:
                        cig = self.grpTaf['cbhight']
                    skyStr = "%s%03d%s" %('BKN',cig,'CB')
                return {'wxStr':fixPcp(tmpStr),'visStr':self.vis['str'],\
                        'skyStr':skyStr}

    def mkProb30(self,pcpn):
      try:
        if self.startTime - self.itime >= 9*3600 and \
            self.grpTaf['prob30'] and pcpn:
            tmpStr=[]
            if not pcpn.get('str'):
                pcpn['str'] = []
            if self.grpTaf['prob30_tstm'] <= pcpn.get('pot',0) < \
                self.grpTaf['tempo_tstm']:
                tmpStr.append('TS')
            if self.grpTaf['prob30_pop'] <= pcpn.get('pop') < \
                self.grpTaf['tempo_pop']:
                tmpStr.append(pcpn['str'])
            if tmpStr:
                #Consolidate 'TS' & precip in tempo and prob30 seperately
                if self.tempo and self.tempo['wxStr'].find('TS') >= 0:
                    tmpStr.insert(0,self.tempo['wxStr'])
                    self.tempo['wxStr'] = fixPcp(tmpStr)
                    return None
                elif 'TS' in tmpStr:
                    self.tempo = None
                if tmpStr == ['TS'] and self.pcp:
                    tmpStr.append(self.pcp)
                skyStr = self.sky['str']
                if 'TS' in tmpStr:
                #make sure cig is below threshold
                    cig = int(self.sky['str'][3:])
                    if cig > self.grpTaf['cbhight']:
                        cig = self.grpTaf['cbhight']
                    skyStr = "%s%03d%s" %('BKN',cig,'CB')
                return {'wxStr':fixPcp(tmpStr),'visStr':self.vis['str'],\
                        'skyStr':skyStr}
      except (ValueError, IndexError, KeyError) :
        return None

    def getData(self):
        return {'visCat':self.visCat,'cigCat':self.cigCat,'tempo':self.tempo,\
            'prob30':self.prob30,'startTime':self.startTime,'endTime':self.endTime,\
            'vis':self.vis,'sky':self.sky,'wind':self.wind,'obv':self.obv,\
            'tstm':self.tstm,'pcp':self.pcp}

class LampProjection(Projection):
    def __init__(self,siteId,grpTaf,fltCat,fcstData,itime):
        self.siteId = siteId
        self.startTime = fcstData.get('time').get('from')
        self.endTime = fcstData.get('time').get('to')
        self.itime = itime
        self.wind = fcstData.get('wind')
        self.vis = fcstData.get('vsby')
        self.sky = fcstData.get('sky')
        self.obv = fcstData.get('obv')
        self.grpTaf = grpTaf
        self.fltCat = fltCat
        self.pcat = fcstData.get('pcp',{}).get('pcat')
        self.tcat = fcstData.get('pcp',{}).get('tcat')
        self.csky = fcstData.get('csky')
        self.cvis = fcstData.get('cvsby')
        self.pcp = self.mkPcp(fcstData['pcp'])
        if self.pcp: #If precip,use conditional cig/vis
            self.vis = self.cvis
            self.sky = self.csky
        self.visCat = self.mkVisCat()
        self.cigCat = self.mkCigCat()
        self.tstm = self.mkTstm(fcstData['pcp'])
        self.tempo = self.mkTempo(fcstData['pcp'])
        #mkProb30 needs to be called after mkTempo since some
        #consolidation is done in mkProb30
        self.prob30 = self.mkProb30(fcstData['pcp'])
        
    def mkPcp(self,pcpn):
        if self.pcat or pcpn['pop'] > self.grpTaf['prev_pop']:
            self.sky = self.csky
            self.vis = self.cvis
            self.visCat = self.mkcVisCat()
            self.cigCat = self.mkcCigCat()
            return pcpn['str']

    def mkcVisCat(self):
        return self.getCat(self.cvis['value'],self.fltCat['vis'])

    def mkcCigCat(self):
        return self.getCat(self.csky['cig'],self.fltCat['cig'])

    def mkTempo(self,pcpn):
        #if self.pcat: return None
        tmpStr=[]
        if self.grpTaf['prev_tstm'] > pcpn.get('pot') >= self.grpTaf['tempo_tstm']:
            tmpStr.append('TS')
        if self.pcat != 1 and self.grpTaf['prev_pop'] > pcpn.get('pop') >= self.grpTaf['tempo_pop']:
            tmpStr.append(pcpn['str'])
        if tmpStr:
            skyStr = self.csky['str']
            visStr = self.cvis['str']
            #dry thunder
            if tmpStr == ['TS'] and self.pcp:
                tmpStr.append(self.pcp)
                skyStr = self.sky['str']
                visStr = self.vis['str']
            if 'TS' in tmpStr:
            #make sure cig is below threshold
                if skyStr != 'SKC':
                    cig = int(skyStr[3:])
                    if cig > self.grpTaf['cbhight']:
                        cig = self.grpTaf['cbhight']
                    skyStr = "%s%03d%s" %('BKN',cig,'CB')
            return {'wxStr':fixPcp(tmpStr),'visStr':visStr,\
                        'skyStr':skyStr}


    def mkProb30(self,pcpn):
        if self.startTime - self.itime >= 9*3600 and self.grpTaf['prob30']:
            tmpStr = []
            if not pcpn.get('str'):
                pcpn['str'] = []
            if self.grpTaf['prob30_tstm'] <= pcpn.get('pot',0) < \
                self.grpTaf['tempo_tstm']:
                tmpStr.append('TS')
            if self.pcat != 1 and self.grpTaf['prob30_pop'] <= \
                pcpn.get('pop') <= self.grpTaf['tempo_pop']:
                tmpStr.append(pcpn['str'])
            if tmpStr:
                #Consolidate tempo and prob30 
                if self.tempo and self.tempo['wxStr'].find('TS') >= 0:
                    tmpStr.insert(0,self.tempo['wxStr'])
                    self.tempo['wxStr'] = fixPcp(tmpStr)
                    return None
                elif 'TS' in tmpStr:
                    self.tempo = None
                skyStr = self.csky['str']
                visStr = self.cvis['str']
                if tmpStr == ['TS'] and self.pcp:
                    tmpStr.append(self.pcp)
                    skyStr = self.sky['str']
                    visStr = self.vis['str']
                if 'TS' in tmpStr:
                #make sure cig is below threshold
                    if skyStr != 'SKC':
                        cig = int(skyStr[3:])
                        if cig > self.grpTaf['cbhight']:
                            cig = self.grpTaf['cbhight']
                        skyStr = "%s%03d%s" %('BKN',cig,'CB')
                return {'wxStr':fixPcp(tmpStr),'visStr':visStr,\
                            'skyStr':skyStr}



class Comparison:

    def __init__(self,proj1,proj2,grpTaf):
        self.dataA = proj1
        self.dataB = proj2
        self.grpTaf = grpTaf

    def compVis(self):
        if self.dataA['visCat'] == self.dataB['visCat']:
            return 1

    def compCig(self):
        if self.dataA['cigCat'] == self.dataB['cigCat']:
            return 1

    def compWind(self):
        if self.dataA['wind'] and self.dataB['wind']:
            aff = self.dataA.get('wind',{}).get('ff',0)
            bff = self.dataB.get('wind',{}).get('ff',0)
            add = self.dataA.get('wind',{}).get('dd',0)
            bdd = self.dataB.get('wind',{}).get('dd',0)
            agg = self.dataA.get('wind',{}).get('gg',0)
            bgg = self.dataB.get('wind',{}).get('gg',0)
            if (agg > 10 or bgg > 10) and abs(agg-bgg) > 5:return None 
            if aff < self.grpTaf['wdthrsh'] and bff < self.grpTaf['wdthrsh']:
                return 1
            ddir = abs(add - bdd)
            dspd = abs(aff - bff)
            if ddir > 180: ddir = 360 - ddir
            if ddir < 30 and dspd < self.grpTaf['wddt']:
                return 1
        elif not self.dataA['wind'] and not self.dataB['wind']:
            return 1
    
    def compPcp(self):
        if self.dataA.get('pcp') == self.dataB.get('pcp'):
            return 1

    def compTstm(self):
        if self.dataA.get('tstm') == self.dataB.get('tstm'):
            return 1

    def compTempo(self):
        if (self.dataA.get('tempo') and self.dataB.get('prob30')) or \
            (self.dataA.get('prob30') and self.dataB.get('tempo')):
            return None
        if not self.dataA.get('tempo') or not self.dataB.get('tempo'):
            return 1
        else:
            if self.dataA.get('tempo').get('wxStr') == \
                self.dataB.get('tempo').get('wxStr') and \
                self.dataA.get('tempo').get('skyStr')[3:] == \
                self.dataB.get('tempo').get('skyStr')[3:] and \
                self.dataA.get('tempo').get('visStr')[-3:] == \
                self.dataB.get('tempo').get('visStr')[-3:]:
                return 1

    def compProb30(self):
        if not self.dataA.get('prob30') or not self.dataB.get('prob30'):
            return 1
        else:
            if self.dataA.get('prob30').get('wxStr') == \
                self.dataB.get('prob30').get('wxStr') \
            and self.dataA.get('prob30').get('skyStr')[3:] == \
                self.dataB.get('prob30').get('skyStr')[3:] \
            and self.dataA.get('prob30').get('visStr')[-3:] == \
                self.dataB.get('prob30').get('visStr')[-3:]:
                return 1

    def isSame(self):
        return self.compVis() and self.compCig() and self.compWind() and \
               self.compPcp() and self.compTempo() and self.compProb30() and \
                self.compTstm()

class Summarize:
    
    def __init__(self,grpTaf,list):
        self.startTimes=[v['startTime'] for v in list]
        self.endTimes=[v['endTime'] for v in list]
        self.grpTaf = grpTaf
        self.vis = [v['vis'] for v in list]
        self.sky = [v['sky'] for v in list]
        self.wdDir = []
        self.wdSpd = []
        self.wdGst = []
        for v in list:
            if v['wind']:
                self.wdDir.append(v['wind'].get('dd'))
                self.wdSpd.append(v['wind'].get('ff'))
                self.wdGst.append(v['wind'].get('gg'))
            else:
                self.wdDir.append(None)
                self.wdSpd.append(None)
                self.wdGst.append(None)
        self.obv = [v['obv'] for v in list]
        #for the following elements,they should be identical in the list
        self.pcp = list[0]['pcp']
        self.tstm = list[0]['tstm']
        self.tempo = [t['tempo'] for t in list]
        self.prob30 = [p['prob30'] for p in list]

    def summTime(self):
        return {'from':self.startTimes[0],'to':self.endTimes[-1]}
        
    def summVis(self):
        if filter(None,self.vis):
            vsby=min([v['value'] for v in self.vis if v is not None])
        else:
            vsby = -1
        return AvnLib.fixTafVsby(vsby)

    def summSky(self):
        d={}
        if filter(None,self.sky):
            for s in self.sky:
                d[s['cig']]=s['str']
            minCig = min(d.keys())
            return {'str':d[minCig]} 
        
    def summObv(self):
        d={}
        for s,v in zip(self.obv,self.vis):
            if s:
                if s['str'] == 'FG' and v['value'] > 0.625:
                    s['str'] = 'BR'
                elif s['str'] == 'BR' and v['value'] <= 0.625:
                    s['str'] = 'FG'
                d[s['str']] = 1
        return {'str':' '.join(d.keys())}

    def varWind(self,wdir,wspd):
        if max(wspd) <= 6 and min(wspd) > self.grpTaf['calmwd']:
            for d1 in wdir:
                for d2 in wdir[1:]:
                    dDir = abs(d1-d2)
                    if dDir > 180:
                        dDir = 360 - dDir
                    if dDir > 30:
                        varW = 'VRB%02dKT' % (sum(wspd)/len(wspd))
                        return {'str': varW}
        

    def summWind(self):
        rt = self.varWind(self.wdDir,self.wdSpd)
        if not rt:
            wdir = [a for a in self.wdDir if a != None]
            wspd = [a for a in self.wdSpd if a != None]
            if wdir and wspd:
                u = sum([math.cos(math.radians(270 - dir))*spd \
                    for dir,spd in zip(wdir,wspd)])
                v = sum([math.sin(math.radians(270 - dir))*spd \
                    for dir,spd in zip(wdir,wspd)])
                dd = math.degrees(math.atan2(v,u))
            else:
                #missing dir or spd, ski
                return ''
            if dd >= -90:
                dd = 270-dd
            else:
                dd = -90-dd
            dd = int(10*((dd+5)//10))
            if dd == 0:
                dd = 360
            ff = int(sum([s for s in wspd])/len(wspd))
            if ff < self.grpTaf['calmwd']:
                dd,ff = 0,0
            gst = filter(None,self.wdGst)
            if gst:
                #calculate average wgust
                gg = sum(gst)/len(gst)
                if gg - ff > 5:
                    return {'str':'%03d%02dG%02dKT' % (dd,ff,gg)}
                else:
                    return {'str':'%03d%02dKT' % (dd,ff)}
            else:
                return {'str':'%03d%02dKT' % (dd,ff)}
        else:
            return rt
    
    def summPcp(self):
        if self.tstm:
            if self.pcp:
                return {'str':fixPcp(['TS',self.pcp])}
            else:
                return {'str':'TS'}
        else:
            return {'str':self.pcp}

    def summOcnlTime(self):
        tp = filter(None,self.tempo)
        pb = filter(None,self.prob30)
        tList = []
        if tp:
            for t,ind in zip(self.tempo,xrange(30)):
                if t:
                    tList.append(ind)
        elif pb:    
            for t,ind in zip(self.prob30,xrange(30)):
                if t:
                    tList.append(ind)
        b = self.startTimes[tList[0]]
        e = self.endTimes[tList[-1]]
        return {'from':b,'to':e}

    def summOcnl(self):
        #The ocnl str should be identical, so pick up the first one
        tempo,prob30 = None,None
        if filter(None,self.tempo):
            tempo = filter(None,self.tempo)[0]
        if filter(None,self.prob30):
            prob30 = filter(None,self.prob30)[0]


        if tempo:
            return {'type':'TEMPO','pcp':{'str':tempo['wxStr']},\
                    'time':self.summOcnlTime(),\
                    'sky':{'str':tempo['skyStr']},\
                    'vsby':{'str':tempo['visStr']}}
        elif prob30:
            return {'type':'PROB','pcp':{'str':prob30['wxStr']},\
                    'time':self.summOcnlTime(),\
                    'sky':{'str':prob30['skyStr']},
                    'vsby':{'str':prob30['visStr']}}

        
    def returnPrd(self):
        prd = {'time':self.summTime()}
        if self.summWind() and self.summWind().get('str'):
            prd['wind'] = self.summWind()
        if self.summPcp() and self.summPcp().get('str'):
            prd['pcp'] = self.summPcp()
        if self.summObv() and self.summObv().get('str'):
            prd['obv'] = self.summObv()
        if self.summVis() and self.summVis().get('str'):
            if 'pcp' not in prd and 'obv' not in prd and \
                self.summVis()['value'] < 6:
                prd['vsby'] = {'str':'6SM','value':6.0}
            else:
                prd['vsby'] = self.summVis()
        if self.summSky() and self.summSky().get('str'):
            prd['sky'] = self.summSky()
        if self.summOcnl():
            return {'prev':prd,'ocnl':self.summOcnl()}
        else:
            return {'prev':prd}

class TafGen:
    def __init__(self,modelId,allFcst,tafType='   ',tafTime=None):
        self.model = modelId
        self.ident = allFcst['ident']['str']
        self.fcst = allFcst['group']
        self.startTimes = [t['time']['from'] for t in self.fcst]
        self.endTimes = [t['time']['to'] for t in self.fcst]
        self.grpTaf = Config(self.ident,modelId).grpTaf()
        self.fltCat = Config(self.ident,modelId).fltCat()
        self.tafType = tafType
        self.formatTafTime=tafTime
        if tafTime:
            self.tafTime = tafTime
        else:
            self.tafTime = AvnLib.getValidTime('taf', tafType)
        #get TAF's start and end projections
        
        try:
            self.tafDuration=int(AvnParser.getTafSiteCfg(self.ident)['thresholds']['tafduration'])
        except:
            self.tafDuration=24
            
        nBeg,nEnd = self.getTafPrd(self.tafDuration)
        if self.model == 'gfslamp':
            self.projData = [LampProjection(self.ident,self.grpTaf,self.fltCat, \
                dat,self.tafTime).getData() for dat in self.fcst[nBeg:nEnd]]
        else:
            self.projData = [Projection(self.ident,self.grpTaf,self.fltCat,dat,\
                self.tafTime).getData() for dat in self.fcst[nBeg:nEnd]]
        #create a subset time series 
        self.subStartTimes = self.startTimes[nBeg:nEnd]
        self.subEndTimes = self.endTimes[nBeg:nEnd]

    def getTafPrd(self,duration=24):
        endtm = ((self.tafTime+duration*3600)//21600)*21600
        times = self.startTimes
        n_st = 0
        n_ed = len(times)
        for i in range(1,len(times)):
            if times[i-1] <= self.tafTime < times[i]:
                n_st = i-1
                break
        for i in range(1,len(times)):
            if times[i-1] < endtm <= times[i]:
                n_ed = i
                break
        return (n_st,n_ed)

    def meetThrshds(short,long,dict):
        longPrd = self.projData[dict[long]]
        if long > 1:
            lgdt = self.subEndTimes[dict[long]] - \
                 self.subEndTimes[dict[long-1]]
        else:
            lgdt = self.subEndTimes[dict[long]] - \
                 self.subStartTimes[dict[long]]
        lgCigCat = longPrd['cigCat']
        lgVisCat = longPrd['visCat']
        shrtPrd = self.projData[dict[short]]
        shrtCigCat = shrtPrd['cigCat']
        shrtVisCat = shrtPrd['visCat']
        if lgdt > self.grpTaf['long_dt']*3600 and \
            abs(shrtVisCat - longVisCat) < self.grpTaf['vis_cat'] and \
            abs(shrtCigCat - longCigCat) < self.grpTaf['cig_cat'] and \
            longVisCat < self.grpTaf['vis_cat']['high_c'] and \
            shrtVisCat < self.grpTaf['vis_cat']['high_c'] and \
            longVisCat > self.grpTaf['vis_cat']['low_c'] and \
            shrtVisCat > self.grpTaf['vis_cat']['low_c'] and \
            longCigCat < self.grpTaf['cig_cat']['high_c'] and \
            shrtCigCat < self.grpTaf['cig_cat']['high_c'] and \
            longCigCat > self.grpTaf['cig_cat']['low_c'] and \
            shrtCigCat > self.grpTaf['cig_cat']['low_c']:
            return 1           

    def isSingleton(self,ref,dict):
        if self.projData[dict[ref]]['tempo'] or\
            self.projData[dict[ref]]['prob30']: return None
        #check if the period is short enough to be a singleton
        dt = self.subEndTimes[dict[ref]]-self.subEndTimes[dict[ref-1]]
        pTime = self.subEndTimes[dict[ref]] - self.subStartTimes[0]
        if dt < self.grpTaf['short_dt']*3600 and pTime > self.grpTaf['low_p']*3600:
            if meetThrshds(ref,ref-1,dict) and \
                Comparison(self.projData[dict[ref]],self.projData[dict[ref-1]],\
                            self.grpTaf).isSame():
                return ref-1
            elif meetThrshds(ref,ref+1,dict) and \
                Comparison(self.projData[dict[ref]],self.projData[dict[ref+1]],\
                            self.grpTaf).isSame():
                return ref+1

    def filterSingleton(self,grpList):
        dic = {}
        for g,ind in zip(grpList,xrange(100)):
            dic[g] = ind
        
        for k in dic.keys()[1:]:
            neighbor = self.isSingleton(k,dic)
            if neighbor and neighbor < k:
                dic[neighbor] = dic[k]
                dic[k] = None
            elif neighbor and neighbor > k:
                dic[k] = None    
        return dic                    
            
                
    def Brk4Tempo(self,gList,fcstD):
        #fcstD is the current proj to be determined if it can be combined with 
        #last group in gList.
        #find the starting proj for the last group in the gList
        startInd = gList.index(gList[-1])
        dt = fcstD['endTime'] - self.subStartTimes[startInd]
        if fcstD['tempo'] and dt > 4*3600:
            return 1
                            
                         
    def checkOcnl(self,grpInd):
        #create the indexes that mark the locations of tempo & prob30
        pstr = ''.join([{True:'1',False:'0'}.get(x.has_key('prob30')) for x in self.projData])
        tstr = ''.join([{True:'1',False:'0'}.get(x.has_key('tempo')) for x in self.projData])

        #The dic that marks the end of each group
        grpDic = {}
        for g,i in zip(grpInd,xrange(30)):
            grpDic[g]=i

        #creat an empty list to hold
        binary = []
        for i in range(len(pstr)):
            binary.append(0)
            
        beg = 0
        for s in grpDic.keys():
            if s > 1:
                beg = grpDic[s-1]+1
            e = grpDic[s]

            #if the group has only one projection, skip it
            if e - beg <= 0: continue 
                 
            pp = pstr[beg:e+1].find('1')      
            pt = tstr[beg:e+1].find('1')        
            pp1,pt1 = -1,-1
            if pp >= 0:
                pp1 = pstr[beg+pp+1:e+1].find('1')
            if pt >= 0:
                pt1 = tstr[beg+pt+1:e+1].find('1')
            if (pp >= 0 and pp1 > 0) or \
               (pt >= 0 and pt1 > 0) or \
                (pp >= 0 and pt >= 0):
                brks = [pp,pt,pp1,pt1]
                brks.sort()
                brkPts = [a for a in brks if a >= 0]
                #we need to break from the 2nd 
                for b in brkPts[1:]:
                    #if the first group,b is identical to index of grpInd
                    if s == 1:
                        binary[b] = 1
                    else:#grpDic[s-1]+1 is the grp begining index
                        binary[b+grpDic[s-1]+1] = 1
                
        #merge grpInd and binary
        lev = 0
        newInd = []
        newInd.append(grpInd[0])
        for g,b in zip(grpInd[1:],binary[1:]):
            if b == 0 :
                newInd.append(g+lev)
            else:
                #if this one was in the same level as the previous,break
                if g == newInd[-1]:
                    lev += 1
                newInd.append(g+lev)
        return newInd
                     
    def formNewDic(self,noGrp):
        period=[]
        if noGrp:
            for p in self.projData:
                period.append(Summarize(self.grpTaf,[p]).returnPrd())
        else:
            #Create index strings for prob30 and tempo
            grpInd = []
            grpDic = {}
            count = 1
            grpInd.append(count)
            for d1,d2 in zip(self.projData,self.projData[1:]):
                if Comparison(d1,d2,self.grpTaf).isSame() and not self.Brk4Tempo(grpInd,d2):
                    grpInd.append(count)
                else:
                    count += 1
                    grpInd.append(count)

            newGrpInd = self.checkOcnl(grpInd)
            #filter-singleton
            grpDic = self.filterSingleton(newGrpInd)

            #summarize period element values
            grpDic.keys().sort()
            for i in grpDic.keys():
                endProj = grpDic[i]
                if i == 1:
                    fcstList = self.projData[:endProj+1]
                    period.append(Summarize(self.grpTaf,fcstList).returnPrd())
                elif i == grpDic.keys()[-1]:
                    fcstList = self.projData[grpDic[i-1]+1:]
                    period.append(Summarize(self.grpTaf,fcstList).returnPrd())
                else:
                    fcstList = self.projData[grpDic[i-1]+1:endProj+1]
                    period.append(Summarize(self.grpTaf,fcstList).returnPrd())
        return period

    def createTaf(self,noGrp=1):
        taf = AvnLib.indentTaf(AvnLib.makeTafFromPeriods(self.ident,self.tafType,\
                                                         self.formNewDic(noGrp), \
                                                         self.formatTafTime,
                                                         self.tafDuration))
        return taf

