#!/usr/local/viz/python/bin/python
#
# Author: M. Oberfield
# Date: 31 March 2009
# Purpose: To validate proper functioning of TAF Decoder OB9.2
#
import os, pprint, re, sys, sets, time, types
import TafDecoder
#
# 06Z Wind checks
# 07Z Visibility checks
# 08Z Sky checks
# 09Z Obvis checks
# 10Z Wx checks
# 11Z LLWS checks
# Additional NWSI 10-813 checks
#
Tests = [
    { 'text':"""TAF
KBOS D10600Z D106/D218 360200KT P6SM SKC
     FMD10603 36001G01KT P6SM SKC
     FMD10606 36001G00KT P6SM SKC
     FMD10609 36001G32KT P6SM SKC
     FMD10612 VRB10KT P6SM SKC
     FMD10615 00003KT P6SM SKC
     FMD10618 01000KT P6SM SKC
     FMD10621 37010KT P6SM SKC
     FMD10624 01510KT P6SM SKC
     FMD10627 010100KT P6SM SKC
     FMD10700 00000KT 1 1/4SM VA SKC
     FMD10703 00000KT 1 3/4SM VA SKC
     FMD10800 00000KT P6SM FEW001 FEW001
     FMD10803 00000KT P6SM FEW002 FEW001
     FMD10806 00000KT P6SM SCT001 FEW002
     FMD10809 00000KT P6SM CLR
     FMD10812 00000KT P6SM OVC001 OVC002
     FMD10815 00000KT P6SM FEW001 FEW002 FEW003 FEW004
     FMD10818 00000KT P6SM FEW029
     FMD10821 00000KT P6SM FEW031
     FMD10824 00000KT P6SM FEW049
     FMD10827 00000KT P6SM FEW050
     FMD10830 00000KT P6SM FEW051
     FMD10833 00000KT P6SM FEW055
     FMD10836 00000KT P6SM FEW060
     FMD10900 00000KT P6SM VA SKC
     FMD10901 00000KT P6SM DRDU SKC
     FMD10902 00000KT P6SM DRSA SKC
     FMD10904 00000KT P6SM MIFG SKC
     FMD10905 00000KT P6SM PRFG SKC
     FMD10906 00000KT P6SM BCFG SKC
     FMD10907 00000KT P6SM BR SKC
     FMD10909 00000KT 6SM FC SKC
     FMD10912 00000KT 6SM BR BR SKC
     FMD11000 00000KT P6SM -RARA FEW001
     FMD11003 00000KT P6SM TS RA OVC001CB
     FMD11006 00000KT P6SM TS FZRA OVC001CB
     FMD11009 00000KT P6SM -IC SKC
     FMD11010 00000KT P6SM +IC SKC
     FMD11011 00000KT P6SM -GR FEW001
     FMD11012 00000KT P6SM +GR SCT001
     FMD11013 00000KT P6SM -GS BKN001
     FMD11014 00000KT P6SM -SHGR VV001
     FMD11015 00000KT P6SM +SHGR SKC
     FMD11016 00000KT P6SM -SHGR FEW002
     FMD11017 00000KT P6SM +SHGR SCT002
     FMD11018 00000KT P6SM -SHGS BKN002
     FMD11019 00000KT P6SM +SHGS OVC002
     FMD11020 00000KT P6SM TSGS VV002CB
     FMD11021 00000KT P6SM TSGR FEW002CB
     FMD11024 00000KT 3SM +PL OVC001
     FMD11027 00000KT 3SM SN BKN001
     FMD11100 00000KT P6SM SKC WS021/01010KT
     FMD11103 00000KT P6SM SKC WS020/010100KT
     FMD11106 00000KT P6SM SKC WS000/01030KT
     FMD11109 00000KT P6SM SKC WS001/01130KT
     FMD11112 00000KT P6SM SKC
      TEMPO D105/D112 P6SM WS010/18050KT
     FMD11115 00000KT P6SM SKC PROB30 D111/D112 WS020/36050KT
     FMD11200 00000KT P6SM TS SCT050
      TEMPO D111/D116 VA
     FMD11300 00000KT 6SM SCT050
     FMD11303 00000KT 1/2SM MIFG SKC
     FMD11306 00000KT 1SM FG SKC
     FMD11309 00000KT 1/2SM BR SKC PROB40 D113/D114 6SM -DZ
     FMD11308 00000KT P6SM BR SKC PROB30 D113/D114 6SM BR
     FMD11400 00000KT P6SM VCBR BKN000
      TEMPO D114/D115
     FMD11500 00000KT P6SM -DZ SKC
      TEMPO D115/D114 5SM
     FMD11600 00000KT P6SM SKC PROB30 D116/D123 1/4SM SN
     FMD11700 00000KT P6SM SKC PROB30 D124/D200 6SM -RA
     FMD11800 00000KT P6SM -RA OVC005
      TEMPO D118/D119 NSW
     FMD11900 00000KT P6SM -RA OVC005
      TEMPO D119/D120 NSW
     FMD12000 00000KTP6SMSKC PROB30 D123/D124 6SM -RA
      AMD NOT AFT 17Z=""",
           
      'results': {'warning': [('wind', ('5.14', '5.24'), 'Suspicious value of wind gust'),
                              ('wind', ('6.14', '6.21'), 'Variable wind speed must be between 1 and 6KT\nwithout convective activity.\n(NWSI 10-813, Appendix C, 1.2.4)'),
                              ('wind', ('11.14', '11.22'), 'Suspicious value of wind speed'),
                              ('sky',  ('19.27', '19.54'), 'Number of cloud groups should not exceed three\n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                              ('obv',  ('34.26', '34.28'), 'Tornadoes or Waterspouts should not be\nforecast in terminal forecasts\n(NWSI 10-813, Appendix B, 4)'),
                              ('llws', ('55.31', '55.45'), 'Suspicious value of llws speed'),
                              ('time', ('72.31', '72.47'), 'The period of time covered by a PROB should be \n6 hours or less (NWSI 10-813, Appendix C, 1.2.9.4)')],
                  'error': [('itime', ('2.5', '2.12'), 'Issue and valid times do not match'),
                            ('vtime', ('2.13', '2.22'), 'The period covered by a TAF shall not exceed 30\nhours'),
                            ('time',  ('2.13', '2.22'), 'The period covered by a TAF shall not exceed 30\nhours'),
                            ('wind', ('2.23', '2.31'), 'Invalid value of wind speed'),
                            ('wind', ('3.14', '3.24'), 'Wind gust <= wind speed'),
                            ('wind', ('4.14', '4.24'), 'Wind gust <= wind speed'),
                            ('wind', ('7.14', '7.21'), 'Invalid value of wind direction'),
                            ('wind', ('8.14', '8.21'), 'Invalid value of wind direction'),
                            ('wind', ('9.14', '9.21'), 'Invalid value of wind direction'),
                            ('wind', ('10.14', '10.21'), 'Invalid value of wind direction'),
                            ('vsby', ('12.22', '12.29'), 'Invalid value of visibility \n(NWSI 10-813, Appendix C, 1.2.5)'),
                            ('vsby', ('13.22', '13.29'), 'Invalid value of visibility \n(NWSI 10-813, Appendix C, 1.2.5)'),
                            ('sky', ('14.27', '14.40'), 'Invalid cloud base \n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('sky', ('15.27', '15.40'), 'Invalid cloud base \n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('sky', ('16.27', '16.40'), 'Invalid sky cover sequence \n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('sky', ('17.27', '17.30'), 'CLR shall not be used in TAF\n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('sky', ('18.27', '18.40'), 'Invalid sky cover sequence \n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('sky', ('21.27', '21.33'), 'Invalid cloud base \n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('sky', ('22.27', '22.33'), 'Invalid cloud base \n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('sky', ('24.27', '24.33'), 'Invalid cloud base \n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('sky', ('25.27', '25.33'), 'Invalid cloud base \n(NWSI 10-813, Appendix C, 1.2.7.1)'),
                            ('obv', ('33.27', '33.29'), 'Invalid weather with visibility >= 6SM \n(NWSI 10-813, Appendix C, 1.2.5)'),
                            ('obv', ('35.26', '35.31'), 'Repeated occurence of weather elements'),
                            ('pcp', ('36.27', '36.32'), 'Repeated occurence of weather elements'),
                            ('pcp', ('37.27', '37.32'), 'Invalid precipitation string\n(NWSI 10-813, Appendix C, 1.2.6)'),
                            ('pcp', ('39.27', '39.30'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('40.27', '40.30'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('41.27', '41.30'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('42.27', '42.30'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('43.27', '43.30'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('44.27', '44.32'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('45.27', '45.32'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('46.27', '46.32'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('47.27', '47.32'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('48.27', '48.32'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('49.27', '49.32'), 'Invalid value of sig weather \n(NWSI 10-813, Appendix B, 4)'),
                            ('pcp', ('52.26', '52.29'), '+PL requires visibility < 3SM\nPL requires visibility <= 6SM (FMH#1, p 8-3)'),
                            ('pcp', ('53.26', '53.28'), '+SN or +DZ requires visibility <= 1/4SM \nSN or DZ requires visibility <= 1/2SM \n(FMH#1, p 8-3)'),
                            ('llws', ('54.31', '54.44'), 'Invalid value of wind shear height'),
                            ('llws', ('56.31', '56.44'), 'Invalid value of wind shear height'),
                            ('llws', ('57.31', '57.44'), 'Invalid value of wind shear direction'),
                            ('time', ('59.6', '59.21'), 'Group time period not within TAF forecast period'),
                            ('time', ('59.6', '59.21'), 'The period covered by a TEMPO group will not \nexceed 4 hours (NWSI 10-813, 4.3.4)'),
                            ('time', ('59.6', '59.21'), 'Valid time of TEMPO/PROB must be >= valid time \nof previous FM group'),
                            ('llws', ('59.27', '59.40'), 'Forecast of non-convective low-level wind shear \nshall not be included in TEMPO or PROB groups \n(NWSI 10-813, Appendix C, 1.2.8)'),
                            ('time', ('60.5', '60.13'), 'Valid time must be >= valid time of previous\nTEMPO or PROB group'),
                            ('time', ('60.31', '60.47'), 'The PROB group shall not be used in the first \n9 hours of the valid TAF forecast \n(NWSI 10-813, Appendix C, 1.2.9.4)'),
                            ('time', ('60.31', '60.47'), 'PROB group must include forecast of a thunderstorm\nor precipitation event (NWSI 10-813, Appendix C, 1.2.9.4)'),
                            ('time', ('60.31', '60.47'), 'Valid time of TEMPO/PROB must be >= valid time \nof previous FM group'),
                            ('llws', ('60.48', '60.61'), 'Forecast of non-convective low-level wind shear \nshall not be included in TEMPO or PROB groups \n(NWSI 10-813, Appendix C, 1.2.8)'),
                            ('pcp', ('61.27', '61.29'), 'Thunderstorm forecast requires CB in the cloud \ngroup (NWSI 10-813, Appendix C, 1.2.7.3)'),
                            ('time', ('62.6', '62.21'), 'The period covered by a TEMPO group will not \nexceed 4 hours (NWSI 10-813, 4.3.4)'),
                            ('time', ('62.6', '62.21'), 'Valid time of TEMPO/PROB must be >= valid time \nof previous FM group'),
                            ('obv', ('62.22', '62.24'), 'Volcanic ash requires visibility forecast \n(NWSI 10-813, Appendix C, 1.2.6)'),
                            ('time', ('63.5', '63.13'), 'Valid time must be >= valid time of previous\nTEMPO or PROB group'),
                            ('vsby', ('63.22', '63.25'), 'Visibility <= 6SM requires forecast of significant\nweather (NWSI 10-813, Appendix C, 1.2.5)'),
                            ('obv', ('64.28', '64.32'), 'FG or FZFG forecast requires visibility < 5/8SM,\nMIFG requires visibility >= 5/8SM\n(NWSI 10-813, Appendix C, 1.2.6)'),
                            ('obv', ('65.26', '65.28'), 'FG or FZFG forecast requires visibility < 5/8SM,\nMIFG requires visibility >= 5/8SM\n(NWSI 10-813, Appendix C, 1.2.6)'),
                            ('obv', ('66.28', '66.30'), 'BR forecast requires visibility between 5/8SM\nand 6SM (NWSI 10-813, Appendix C, 1.2.6)'),
                            ('time', ('66.35', '66.51'), 'Only PROB30 is allowed'),
                            ('time', ('67.5', '67.13'), 'Valid time of FM group must be > valid time for \nthe entire TAF or a previous FM group'),
                            ('obv', ('67.27', '67.29'), 'Invalid weather with visibility >= 6SM \n(NWSI 10-813, Appendix C, 1.2.5)'),
                            ('time', ('67.34', '67.50'), 'The PROB group shall not be used in the first \n9 hours of the valid TAF forecast \n(NWSI 10-813, Appendix C, 1.2.9.4)'),
                            ('time', ('67.34', '67.50'), 'PROB group must include forecast of a thunderstorm\nor precipitation event (NWSI 10-813, Appendix C, 1.2.9.4)'),
                            ('time', ('67.34', '67.50'), 'Valid time of TEMPO/PROB must be >= valid time \nof previous FM group'),
                            ('vcnty', ('68.27', '68.31'), 'Invalid VC weather. Allowed entries:\nVCFG, VCSH, VCTS. \n(NWSI 10-813, Appendix C, 1.2.6.1)'),
                            ('sky', ('68.32', '68.38'), 'Cannot forecast partial obscuration \n(NWSI 10-813, Appendix C, 1.2.7.2)'),
                            ('time', ('69.6', '69.21'), 'Missing weather in this group'),
                            ('time', ('71.6', '71.21'), 'Bad TEMPO/PROB group duration'),
                            ('vsby', ('71.22', '71.25'), 'When reduction in visibility is forecast\nto change in TEMPO group, the significant weather\ncausing the deterioration shall be included \n(NWSI 10-813, Appendix C, 1.2.9.2)'),
                            ('pcp', ('72.54', '72.56'), '+SN or +DZ requires visibility <= 1/4SM \nSN or DZ requires visibility <= 1/2SM \n(FMH#1, p 8-3)'),
                            ('time', ('73.5', '73.13'), 'Valid time must be >= valid time of previous\nTEMPO or PROB group'),
                            ('time', ('73.31', '73.47'), 'Invalid start hour'),
                            ('time', ('73.31', '73.47'), 'Invalid end hour'),
                            ('time', ('73.31', '73.47'), 'Bad TEMPO/PROB group duration'),
                            ('time', ('74.5', '74.13'), 'Valid time must be >= valid time of previous\nTEMPO or PROB group'),
                            ('nsw',  ('75.22', '75.25'), 'P6SM needed with NSW in this group\n(NWSI 10-813, Appendix C, 1.2.6)'),
                            ('nsw',  ('77.22', '77.25'), 'Consecutive NSW groups not permitted\n(NWSI 10-813, Appendix C, 1.2.6)'),
                            ('wind', ('78.14', '78.21'), 'Missing terminating blank'),
                            ('vsby', ('78.21', '78.25'), 'Missing terminating blank'),
                            ('amd', ('79.6', '79.22'), 'Invalid AMD phrase. Valid phrases are:\nAMD NOT SKED\nAMD NOT SKED AFT ddHHmm\nAMD NOT SKED TIL ddHHmm\nAMD NOT SKED ddHH/ddHH\nAMD LTD TO (element list) (AFT ddHHmm, or TIL ddHHmm, or\nddHH/ddHH)')]}},
      
    { 'text': """TAF
KPVD D10459Z D106/D212 00000KT 6SM +PL OVC002 WS020/120010KT
      TEMPO D106/D107 6SM PL
     FMD10700 00000KT 1/2SM +SS SKC
     FMD10701 00000KT 1/2SM SS SKC
     FMD10702 00000KT 1/4SM +SS SKC
     FMD10703 00000KT 1/4SM SS SKC
     FMD10704 00000KT 1/2SM +DS SKC
     FMD10705 00000KT 1/2SM DS SKC
     FMD10706 00000KT 1/4SM +DS SKC
     FMD10707 00000KT 1/4SM DS SKC
     FMD10708 00000KT 1/2SM +SN SKC
     FMD10709 00000KT 1/2SM SN SKC
     FMD10710 00000KT 1/4SM +SN SKC
     FMD10711 00000KT 1/4SM SN SKC
     FMD10712 00000KT 1/2SM +DZ SKC
     FMD10713 00000KT 1/2SM DZ SKC
     FMD10714 00000KT 1/4SM +DZ SKC
     FMD10715 00000KT 1/4SM DZ SKC
      AMD NOT SKED D100/D110=""",
      'results': {'warning': [('amd', ('20.6', '20.29'),'AMD restriction not within TAF forecast period')],
                  'error': [('itime', ('2.5', '2.12'), 'Issue and valid times do not match'),
                            ('pcp', ('2.35', '2.38'), '+PL requires visibility < 3SM\nPL requires visibility <= 6SM (FMH#1, p 8-3)'),
                            ('llws', ('2.46', '2.60'), 'Unnecessary leading zero for wind shear speed'),
                            ('obv', ('4.28', '4.31'), '+SS or +DS requires visibility <= 1/4SM \nSS or DS requires visibility <= 1/2SM \n(NWSI 10-813, B-4)'),
                            ('obv', ('7.28', '7.30'), '+SS or +DS requires visibility <= 1/4SM \nSS or DS requires visibility <= 1/2SM \n(NWSI 10-813, B-4)'),
                            ('obv', ('8.28', '8.31'), '+SS or +DS requires visibility <= 1/4SM \nSS or DS requires visibility <= 1/2SM \n(NWSI 10-813, B-4)'),
                            ('obv', ('11.28', '11.30'), '+SS or +DS requires visibility <= 1/4SM \nSS or DS requires visibility <= 1/2SM \n(NWSI 10-813, B-4)'),
                            ('pcp', ('12.28', '12.31'), '+SN or +DZ requires visibility <= 1/4SM \nSN or DZ requires visibility <= 1/2SM \n(FMH#1, p 8-3)'),
                            ('pcp', ('15.28', '15.30'), '+SN or +DZ requires visibility <= 1/4SM \nSN or DZ requires visibility <= 1/2SM \n(FMH#1, p 8-3)'),
                            ('pcp', ('16.28', '16.31'), '+SN or +DZ requires visibility <= 1/4SM \nSN or DZ requires visibility <= 1/2SM \n(FMH#1, p 8-3)'),
                            ('pcp', ('19.28', '19.30'), '+SN or +DZ requires visibility <= 1/4SM \nSN or DZ requires visibility <= 1/2SM \n(FMH#1, p 8-3)'),]}},
    
    { 'text':"""TAF AMD
KBDL D11243Z D113/D212 20006KT 6SM BR FEW008
      TEMPO D113/D114 P6SM NSW BKN008
     FMD11400 20008KT 3SM BR SCT010
      TEMPO D114/D115 P6SM NSW
     FMD11600 19012G18KT P6SM SKC
     FMD20100 17008KT P6SM SCT250
      TEMPO D201/D202 P6SM NSW
     FMD20200 16010KT 5SM BR FEW002
     FMD20300 00000KT 3SM BR SCT002
      TEMPO D203/D204 NSW
     FMD20400 00000KT P6SM SCT002 VV003
      TEMPO D204/D205 VCSH BKN009=""",
      
      'results':{'warning':[],
                 'error': [('nsw',('5.27', '5.30'),'Consecutive NSW groups not permitted\n(NWSI 10-813, Appendix C, 1.2.6)'),
                           ('nsw', ('8.27', '8.30'), 'NSW not needed'),
                           ('nsw', ('11.22', '11.25'), 'P6SM needed with NSW in this group\n(NWSI 10-813, Appendix C, 1.2.6)'),
                           ('vcnty', ('13.22', '13.26'), 'Weather in the vicinity shall not be used in TEMPO\nor PROB groups (NWSI 10-813, Appendix C, 1.2.6.1)'),
                           ('sky', ('12.27', '12.39'), 'Invalid cloud amount\n(NWSI 10-813, Appendix C, 1.2.7.1)')],}},
      ]

ScreenColors = { 'FAIL':31, 'OK':32, 'WARN':33, 'N/A': 34 }
now = time.time()
D1 = '%02d' % time.gmtime(now)[2]
D2 = '%02d' % time.gmtime(now + 86400.)[2]
reD1 = re.compile('D1')
reD2 = re.compile('D2')
d = {'error': [], 'warning': []}

def prepareText(text):
    """Create recent day/time groups"""
    text = reD1.sub(D1,text)
    text = reD2.sub(D2,text)
    for n,lne in enumerate(text.split('\n')):
    #    print '%d: %s' %(n+1,lne)
	    print lne
    return text

def compareResults(ResultsKey,results):
    """Examine results from decoder and output results"""
    #
    overall = 'OK'
    #
    # Do warnings and errors separately
    for resultType in ResultsKey:
        if len(ResultsKey[resultType]) == 0:
            continue
        
        sys.stdout.write( '\nExamining %ss\n' % resultType)
        #
        uniqueTokenTypes = sets.Set([x[0] for x in ResultsKey[resultType]])
        for tokenType in uniqueTokenTypes:
            sys.stdout.write( '\n\033[3G%s %s checks\n' % (tokenType,resultType))
            #
            # Separate out results by token type
            rresults = [(x[1],x[2],x[3]) for x in results[resultType] if x[0] == tokenType]
            kresults = [(x[1],x[2]) for x in ResultsKey[resultType] if x[0] == tokenType]
            #
            # For each line in the Key
            for keyanswer in kresults:
                keylocation,keymsg = keyanswer

                locationFound = 'FAIL'
                msgFound = 'FAIL'
                TAFtext =''
                for answer in rresults:
                    if keylocation == answer[0]:
                        locationFound = 'OK'
                        TAFtext = answer[2]
                        if keymsg in answer[1]:
                            msgFound = 'OK'            
                            break
                        
                if locationFound == 'FAIL':
                    msgFound = 'N/A'
                    overall = 'FAIL'

                if overall == 'OK' and msgFound == 'FAIL':
                    overall = 'WARN'
                    
                sys.stdout.write('L:%s' % keylocation[0].split('.')[0])
                PrettyPrintText(TAFtext, locationFound)
                PrettyPrintMsg(' '.join(keymsg.split('\n')), msgFound)
                
        time.sleep(0.9)
        
    return overall

def PrettyPrintText(text,status):
    sys.stdout.write('\033[6GText: %s\033[40G[ \033[1;%dm%s\033[0;39m ]' %
                     (text,ScreenColors.get(status,32),status))

def PrettyPrintMsg(text,status):
    sys.stdout.write(' Msg: %s\033[120G[ \033[1;%dm%s\033[0;39m ]\n' %
                     (text[:67],ScreenColors.get(status,32),status))

def walk(item, k=None):
    if type(item) == types.ListType:
        for i in item:
            walk(i)
    elif type(item) == types.DictType:
        if 'error' in item:
            d['error'].append((k, item['index'], item['error'], item['str']))
        elif 'warning' in item:
            d['warning'].append((k, item['index'], item['warning'], item['str']))
        elif 'fatal' in item:
            print 'Fatal Error Location:', item['index']
            for text in item['fatal']:
                print 'Fatal Error: %s' % text
        else:
            for k, v in item.iteritems():
                walk(v, k)

def walk1(item, k=None):
    if type(item) == types.ListType:
        for i in item:
            walk1(i)
    elif type(item) == types.DictType:
        if 'error' in item:
            d['error'].append((k, item['index'], item['error']))
        elif 'warning' in item:
            d['warning'].append((k, item['index'], item['warning']))
        elif 'fatal' in item:
            print 'Fatal Error Location:', item['index']
            for text in item['fatal']:
                print 'Fatal Error: %s' % text
        else:
            for k, v in item.iteritems():
                walk1(v, k)

def mysort(a,b):
    return cmp(int(a[1][0].split('.')[0]),int(b[1][0].split('.')[0])) or \
           cmp(int(a[1][0].split('.')[1]),int(b[1][0].split('.')[1]))

if __name__ == '__main__':
    decoder = TafDecoder.Decoder()
    pp=pprint.PrettyPrinter(indent=1)
    print "Tests for OB9.2 TAF Syntax Check (decoder)"
    
    for test in Tests:
        if len(sys.argv) > 1 and sys.argv[1] == '-d':
            walk1(decoder(prepareText(test['text']),'',strict=True))
            d['warning'].sort(mysort)
            d['error'].sort(mysort)
            pp.pprint(d)
            
        d = {'error': [], 'warning': []}
        walk(decoder(prepareText(test['text']),'',strict=True))
        status = compareResults(test['results'],d)
        sys.stdout.write('\n\033[3GOverall: [ \033[1;%dm%s\033[0;39m ]\n' %
                         (ScreenColors.get(status,32),status))

        d = {'error': [], 'warning': []}
        sys.stdout.write('\n')
