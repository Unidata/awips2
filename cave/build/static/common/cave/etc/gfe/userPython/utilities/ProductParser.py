#!/usr/bin/env python
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
# CI block
#   - wmoAbrevHeading  (TTAAii CCCC DDHHMM [BBB])
#      * TTAAii (data type/location)
#      * ID of issuing office
#      * product issuance time (UTC)
#      * "funny field"
#   - awipsID  (NNNXXX) (PIL)
#      * NNN = specific product categogy (PIL)
#      * XXX = NWS location id

# blank line

# MND block
#   - [Broacast instruction]
#       * WORD _'-'_ junk
#   - Product type  (one line)
#       At end of line (optional) ...UPDATED/AMENDED/CORRECTED/
#                                    RESENT/DELAYED/TEST
#         with TEST "TEST..." is also prepended to the line
#   - Issuing office
#       (NATIONAL WEATHER SERVICE CITY STATE) (one line)
#                 or
#       (NWS NATIONALCENTER CITY STATE)
#                   or (special cases follow)
#       ISSUED_BY_NATIONAL_WEATHER_SERVICE_CITY_SS (second line)
#                   or
#       EXTERNAL_AGENCY_CITY/COUNTY/STATE_SS
#       RELAYED_BY_NATIONAL_WEATHER_SERVICE_CITY_SS
#   - Issuance data/time - local time
#       HHMM (AM/PM LST or LDT) day_of_week(3 char) month(3 char) day year
#             first H is not used if 0, both Ms required.
#             Multiple times can be used (refer to same UTC time).  These
#             times are seperated by '/' characters.  Line breaks can
#             occur anywhere.  '/' is just a seperator or maybe at end.
# blank line (optional and only if followed by:
#   - (Optional) reason for action line starts with
#       CORRECTED,UPDATED, or AMENDED.  This line occurs after
#



# blank line


# Product content block

# UGC "line"
# SSFNNN-NNN>NNN-SSFNNN-DDHHMM-
#

# Warnings
# (optional) headlines
#
# attribution paragraph
#
# text (multiple paragraph)
#
# (optional) call to action (multiple paragraphs)

#$$

import re, bisect, cStringIO, LogStream, JUtil

sl = r'^'                            # start of line
el = r'\s*?\n'                       # end of line
id3 = r'[A-Za-z]{3}'                 # 3 charater word
empty = r'^\s*' + el                 # empty line

wmoid = r'(?P<wmoid>[A-Z]{4}\d{2})' # wmoid
fsid  = r'(?P<fsid>[A-Z]{4})'       # full station id
pit   = r'(?P<pit>\d{6})'           # product issuance time UTC
ff    = r'(?P<funnyfield> ' + id3 + ')?'          # "funny" field

# NWS time format
ntime = r'\d{3,4}\s+[A-Z]{2}\s+[1-Z]{3,4}\s+' + id3 + r'\s+' + id3 \
        + r'\s+\d{1,2}\s+\d{4}\s*?'
nwstime = sl + r'(?P<nwstime>' + ntime + r'(?:\s*/\s*\n?' + ntime + r'/)*\n)'
#nwstime = sl + r'(?P<nwstime>' + ntime + r'(?:/\s*\n?' + ntime + r')*/?\n)'


# CI block
ci_start = sl + wmoid + ' ' + fsid + ' ' + pit + ff + el
awipsid = r'(?P<pil>(?P<cat>[A-Z0-9]{3})(?P<lid>[A-Z0-9]{1,3}))' + el
ci_block = r'(?P<ciblock>' + ci_start + awipsid + '\n?)' #+ empty + r')'

ci_re = re.compile(ci_block)

# MND block
bi = r'(^(?P<biword>[A-Z]+) - (?P<biinfo>.*)\n)?'  # broadcast instruction
pt = sl + r'(?P<pline>(?P<pt>.*)(?P<ptmeta>\.\.\.[A-Z]+)*)' + el # product type
io = sl + r'(?P<io>.*)\n'                            # issuing office
ibo = sl + r'((?P<ibo>.*)\n)?'                       # issued by office
mnd = empty + r'(?P<mnd>' + bi + pt + io + ibo + nwstime + r')' #+ empty

mnd_re = re.compile(mnd, re.M)

# UGC block
nnn = r'(?:[A-Z]{2}[ZC])?\d{3}'
purge = r'(?P<purgeT>\d{6})-'
ugc = r'\n(?P<uhdr>' + r'[A-Z]{2}[Z|C](?:(?:\d{3})|(?:ALL))' + r'(?:[->]\n?' + nnn + \
      r')*-\n?' + purge + el + r')'
cityh = r'-\n(?P<incc>(?:\s*\n)*(?:INCLUDING THE (?:CITIES|CITY) OF...)?)'

body = r'(?P<body>(?:^.*\n)*?)'
#body = r'.*'
term = r'(?P<term>' +  r'^\n\$\$\n)'
vtec = r'(?P<vtec>(?:^/[-A-Z0-9.]+/\s+?)*)'

updateWrds = r'(?:(?:UPDATED)|(?:CORRECTED)|(?:AMENDED))'
reason = r'(?P<reason>(?:^' + updateWrds + r'\s(?:.|\n)*?' + empty + r')?)'

headlines = r'(?P<headlines>(?:^\.\.\.(?:.|\n)*?\.\.\.\n)*)'

#ugc_re = re.compile(r'(?P<ugc>(?P<header>' + ugc + vtec + r'(?:^.*\n)*?'
#                    + '(?:' + nwstime + ')?' + r')'
#                    + empty + reason + headlines + body + r'^(?P<term>\$\$))'
#                    + el, re.M)

# MAFOR (funky marine product which omits the blank line
#        after a ugc header)
mafor = r'(?:^MAFOR .*\n)'

ugch_re = re.compile(ugc + vtec, re.M)
cityh_re = re.compile(cityh)
ghend_re1 = re.compile(r'(?:' + nwstime + r')', re.M)
ghend_re2 = re.compile(r'(?:' + empty + r'|' + mafor + r')', re.M)

gend_re = re.compile(term, re.M)
headlines2 = r'(?P<headlines>(?:^\.\.\.(?:.|\n)*?\.\.\.\s+?)+)'
head_re = re.compile(headlines2, re.M)

# Single headline re
headlines3 = r'^\.\.\.(?:.|\n)*?\.\.\.\s+'
single_head_re = re.compile(headlines3, re.M)

# Framing code
frame_re = re.compile(r'(?P<frame>\|\*(.|\n)*?\*\|)', re.M)

# This is the list of words which will trigger an unlocked section
# of a headline
# locWords = r'((IN)(?!\s((EFFECT)|(PLACE)))|(ABOVE)|(BELOW)|(NEAR)|((FOR)(?!\s((MARINE)|(ROUGH BAR)|(TEST PURPOSES ONLY)|(WINDS)|(HAZARDOUS))))|(AROUND)|(DUE)|(ALONG)|(ACROSS)|(AWAY)|(NORTH)|(NORTHEAST)|(EAST)|(SOUTHEAST)|(SOUTH)|(SOUTHWEST)|(WEST)|(NORTHWEST))'
# local = r'(?P<local>\s' + locWords + r'\s(.|\n)*?)?'
# headline = r'(?P<pre>^\.\.\.(.|\n)*?)' + local + r'(?P<post>(TEST)?\.\.\.\n)'
# headline_re = re.compile(headline, re.M)

# These words define the end of a locked section of headline
headlineEnders = ['AFTERNOON', 'CANCELLED', 'EFFECT', 'EXPIRED',
                  'EVENING', 'FRIDAY', 'MONDAY', 'MORNING', 'NIGHT',
                  'SATURDAY', 'SUNDAY', 'THURSDAY', 'TODAY', 'TONIGHT',
                  'TUESDAY', 'WEDNESDAY', 'IS FOR TEST PURPOSES ONLY']

endWords = '(' + '|'.join(map(lambda x: '(' + x + ')', headlineEnders)) + ')'
local = r'(?P<local>(.|\n)*?)'
headline = r'(?P<pre>^\.\.\.((.|\n)*\s' + endWords + r')+)' + local \
           + r'(?P<post>(TEST)?\.\.\.\n)'
headline_re = re.compile(headline, re.M)

class ProductParser:
    def __init__(self):
        pass

    # Convert an offset to a Tk line,col
    def tkc(self, offset):
        i = bisect.bisect(self._totals, offset) - 1
        return (i+1, offset - self._totals[i])


    def processHeadline(self, rval, m):
        str = m.group('headlines')
        start = m.start('headlines')
        hdlns = []
        #l = headline_re.finditer(str)
        l = single_head_re.finditer(str)
        for m in l:
            if m is not None:                
                #print 'phl m = ', m
                newstart = start + m.start()
                m = headline_re.match(m.group(0))
                if m is not None:
                    hdlns.append(self.dumpMatch(m, newstart))
                    
        #print 'hdlns = ', hdlns
        rval['headInfo'] = hdlns

    def dumpMatch(self, m, offset=0, rval=None):
        if rval is None:
            rval = {}
        
        #print 'dumpmatch m = ', m.groupdict()
        for k in m.groupdict().keys():
            if m.start(k) != -1 and m.start(k) != m.end(k):
                if k == 'headlines':
                    self.processHeadline(rval, m)
                span = m.span(k)
                rval[k] = (self.tkc(span[0] + offset),
                           self.tkc(span[1] + offset))
        #print 'dumpmatch rval = ', rval
        return rval

    def matchCoords(self, m):
        return self.tkc(m.span(0)[0]), self.tkc(m.span(0)[1])

    def parse(self):
        rval = {}
        m = ci_re.search(self._str)
        if m is not None:
            #print 'ci -- ', m.group()
            rval['ci'] = self.dumpMatch(m)

        m = mnd_re.search(self._str)
        if m is not None:
            #print 'mnd -- ', m.group()
            rval['mnd'] = self.dumpMatch(m)

        segs = []
        l = ugch_re.finditer(self._str)
        
        for m in l:
            if m is not None:
                m1 = cityh_re.search(self._str, m.end())
                m21 = ghend_re1.search(self._str, m.end())
                m22 = ghend_re2.search(self._str, m.end())
                m3 = gend_re.search(self._str, m.end())
                if m3 is None:
                    continue
                if m21 is not None and m21.start() < m3.start():
                    m2 = m21
                elif m22 is not None and m22.start() < m3.start():
                    m2 = m22
                else:
                    continue
                m4 = head_re.search(self._str, m.end(), m3.end())
                
                d = self.dumpMatch(m)
                d = self.dumpMatch(m2, rval=d)
                d = self.dumpMatch(m3, rval=d)
                d['header'] = (self.tkc(m.start('uhdr')),
                               self.tkc(m2.end()))
                if m1 is not None and m1.start('incc') < m2.start():
                    d['city'] = (self.tkc(m1.start('incc')),
                                 self.tkc(m2.start()))
                    mm = frame_re.search(self._str, m1.start(), m2.start())
                    if mm is not None:
                        d['cframe'] = (self.tkc(mm.start()),
                                       self.tkc(mm.end()))
                else:
                    d['city'] = (self.tkc(m2.start()),
                                 self.tkc(m2.start()))
                    
                if m4 is not None:
                    #print 'm4 = ', m4.group()
                    d = self.dumpMatch(m4, rval=d)
                d['ugc'] = (self.tkc(m.start() + 1),
                            self.tkc(m3.end() - 1))
                segs.append(d)
        #print 'segs = ', segs
        rval['segs'] = segs

        frames = []
        l = frame_re.finditer(self._str)
        for m in l:
            if m is not None:
                frames.append(self.dumpMatch(m))
        rval['frames'] = frames

        return rval
    
    def parseFromJava(self, text):
        self._str = text
        self._ci = None
        lines = map(lambda x: len(x), text.splitlines(1))
        count = 0
        lc = []
        for l in lines:
            lc.append(count)
            count += l
        self._totals = lc
        
        #print 'text START ----------------------'
        #print text
        #print 'text END ------------------------'
        
        result = self.parse()
        
        #print 'result = ', result
        
        return JUtil.pyDictToJavaMap(result)
