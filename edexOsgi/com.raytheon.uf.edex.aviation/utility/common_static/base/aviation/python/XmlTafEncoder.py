#
# xmlTafEncoder.py
#
# Purpose: Encodes a python dictionary consisting of TAF components into a XML
#          document according to the IWXXM 3.0 TAF schema.
#
# Author: Mark Oberfield MDL/OSTI/NWS/NOAA
#
# Date: 5 April 2019
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# May 15, 2019  20693    mgamazaychikov  Initial Creation
# Aug 27, 2019  21545    mgamazaychikov  Minor bugs in XmlTafEncoder.encode()
# Sep 30, 2019  21615    mgamazaychikov  Fixed incorrect dictionary key in
#                                        XmlTafEncoder.write()
# Jan 29, 2020  21611    mgamazaychikov  Upgrade to version 3.4;
#                                        change write method to return command
#                                        line option-argument pairs for
#                                        msg_send command
# Apr 11, 2022  8846     randerso        Fix Python 3 issue and correct bug in
#                                        XmlTafEncoder.write()
# Apr 14, 2022  8846     randerso        Additional changes to ensure the xml
#                                        declaration is written to the file.
#
import io
import logging
import os
import re
import time
import uuid

import TafDecoder as TD
import UFStatusHandler
import XmlTafConfig as des
import xml.etree.ElementTree as ET

logHandler = UFStatusHandler.UFStatusHandler("com.raytheon.uf.edex.aviation", "EDEX")
_Logger = logging.getLogger("XmlTafEncoder")
_Logger.addHandler(logHandler)


class Encoder:
    """
    Encodes a python dictionary consisting of TAF components into a XML document
    according to the IWXXM/IWXXM-US 3.0 TAF schemas.
    """

    def __init__(self, codesFile=des.CodesFilePath):

        self._program_name = 'IWXXM TAF Encoder'
        self._description = 'To encode Terminal Aerodrome Forecast information in IWXXM %s format.' % des._iwxxm
        self._version = '3.4'  # Software version, not IWXXM schema version.
        self._annex3_amd = '78'
        #
        self.NameSpaces = {'aixm': 'http://www.aixm.aero/schema/5.1.1',
                           'gml': 'http://www.opengis.net/gml/3.2',
                           '': des.IWXXM_URI,
                           'xlink': 'http://www.w3.org/1999/xlink',
                           'xsi': 'http://www.w3.org/2001/XMLSchema-instance'}
        #
        # For US extension blocks
        self.usTAFAmendmentParameters = {'None': {'href': '%s/NONE' % des.US_TAF_CODE_REGISTRY_URL,
                                                  'title': 'No amendments will be issued'},
                                         'CLD': {'href': '%s/CEILING' % des.US_TAF_CODE_REGISTRY_URL,
                                                 'title': 'Amendments based on cloud ceilings will be issued'},
                                         'VIS': {'href': '%s/VISIBILITY' % des.US_TAF_CODE_REGISTRY_URL,
                                                 'title': 'Amendments based on horizontal visibility will be issued'},
                                         'WIND': {'href': '%s/WIND' % des.US_TAF_CODE_REGISTRY_URL,
                                                  'title': 'Amendments based on wind will be issued'},
                                         'WX': {'href': '%s/WEATHER' % des.US_TAF_CODE_REGISTRY_URL,
                                                'title': 'Amendments based on weather phenomenon will be issued'}}
        #
        self._re_cloudLyr = re.compile(r'(?P<AMT>VV|SKC|CLR|FEW|SCT|BKN|OVC)(?P<HGT>\d{3})?')

        #
        self._bbbCodes = {'A': 'AMENDMENT', 'C': 'CORRECTION'}
        self.ForecastResults = ['vsby', 'wind', 'pcp', 'vcnty', 'obv', 'nsw', 'sky', 'llws']
        #
        self.parseCodeRegistryTables(codesFile, des.PreferredLanguageForTitles)
        #
        # map several token ids to a single function
        setattr(self, 'obv', self.pcp)
        setattr(self, 'vcnty', self.pcp)

    def __call__(self, decodedTaf, tacString):
        """
        A decoded TAF is re-assembled into an IWXXM XML document.
            decodedTaf - TAF in python dictionary form which is obtained from AWIPS II TafDecoder.py
            tacString - Original alphanumeric code form of TAF obtained from AvnFPS TAF Editor
        """
        self.tacString = tacString
        #
        self.decodedTaf = decodedTaf
        self.decodingFailure = False
        self.iwxxmUSPrefix = False
        #
        # Root element
        self.XMLDocument = ET.Element('TAF')
        for prefix, uri in list(self.NameSpaces.items()):
            if prefix == '':
                self.XMLDocument.set('xmlns', uri)
            else:
                self.XMLDocument.set('xmlns:%s' % prefix, uri)
        #
        # Count how many non-Annex 3 elements found.
        if self.nonAnnexElementsCount() > 0:
            self.iwxxmUSPrefix = True
            self.XMLDocument.set('xmlns:%s' % 'iwxxm-us', des.IWXXM_US_URI)

        if self.iwxxmUSPrefix:
            self.XMLDocument.set('xsi:schemaLocation', '%s %s %s %s' %
                (des.IWXXM_URI, des.IWXXM_URL, des.IWXXM_US_URI, des.IWXXM_US_URL))
        else:
            self.XMLDocument.set('xsi:schemaLocation', '%s %s' % (des.IWXXM_URI, des.IWXXM_URL))

        self.XMLDocument.set('reportStatus', self._bbbCodes.get(self.decodedTaf['bbb'][0], 'NORMAL'))
        self.XMLDocument.set('permissibleUsage', 'OPERATIONAL')
        #
        # If there was a decoding problem, set the attributes to indicate this. However, this should
        # never happen in AWIPS provided the TAF Syntax Check works and is not overridden by forecaster.
        #
        if 'err_msg' in self.decodedTaf:

            self.XMLDocument.set('translationFailedTAC', self.tacString)
            self.XMLDocument.set('permissibleUsageSupplementary', self.decodedMetar.get('err_msg'))
            self.decodingFailure = True

        self.doIt()
        return self.XMLDocument

    def doIt(self):
        #
        # Issuance time and Aerodrome identifier should always be available
        self.itime(self.XMLDocument, self.decodedTaf['itime'])
        self.aerodrome(self.XMLDocument, self.decodedTaf['ident'])
        #
        try:
            self.vtime(ET.SubElement(self.XMLDocument, 'validPeriod'),
                       self.decodedTaf['vtime'])
            if self.decodingFailure:
                return
        #
        # No valid time for NIL TAF
        except KeyError:
            self.XMLDocument._children.pop()
        #
        # Set the "base" forecast, which is the initial prevailing condition of the TAF
        try:
            base = self.decodedTaf['group'].pop(0)
            try:
                self.baseFcst(self.XMLDocument, base['prev'])
                self.changeGroup(self.XMLDocument, base['ocnl'])

            except KeyError:
                pass
        #
        # There is no initial forecast if TAF NIL'd
        except IndexError:
            indent = ET.SubElement(self.XMLDocument, 'baseForecast')
            indent.set('nilReason', des.NIL_MSSG_URL)
        #
        # Now the rest of the forecast "evolves" from the initial condition
        for group in self.decodedTaf['group']:
            self.changeGroup(self.XMLDocument, group['prev'])
            try:
                self.changeGroup(self.XMLDocument, group['ocnl'])

            except KeyError:
                pass
        #
        # Find limits to amendments, if any
        try:
            extension = ET.Element('extension')
            self.amd(extension, self.decodedTaf['amd'])
            self.XMLDocument.append(extension)

        except KeyError:
            pass
        #
        # Option to provide URL to NWS Instruction on TAFs
        if des.NWSI10813_INSERTION:
            extension = ET.SubElement(self.XMLDocument, 'extension')
            if self.iwxxmUSPrefix:
                metaData = ET.SubElement(extension, 'iwxxm-us:USMetadata')
                procedure = ET.SubElement(metaData, 'iwxxm-us:procedure')
            else:
                metaData = ET.SubElement(extension, 'USMetadata')
                metaData.set('xmlns', des.IWXXM_US_URI)
                metaData.set('xsi:schemaLocation', '%s %s' % (des.IWXXM_US_URI, des.IWXXM_US_URL))
                procedure = ET.SubElement(metaData, 'procedure')

            procedure.set('xlink:href', des.NWSI10813_URL)
            procedure.set('xlink:title', des.NWSI10813_TITLE)

    def itime(self, parent, token):

        value = token['value']
        parent.set('gml:id', 'uuid.%s' % uuid.uuid4())

        indent1 = ET.SubElement(parent, 'issueTime')
        indent2 = ET.SubElement(indent1, 'gml:TimeInstant')
        indent2.set('gml:id', 'uuid.%s' % uuid.uuid4())

        indent3 = ET.SubElement(indent2, 'gml:timePosition')
        indent3.text = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(value))

    def aerodrome(self, parent, token):

        indent = ET.SubElement(parent, 'aerodrome')
        indent1 = ET.SubElement(indent, 'aixm:AirportHeliport')
        indent1.set('gml:id', 'uuid.%s' % uuid.uuid4())

        indent2 = ET.SubElement(indent1, 'aixm:timeSlice')
        indent3 = ET.SubElement(indent2, 'aixm:AirportHeliportTimeSlice')
        indent3.set('gml:id', 'uuid.%s' % uuid.uuid4())

        indent4 = ET.SubElement(indent3, 'gml:validTime')
        indent4 = ET.SubElement(indent3, 'aixm:interpretation')
        indent4.text = 'SNAPSHOT'

        indent4 = ET.SubElement(indent3, 'aixm:designator')
        indent4.text = token['str']

        try:
            indent4 = ET.Element('aixm:name')
            indent4.text = token['name'].strip().upper()
            if len(indent4.text):
                indent3.append(indent4)

        except KeyError:
            pass
        #
        indent4 = ET.SubElement(indent3, 'aixm:locationIndicatorICAO')
        indent4.text = token['str']

        try:
            indent4 = ET.Element('aixm:ARP')
            indent5 = ET.SubElement(indent4, 'aixm:ElevatedPoint')
            indent6 = ET.SubElement(indent5, 'gml:pos')
            indent6.text = ' '.join(token['location'].split(' ')[:2])
            indent5.set('srsDimension', des.srsDimension)
            indent5.set('srsName', des.srsName)
            indent5.set('axisLabels', 'Lat Long')
            indent5.set('gml:id', 'uuid.%s' % uuid.uuid4())
            #
            # If vertical datum information is known, then use it.
            if des.srsDimension == '3':
                try:
                    indent6 = ET.Element('aixm:elevation')
                    indent6.text = token['location'].split(' ')[2]
                    indent6.set('uom', des.elevationUOM)
                    indent5.append(indent6)

                    indent6 = ET.SubElement(indent5, 'aixm:verticalDatum')
                    indent6.text = des.verticalDatum

                except IndexError:
                    pass

            indent3.append(indent4)

        except KeyError:
            pass

    def vtime(self, parent, token):

        indent = ET.SubElement(parent, 'gml:TimePeriod')
        indent.set('gml:id', 'uuid.%s' % uuid.uuid4())

        indent1 = ET.SubElement(indent, 'gml:beginPosition')
        indent1.text = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(token['from']))
        indent1 = ET.SubElement(indent, 'gml:endPosition')
        indent1.text = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(token['to']))

    def baseFcst(self, parent, token):

        indent = ET.SubElement(parent, 'baseForecast')
        indent1 = ET.SubElement(indent, 'MeteorologicalAerodromeForecast')
        self.vtime(ET.SubElement(indent1, 'phenomenonTime'), token['time'])
        #
        # Finally the "base" forecast
        self.result(indent1, token, True)

    def changeGroup(self, parent, fcsts):

        if type(fcsts) == type({}):
            fcsts = [fcsts]

        for token in fcsts:
            indent = ET.SubElement(parent, 'changeForecast')
            indent1 = ET.SubElement(indent, 'MeteorologicalAerodromeForecast')
            self.vtime(ET.SubElement(indent1, 'phenomenonTime'), token['time'])
            self.result(indent1, token)

    def result(self, parent, token, baseFcst=False):

        parent.set('cloudAndVisibilityOK', 'false')
        if not baseFcst:
            if token['type'] == 'PROB':
                parent.set('changeIndicator', 'PROBABILITY_30')
            elif token['type'] == 'TEMPO':
                parent.set('changeIndicator', 'TEMPORARY_FLUCTUATIONS')
            else:
                parent.set('changeIndicator', 'FROM')

        parent.set('gml:id', 'uuid.%s' % uuid.uuid4())
        #
        for element in self.ForecastResults:
            function = getattr(self, element)
            try:
                function(parent, token[element])

            except KeyError:
                pass

    def wind(self, parent, token):

        indent = ET.SubElement(parent, 'surfaceWind')
        indent1 = ET.Element('AerodromeSurfaceWindForecast')
        if token['str'].startswith('VRB'):
            indent1.set('variableWindDirection', 'true')
        else:
            try:
                indent1.set('variableWindDirection', 'false')
                indent2 = ET.Element('meanWindDirection')
                indent2.text = str(token['dd'])
                indent2.set('uom', 'deg')

            except KeyError:
                pass

            indent1.append(indent2)

        try:
            indent2 = ET.Element('meanWindSpeed')
            indent2.text = str(token['ff'])
            indent2.set('uom', '[kn_i]')
            indent1.append(indent2)

        except KeyError:
            pass

        try:
            indent2 = ET.Element('windGustSpeed')
            indent2.text = str(token['gg'])
            indent2.set('uom', '[kn_i]')
            indent1.append(indent2)

        except KeyError:
            pass

        if len(indent1):
            indent.append(indent1)

    def vsby(self, parent, token):

        indent = ET.SubElement(parent, 'prevailingVisibility')
        indent.set('uom', 'm')
        indent.text = str(self.checkVisibility(token['value'], '[mi_i]'))
        #
        # Visbility above 6SM (P6SM)
        if token['value'] > 7:
            indent = ET.SubElement(parent, 'prevailingVisibilityOperator')
            indent.text = 'ABOVE'

    def pcp(self, parent, token):
        for ww in token['str'].split():
            #
            # Search BUFR table
            try:
                codes = self.codes[ww]
                indent = ET.SubElement(parent, 'weather')
                indent.set('xlink:href', codes[0])
                if (des.TITLES & des.Weather):
                    indent.set('xlink:title', codes[1])
            #
            # Initial weather phenomenon token not matched
            except KeyError:
                self.wxrPhenomenonSearch(parent, ww)

    def wxrPhenomenonSearch(self, parent, ww):
        #
        # Split the weather string into two; both pieces must be found
        pos = -2
        ww1 = ww[:pos]
        ww2 = ww[pos:]

        while len(ww1) > 1:
            try:
                codes1 = self.codes[ww1]
                codes2 = self.codes[ww2]

                indent = ET.SubElement(parent, 'weather')
                indent.set('xlink:href', codes1[0])
                if (des.TITLES & des.Weather):
                    indent.set('xlink:title', codes1[1])

                indent = ET.SubElement(parent, 'weather')
                indent.set('xlink:href', codes2[0])
                if (des.TITLES & des.Weather):
                    indent.set('xlink:title', codes2[1])
                break

            except KeyError:
                pos -= 2
                ww1 = ww[:pos]
                ww2 = ww[pos:]

    def nsw(self, parent, ignored):

        indent = ET.SubElement(parent, 'weather')
        indent.set('nilReason', des.NIL_NOOPRSIG_URL)

    def sky(self, parent, token):

        indent = ET.SubElement(parent, 'cloud')
        for numberLyr, layer in enumerate(token['str'].split()):
            if layer[:2] == 'VV':
                try:
                    indent1 = ET.SubElement(indent, 'AerodromeCloudForecast')
                    indent1.set('gml:id', 'uuid.%s' % uuid.uuid4())

                    height = int(layer[2:]) * 100
                    indent2 = ET.Element('verticalVisibility')
                    indent2.text = str(height)
                    indent2.set('uom', '[ft_i]')
                    indent1.append(indent2)

                except ValueError:
                    parent.remove(indent)

            elif layer == 'NSC':
                indent.set('nilReason', des.NIL_NOOPRSIG_URL)

            else:
                if numberLyr == 0:
                    indent1 = ET.SubElement(indent, 'AerodromeCloudForecast')
                    indent1.set('gml:id', 'uuid.%s' % uuid.uuid4())

                self.doCloudLayer(indent1, layer)

    def doCloudLayer(self, parent, layer):

        indent = ET.SubElement(parent, 'layer')
        indent1 = ET.SubElement(indent, 'CloudLayer')
        desc = self._re_cloudLyr.match(layer)

        try:
            amount = desc.group('AMT')
            indent2 = ET.Element('amount')
            indent2.set('xlink:href', '%s%s' % (des.CLDCVR_URL, amount))
            if (des.TITLES & des.CloudAmt):
                indent2.set('xlink:title', des.CldCvr[amount])
            indent1.append(indent2)

        except TypeError:
            return

        indent2 = ET.SubElement(indent1, 'base')
        indent2.set('uom', '[ft_i]')

        try:
            height = int(desc.group('HGT')) * 100
            indent2.text = str(height)

        except TypeError:
            if amount in ['CLR', 'SKC']:
                indent2.set('uom', 'N/A')
                indent2.set('xsi:nil', 'true')
                indent2.set('nilReason', des.NIL_NA_URL)

        if layer.endswith('CB'):
            indent2 = ET.SubElement(indent1, 'cloudType')
            indent2.set('xlink:href', des.CUMULONIMBUS)

        if layer.endswith('TCU'):
            indent2 = ET.SubElement(indent1, 'cloudType')
            indent2.set('xlink:href', des.TWRNGCUMULUS)

    def llws(self, parent, token):

        child = ET.SubElement(parent, 'extension')
        NonConvectiveLLWS = ET.SubElement(child, 'iwxxm-us:NonConvectiveLowLevelWindShear')

        LLWSDir = ET.SubElement(NonConvectiveLLWS, 'iwxxm-us:windDirection')
        LLWSSpd = ET.SubElement(NonConvectiveLLWS, 'iwxxm-us:windSpeed')
        self.layerAboveAerodrome(NonConvectiveLLWS, str(token['hgt'] * 100), '0', '[ft_i]')
        LLWSDir.set('uom', 'deg')
        LLWSSpd.set('uom', '[kn_i]')
        LLWSDir.text = str(token['dd'])
        LLWSSpd.text = str(token['ff'])

    def layerAboveAerodrome(self, parent, upper, lower, uom):

        child = ET.SubElement(parent, 'iwxxm-us:layerAboveAerodrome')
        lowerLimit = ET.SubElement(child, 'iwxxm-us:lowerLimit')
        upperLimit = ET.SubElement(child, 'iwxxm-us:upperLimit')

        lowerLimit.set('uom', uom)
        lowerLimit.text = lower
        upperLimit.set('uom', uom)
        upperLimit.text = upper

    def amd(self, parent, limits):
        #
        # Get references to time
        alist = []
        s = limits['str']
        _TimePhrase = '(AFT|TIL)\s+(\d{6})|(\d{4}/\d{4})'
        _AmdPat = re.compile(r'AMD\s+NOT\s+SKED(\s+(%s))?|AMD\s+LTD\s+TO(\s+(CLD|VIS|WX|AND|WIND)){1,5}(\s+(%s))?' % (_TimePhrase, _TimePhrase))
        tms = list(time.gmtime(self.decodedTaf['vtime']['from']))

        m = _AmdPat.match(s)
        if m:
            #
            # If reference to time is found in the AMD clause one of these
            # groups will have it.
            #
            timestr = m.group(4) or m.group(5) or m.group(11) or m.group(12)
            #
            # (AFT|TIL) DDHHMM clause
            if m.group(4) or m.group(11):

                tms[2:6] = int(timestr[:2]), int(timestr[2:4]), int(timestr[-2:]), 0
                self.fix_date(tms)

                if (m.group(3) or m.group(10)) == 'TIL':
                    limits['time'] = {'from': self.decodedTaf['itime']['value'], 'to': time.mktime(tuple(tms))}
                elif (m.group(3) or m.group(10)) == 'AFT':
                    limits['time'] = {'from': time.mktime(
                        tuple(tms)), 'to': self.decodedTaf['vtime']['to']}
            #
            # The D1H1/D2H2 case
            elif m.group(5) or m.group(12):

                for key, timestr in zip(['from', 'to'], timestr.split('/')):
                    tms[2:6] = int(timestr[0:2]), int(timestr[2:4]), 0, 0
                    self.fix_date(tms)
                    alist.append((key, time.mktime(tuple(tms))))

                limits['time'] = dict(alist)
            #
            # If no reference to time, then its the entire time period of the TAF
            else:
                limits['time'] = self.decodedTaf['vtime'].copy()
                limits['time']['from'] = self.decodedTaf['itime']['value']

        TAFAmendmentLimitations = ET.SubElement(parent, 'iwxxm-us:TAFAmendmentLimitations')

        if limits['str'].find('AMD NOT SKED') == 0:
            amdTAFParameter = ET.SubElement(TAFAmendmentLimitations, 'iwxxm-us:amendableTAFParameter')
            amdTAFParameter.set('xlink:href', self.usTAFAmendmentParameters['None']['href'])
        else:
            for parameter in ['CLD', 'VIS', 'WIND', 'WX']:
                if limits['str'].find(parameter) > 0:
                    amdTAFParameter = ET.SubElement(TAFAmendmentLimitations, 'iwxxm-us:amendableTAFParameter')
                    amdTAFParameter.set('xlink:href', self.usTAFAmendmentParameters[parameter]['href'])

        periodOfLimitation = ET.SubElement(TAFAmendmentLimitations, 'iwxxm-us:periodOfLimitation')
        periodOfLimitation.set('gml:id', 'uuid.%s' % uuid.uuid4())
        beginPosition = ET.SubElement(periodOfLimitation, 'gml:beginPosition')
        beginPosition.text = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(limits['time']['from']))
        endPosition = ET.SubElement(periodOfLimitation, 'gml:endPosition')
        endPosition.text = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(limits['time']['to']))

    def nonAnnexElementsCount(self):

        count = 0
        if 'amd' in self.decodedTaf:
            count = 1

        for g in self.decodedTaf['group']:
            if 'llws' in g['prev']:
                count += 1

        return count

    def parseCodeRegistryTables(self, fname, preferredLanguage='en'):

        events = 'start', 'start-ns'
        top = None
        nameSpaces = {'xml':'http://www.w3.org/XML/1998/namespace'}
        self.codes = {}
        neededNS = ['ldp', 'skos', 'rdf', 'rdfs']
        #
        for event, elem in ET.iterparse(fname, events):
            if event == 'start' and top == None:
                top = elem
            elif neededNS and event == 'start-ns':
                if elem[0] in neededNS:
                    nameSpaces[elem[0]] = elem[1]
                    neededNS.remove(elem[0])
        #
        # Now that we have the required namespaces for searches
        Containers = '{%s}Container' % nameSpaces.get('ldp')
        Concept = '{%s}Concept' % nameSpaces.get('skos')
        about = '{%s}about' % nameSpaces.get('rdf')
        label = '{%s}label[@{%s}lang="%s"]' % (nameSpaces.get('rdfs'), nameSpaces.get('xml'), preferredLanguage)
        enlabel = '{%s}label[@{%s}lang="%s"]' % (nameSpaces.get('rdfs'), nameSpaces.get('xml'), 'en')
        nolang = '{%s}label' % nameSpaces.get('rdfs')

        root = ET.ElementTree(top)
        for container in root.iter(Containers):
            uri = container.get(about)
            for concept in container.iter(Concept):
                try:
                    uri = concept.get(about)
                    key = uri[uri.rfind('/') + 1:]
                    text = ''
                    try:
                        text = concept.find(label).text
                    except AttributeError:
                        if preferredLanguage != 'en':
                            text = concept.find(enlabel).text
                        else:
                            text = concept.find(nolang).text
                    finally:
                        self.codes[key] = (uri, text)
                except AttributeError:
                    pass

    #
    # Returns values (in meters) according to Annex 3 Amd 77
    def checkVisibility(self, value, uom='m'):

        if type(value) == type(''):

            def function(x):
                return str(x)

            value = float(value)
        else:

            def function(x):
                return int(x)

        if uom == '[mi_i]':
            value *= 1609.34
        elif uom == '[ft_i]':
            value *= 0.3048

        mod = 1
        value = int(value)
        if value < 800:
            mod = 50
        elif 800 <= value < 5000:
            mod = 100
        elif value < 9999:
            mod = 1000
        else:
            value = 10000

        return function(value - (value % mod))

    def fix_date(self, tms):

        now = time.time()
        t = time.mktime(tuple(tms))
        if t > now + 86400.0: # previous month
            if tms[1] > 1:
                tms[1] -= 1
            else:
                tms[1] = 12
                tms[0] -= 1

        elif t < now - 25 * 86400.0: # next month
            if tms[1] < 12:
                tms[1] += 1
            else:
                tms[1] = 1
                tms[0] += 1


class XmlTafEncoder():
    """
    Constructs MeteorologicalBulletins consisting of one or more IWXXM TAF documents
    having the same WMO Abbreviated Header Line (AHL)
    """

    def __init__(self):
        #
        # Initialize the TAF decoder and encoder
        self.encoder = Encoder()
        self.decoder = TD.Decoder()
        #
        # Cache of IWXXM documents grouped together by AHL
        self.docs = {}
        #
        # Regular expression to determine type of issuance:
        #   routine, amended, corrected, or routinely delayed
        self.prefix = re.compile(r'TAF(\s+([ACR][A-Z]{2}))?\s+[KPT]\w{3}\s+\d{6}Z')
    #
    # Convert TAF TAC to IWXXM XML

    def encode(self, tac, ahl, geolocation):

        """
        The traditional alphanumeric (TAC) text is converted into IWXXM form and cached for
        later retrieval
            tac = TAF text
            ahl = Abbreviated Header Line, consisting of 'LTAAii CCCC YYGG00 BBB'
            geolocation = string with the latitude, longitude and elevation of the airport
                          reference point (ARP) separated by spaces.

            latitude and longitude are floating point numbers, e.g  30' (minutes) == 0.5
            elevation shall be specified in meters w.r.t a known vertical datum (see
            xmlConfig.py) for allowed values.
        """
        #
        # The TAF decoder needs to know the BBB code
        try:
            bbb = ahl.split(' ')[3]
            if bbb[0] == '_':
                bbb = ' '
                #
                # Remove the underscore BBB code from the key
                ahl = ' '.join(ahl.split(' ')[:3])

        except IndexError:
            bbb = ' '

        re_start = self.prefix.search(tac)
        #
        # Call AWIPS II AvnFPS TAF Decoder
        try:
            tafDictionary = self.decoder(tac[re_start.start():], bbb)
            tafDictionary['ident']['location'] = geolocation
        #
        # If decoding the TAF failed (should never happen)
        except KeyError:
            _Logger.exception('Unable to decode TAF:\n%s', tac)
        #
        # Build the XML TAF document and place into the cache.
        self.docs.setdefault(ahl, []).append(self.encoder(tafDictionary, tac))

    def write(self):

        """
        Return <MeteorologicalBulletin> documents until cache of unique 'AHL'
        codes is exhausted.
        """
        import JUtil
        #
        # Check to see if there's anything to write to the file. If not, log and return
        #
        if not self.docs:
            _Logger.info('No more MeteorologicalBulletin messages to send.')
            return None
        #
        # The IWXXM TAF product(s) needs to be wrapped up in a Meteorological Bulletin "envelope".
        #
        # Construct the root element
        bulletin = ET.Element('MeteorologicalBulletin')
        bulletin.set('xmlns', 'http://def.wmo.int/collect/2014')
        bulletin.set('xmlns:gml', 'http://www.opengis.net/gml/3.2')
        bulletin.set('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance')
        bulletin.set('xsi:schemaLocation',
                     'http://def.wmo.int/collect/2014 http://schemas.wmo.int/collect/1.2/collect.xsd')
        bulletin.set('gml:id', 'uuid.%s' % uuid.uuid4())
        #
        # For each write() call, a unique AHL IWXXM XML bulletin is made
        keys = list(self.docs.keys())
        try:
            self.ahl = keys.pop()

        except IndexError:
            return None
        #
        # Construct the WMO AHL Line
        try:
            ttaaii, cccc, yygg, bbb = self.ahl.split(' ')

        except ValueError:
            ttaaii, cccc, yygg = self.ahl.strip().split(' ')
            bbb = ''

        if bbb == '':
            _Logger.info('Sending %d routinely issued TAFs in a MeteorologicalBulletin.', len(self.docs[self.ahl]))
        else:
            _Logger.info('Sending %d %s (%s) TAFs in a MeteorologicalBulletin',
                             len(self.docs[self.ahl]),
                             { 'A': 'amended',
                               'C': 'corrected',
                               'R': 'routinely delayed'
                             }.get(bbb[0]),
                             bbb
                         )
        #
        while True:
            try:
                iwxxm = self.docs[self.ahl].pop()
                child = ET.SubElement(bulletin, 'meteorologicalInformation')
                child.append(iwxxm)

            except IndexError:
                del self.docs[self.ahl]
                break
        #
        # Construct the full day/time stamp for the product
        child = ET.SubElement(bulletin, 'bulletinIdentifier')
        #
        # Construct the name of the bulletin
        child.text = 'A_%s%s%s%s_C_%s_%s.xml' % (ttaaii, cccc, yygg, bbb, cccc, time.strftime('%Y%m%d%H%M%S'))
        #
        # Serialize
        tree = ET.ElementTree(element=bulletin)
        xmlBytes = io.BytesIO()
        tree.write(xmlBytes, encoding="UTF-8", xml_declaration=True, method="xml", short_empty_elements=True)
        xmlBytes = xmlBytes.getvalue().replace(' />'.encode("UTF-8"), '/>'.encode("UTF-8"))
        #
        # Write XML document to AWIPS outgoing directory
        filename = os.path.join(des.MHS_OUTPUT_FULL_PATH_DIR, child.text)
        try:
            with open(filename, 'wb') as _fh:
                _fh.write(f"{self.ahl}\n".encode("UTF-8"))
                _fh.write(xmlBytes)

        except OSError:
            _Logger.exception("Error writing XML TAF document to %s", filename)
            return None

        mhsI = "-i%s" % (self.ahl)
        mhsE = "-e%s" % (filename)
        mhsArgs = [des.MHS_CODE, des.MHS_SUBJECT, des.MHS_ADDRESSEE, des.MHS_PRIORITY, mhsI, mhsE]
        return JUtil.pyValToJavaObj(mhsArgs)

    def getAHL(self):

        """Return the unique AHL line"""

        return self.ahl

