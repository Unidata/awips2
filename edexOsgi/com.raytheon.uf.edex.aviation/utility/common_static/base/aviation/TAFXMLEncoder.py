#
# TAFXMLEncoder.py
#
# Purpose: Encodes a python dictionary consisting of TAF components into a XML
#          document according to the IWXXM 2.1 schema.
#
# Author: Mark Oberfield MDL/OSTI/NWS/NOAA
#
# Date: 09 October 2016
#
#

#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/17/2017      6065          tgurney        Use IWXXM 2.1 schema
#    04/18/2017      6065          tgurney        Include previous valid time
#                                                 for AMD/COR tafs
#    12/11/2017      6551          tgurney        Move "NO AMD" statements to
#                                                 the end of the document
#
#    12/11/2017      6548          tgurney        Fix PROB30 encoding
#    12/13/2017      6550          tgurney        Fix XML previous report valid
#                                                 period
#

from __future__ import print_function

import difflib, re, time, uuid
import xml.etree.ElementTree as ET
#
NameSpaces = { 'aixm':'http://www.aixm.aero/schema/5.1.1',
               'gml':'http://www.opengis.net/gml/3.2',
               '':'http://icao.int/iwxxm/2.1',
               'om':'http://www.opengis.net/om/2.0',
               'sf':'http://www.opengis.net/sampling/2.0',
               'sams':'http://www.opengis.net/samplingSpatial/2.0',
               'metce':'http://def.wmo.int/metce/2013',
               'xlink':'http://www.w3.org/1999/xlink',
               'xsi':'http://www.w3.org/2001/XMLSchema-instance' }
#
# For US extension blocks
usTAFAmendmentParameters = {'None':{'href':'http://nws.weather.gov/codes/NWSI10-813/2012/AmendableTAFParameter/NONE/',
                                    'title':'No amendments will be issued'},
                            'CLD':{'href':'http://nws.weather.gov/codes/NWSI10-813/2012/AmendableTAFParameter/CEILING/',
                                   'title':'Amendments based on cloud ceilings will be issued'},
                            'VIS':{'href':'http://nws.weather.gov/codes/NWSI10-813/2012/AmendableTAFParameter/VISIBILITY/',
                                   'title':'Amendments based on horizontal visibility will be issued'},
                            'WIND':{'href':'http://nws.weather.gov/codes/NWSI10-813/2012/AmendableTAFParameter/WIND/',
                                    'title':'Amendments based on wind will be issued'},
                            'WX':{'href':'http://nws.weather.gov/codes/NWSI10-813/2012/AmendableTAFParameter/WEATHER/',
                                'title':'Amendments based on weather phenomenon will be issued'},
                            }

IWXXM_US_URI='http://nws.weather.gov/iwxxm-us/2.0'
IWXXM_US_URL='http://nws.weather.gov/schemas/IWXXM-US/2.0/Release/schemas/usTaf.xsd'
IWXXM_URL='http://icao.int/iwxxm/2.1 http://schemas.wmo.int/iwxxm/2.1/iwxxm.xsd'

_re_cloudLyr = re.compile(r'(?P<AMT>VV|SKC|CLR|FEW|SCT|BKN|OVC)(?P<HGT>(///|\d{3}))?')
#
_changeIndicator = {'BECMG':'BECOMING','TEMPO':'TEMPORARY_FLUCTUATIONS','PROB30':'PROBABILITY_30','PROB40':'PROBABILITY_40',
                    'PROB30 TEMPO':'PROBABILITY_30_TEMPORARY_FLUCTUATIONS','PROB40 TEMPO':'PROBABILITY_40_TEMPORARY_FLUCTUATIONS'}

_CldCvr = {'SKC':(0,'Clear'),'FEW':(1,'Few'),'SCT':(2,'Scattered'),'BKN':(3,'Broken'),'OVC':(4,'Overcast')}
_bbbCodes = {'A':'AMENDMENT','c':'CANCELLATION','C':'CORRECTION','N':'MISSING'}

def parseAndGetNameSpaces(fname,References=None):
    #
    if References is None:
        References = {}
    events = 'start','start-ns'
    root = None
    ns = {}
    requiredNS = References.keys()
    allLocations = [x[0] for x in References.values()]
    #
    for event, elem in ET.iterparse(fname,events):
        if event == 'start' and root == None:
            root = elem
            for prefix, uri in ns.items():
                if prefix not in ['','xs']:
                    try:
                        elem.set("xmlns:"+prefix,"http://%s" % difflib.get_close_matches(uri,allLocations,1)[0])
                    except IndexError:
                        elem.set("xmlns:"+prefix,uri)
                try:
                    requiredNS.pop(requiredNS.index(prefix))
                except ValueError:
                    pass
        #
        elif event == 'start-ns':
            if elem[0] in ns and ns[elem[0]] != elem[1]:
                #
                # NOTE: It is perfectly valid to have the same prefix refer
                #       to different URI namespaces in different parts of the
                #       document. This exception servers as a reminder that
                #       this solution is not robust.
                raise KeyError("Duplicate prefix with different URI found.")
            ns[elem[0]] = elem[1]
    #
    while True:
        try:
            key = requiredNS.pop()
            root.set("xmlns:%s" % key,"http://%s" % References[key][0])
        except IndexError:
            break
    #
    return ET.ElementTree(root),ns

def getGeography(geoFile):
    fh = open(geoFile,'r')
    d = {}

    for lne in fh:
        if lne.startswith('#'):
            continue

        key,lat,lon,elev,name,junk,junk=lne.split('|')
        d[key.strip()] = ('%s %s %s' % (lat.strip(),lon.strip(),elev.strip()),name.strip())

    return d

class XMLEncoder:
    def __init__(self,wwCodesFile='./data/ww.xml',tafStationInfoFile='./data/metarStationInfo.txt'):
        #
        # Populate METAR metadata dictionary
        self.tafMetaData = getGeography(tafStationInfoFile)
        #
        # Populate the precipitation/obstruction and other phenomenon dictionary
        root,wwCodeSpaces = parseAndGetNameSpaces(wwCodesFile)
        self.wwCodes = {}
        for concept in root.iter('{%s}Concept' % wwCodeSpaces.get('skos')):
            try:
                uri = concept.get('{%s}about' % wwCodeSpaces.get('rdf'))
                for elem in concept:
                    title = elem.text

                key = uri[uri.rfind('/')+1:]
                self.wwCodes[key] = dict([('uri',uri),('title',title)])

            except KeyError:
                pass
        #
        # map several token ids to a single function
        setattr(self,'obv',self.pcp)
        setattr(self,'vcnty', self.pcp)

    def __call__(self,decodedTaf,lastTafRangeStart,lastTafRangeEnd):
        self.lastTafRangeStart = lastTafRangeStart
        self.lastTafRangeEnd = lastTafRangeEnd

        # see if we have the metadata for the observation
        self.ident(decodedTaf['ident'])
        self.ICAOLatLonElev, self.ICAOName = self.tafMetaData[self.ICAOId]

        self.decodedTaf = decodedTaf
        self.doingUSTAF = False

        # Determine if US TAF.
        if self.ICAOId[0] in ['K','P'] or self.ICAOId[:2] == 'TJ':
            self.doingUSTAF = True

        # Root element
        self.XMLDocument = ET.Element('TAF')
        for prefix,uri in NameSpaces.items():
            if prefix == '':
                self.XMLDocument.set('xmlns',uri)
            else:
                self.XMLDocument.set('xmlns:%s' % prefix,uri)

        self.XMLDocument.set('xsi:schemaLocation', IWXXM_URL)

        bbb = self.decodedTaf['bbb']
        self.XMLDocument.set('status',_bbbCodes.get(bbb[0],'NORMAL'))
        self.XMLDocument.set('permissibleUsage','OPERATIONAL')

        self.doIt()

        xmlstring = ET.tostring(self.XMLDocument,encoding="utf-8",method="xml")
        return xmlstring.replace(' />','/>')

    def getUUID(self,prefix='uuid.'):
        return '%s%s' % (prefix,uuid.uuid4())

    def ident(self,ident):
        self.ICAOId = ident['str']

    def doIt(self):

        self.itime(self.XMLDocument,self.decodedTaf['itime'])
        #
        # No valid time for NIL TAF
        try:
            self.vtime(ET.SubElement(self.XMLDocument,'validTime'),self.decodedTaf['vtime'])
            self.entireValidTimeID = self.validTimeID

        except KeyError:
            rmv = self.XMLDocument[-1]
            self.XMLDocument.remove(rmv)
        #
        # Set the "base" forecast, which is the initial prevailing condition of the TAF
        try:
            base = self.decodedTaf['group'].pop(0)
            try:
                self.baseFcst(self.XMLDocument,base['prev'])
                self.changeGroup(self.XMLDocument,base['ocnl'])
            except KeyError:
                pass

        except IndexError:
            pass

        # Now the rest of the forecast "evolves" from the initial condition
        for group in self.decodedTaf['group']:
            self.changeGroup(self.XMLDocument,group['prev'])
            try:
                self.changeGroup(self.XMLDocument,group['ocnl'])
            except KeyError:
                pass

        # If the TAF was canceled (non-domestic TAFs only)
        if self.XMLDocument.get('status') == 'CANCELLATION':
            indent = ET.SubElement(self.XMLDocument,'previousReportAerodrome')
            self.vtime(ET.SubElement(indent,'previousReportValidPeriod'),
-                      self.decodedTaf['prevtime'])

        # If the TAF was amended or corrected, include the valid time
        # of the previous TAF issuance.
        elif self.XMLDocument.get('status') in ['AMENDMENT','CORRECTION']:
            previousTime = ET.SubElement(self.XMLDocument,'previousReportValidPeriod')
            try:
                if self.lastTafRangeStart and self.lastTafRangeEnd:
                    validPeriod =  {'from': self.lastTafRangeStart, 'to': self.lastTafRangeEnd}
                    self.vtime(previousTime, validPeriod)
                else:
                    previousTime.set('nilReason','unknown')
            except KeyError as e:
                previousTime.set('nilReason','unknown')

        # Find limits to amendments
        if self.doingUSTAF:
            try:
                extension = ET.Element('extension')
                self.amd(extension,self.decodedTaf['amd'])
                self.XMLDocument.append(extension)
            except KeyError:
                pass

    def itime(self,parent,token):

        value = token['value']
        parent.set('gml:id',self.getUUID())

        indent1 = ET.SubElement(parent,'issueTime')
        indent2 = ET.SubElement(indent1,'gml:TimeInstant')
        indent2.set('gml:id',self.getUUID())
        self.resultTimeTag = '#%s' % indent2.get('gml:id')

        indent3 = ET.SubElement(indent2,'gml:timePosition')
        indent3.text = time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime(value))

    def vtime(self,parent,token):

        indent = ET.SubElement(parent,'gml:TimePeriod')
        indent.set('gml:id',self.getUUID())

        indent1 = ET.SubElement(indent,'gml:beginPosition')
        indent1.text = time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime(token['from']))
        indent1 = ET.SubElement(indent,'gml:endPosition')
        indent1.text = time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime(token['to']))

        self.validTimeID = '#%s' % indent.get('gml:id')
    #
    def baseFcst(self,parent,token):
        #
        indent  = ET.SubElement(parent,'baseForecast')
        indent1 = ET.SubElement(indent,'om:OM_Observation')
        indent1.set('gml:id',self.getUUID())

        indent2 = ET.SubElement(indent1,'om:type')
        indent2.set('xlink:href','http://codes.wmo.int/49-2/observation-type/IWXXM/1.0/MeteorologicalAerodromeForecast')

        if self.decodedTaf['bbb'] == 'NIL':
            indent2 = ET.SubElement(indent1,'om:phenomenonTime')
            indent2.set('xlink:href',self.resultTimeTag)
        else:
            self.vtime(ET.SubElement(indent1,'om:phenomenonTime'),token['time'])

        indent2 = ET.SubElement(indent1,'om:resultTime')
        indent2.set('xlink:href',self.resultTimeTag)

        if self.decodedTaf['bbb'] != 'NIL':
            indent2 = ET.SubElement(indent1,'om:validTime')
            indent2.set('xlink:href',self.entireValidTimeID)

        indent2 = ET.SubElement(indent1,'om:procedure')
        indent3 = ET.SubElement(indent2,'metce:Process')
        indent3.set('gml:id',self.getUUID())
        self.procedureID = '#%s' % indent3.get('gml:id')

        indent4 = ET.SubElement(indent3,'gml:description')
        if self.doingUSTAF:
            indent4.text = 'United States National Weather Service Instruction 10-813 Terminal Aerodrome Forecasts'
        else:
            indent4.text = 'WMO No. 49 Volume 2 Meteorological Service for International Air Navigation APPENDIX 5 TECHNICAL SPECIFICATIONS RELATED TO FORECASTS'

        indent2 = ET.SubElement(indent1, 'om:observedProperty')
        indent2.set('xlink:href','http://codes.wmo.int/49-2/observable-property/MeteorologicalAerodromeForecast')
        indent2 = ET.SubElement(indent1, 'om:featureOfInterest')

        indent3 = ET.SubElement(indent2,'sams:SF_SpatialSamplingFeature')
        indent3.set('gml:id',self.getUUID())
        self.featureOfInterestID = '#%s' % indent3.get('gml:id')

        indent4 = ET.SubElement(indent3,'sf:type')
        indent4.set('xlink:href','http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint')

        indent4 = ET.SubElement(indent3,'sf:sampledFeature')

        indent5 = ET.SubElement(indent4,'aixm:AirportHeliport')
        indent5.set('gml:id',self.getUUID())

        indent6 = ET.SubElement(indent5,'aixm:timeSlice')
        indent7 = ET.SubElement(indent6,'aixm:AirportHeliportTimeSlice')
        indent7.set('gml:id',self.getUUID())

        indent8 = ET.SubElement(indent7,'gml:validTime')
        indent8.set('xlink:href',self.resultTimeTag)
        indent8 = ET.SubElement(indent7,'aixm:interpretation')
        indent8.text = 'SNAPSHOT'
        indent8 = ET.SubElement(indent7,'aixm:designator')
        indent8.text = self.ICAOId
        indent8 = ET.SubElement(indent7,'aixm:name')
        indent8.text = self.ICAOName
        indent8 = ET.SubElement(indent7,'aixm:locationIndicatorICAO')
        indent8.text = self.ICAOId

        indent4 = ET.SubElement(indent3,'sams:shape')
        indent5 = ET.SubElement(indent4,'gml:Point')
        indent5.set('gml:id',self.getUUID())
        indent5.set('uomLabels','deg deg m')
        indent5.set('axisLabels','lat lon altitude')
        indent5.set('srsName','urn:ogc:crs:EPSG::4979')
        indent6 = ET.SubElement(indent5,'gml:pos')
        indent6.text = self.ICAOLatLonElev
        #
        # Finally the "base" forecast
        self.result(indent1,token,True)

    def changeGroup(self,parent,fcsts):
        #
        if type(fcsts) == type({}):
            fcsts = [fcsts]

        for token in fcsts:
            indent = ET.SubElement(parent,'changeForecast')

            indent1 = ET.SubElement(indent,'om:OM_Observation')
            indent1.set('gml:id',self.getUUID())

            indent2 = ET.SubElement(indent1,'om:type')
            indent2.set('xlink:href','http://codes.wmo.int/49-2/observation-type/IWXXM/1.0/MeteorologicalAerodromeForecast')

            self.vtime(ET.SubElement(indent1,'om:phenomenonTime'),token['time'])

            indent2 = ET.SubElement(indent1,'om:resultTime')
            indent2.set('xlink:href',self.resultTimeTag)

            indent2 = ET.SubElement(indent1,'om:validTime')
            indent2.set('xlink:href',self.entireValidTimeID)

            indent2 = ET.SubElement(indent1,'om:procedure')
            indent2.set('xlink:href',self.procedureID)

            indent2 = ET.SubElement(indent1,'om:observedProperty')
            indent2.set('xlink:href','http://codes.wmo.int/49-2/observable-property/MeteorologicalAerodromeForecast')

            indent2 = ET.SubElement(indent1,'om:featureOfInterest')
            indent2.set('xlink:href',self.featureOfInterestID)

            self.result(indent1,token)
    #
    # Do <om:result>
    def result(self,parent,token,baseFcst=False):

        indent = ET.SubElement(parent,'om:result')
        if self.decodedTaf['bbb'] == 'NIL':
            indent.set('nilReason','missing')
            return

        indent = ET.SubElement(indent,'MeteorologicalAerodromeForecastRecord')
        indent.set('gml:id',self.getUUID())
        #
        indent.set('cloudAndVisibilityOK',token['cavok'])
        if token['cavok'] == 'true':
            self.ForecastResults = ['wind','temp']
        else:
            self.ForecastResults = ['vsby','wind','pcp','vcnty','obv','nsw','sky','temp']

        if not baseFcst:
            if token['type'] == 'PROB':
                changeToken = token['time']['str'].split()[0]
                indent.set('changeIndicator', _changeIndicator.get(changeToken,'PROBABILITY_30'))
            else:
                indent.set('changeIndicator', _changeIndicator.get(token['type'],'FROM'))

        indent.set('gml:id',self.getUUID())
        #
        for element in self.ForecastResults:
            function = getattr(self,element)
            try:
                function(indent,token[element])
            except KeyError:
                pass
        #
        # US NWS and Air Force TAFs may have additional forecast content which is recorded
        # in a <extension> block.
        #
        if self.doingUSTAF:

            MAFRE = ET.Element('MeteorologicalAerodromeForecastRecordExtension')
            MAFRE.set('xmlns',IWXXM_US_URI)
            MAFRE.set('xsi:schemaLocation','%s %s' % (IWXXM_US_URI,IWXXM_US_URL))
            for element in ['altim','volcanicAshLayer','peakdd','llws','icng','turb']:

                function = getattr(self,element)
                try:
                    function(MAFRE,token[element])
                except KeyError:
                    pass

            if len(MAFRE):
                extension = ET.SubElement(indent,'extension')
                extension.append(MAFRE)

    def wind(self,parent,token):

        if token['str'][:5] == '/////':
            return

        indent = ET.SubElement(parent,'surfaceWind')
        indent1 = ET.Element('AerodromeSurfaceWindForecast')
        if token['str'].startswith('VRB'):
            indent1.set('variableWindDirection','true')
        else:
            try:
                indent1.set('variableWindDirection','false')
                indent2 = ET.Element('meanWindDirection')
                indent2.text = token['dd']
                indent2.set('uom','deg')

            except KeyError:
                pass

            indent1.append(indent2)

        try:
            indent2 = ET.Element('meanWindSpeed')
            indent2.text = token['ff']
            indent2.set('uom',token['uom'])

        except KeyError:
            pass

        indent1.append(indent2)

        try:
            indent2 = ET.Element('windGustSpeed')
            indent2.text = token['gg']
            indent2.set('uom',token['uom'])
            indent1.append(indent2)

        except (KeyError,ValueError):
            pass

        if len(indent1):
            indent.append(indent1)

    def vsby(self,parent,token):

        indent = ET.SubElement(parent,'prevailingVisibility')
        indent.set('uom',token['uom'])
        indent.text = token['value']
        #
        # Visbility above 6SM (P6SM) or 10KM
        if (token['uom'] == '[mi_i]' and token['value'] == '7'):
            indent.text = '6'
            indent = ET.SubElement(parent,'prevailingVisibilityOperator')
            indent.text = 'ABOVE'
        elif token['value'] == '10000':
            indent = ET.SubElement(parent,'prevailingVisibilityOperator')
            indent.text = 'ABOVE'

    def pcp(self,parent,token):
        for ww in token['str'].split():
            #
            # Search BUFR table
            try:
                codes = self.wwCodes[ww]
                indent = ET.SubElement(parent,'weather')
                indent.set('xlink:href',codes['uri'])
                indent.set('xlink:title',codes['title'])
            #
            # Initial weather phenomenon token not matched
            except KeyError:
                self.wxrPhenomenonSearch(parent,ww)

    def wxrPhenomenonSearch(self,parent,ww):
        #
        # Split the weather string into two; both pieces must be found
        pos=-2
        ww1 = ww[:pos]
        ww2 = ww[pos:]

        while len(ww1) > 1:
            try:
                codes1 = self.wwCodes[ww1]
                codes2 = self.wwCodes[ww2]

                indent = ET.SubElement(parent,'weather')
                indent.set('xlink:href',codes1['uri'])
                indent.set('xlink:title',codes1['title'])

                indent = ET.SubElement(parent,'weather')
                indent.set('xlink:href',codes2['uri'])
                indent.set('xlink:title',codes2['title'])
                break

            except KeyError:

                pos -= 2
                ww1 = ww[:pos]
                ww2 = ww[pos:]

    def nsw(self,parent,ignored):

        indent = ET.SubElement(parent,'weather')
        indent.set('nilReason','http://codes.wmo.int/common/nil/nothingOfOperationalSignificance')

    def sky(self,parent,token):

        indent = ET.SubElement(parent,'cloud')
        for numberLyr, layer in enumerate(token['str'].split()):
            if layer[:2] == 'VV':
                try:
                    indent1 = ET.SubElement(indent,'AerodromeCloudForecast')
                    indent1.set('gml:id',self.getUUID())

                    height = int(layer[2:])*100
                    indent2 = ET.Element('verticalVisibility')
                    indent2.text = str(height)
                    indent2.set('uom','[ft_i]')
                    indent1.append(indent2)

                except ValueError:
                    parent.remove(indent)

            elif layer == 'NSC':
                indent.set('nilReason','http://codes.wmo.int/common/nil/nothingOfOperationalSignificance')

            else:
                if numberLyr == 0:
                    indent1 = ET.SubElement(indent,'AerodromeCloudForecast')
                    indent1.set('gml:id',self.getUUID())

                self.doCloudLayer(indent1,layer)

    def doCloudLayer(self,parent,layer):

        indent  = ET.SubElement(parent,'layer')
        indent1 = ET.SubElement(indent,'CloudLayer')
        desc = _re_cloudLyr.match(layer)

        try:
            amount = desc.group('AMT')
            indent2 = ET.Element('amount')
            indent2.set('xlink:href','http://codes.wmo.int/bufr4/codeflag/0-20-008/%d' % _CldCvr[amount][0])
            indent2.set('xlink:title', _CldCvr[amount][1])
            indent1.append(indent2)

        except TypeError:
            return

        indent2 = ET.SubElement(indent1,'base')
        indent2.set('uom','[ft_i]')

        try:
            height = int(desc.group('HGT'))*100
            indent2.text = str(height)

        except TypeError:
            if _CldCvr[amount][0] == 0:
                indent2.set('uom','N/A')
                indent2.set('xsi:nil','true')
                indent2.set('nilReason','inapplicable')

        if layer.endswith('CB'):
            indent2 = ET.SubElement(indent1,'cloudType')
            indent2.set('xlink:href','http://codes.wmo.int/bufr4/codeflag/0-20-012/9')
            indent2.set('xlink:title','Cumulonimbus')

        if layer.endswith('TCU'):
            indent2 = ET.SubElement(indent1,'cloudType')
            indent2.set('xlink:href','http://codes.wmo.int/bufr4/codeflag/0-20-012/32')
            indent2.set('xlink:title','Towering cumulus')

    def temp(self,parent,token):

        indent = ET.SubElement(parent,'temperature')
        indent1 = ET.SubElement(indent,'AerodromeAirTemperatureForecast')
        try:
            indent2 = ET.Element('maximumAirTemperature')
            maxt = token['max']
            indent2.text = str(maxt['value'])
            indent2.set('uom','Cel')
            indent1.append(indent2)

            indent2 = ET.Element('maximumAirTemperatureTime')
            indent3 = ET.SubElement(indent2,'gml:TimeInstant')
            indent3.set('gml:id',self.getUUID())
            indent4 = ET.SubElement(indent3,'gml:timePosition')
            indent4.text = time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime(maxt['at']))

            indent1.append(indent2)

        except KeyError:
            pass

        try:
            indent2 = ET.Element('minimumAirTemperature')
            mint = token['min']
            indent2.text = str(mint['value'])
            indent2.set('uom','Cel')
            indent1.append(indent2)

            indent2 = ET.Element('minimumAirTemperatureTime')
            indent3 = ET.SubElement(indent2,'gml:TimeInstant')
            indent3.set('gml:id',self.getUUID())
            indent4 = ET.SubElement(indent3,'gml:timePosition')
            indent4.text = time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime(mint['at']))
            indent1.append(indent2)

        except KeyError:
            pass
    #
    #
    # US extension codes
    def altim(self,parent,token):

        QNH = ET.SubElement(parent,'qnh')
        QNH.set('uom',token['uom'])
        QNH.text = token['value']

    def volcanicAshLayer(self,parent,token):
        self.layerAboveAerodrome(ET.SubElement(parent,'volcanicAshLayer'),token['upper'],token['lower'],token['uom'])

    def peakdd(self,parent,token):
        peakDD = ET.SubElement(parent,'peakWindGustDirection')
        peakDD.set('uom','deg')
        peakDD.text = token['value']

    def llws(self,parent,token):
        #
        NonConvectiveLLWS = ET.SubElement(parent,'nonConvectiveLLWS')

        LLWSDir = ET.SubElement(NonConvectiveLLWS, 'lowLevelWindShearWindDirection')
        LLWSSpd = ET.SubElement(NonConvectiveLLWS, 'lowLevelWindShearWindSpeed')
        self.layerAboveAerodrome(NonConvectiveLLWS,token['hgt'],'0','[ft_i]')
        LLWSDir.set('uom','deg')
        LLWSSpd.set('uom','[kn_i]')
        LLWSDir.text = token['dd']
        LLWSSpd.text = token['ff']

    def icng(self,parent,token):
        #
        if token['top'] != '0':
            IcingAboveAerodrome = ET.SubElement(parent,'icingAboveAerodrome')
            icingType = ET.SubElement(IcingAboveAerodrome,'icingType')
            icingType.set('xlink:href','http://nws.weather.gov/codes/NWSI10-813/2012/IcingType/%s' % token['type'])
            self.layerAboveAerodrome(IcingAboveAerodrome,token['top'],token['base'],token['uom'])

    def turb(self,parent,token):
        #
        if token['top'] != '0':
            TurbulenceAboveAerodrome = ET.SubElement(parent,'turbulenceAboveAerodrome')
            turbulenceType = ET.SubElement(TurbulenceAboveAerodrome,'turbulenceType')
            turbulenceType.set('xlink:href','http://nws.weather.gov/codes/NWSI10-813/2012/TurbulenceType/%s' % token['type'])
            self.layerAboveAerodrome(TurbulenceAboveAerodrome,token['top'],token['base'],token['uom'])

    def layerAboveAerodrome(self,parent,upper,lower,uom):
        #
        LayerAboveAerodrome = ET.SubElement(parent,'layerAboveAerodrome')
        lowerLimit = ET.SubElement(LayerAboveAerodrome,'lowerLimit')
        upperLimit = ET.SubElement(LayerAboveAerodrome,'upperLimit')

        lowerLimit.set('uom',uom)
        lowerLimit.text = lower
        upperLimit.set('uom',uom)
        upperLimit.text = upper

    def amd(self,parent,limits):

      TAFAmendmentLimitations = ET.SubElement(parent,'TAFAmendmentLimitations')
      TAFAmendmentLimitations.set('xmlns',IWXXM_US_URI)
      TAFAmendmentLimitations.set('xsi:schemaLocation','%s %s' % (IWXXM_US_URI,IWXXM_US_URL))

      periodOfLimitation = ET.SubElement(TAFAmendmentLimitations,'periodOfLimitation')
      periodOfLimitation.set('gml:id',self.getUUID())
      beginPosition = ET.SubElement(periodOfLimitation,'gml:beginPosition')
      beginPosition.text = time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime(limits['time']['from']))
      endPosition = ET.SubElement(periodOfLimitation,'gml:endPosition')
      endPosition.text = time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime(limits['time']['to']))

      if limits['str'].find('AMD NOT SKED') == 0:
          amdTAFParameter = ET.SubElement(TAFAmendmentLimitations,'amendableTAFParameter')
          amdTAFParameter.set('xlink:href',usTAFAmendmentParameters['None']['href'])
          amdTAFParameter.set('xlink:title',usTAFAmendmentParameters['None']['title'])
      else:
          for parameter in ['CLD','VIS','WIND','WX']:
              if limits['str'].find(parameter) > 0:
                  amdTAFParameter = ET.SubElement(TAFAmendmentLimitations,'amendableTAFParameter')
                  amdTAFParameter.set('xlink:href',usTAFAmendmentParameters[parameter]['href'])
                  amdTAFParameter.set('xlink:title',usTAFAmendmentParameters[parameter]['title'])

if __name__ == '__main__':

    def Usage():
        """Provide instructions and explanation of TAF encoder command line arguments"""

        d = argparse.ArgumentParser(description="Run the TAF encoder unit testing",
                                    epilog="""Encoder will wait for input on standard input for TAC.
                                    Pressing ^D (ETX) will send the raw text to the decoder. Corresponding
                                    XML document is written to standard output. Pressing ^C will
                                    end the unit testing.""",
                                    formatter_class=argparse.ArgumentDefaultsHelpFormatter)

        d.add_argument("--version", action="version", version="%(prog)s 1.0")
        #
        d.add_argument('-n','--namespaces', default=False, action='store_true',
                       help='XML encoder will include namespace declarations in root element')
        return d
    #
    # Process command line
    parser = Usage()
    cmdlne = parser.parse_args()

    decoder = TAFDecoder.Decoder()
    encoder = XMLEncoder()
    print('Press ^D (End of Text) to de/encode TAF. Press ^C to quit unit test.')
    #
    # While intr (^C) sequence not pressed . . .
    while True:
        try:
            #
            # Read input until ^D (ETX)
            taf = ''.join(sys.stdin.readlines())
            result = decoder(taf.replace('\n',''))

        except tpg.SyntacticError, e:
            print(taf)
            print(str(e))

        except KeyboardInterrupt:
            break
        #
        # Pass results to encoder. The first argument, the python dictionary,
        # is required. The other two arguments affect the resulting XML document.
        #
        try:
            encoder(result,nameSpaceDeclarations=cmdlne.namespaces,report=taf)
            encoder.printXML(sys.stdout,readable=True)

        except KeyError:
            print('%s not found in station directory file' % result['ident']['str'])
