#!/awips2/python/bin/python3

##
# This script is a collection of utility function to be used for extracting PGEN
# products from EDEX and to store PGEN activities to EDEX.
#
# Users can override the default EDEX server and port name by specifying them
# in the $DEFAULT_HOST and $DEFAULT_PORT shell environment variables.
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# 05/16/2016    R9714     byin        Replaced micro-engine with thrift client
##

import os
import re
import xml.etree.ElementTree as ET
from ufpy import ThriftClient
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.pgen.request import RetrieveActivityMapRequest

class ActivityUtil:

    #
    #  Sends a RetrieveActivityMapRequest to the EDEX thrift to get a list of
    #  PGEN Activity Types, Subtypes, Labels, refTimes, and associated
    #  dataURIs in the pgen database tables.
    #
    def getActivityMap(self):
        host = os.getenv("DEFAULT_HOST", "localhost")
        port = os.getenv("DEFAULT_PORT", "9581")

        request = RetrieveActivityMapRequest()
        response = None

        try:
            thriftClient = ThriftClient.ThriftClient(host, port)
            response = thriftClient.sendRequest(request)
        except Exception as e:
            print(e)
            exit(1)

        if response is None:
            print("ThriftClient returns None!")
            exit(1)
        else:
            return self.__generateMap(response)

    #
    #  Generates a map of activity types/subtypes, labels, refTimes, and dataURIs from
    #  the response returned from EDEX thrift
    #
    #  The map is a dictionary (dict) of Activity Types in form of "type(subtype)" whose values
    #  are a list of dicts which have keys "activityType", "activityLabel", "dataTime.refTime",
    #  and "dataURI".
    #
    def __generateMap(self, resp):
        aMap = dict()
        for item in resp.getData():
            record = dict()
            record.update( { 'dataURI' : item.getDataURI() } )
            record.update( { 'activityName' : item.getActivityName() } )
            record.update( { 'activitySubtype' : item.getActivitySubtype() } )
            record.update( { 'dataTime.refTime' : item.getRefTime() } )
            record.update( { 'activityLabel' : item.getActivityLabel() } )
            record.update( { 'activityType' : item.getActivityType() } )

            atype = record['activityType']
            stype = record['activitySubtype']

            if ( stype != None and len(stype.lstrip()) > 0):
                atype = atype + "(" + stype.lstrip() + ")"

            if atype in aMap:
                aMap[atype].append(record)
            else:
                aMap.update( {atype: [record]} )

        return aMap

    #
    #  Compare if a command line string matches an string in activity.
    #  This uses string methods.
    #
    def matcher(self, cmdstr, activitystr):

        matched = False
        if cmdstr == None:
            matched = True
        else:
            if activitystr == None:
                matched = False
            else:
                realstr = cmdstr.strip("*")
                if ( cmdstr.startswith("*") ):
                    if ( cmdstr.endswith("*") ):
                        if ( activitystr.find( realstr ) >= 0 ):
                            matched = True
                    else:
                        if activitystr.endswith( realstr):
                            matched = True
                elif cmdstr.endswith("*"):
                    if activitystr.startswith( realstr):
                        matched = True
                else:
                    if ( activitystr == cmdstr ):
                        matched = True

        return matched

    #
    #  Compare if a command line string matches an string in activity.
    #  This uses regular expression matching.
    #
    #  cmdstr - input from command line, could use "*" anywhere to match one or more character.
    #  activitystr - value saved in PGEN DB for an activity, such as type, label, ...
    #
    def stringMatcher(self, cmdstr, activitystr):

        matched = False

        if cmdstr == None:
            matched = True
        elif activitystr == None:
            matched = False
        else:
            #parenthese should be escaped.
            ps = cmdstr.replace("(", "\(")
            pe = ps.replace(")", "\)")

            # "*" could match any one or more characters.
            pn = pe.replace("*", "(.*)")

            mb = re.match(pn, activitystr)
            if mb != None:
                matched = True
        return matched

    #
    #  Parses the XML response from the uEngine and extracts
    #  the value for the dataURI field.  If multiple are returned, the last
    #  one is used.
    #
    def __parseResponse(self, xml):
        tree = ET.fromstring(xml)
        for attr in tree.iter('attributes'):
            if attr.attrib['field'] == 'dataURI':
                duri = attr.attrib['value']

        return duri
