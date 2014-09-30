#!/usr/bin/env python

##
# This script is a collection of utility function to be used for extracting PGEN 
# products from EDEX and to store PGEN activities to EDEX.
# 
# Users can override the default EDEX server and port name by specifying them
# in the $DEFAULT_HOST and $DEFAULT_PORT shell environment variables.
# 
##

import os
import re
import xml.etree.ElementTree as ET
import lib.CommHandler as CH

class ActivityUtil:
    
    #
    #  Sends a CatalogQuery to the EDEX uEngine to get a list of 
    #  PGEN Activity Types, Subtypes, Labels, refTimes, and associated 
    #  dataURIs in the pgen database tables.
    #
    def getActivityMap(self):
        script='''import CatalogQuery
query = CatalogQuery.CatalogQuery("pgen")
query.addReturnedField("activityType")
query.addReturnedField("activitySubtype")
query.addReturnedField("activityLabel")
query.addReturnedField("dataTime.refTime")
query.addReturnedField("activityName")
query.addReturnedField("dataURI")
return query.execute()'''

        service = '/services/pyproductjaxb'
        host = os.getenv("DEFAULT_HOST", "localhost")
        port = os.getenv("DEFAULT_PORT", "9581")
        connection=str(host+":"+port)
        ch = CH.CommHandler(connection,service)
        ch.process(script)

        if not ch.isGoodStatus():
            print ch.formatResponse()
            exit(1)

        return self.__generateMap( ch.getContents() )

    #
    #  Generates a map of activity types/subtypes, labels, refTimes, and dataURIs from 
    #  the XML returned from EDEX uEngine 
    #
    #  The map is a dictionary (dict) of Activity Types in form of "type(subtype)" whose values 
    #  are a list of dicts which have keys "activityType", "activityLabel", "dataTime.refTime",
    #  and "dataURI".
    #
    def __generateMap(self, xml):
        aMap = dict()
        tree = ET.fromstring(xml)
        for item in tree.iter('items'):
            record = dict()
            for attr in item.iter('attributes'):
                record.update( {attr.attrib['field'] : attr.attrib['value'] } )

            atype = record['activityType']
            stype = record['activitySubtype']
            if ( stype != None and len(stype.lstrip()) > 0):
                atype = atype + "(" + stype.lstrip() + ")"

            if aMap.has_key(atype):
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
                matched = False;
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
    #  This method sends a CatalogQuery request to the EDEX uEngine
    #  for the dataURI associated with the given activity type and label
    #
    def getDataURI( self, atype, label):

        script='''import CatalogQuery
query = CatalogQuery.CatalogQuery("pgen")
query.addConstraint("activityType","{0}","=")
query.addConstraint("activityLabel","{1}","=")
query.addReturnedField("dataURI")
query.addReturnedField("dataTime.refTime")
return query.execute()'''.format(atype,label)


        service = '/services/pyproductjaxb'
        host = os.getenv("DEFAULT_HOST", "localhost")
        port = os.getenv("DEFAULT_PORT", "9581")
        connection=str(host+":"+port)
        ch = CH.CommHandler(connection,service)
        ch.process(script)

        if not ch.isGoodStatus():
            print ch.formatResponse()
            exit(1)

        logger.debug( ch.getContents() )
        return __parseResponse( ch.getContents() )

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