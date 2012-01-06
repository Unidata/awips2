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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Interfaces.py
# Methods for interfacing to the Text Formatter infrastructure components.
#
# Author: hansen
# ----------------------------------------------------------------------------

import string, types
import Analysis
import Translator
import TextUtils
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
 
class Interfaces(TextUtils.TextUtils):
    def __init__(self):         
         TextUtils.TextUtils.__init__(self)     

    def getSampler(self, argDict, sampleInfo, sampleFromServer=0):
        # Get a HistoSampler given
        #   sampleInfo, which is a list of tuples, or just a single tuple
        #     of tuples ([elements], [periods], [areas])
        #   the elements are [(name, method)] -- basically the analysis list
        #   the periods [(timeRange, label)]
        #   areas [(name,label)] or [(refData, label)] or [(refID, label)]
        ifpClient = argDict["ifpClient"]
        databaseID = argDict["databaseID"]

        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID, ReferenceData
        from com.raytheon.viz.gfe.sampler import SamplerRequest, HistoSampler
        from java.util import ArrayList

        # convert input sampleInfo to samplerRequests
        samplerRequests = ArrayList()
        if type(sampleInfo) == tuple:
            sampleInfo = [sampleInfo]
        for si in sampleInfo:
            elements, periods, areas = si
            for e in elements:
                parmID = self.getParmID(e[0], databaseID)
                for p in periods:
                    for a in areas:
                        if type(a[0]) is str:
                            samplerRequests.add(SamplerRequest( \
                              parmID, ReferenceID(a[0]), p[0].toJavaObj()))
                        elif str(type(a[0])) == "<type 'PyJobject'>":
                            samplerRequests.add(SamplerRequest( \
                              parmID, a[0], p[0].toJavaObj()))
                        else:
                            raise Exception, "area specification incorrect"

        # do sampling
        if sampleFromServer:
            sampler = ifpClient.sampleRequest(samplerRequests)
        else:
            sampler = HistoSampler(ifpClient, samplerRequests)        
        if sampler.isValid() != 1:
            print "Cannot Sample: Check for invalid Weather Elements, ",\
              "Invalid Areas", str(samplerRequests)
            return None
        #print "sampler ", sampler
        return sampler

    # Interfaces to Analysis
    def getStatList(self, sampler, analysisList, timeRanges, editArea, componentName=None):
        # For each period, get Statistics specified in analysisList
        # over the Edit Area
        statList = []
        for timeRange, label in timeRanges:
            stats = self.getStatDict(
                sampler, analysisList, timeRange, editArea, componentName)
            statList.append(stats)
        return statList

    def getStatDict(self, sampler, analysisList, timeRange, area, componentName=None):
        # Get Statistic dictionary for given analysisList, timeRange,
        #     and area
        # Area can be ReferenceData or text string

        # Convert area to ID if necessary
        if area is None:
            return None
        if isinstance(area, str):
            area = ReferenceID(area)                
        else:
            area = area.getId()
            
        return Analysis.Analysis(sampler).createStats(
            analysisList, area, timeRange, componentName)

    def getStats(self, statDict, entry, argDict=None):
        if statDict is None:
            return None
        try:
            stats = statDict[entry]
        except:
            stats = None
            # Look for any temporal resolution e.g.
            # if entry = "Wind__vectorAvg",
            # return any temporal resolution for
            # vectorAvg
            if string.find(entry, "__") >= 0:
                for key in statDict.keys():
                    if string.find(key, entry) >= 0:
                        stats = statDict[key]                         
        #if stats is None:
            # Check for notification
        #    self.checkMissingData_notification(statDict, argDict, entry)                
        return stats        


    # Interfaces to TextFormatter
    def generateProduct(self, productName, argDict, area=None,
                         timeRange=None, elements=None, areaLabel="",
                         timeRangeName=""):
        # Generate the given product and return a text string
        # representing the results.
        # If areas are specified or a timeRange given, the
        # product will be generated accordingly.
        # Time Range is assumed to be an AFPS.TimeRange.
        # Time Range Name is the name of the time range, if available
        # Area is the name of the area over which to generate the
        #   product.  This need only be supplied if you are
        #   looping through areas.
        # Otherwise, this information is assumed to be included
        # in argDict.

        # Set up time range
        if timeRange is not None:
            argDict["timeRange"] = timeRange
            argDict["useRawTR"] = 1
            argDict["timeRangeName"] = timeRangeName
        # Set up area -- It must be a ReferenceID
        if area is not None:
            argDict["editAreas"] = [(area, areaLabel)] 
            if "areaList" in argDict.keys():
                #area = self.getEditArea(area, argDict)
                argDict["areaList"][0] = (area, areaLabel)

        # Reinitialize variables
        argDict["timePeriod"] = None
        argDict["language"] = None
        # Preserve information
        saveSelf = argDict["self"]
        combinations = argDict["combinations"]
        # Generate the forecast
        getForecast = argDict["getForecast"]
        fcst = getForecast(productName, argDict)

        # Restore information
        argDict["self"] = saveSelf
        argDict["combinations"] = combinations
        return fcst    

    # Interfaces to Translator
    def translateExpr(self, phrase, language):
        "Translate the time or heading"
        if language == "english" or phrase == "":
            return phrase
        trans = Translator.Translator(language)
        return trans.getExpression(phrase)

    def translateForecast(self, forecast, language):
        "Translate the forecast"
        if language == "english":
            return forecast
        trans = Translator.Translator(language)
        return trans.getForecast(forecast)
