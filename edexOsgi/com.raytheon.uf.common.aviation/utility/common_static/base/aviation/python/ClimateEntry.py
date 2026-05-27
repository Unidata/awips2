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
# Java entry point to climate python modules.
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/17/09                      avarani        Initial Creation.
#    01/26/2016      18395         zhao           Modified process()
#    Jun 16, 2016    5693          rferrel        Added fixCLimateFiles().
#    Aug  6, 2016    7878          tgurney        Combine with ClimateDataEntry.py.
#                                                 Fix for Python 3.
#
#
#

##
# This is a base file that is not intended to be overridden.
##


# NOTE: Since these are going through separate processes, please do not import
# anything globally in this file.  Import only inside methods.

def get_metars(id_, year, month, day, ndays, decoded, fname, listener):
    import MetarDisplay
    return MetarDisplay.get_metars(id_, year, month, day, ndays, decoded, fname, listener)

def get_windrose(month, end_month, flight_cat, id_, fname, configFile, listener):
    import WindRose
    wr = WindRose.WindRose()
    wr.get_data(month, end_month, flight_cat, id_, fname, configFile, listener)

def get_cvdata(id_, fname, listener):
    import cvdata
    cvdata.retrieveData(id_, fname, listener)

def get_cigvistrend_data(cigRange0, cigRange1, vsbyRange0, vsbyRange1, wndSpdRange0, wndSpdRange1,
                         wndDirRange0, wndDirRange1, hourRange0, hourRange1, dayRange0, dayRange1,
                         pcp, cur_hour, id_, hours, fname, listener):
    selectionDict = {}
    selectionDict['cig'] = [cigRange0, cigRange1]
    selectionDict['vsby'] = [vsbyRange0, vsbyRange1]
    selectionDict['wind_speed'] = [wndSpdRange0, wndSpdRange1]
    selectionDict['wind_dir'] = [wndDirRange0, wndDirRange1]
    selectionDict['hour'] = [hourRange0, hourRange1]
    selectionDict['yday'] = [dayRange0, dayRange1]
    selectionDict['pcp'] = pcp
    selectionDict['cur_hour'] = cur_hour
    import CigVisTrend
    g = CigVisTrend.Gui()
    g.get_data(selectionDict, id_, hours, fname, listener)

def get_cigvistrend_metar(siteID, listener):
    import CigVisTrend
    g = CigVisTrend.Gui()
    metar = g.get_metar(siteID)
    listener.sendObj(metar)

def get_id_nums(idents, ishDirPath, climateFilePaths, listener):
    import ClimateDataUtils as cdutils
    for ident, climateFilePath in zip(idents, climateFilePaths):
        results = cdutils.getIdnumsList(ishDirPath, climateFilePath, listener, ident)
        returnObj = {'method': 'get_id_nums', 'ident': ident, 'results': results}
        listener.sendObj(returnObj)

def get_stations_map(listener, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(listener, stnPickle)
    cdupdate.getStationsMap()

# Assess Data button
def start(listener, append, sites, climateDir):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(listener, climateDir = climateDir)
    cdupdate.assessData(append = append, sites = sites)


# Process Data button
def process(listener, stnPickle, append, sites, climateDir):
    import ClimateDataUpdate
    import JUtil
    import pickle
    o = JUtil.javaUnpickle(stnPickle)
    if set(sites) == set(o['sites']):
        cdupdate = ClimateDataUpdate.ClimateDataUpdate(listener, stnPickle)
        cdupdate.assessData(bypass = True)
    else:
        cdupdate = ClimateDataUpdate.ClimateDataUpdate(listener, climateDir = climateDir)
        cdupdate.assessData(append = append, sites = sites)

# Validate button
def validate(listener, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(listener, stnPickle)
    cdupdate.validate()

# Commit button
def commit(listener, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(listener, stnPickle)
    cdupdate.commit()

# QC file generation part of the commit action.
def genQCFiles(siteList, monthList, climateDir, listener):
    import avnqcstats
    avnqcstats.genFiles(siteList, monthList, climateDir, listener)

# Reject button
def reject(listener, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(listener, stnPickle)
    cdupdate.reject()

# Clean up old state
def kill(listener, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(listener, stnPickle)
    cdupdate.kill()

def fixClimateFiles(listener, sites, climateDir):
    import fixClimateFiles
    fixClimateFiles.fixClimateFiles(sites, climateDir)
