##
##


#
# Java entry point to retrieve climate data through other python classes
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/15/09                      njensen       Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##


# NOTE: Since these are going through separate processes, please do not import
# anything globally in this file.  Import only inside methods.

def get_metars(id_, year, month, day, ndays, decoded, fname, queue):
    import MetarDisplay
    return MetarDisplay.get_metars(id_, year, month, day, ndays, decoded, fname, queue)

def get_windrose(month, end_month, flight_cat, id_, fname, configFile, queue):
    import WindRose
    wr = WindRose.WindRose()
    wr.get_data(month, end_month, flight_cat, id_, fname, configFile, queue)

def get_cvdata(id_, fname, queue):
    import cvdata
    cvdata.retrieveData(id_, fname, queue)

def get_cigvistrend_data(cigRange0, cigRange1, vsbyRange0, vsbyRange1, wndSpdRange0, wndSpdRange1,
                         wndDirRange0, wndDirRange1, hourRange0, hourRange1, dayRange0, dayRange1,
                         pcp, cur_hour, id_, hours, fname, queue):
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
    g.get_data(selectionDict, id_, hours, fname, queue)

def get_cigvistrend_metar(siteID):
    import CigVisTrend
    g = CigVisTrend.Gui()
    return g.get_metar(siteID)


    