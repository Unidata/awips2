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
# Java entry point to the ClimateData* python modules.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/17/09                      avarani        Initial Creation.
#    01/26/2016      18395         zhao           Modified process()
#    
# 
#

def get_id_nums(ident, ishDirPath, climateFilePath, queue):
    import ClimateDataUtils as cdutils
    results = cdutils.getIdnumsList(ishDirPath, climateFilePath, ident)
    returnObj = {'method': 'get_id_nums', 'ident': ident, 'results': results}
    queue.put(returnObj)

def get_stations_map(queue, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(queue, stnPickle)
    map = cdupdate.getStationsMap()

# Assess Data button
def start(queue, append, sites, climateDir):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(queue, climateDir = climateDir)
    cdupdate.assessData(append = append, sites = sites)
    #stop()
    #mythread = cdupdate.ClimateDataUpdate(append, sites, climateDir, queue, name="Climate Data Update Thread")
    #mythread.start()

# Process Data button
def process(queue, stnPickle, append, sites, climateDir):
    import ClimateDataUpdate
    import cPickle as pickle
    o = pickle.loads(stnPickle)
    if set(sites) == set(o['sites']):
        cdupdate = ClimateDataUpdate.ClimateDataUpdate(queue, stnPickle)
        cdupdate.assessData(bypass = True)
    else:
        cdupdate = ClimateDataUpdate.ClimateDataUpdate(queue, climateDir = climateDir)
        cdupdate.assessData(append = append, sites = sites)
    # def __continue():
    #    if mythread:
    #        mythread.bypass = True
    #    else:
    #        start()
    #        mythread.bypass = True

# Validate button
def validate(queue, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(queue, stnPickle)
    cdupdate.validate()

# Commit button
def commit(queue, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(queue, stnPickle)
    cdupdate.commit()

# QC file generation part of the commit action.
def genQCFiles(siteList, monthList, climateDir, queue):
    import avnqcstats
    avnqcstats.genFiles(siteList, monthList, climateDir)
    
# Reject button
def reject(queue, stnPickle):
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(queue, stnPickle)
    cdupdate.reject()
    
# Clean up old state
def kill(queue, stnPickle) :
    import ClimateDataUpdate
    cdupdate = ClimateDataUpdate.ClimateDataUpdate(queue, stnPickle)
    cdupdate.kill()
