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
#    Name:
#       ProbReader.py
#       GFS1-NHD:A10029.0000-SCRIPT;5
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision AWIPS II                           NJENSEN
#          Updated to work with h5py since pytables does not play well
#          in embedded interpreter.
#
#       Revision 5 (DELIVERED)
#         Created:  03-DEC-2007 09:29:23      OBERFIEL
#           Removed CR characters and corrected path to threshold
#           files.
#       
#       Revision 4 (DELIVERED)
#         Created:  30-NOV-2007 14:25:44      GILMOREDM
#           Changed to accommodate more efficient HDF files
#       
#       Revision 3 (DELIVERED)
#         Created:  17-NOV-2007 10:04:15      OBERFIEL
#           Minor formatting changes.
#       
#       Revision 2 (INITIALIZE)
#         Created:  17-NOV-2007 09:46:21      OBERFIEL
#           Added exception handle to make sure file descriptors
#           weren't left open. Added code to allow to be tested as a
#           stand-alone
#           module.  String building using '+' is to be avoided as much
#           as possible, especially in loops such as this.
#       
#       Revision 1 (REVIEW)
#         Created:  16-NOV-2007 13:11:21      OBERFIEL
#           Simple reader for GFSLAMP thresholds
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7376
#       	Action Date:       02-JUN-2008 20:44:53
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: AvnFPS data ingest server fails to start
#       
#
import h5py
import os
import time
import logging
import Avn

_Logger = logging.getLogger(Avn.CATEGORY)

ThresholdDir = 'etc'
try:
    from com.raytheon.uf.common.localization import PathManagerFactory
    from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType, LocalizationContext_LocalizationLevel as LocalizationLevel
    pathMgr = PathManagerFactory.getPathManager()
    ctx = pathMgr.getContext(LocalizationType.valueOf('CAVE_STATIC'), LocalizationLevel.valueOf('BASE'))
    ThresholdDir = pathMgr.getFile(ctx, os.path.join('aviation', 'thresholds')).getPath()               
except:
    _Logger.exception("Error determining AvnFPS thresholds directory")
    pass

def prob_reader(siteId,fDate):
    """Based on Julian day, get the appropriate thresholds for siteId"""

    def _translate_season(season):
        if season in ['cs','pcs','tcs']:
            return 0
        if season in ['ws','pws','tws']:
            return 1
        return 2

    def _season(jday,row):
        if (0 < jday <= row['COLD_SEASON_END']) or (row['COLD_SEASON_START'] < jday):
            return _translate_season(row['COLD_SEASON_NAME'])
        if (row['WARM_SEASON_START'] <= jday <= row['WARM_SEASON_END']):
            return _translate_season(row['WARM_SEASON_NAME'])
        return _translate_season(row['OTHER_SEASON_NAME'])
    
    def translateRow(dataset, row):
        rowdict = {}
        names = dataset.dtype.names
        for i in range(len(names)):
            rowdict[names[i]] = row[i]
        return rowdict 
    
    d = {}
    fday, fhour = fDate

    try:
        fh = None
        fh = h5py.File(os.path.join(ThresholdDir, siteId + '.hdf5'), 'r')        
        #determine season for each element
        seasons = {}        
        dataset = fh.get('Seasons', [])
        for row in dataset:            
            trow = translateRow(dataset, row)
            seasons[trow['ELEMENT']] = _season(fday,trow)
        table = fh.get("/c" + fhour + "Z", None) 
        colnames = []
        tablecols = table.dtype.names
        for name in tablecols:
            if name != 'PROJ':
                colnames.append(name)
        #loop through all the rows
        for row in table:
            trow = translateRow(table, row)
            #if the dictionary doesn't yet have a key for this hour, create one
            if not d.has_key(trow['PROJ']):
                d[trow['PROJ']] = {}
            for col in colnames:
                d[trow['PROJ']][col.lower()] = list(trow[col][:][seasons[col]])
    except:
        _Logger.error('Error reading hdf5 file: %s' % os.path.join(ThresholdDir, siteId + '.hdf5'))
    finally:
        if fh:
            fh.close()            

    return d
