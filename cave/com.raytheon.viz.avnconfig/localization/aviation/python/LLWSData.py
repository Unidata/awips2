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
#       LLWSData.py
#       GFS1-NHD:A8108.0000-SCRIPT;1.3
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.3 (DELIVERED)
#         Created:  09-OCT-2009 21:21:16      OBERFIEL
#           Downgraded log message severity to DEBUG when TAF LLWS file
#           is not found.
#
#       Revision 1.2 (DELIVERED)
#         Created:  07-MAY-2005 11:34:31      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.1 (DELIVERED)
#         Created:  02-NOV-2004 16:35:54      OBERFIEL
#           date and time created 11/02/04 16:35:42 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7430
#       	Action Date:       21-OCT-2009 08:03:43
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Incorrect file permission on ISH files
#       
#
#**
#* 
#* 
#* <pre>
#* SOFTWARE HISTORY
#* Date         Ticket#     Engineer    Description
#* ------------ ----------  ----------- --------------------------
#*                                      Initial creation.
#* Mar 25, 2013 1735        rferrel     Retrieve only the last 24 hours of acars records.
##  

from com.raytheon.viz.aviation.monitor import LlwsManager
import logging, os, time
import LLWSThread, Avn
import NoDataException

_Logger = logging.getLogger(Avn.CATEGORY)

#################################################################################
def readLLWS(ident):
    d = {}
    try:
        for line in file(os.path.join('data', 'llws', ident)):
            if not line.strip():
                break
            radarid, rpttime, value, wsstring = line.split()
            d[radarid] = {'time': float(rpttime), \
                          'value': float(value), 'str': wsstring}
        return d
    except IOError:
        _Logger.debug('Cannot access LLWS file for %s', ident)
    except (IndexError, ValueError):
        _Logger.error('Cannot parse LLWS file for %s', ident)
        
    return {}
        
def writeLLWS(ident, data):
    try:
        fp = file(os.path.join('data', 'llws', ident), 'w')
        for radarid in data:
            llws = data[radarid]
            fp.write('%s %.0f %.3f %s\n' % (radarid, llws['time'], \
                                            llws['value'], llws['str']))
        fp.close()
    except IOError:
        _Logger.error('Cannot write LLWS file for %s', ident)

def retrieve(siteID, info):
    from datetime import timedelta
    day = timedelta(days=1)
    secondsPerDay = day.total_seconds()
    msPerSecond = 1000.0
    th = LLWSThread.Server(info)
    t = 0
    d = {}
    try :
        th.processMetarData(siteID)
    except NoDataException.NoDataException:
        raise NoDataException.NoDataException("No METAR data available for site %s" % siteID)
    profilerIds = th.processProfilerData(siteID)
    for profilerId in profilerIds:
        try :
            shear = th.genShear(siteID, profilerId)
            d[profilerId] = shear
        except LLWSThread.InValid:
            pass
    
    # This gets all acarsRec in the database since 0 retrieves from the epoch.
    # This may be ok if database is purged frequently.
    # How far back should it go 1, 6, 12, 24 hours?
    #    acarsRec = LlwsManager.getAcarsRecord(siteID, 0)
    refTime = long((time.time() - secondsPerDay) * msPerSecond)
    acarsRec = LlwsManager.getAcarsRecord(siteID, refTime)
    if acarsRec:
        acarsId = siteID[1:]
        th.processAcarsData(acarsId,acarsRec)
        try:
            shear = th.genShear(siteID, acarsId)
            d[acarsId] = shear
        except LLWSThread.InValid:
            pass
    else:
        _Logger.info('Missing ACARS Sounding data for %s.', siteID)
    radars = info['sites']['radars']
    for radar in radars:
        vwp = LlwsManager.getVerticalWindProfile(radar, 0)
        if vwp.size() == 0:
            return None
        th.processRadarData(radar, vwp)
        try:
            shear = th.genShear(siteID, radar)
            d[radar] = shear
        except LLWSThread.InValid:
            pass
    return d
