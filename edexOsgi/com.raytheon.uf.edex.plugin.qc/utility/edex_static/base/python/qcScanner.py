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

import glob
import os
import os.path
import sys
import time

import pupynere as netcdf

from java.lang import Integer
from java.util import ArrayList
from java.util import Date
from com.raytheon.uf.edex.database.query import DatabaseQuery
from com.raytheon.uf.common.dataplugin.qc import QCRecord
from com.raytheon.uf.common.pointdata.spatial import SurfaceObsLocation
from com.raytheon.uf.common.time import DataTime
from com.raytheon.uf.edex.database.plugin import PluginFactory

import logging, UFStatusHandler

_logger = logging.getLogger("QCScanner")
_logger.addHandler(UFStatusHandler.UFStatusHandler("com.raytheon.uf.edex.plugin.qc", "QCScanner", level=logging.INFO))
_logger.setLevel(logging.INFO)


# TODO: use jep.jarray

class NcSet(object):
    def __init__(self, path, qcType):
        self.path = path
        self.lastModTime = None
        self.qcType = qcType
        max_index = qcDao.getMaxRecordIndex(os.path.basename(path))
        if max_index >= 0:
            self.lastRecordCount = max_index + 1
        else:
            self.lastRecordCount = None

    def incrementalScan(self, max_records = None):
        try:
            last_mod_time = os.stat(self.path).st_mtime
        except Exception, e:
            _logger.error("%s: %s", self.path, e)
            return
        if last_mod_time == self.lastModTime:
            return
        self.lastModTime = last_mod_time
        try:
            f = netcdf.NetCDFFile(self.path, 'r')
        except Exception, e:
            _logger.error("%s: %s", self.path, e, exc_info=True)
            return
        
        # TODO: find last record ~
        #   select ncSet, max(ncIndex) from qc group by ncNet;
        
        try :
            n_records = f.variables['prevRecord'].shape[0]
            if self.lastRecordCount is None or n_records > self.lastRecordCount:
                i = self.lastRecordCount is not None and self.lastRecordCount or 0
                if max_records is not None:
                    record_limit = min(i + max_records, n_records)
                else:
                    record_limit = n_records
                fn = os.path.basename(self.path)
                idVariables = []
                for idVar in f.__getattribute__('idVariables').split(','):
                    idVariables.append(f.variables[str(idVar)])
                timeVariables = f.__getattribute__('timeVariables').split(',')
                vObsTime = f.variables[str(timeVariables[0])]
                vObsTimeFillValue = vObsTime.__getattribute__("_FillValue")
                try :
                    vObsTimeMissingValue = vObsTime.__getattribute__("missing_value")
                except AttributeError:
                    vObsTimeMissingValue = vObsTimeFillValue
                vLat = f.variables['latitude']
                vLon = f.variables['longitude']
                vElev = f.variables['elevation']
                results = []
                _logger.debug("adding %d records from %s", record_limit - i, self.path)
                while i < record_limit:
                    rec = QCRecord()
                    rec.setPluginName("qc")
                    if not vObsTime[i] == vObsTimeFillValue and not vObsTime[i] == vObsTimeMissingValue:
                        rec.setDataTime(DataTime(Date(int(vObsTime[i] * 1000))))
                        loc = SurfaceObsLocation()
                        loc.assignLocation(float(vLat[i]), float(vLon[i]))
                        loc.setElevation(Integer(int(vElev[i])))
                        stationId = []
                        for idVar in idVariables:
                            stationId.append(''.join(idVar[i]).strip().strip('\0'))
                        loc.setStationId(''.join(stationId))
                        rec.setLocation(loc)
                    
                        rec.setNcSet(fn)
                        rec.setNcIndex(i)
                        rec.setQcType(self.qcType)

                        rec.constructDataURI()
                        results.append(rec)
                    
                    i += 1
                    
                self.lastRecordCount = record_limit
                return results
        finally:
            f.close()
            
class QCScanner(object):
    def __init__(self, dir, qcType):
        self.directory = dir
        self.qcType = qcType
        self.ncSets = { }

    def incrementalScan(self, max_records = None):
        currentNcSets = self.findNcSetsNow()
        newSets = [ ]
        delSets = [ ]
        for ncSet in currentNcSets:
            if not self.ncSets.get(ncSet):
                newSets.append(ncSet)
        for ncSet in self.ncSets.keys():
            if ncSet not in currentNcSets:
                delSets.append(ncSet)
        if len(delSets):
            for ncSet in delSets:
                del self.ncSets[ncSet]
        if len(newSets):
            for ncSet in newSets:
                self.ncSets[ncSet] = NcSet(os.path.join(self.directory, ncSet), self.qcType)
        ncSets = self.ncSets.keys()
        ncSets.sort()
        
        results = None
        for ncSet in ncSets:
            partial = self.ncSets[ncSet].incrementalScan(max_records)
            if partial is not None:
                if max_records is not None:
                    max_records -= len(partial)
                if results is None:
                    results = ArrayList(len(partial))
                for rec in partial:
                    results.add(rec)
                    
        if results is None:
            results = ArrayList()
        return results

    def findNcSetsNow(self):
        paths = glob.glob(self.directory + '/[0-9]*_[0-9]*')
        return [ os.path.basename(x) for x in paths ]

scanner = None
qcDao = None

def init(directory, qcType):
    global scanner, qcDao
    scanner = QCScanner(directory, qcType)
    qcDao = PluginFactory.getInstance().getPluginDao("qc");

def scan(max_records = None):
    sys.stdout.flush()
    if scanner is not None:
        return scanner.incrementalScan(max_records)
