#!/usr/bin/python
# 
"""
 Convert to append the coverageid to the satellite datauris, and
 modify the associated satellite hdf5 group names to append the
 coverageid. The new groups are added as an alias to the existing
 datasets.
 Date         Ticket#    Engineer    Description
 ------------ ---------- ----------- --------------------------
 20120711            798 jkorman     Initial Development
 20120822            798 jkorman     Added ability to account for files containing hdfFileId.
"""
from subprocess import Popen, PIPE
import sys
from time import time
import h5py

POSTGRES_CMD = "psql -U awips -d metadata -t -q -A -c "
HDF5_LOC = "/awips2/edex/data/hdf5"

ID_IDX = 0
DATAURI_IDX = 1
COVERAGE_IDX = 2
SECTORID_IDX = 3
PHYSICAL_ELEMENT_IDX = 4
REFTIME_IDX = 5
HDFFILEID_IDX = 6

def update_satellite_table():
    """
    Add the interpolationLevels column to the satellite table.
    """
    result = queryPostgres("select count(*) from information_schema.columns where table_name='satellite' and column_name='interpolationlevels';")
    if(result[0][0] == '0'):
        result = queryPostgres("alter table satellite add column interpolationlevels integer;")
        print "Adding interpolationlevels column to satellite table"

def formatFileTime(refTime):
    """
    Extract and format the year (YYYY), month (MM), day (DD), and hour (HH)
    from the reference time. The output is formatted as YYYY-MM-DD-HH
    """
    return refTime[0:4] + "-" + refTime[5:7] + "-" + refTime[8:10] + "-" + refTime[11:13]

def getFilename(refTime, h5FileId):
    """
    Create the satellite data hdf filename corresponding to the given reference time and an
    possible hdf5 file identifier.
    """
    if(len(h5FileId) > 0):
        filename = "satellite-" + formatFileTime(refTime) + "-" + h5FileId + ".h5"
    else:
        filename = "satellite-" + formatFileTime(refTime) + ".h5"
    return filename

def queryPostgres(sql):
    """
    Extract and format the year (YYYY), month (MM), day (DD), and hour (HH)
    from the reference time. The output is formatted as YYYY-MM-DD-HH
    """
    result = Popen(POSTGRES_CMD + "\"" + sql + "\"", stdout=PIPE, shell=True)
    retVal = []
    for line in result.stdout:
        retVal.append(line.strip().split("|"))
    return retVal

def get_sectorids():
    """
    Get a list of unique sector identifiers from the satellite table.
    """
    return queryPostgres("select distinct sectorid from satellite;")
    
def get_satellite_rows(sectorid):
    """
    Extract and format the year (YYYY), month (MM), day (DD), and hour (HH)
    from the reference time. The output is formatted as YYYY-MM-DD-HH
    """
    keys = {}
    rows = queryPostgres("select id, dataURI, coverage_gid, sectorid, physicalelement, reftime, hdffileid from satellite where sectorid=" + repr(sectorid) + ";")
    for row in rows:
        # create the key for this entry.
        key = "/satellite/" + row[SECTORID_IDX] + "/" + row[PHYSICAL_ELEMENT_IDX] + "/" + getFilename(row[REFTIME_IDX], row[HDFFILEID_IDX])
        #print "key = " + key
        # have we found this key already?
        if(key in keys):
            # if so, get the row list for this key
            rowList = keys[key]
        else:
            # otherwise create an empty list to put the row in
            rowList = []
            # add it to the collection
            keys[key] = rowList
        # and add the row to the list
        rowList.append(row)
    return keys

def process_all_satellite():
    """
    Process all entries in the satellite table.
    Do one sector id at a time.
    """
    sectorids = get_sectorids()
    if(sectorids):
        for sectorid in sectorids:
            print "Processing sector " + sectorid[0]
            keys = get_satellite_rows(sectorid[0])
            if(keys):
                for key in keys:
                    print "=========================================================="
                    print " Processing key = " + key
                    fname = HDF5_LOC + key
                    try:
                        f = h5py.File(fname,'r+')
                        for row in keys[key]:
                            dataURI = row[1]
                            coverage = row[2]

                            newGroupName = dataURI + "/" + coverage
                            group = f.create_group(newGroupName)
                            group = f.create_group(newGroupName + "/Data-interpolated")

                            oldds = row[DATAURI_IDX] + "/Data"
                            newds = newGroupName + "/Data"
                            # Link to the old data set
                            f[newds] = h5py.SoftLink(oldds) 

                            group = f[row[DATAURI_IDX] + "/Data-interpolated"]
                            numLevels = 0
                            for n in group.keys():
                                newds = newGroupName + "/Data-interpolated/" + n
                                if (n == '0'):
                                    # special case for this link.
                                    # dataset /Data-interpolated/0 points to /Data
                                    oldds = row[DATAURI_IDX] + "/Data"
                                else:
                                    numLevels += 1
                                    oldds = row[DATAURI_IDX] + "/Data-interpolated/" + n
                                f[newds] = h5py.SoftLink(oldds)
                            updateSql = "update satellite set datauri='" + row[DATAURI_IDX] + "/" + row[COVERAGE_IDX] + "'"
                            updateSql += ", interpolationlevels="  + repr(numLevels)
                            updateSql += " where id=" + row[ID_IDX] + ";"
                            queryPostgres(updateSql)
                        f.close()
                    except Exception, e:
                        print e
                        print "Error occurred processing file " + fname
            else:
                print "No keys found for the sector id " + sectorid[0]
    else:
        print "No sector identifiers found in the satellite table"

if __name__ == '__main__':
    t = time()
    update_satellite_table()
    process_all_satellite()
    print "Total Conversion time %ds" % (time() - t)
