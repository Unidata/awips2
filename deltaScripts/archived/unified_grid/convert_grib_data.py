#!/usr/bin/python
# This script will add register the gridcoverage plugin, which was previosly part of grib
#
# This needs to be performed with build ????
# create_grid_tables.sh must be run before running this script.

from shutil import copytree, move, copy
from subprocess import Popen, PIPE
from thread import start_new_thread, allocate_lock
import sys
from os.path import exists, isdir
from os import mkdir
from time import sleep, time
import h5py
import struct

# this is generally a disk bound process so more threads won't help unless the disk is fast
numThreads = 1
# setting too many records at once causes the exec to psql to fail because of the long arg list
maxRecords = 200

postgresCmd = "psql -U awips -d metadata -t -q -A -c "
hdf5loc = "/awips2/edex/data/hdf5/"

quadrantGrids = ["ENSEMBLE37", "ENSEMBLE38", "ENSEMBLE39", "ENSEMBLE40",
                 "ECMF1", "ECMF2", "ECMF3", "ECMF4", "ECMF5", "ECMF6", "ECMF7", "ECMF8",
                 "UKMET40", "UKMET39", "UKMET38", "UKMET37"]

akGrids = ["MOSGuide-AK", "AK-NamDNG5","AK-RTMA", "AKWAVE239", "AKwave10", "AKwave4", "HiResW-NMM-AK", "HiResW-ARW-AK",
           "ETA242", "mesoEta217", "mesoEta216","ETA207", "AVN203", "MRF203", "GFS160"]

prGrids = ["HiResW-NMM-SJU", "HiResW-ARW-SJU", "PR-NamDNG5", "PR-RTMA", "MRF205",  "GFS161", "mesoEta237"]

hiGrids = ["HI-NamDNG5", "HI-RTMA", "HiResW-NMM-HI", "HiResW-ARW-HI", "MRF204", "AVN225", "GFS254", "SREF243"]

guGrids = ["HiResW-NMM-GU", "HiResW-ARW-GU"]

blacklistGrids = {"quadrant grids which have already been converted in an assembled format":quadrantGrids,
                  "grids over Alaska":akGrids, "grids over Puerto Rico":prGrids,
                  "grids over Hawaii and the Pacific Region":hiGrids, "grids over Guam":guGrids}


parameters = {}
models = []
gridinfo_seq = []
models_lock = allocate_lock()


def queryPostgres(sql):
    result = Popen(postgresCmd + "\"" + sql + "\"", stdout=PIPE, shell=True)
    retVal = []
    for line in result.stdout:
        retVal.append(line.strip().split("|"))
    return retVal

def convertModel(modelName):
    sqlTime = 0
    hdfTime = 0
    totTime = 0
    totTime -= time()
    print modelName, "Loading existing grid_info"
    infoMap = loadGridInfo(modelName)
    infoSql = None
    print modelName, "Querying grib database"
    rows = queryPostgres("select grib.forecasttime, grib.reftime, grib.utilityflags, grib.rangeend,grib.rangestart, grib.inserttime, grib.datauri, gridcoverage.id, grib_models.level_id, grib_models.location_id from grib, grib_models, gridcoverage, level where grib.modelinfo_id = grib_models.id and grib_models.location_id = gridcoverage.id and grib_models.level_id = level.id and grib_models.modelName = '%s' order by grib.forecasttime, grib.reftime, level.masterlevel_name" % modelName)
    print modelName, "Converting %d records" % len(rows)
    gridSql = None
    lastFile = None
    gribFiles = hdf5loc + "grib/" + modelName + "/"
    gridFiles = hdf5loc + "grid/" + modelName + "/"
    if not(isdir(hdf5loc + "grid/")):
        mkdir(hdf5loc + "grid/")
    if not(isdir(gridFiles)):
        mkdir(gridFiles)
    count = 0;
    for row in rows:
        gribforecasttime = row[0]
        gribreftime = row[1]
        gributilityflags = row[2]
        gribrangeend = row[3]
        gribrangestart = row[4]
        gribinserttime = row[5]
        gribdatauri = row[6]
        gridcoverageid = row[7]
        gribmodelslevelid = row[8]
        gribmodelslocationid = row[9]
        datauriparts = gribdatauri.split("/")
        datatime = datauriparts[2]
        paramabbrev = datauriparts[4]
        masterlevel = datauriparts[5]
        levelone = datauriparts[6]
        leveltwo = datauriparts[7]
        pert = datauriparts[9]
        version = datauriparts[10]
        secondaryId = "null"
        if version != "0":
            secondaryId = "Version" + version
        ensembleId = convertPert(pert)
        newdatauri = "/grid/" + datatime + "/" + modelName + "/" + secondaryId + "/" + ensembleId + "/" + gridcoverageid + "/" + paramabbrev + "/" + masterlevel + "/" + levelone + "/" + leveltwo
        hdfTime -= time()
        try:
            forecast = int(gribforecasttime)/3600
            prevgrp = gribdatauri
            newgrp = newdatauri
            dataset="Data"
            if paramabbrev.startswith("static"):
                prevgrp = "/"
                newgrp = "/" + gridcoverageid
                dataset=paramabbrev
            filebase = "/%s-%s-FH-%.3d.h5" % (modelName, gribreftime.split(":")[0].replace(" ", "-"), forecast)
            hdf5file = gridFiles + masterlevel + filebase
            if not(paramabbrev.startswith("static")) or forecast == 0:
                if lastFile != None and lastFile.filename != hdf5file:
                    #print "Closing", lastFile.filename
                    lastFile.close()
                    lastFile = None
                if lastFile == None:
                    if not(exists(hdf5file)):
                        t0 = time()
                        if not(isdir(gridFiles + masterlevel)):
                            mkdir(gridFiles + masterlevel)
                        move(gribFiles + masterlevel + filebase, gridFiles + masterlevel)
                        hdfTime -= (time() - t0)
                    #print "Opening", hdf5file
                    lastFile = h5py.File(hdf5file)
                copyH5(lastFile, prevgrp, newgrp, dataset)
        except:
            print modelName, "Error", gribdatauri
            print sys.exc_info()[1]
            hdfTime += time()
            continue
        hdfTime += time()
        infokey = modelName + ":::" + secondaryId + ":::" + ensembleId + ":::" +  gribmodelslevelid + ":::" + gribmodelslocationid + ":::" +  paramabbrev
        infoid = infoMap.get(infokey)
        if infoid == None:
            infoid = nextGridInfoSeq()
            infoMap[infokey] = infoid
            if secondaryId == "null":
                secondaryId = "NULL"
            else:
                secondaryId = "\'" + secondaryId + "\'"
            if ensembleId == "null":
                ensembleId = "NULL"
            else:
                ensembleId = "\'" + ensembleId + "\'"
            if infoSql == None:
                infoSql = "insert into grid_info (id, datasetid, secondaryid, ensembleid, level_id, location_id, parameter_abbreviation) values "
            else:
                infoSql = infoSql + ", "
            infoSql = infoSql + ("(%d, '%s', %s, %s, %s, %s, '%s')" % (infoid, modelName, secondaryId, ensembleId, gribmodelslevelid, gribmodelslocationid, paramabbrev))
        if gridSql == None:
            gridSql = "insert into grid (id, forecasttime, reftime, utilityflags, rangeend, rangestart, datauri, inserttime, info_id) values "
        else:
            gridSql = gridSql + ", "
        gridSql = gridSql + ("(nextval(\'hibernate_sequence\'), %s, '%s', '%s', '%s', '%s', '%s', '%s', %d)" % (gribforecasttime, gribreftime, gributilityflags, gribrangeend, gribrangestart, newdatauri, gribinserttime, infoid))
        count += 1
        if count % maxRecords == 0:
            print modelName, "Commiting %d grid records %d%%" % (maxRecords,100*count/len(rows))
            sqlTime -= time()
            if infoSql != None:
                #print infoSql
                queryPostgres(infoSql)
                infoSql = None
            if gridSql != None:
                #print gridSql
                queryPostgres(gridSql)
                gridSql = None
            sqlTime += time()
    print modelName, "Commiting remaining grid records"
    sqlTime -= time()
    if infoSql != None:
        #print infoSql
        queryPostgres(infoSql)
    if gridSql != None:
        #print gridSql
        queryPostgres(gridSql)
    if lastFile != None:
        lastFile.close()
    sqlTime += time()
    totTime += time()
    print modelName, "Time in sql commits = %ds" % (sqlTime)
    print modelName, "Time in hdf5 links  = %ds" % (hdfTime)
    print modelName, "Total process Time  = %ds" % (totTime)

def loadGridInfo(modelName):
    infoMap = {}
    for row in queryPostgres("select distinct id, datasetid, secondaryid, ensembleid, level_id, location_id, parameter_abbreviation, id from grid_info where datasetid = '%s'" % (modelName)):
        infokey = row[1] + ":::" + row[2] + ":::" + row[3] + ":::" +  row[4] + ":::" + row[5] + ":::" +  row[6]
        infoMap[infokey] = int(row[0])
    return infoMap

def nextGridInfoSeq():
    if len(gridinfo_seq) == 0:
        # The number of ids we need per model varies wildly from 1 to 263, 
        # but on average 50 per model will grab enough ids that we don't
        # ever need to go back to the db, although if we do it's not really
        # a big deal, this is just trying to avoid excessive trips back
        n = max(len(models),1)*50
        for row in queryPostgres("select nextval('gridinfo_seq') from generate_series(1,%d);" % (n)):
            gridinfo_seq.append(int(row[0]))
    return gridinfo_seq.pop()

def convertPert(pert):
    if pert == "1":
       return "ctl1"
    elif pert == "2":
       return "ctl2"
    elif pert == "3":
       return "n1"
    elif pert == "4":
       return "p1"
    elif pert == "5":
       return "n2"
    elif pert == "6":
       return "p2"
    elif pert == "7":
       return "n3"
    elif pert == "8":
       return "p3"
    elif pert == "9":
       return "n4"
    elif pert == "10":
       return "p4"
    elif pert == "11":
       return "n5"
    elif pert == "12":
       return "p5"
    return "null"
        
def copyH5(h5, gribdatauri, griddatauri, dataset="Data"):
    gribgrp = h5['/']
    gridgrp = gribgrp
    for part in gribdatauri.split('/'):
        if part:
            gribgrp = gribgrp[part]
    for part in griddatauri.split('/'):
        if part:
            gridgrp = gridgrp.require_group(part)
    if not(dataset in gridgrp.keys()):
        plists = {'lcpl': gribgrp[dataset]._lcpl, 'lapl': gribgrp[dataset]._lapl}
        plists['lcpl'].set_create_intermediate_group(False)
        h5py.h5o.link(gribgrp[dataset].id, gridgrp.id, dataset, **plists)


def processAllParameters():
    print "Populating parameter table from grib_models"
    sql = None
    c = 0
    for row in queryPostgres("select distinct abbreviation, name, unit from parameter"):
        p = {"abbreviation":row[0], "name":row[1], "unit":row[2]}
        parameters[row[0]] = p
    for row in queryPostgres("select distinct parameterabbreviation, parametername, parameterunit from grib_models"):
        if row[0] in parameters:
            continue
        p = {"abbreviation":row[0], "name":row[1], "unit":row[2]}
        parameters[row[0]] = p
        if sql == None:
            sql = "insert into parameter (abbreviation, name, unit) values "
        else:
            sql = sql + ", "
        c += 1
        sql = sql + ("('%s', '%s', '%s')" % (row[0], row[1], row[2]))
    if sql != None:
        queryPostgres(sql)
    print "Done populating parameter table, %d new rows added" % (c)

def processModels():
    while(True):
        models_lock.acquire()
        if len(models) == 0:
            global numThreads
            numThreads -= 1
            models_lock.release()
            break
        model = models.pop()
        models_lock.release()
        try:
            convertModel(model)
        except:
            print model, "Error model aborted"
            print sys.exc_info()[1]
            
def loadAll():
    global models
    print "This script will convert grib data in edex to use the new grid format"
    print "You provided no arguments so this will convert almost all data."
    print "To convert only specific models you can cancel and list models as arguments"
    print ""
    for row in queryPostgres("select distinct modelname from grib_models"):
        models.append(row[0])
    print "To save time some grid models will be skipped, these grids will not be"
    print "available until the next model run is ingested. If you would like to convert any" 
    print "of these models simply run the conversion script again with a list of models as arguments."
    print ""
    bad = []
    good = []
    for model in models:
        if model.startswith("UnknownModel"):
            bad.append(model)
        else:
            good.append(model)
    if len(bad) > 0:
        print "These Unknown Models will not be converted:",
        for model in bad:
            print "\"" + model + "\"", 
        print ""
        print ""
    models = good
    for key in blacklistGrids:
        blacklist = blacklistGrids[key]
        bad = []
        good = []
        for model in models:
            if model in blacklist:
                bad.append(model)
            else:
                good.append(model)
        if len(bad) > 0:
            print "These " + key + " will not be converted:",
            for model in bad:
                print "\"" + model + "\"", 
            print ""
            print ""
        models = good
    print "To continue converting the data Press Enter or Ctrl-C to cancel."
    raw_input()
    
def check_table(tablename):
    rows = queryPostgres("SELECT count(*) FROM information_schema.tables WHERE table_name = '" + tablename + "';")
    if(rows[0][0] != "1"):
        print tablename, "table does not exist, please create tables"
        sys.exit(1)
    
if __name__ == '__main__':
    t = time()
    check_table("grid")
    check_table("grid_info")
    check_table("parameter")
    if len(sys.argv) == 1:
        loadAll()
    else:
        for i in range(1,len(sys.argv)):
            models.append(sys.argv[i])
    processAllParameters()
    print "Starting %d threads to process models" % (numThreads)
    for i in range(numThreads-1):
        start_new_thread(processModels, ())
    processModels()
    while numThreads > 0:
        sleep(5)
    print "Total Conversion time %ds" % (time() - t)