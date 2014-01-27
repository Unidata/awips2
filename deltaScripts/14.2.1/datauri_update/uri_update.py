#!/usr/bin/env python

from subprocess import Popen, PIPE
import sys
import h5py
import re
import os

postgresCmd = "psql -U awips -d metadata -t -q -A "
hdf5loc = os.sep + "awips2" + os.sep + "edex" + os.sep + "data" + os.sep + "hdf5" + os.sep
postgres_dataURISeparator = '/'
hdf5_dataURISeparator = '::'

ID_ID = 'id'
DATAURI_ID = 'datauri'
REFTIME_ID = 'to_char(reftime, \'YYYY-MM-DD-HH24\') as reftime'

def printUsage():
    print "usage: <plugin> <index1> <findAt_Index1> <replaceWith_Index1> (1-n times) <optional hdf5 file path relative to plugin name>"
    sys.exit()
    
def executePostgresSQL(sql):
    result = Popen(postgresCmd + "-c \"" + sql + "\"", stdout=PIPE, shell=True)
    retVal = []
    for line in result.stdout:
        retVal.append(line.strip().split("|"))
    return retVal

def executePostgresSQLFile(file):
    result = Popen(postgresCmd + "-f \"" + file + "\"", stdout=PIPE, shell=True)
    retVal = []
    for line in result.stdout:
        retVal.append(line.strip().split("|"))
    return retVal
    
    
def processReplacements(plugin, replacements, hdf5Path):
    columns = [ID_ID, DATAURI_ID]
    hdf5Columns = []
    if hdf5Path is not None:
        columns.append(REFTIME_ID)
        regex = re.compile("\[([\w]+)\]")
        for column in regex.findall(hdf5Path):
            hdf5Columns.append(column)
            if column not in columns:
                columns.append(column)
    
    sql = "SELECT " + columns[0] + ", " + columns[1]
    for i in range(2, len(columns)):
        sql = sql + ", " + columns[i]
        
    sql = sql + " FROM " + plugin
    
    results = executePostgresSQL(sql)
    toUpdate = []
    
    id_idx = columns.index(ID_ID)
    uri_idx = columns.index(DATAURI_ID)
    reftime_idx = columns.index(REFTIME_ID)
    
    for result in results:
        uri = result[uri_idx]
        parts = uri.split(postgres_dataURISeparator)
        update = False
        for replacement in replacements:
            idx = replacement[0] + 1;
            find = replacement[1]
            replace = replacement[2]
            if parts[idx].find(find) != -1:
                parts[idx] = parts[idx].replace(find, replace)
                update = True
        
        if update:
            uri = ""
            for i in range(1, len(parts)):
                uri = uri + postgres_dataURISeparator + parts[i]
            result.append(uri) # Append new uri to results
            toUpdate.append(result)
    
    if len(toUpdate) > 0:
        hdf5_file_mapping = {}
        pathIndexes = []
        for hdf5PathColumn in hdf5Columns:
            pathIndexes.append(columns.index(hdf5PathColumn))
         
        updateFileName = os.sep + "tmp" + os.sep + plugin + ".uri_update_sql"
        update_file = open(updateFileName, "w")
        for update in toUpdate:
            # Write UPDATE statement to sql file
            id = update[id_idx]
            new_uri = update[len(update)-1] # Last entry is updated uri
            update_file.write("UPDATE " + plugin + " SET " + DATAURI_ID 
                              + "='" + new_uri + "' WHERE " + ID_ID + "=" 
                              + id + ";\n")
            
            if hdf5Path is not None:
                path = plugin + os.sep
                for pathIndex in pathIndexes:
                    path = path + update[pathIndex] + os.sep
                path = path + plugin + "-" + update[reftime_idx] + ".h5"
                file_updates = hdf5_file_mapping.get(path, None)
                if file_updates is None:
                    file_updates = []
                    hdf5_file_mapping[path] = file_updates
                file_updates.append(update)
            
        update_file.close()
        
        # Execute and delete temporary file
        executePostgresSQLFile(updateFileName)
        os.remove(updateFileName)
        
        # Create hdf5 links from new uri to old uri
        for hdf5File in hdf5_file_mapping.keys():
            absolutePath = hdf5loc + hdf5File
            if os.path.exists(absolutePath) == True:
                h5pyFile = h5py.File(absolutePath)
                for entry in hdf5_file_mapping[hdf5File]:
                    old_uri = entry[uri_idx].replace(postgres_dataURISeparator, hdf5_dataURISeparator)[2:]
                    new_uri = entry[len(entry)-1].replace(postgres_dataURISeparator, hdf5_dataURISeparator)[2:]
                    hasOldUri = old_uri in h5pyFile
                    hasNewUri = new_uri in h5pyFile
                    if hasOldUri and not hasNewUri:
                        h5pyFile[new_uri] = h5pyFile[old_uri]
                    else:
                        print "Skipping linking", old_uri, "to", new_uri + ".", hasOldUri, hasNewUri
            else:
                print "Skipping non-existing file:", absolutePath
        
if __name__ == '__main__':
    numArgs = len(sys.argv)
    if numArgs < 5:
        printUsage()
    
    pluginName = sys.argv[1]
    
    replacements = []
    
    inc = 3
    for i in range(2, numArgs, inc):
        if (i + inc) <= numArgs:
            replacements.append((int(sys.argv[i]), sys.argv[i + 1], sys.argv[i + 2]))
        
    if len(replacements) == 0:
        printUsage()
        
    replacementArgs = len(replacements) * inc
    
    hdf5Path = None
    
    if (2 + replacementArgs) < numArgs:
        hdf5Path = sys.argv[numArgs - 1]  
    
    processReplacements(pluginName, replacements, hdf5Path)
