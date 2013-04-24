# Called by normalizeGfe.sh to parse the distinct parmIds into table insert
import sys
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID

fileName = sys.argv[1]
f = open(fileName, 'r')
dbIdInsertFile = open('dbIdInserts.sql', 'w')
parmIdInsertFile = open('parmIdInserts.sql', 'w')
recordUpdateFile = open('gfeToParmIdUpdates.sql', 'w')

dbIds={}
parmIds={}
dbIdCounter = 1
parmIdCounter = 1

for parmIdString in f:
    # Strip new line
    parmIdString = parmIdString.strip()

    # skip last line of file that's empty
    if len(parmIdString.strip()) > 0:
        if not parmIds.has_key(parmIdString):
            parmIds[parmIdString] = parmIdCounter
            parmId = ParmID(parmIdString)
            dbId = parmId.getDbId()
            dbIdString = dbId.getModelId()

            if not dbIds.has_key(dbIdString):
                dbIds[dbIdString] = dbIdCounter
                dbIdInsertFile.write("INSERT INTO gfe_dbid (id, dbtype, format, modelname, modeltime, siteid) VALUES (" +
                                     str(dbIdCounter) + ", '" + dbId.getDbType() + "', '" + dbId.getFormat() + "', '" +
                                     dbId.getModelName() + "', '" + dbId.getModelTime() + "', '" + dbId.getSiteId() + "');\n")
                dbIdCounter += 1
            dbIdVal = dbIds[dbIdString]
            parmIdInsertFile.write("INSERT INTO gfe_parmid (id, parmlevel, parmname, dbid_id) VALUES (" +
                                     str(parmIdCounter) + ", '" + parmId.getParmLevel() + "', '" + 
                                     parmId.getParmName() + "', " + str(dbIdVal) + ");\n")
            recordUpdateFile.write("UPDATE gfe set parmId_id = " + str(parmIdCounter) +
                                   " WHERE parmId = '" + parmIdString + "';\n")
            parmIdCounter+=1
        else:
            # should never happen if query feeding this is using distinct
            print "Received duplicate parmId: " + parmIdString

dbIdInsertFile.write("CREATE SEQUENCE gfe_dbid_seq INCREMENT 1 MINVALUE 1 MAXVALUE 9223372036854775807 START "
                     + str((dbIdCounter / 50) + 1) + " CACHE 1;\nALTER TABLE gfe_dbid_seq OWNER TO awips;")
parmIdInsertFile.write("CREATE SEQUENCE gfe_parmid_seq INCREMENT 1 MINVALUE 1 MAXVALUE 9223372036854775807 START "
                     + str((parmIdCounter / 50) + 1) + " CACHE 1;\nALTER TABLE gfe_parmid_seq OWNER TO awips;")
f.close()
dbIdInsertFile.close()
parmIdInsertFile.close()
recordUpdateFile.close()
