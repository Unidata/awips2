# Called by updateWarningTables.sh to parse the ugc zones in table updates
import sys
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID

table = sys.argv[1]
fileName = sys.argv[2]
f = open(fileName, 'r')
ugcZonesUpdateFile = open(table + 'UgcZonesUpdates.sql', 'w')

prevParentId = None
parentId = None
ugcZones = None

for line in f:
    # break line by columns
    columns=line.split('|')

    if len(columns) > 1:
         parentId=columns[0].strip()
         ugcZone=columns[1].strip()

         if parentId == prevParentId:
             ugcZones.append(ugcZone)
         else:
             if ugcZones is not None:
                 zoneStr = ", ".join(ugcZones)
                 ugcZonesUpdateFile.write("UPDATE " + table + " SET ugczones = '" + zoneStr + "' WHERE id = " + prevParentId + ";\n")
             ugcZones = [ugcZone]
             prevParentId = parentId

if ugcZones is not None:
    zoneStr = ", ".join(ugcZones)
    ugcZonesUpdateFile.write("UPDATE " + table + " SET ugczones = '" + zoneStr + "' WHERE id = " + prevParentId + ";\n")

f.close()
ugcZonesUpdateFile.close()
