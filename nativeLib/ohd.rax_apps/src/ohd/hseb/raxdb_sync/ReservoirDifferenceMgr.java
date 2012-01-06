package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.ReservoirRecord;
import ohd.hseb.raxdb.generated.RaxReservoirRecord;
import ohd.hseb.raxdb.generated.RaxReservoirTable;

public class ReservoirDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    public ReservoirDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);
        
        setupOutputFiles("Reservoir");

    } // end of Constructor
    


    public DiffSet findDifferences()
    {
        DiffSet tableDifferences = new DiffSet();
        
        List ihfsRecordList = null;
        List raxRecordList = null;
        
        RecordDifference oneDifference = null;

        logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing Reservoir Table records...");
                
        ihfsRecordList = _dm.getIhfsReservoirRecordList();
        _numberOfIhfsRecords = _numberOfIhfsRecords + ihfsRecordList.size();
        raxRecordList = _dm.getRaxReservoirRecordList();


        //add the RAX records to a Map
        Map raxLidMap = new HashMap();
        for (int i=0; i < raxRecordList.size(); i++)
        {
            RaxReservoirRecord record = (RaxReservoirRecord) raxRecordList.get(i);
            
            raxLidMap.put(record.getLid(), record);
        } // end of raxRecordList for loop
        
        // loop through IHFS record list and check if same lid found in RAX
        // if not, this needs to be added as new to RAX
        // if same lid found then check to see if they are "different"
        for (int i=0; i<ihfsRecordList.size(); i++)
        {
            List fieldDifferenceList = null;
            
            ReservoirRecord currentIhfsRecord = (ReservoirRecord) ihfsRecordList.get(i);
            String currentIhfsLid = currentIhfsRecord.getLid();
            RaxReservoirRecord raxRecord = (RaxReservoirRecord) raxLidMap.get(currentIhfsLid);

            if (raxRecord == null)  //not in the rax database
            {
                oneDifference = createDifferenceEntry(RecordDifferenceOriginType.NEW, currentIhfsRecord, null, null);
                tableDifferences.addDifference(oneDifference);
                _numberOfNewRecords++;
            }
            else //match found
            {
                fieldDifferenceList = getFieldDifferenceList(currentIhfsRecord, raxRecord);
                if (fieldDifferenceList.size() == 0) //same
                {
                    _numberOfSameRecords++;
                }
                else //different
                {
                    oneDifference = createDifferenceEntry(RecordDifferenceOriginType.MOD, currentIhfsRecord, raxRecord, fieldDifferenceList);
                    tableDifferences.addDifference(oneDifference);
                    _numberOfModRecords++;
                } 
            }

        } // end of ihfsRecordList for loop

        displayEndOfFindDifferencesMessage("Reservoir");
        
        return tableDifferences;

    } // end of findDifferences method

    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(ReservoirRecord ihfsRecord, RaxReservoirRecord raxRecord)
    {
        List fieldDifferenceList = new ArrayList();

        checkAndAddToFieldDifferenceList("name", ihfsRecord.getName(), raxRecord.getName(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("type", ihfsRecord.getType(), raxRecord.getType(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("owner", ihfsRecord.getOwner(), raxRecord.getOwner(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("deadpool", ihfsRecord.getDeadpool(), raxRecord.getDeadpool(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("conserpool", ihfsRecord.getConserpool(), raxRecord.getConserpool(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("floodpool", ihfsRecord.getFloodpool(), raxRecord.getFloodpool(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("spillway", ihfsRecord.getSpillway(), raxRecord.getSpillway(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("sill", ihfsRecord.getSill(), raxRecord.getSill(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("top", ihfsRecord.getTop(), raxRecord.getTop(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("surchg", ihfsRecord.getSurchg(), raxRecord.getSurchg(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("elev", ihfsRecord.getElev(), raxRecord.getElev(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("gates", ihfsRecord.getGates(), raxRecord.getGates(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("impounded", ihfsRecord.getImpounded(), raxRecord.getImpounded(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("uses", ihfsRecord.getUses(), raxRecord.getUses(), fieldDifferenceList);

        return fieldDifferenceList;

    } // end of getFieldDifferenceList method
    
    //----------------------------------------------------------------------------------------------
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, ReservoirRecord ihfsRecord,
            final RaxReservoirRecord raxRecord, List fieldDiffList)
    {
        RecordDifference oneDifferenceRecord;
        RaxReservoirRecord newRaxRecord = null;

        if (type == RecordDifferenceOriginType.NEW)
        {
            newRaxRecord = new RaxReservoirRecord();

            newRaxRecord.setLid(ihfsRecord.getLid());
            newRaxRecord.setSbd(System.currentTimeMillis()); // set to Today's date
            newRaxRecord.setSed(DbTable.getNullDate());    // set to null to show as most current record
            newRaxRecord.setName(makeMixedCase(ihfsRecord.getName()));
            newRaxRecord.setType(ihfsRecord.getType());
            newRaxRecord.setOwner(ihfsRecord.getOwner());
            newRaxRecord.setDeadpool(ihfsRecord.getDeadpool());
            newRaxRecord.setConserpool(ihfsRecord.getConserpool());
            newRaxRecord.setFloodpool(ihfsRecord.getFloodpool());
            newRaxRecord.setSpillway(ihfsRecord.getSpillway());
            newRaxRecord.setSill(ihfsRecord.getSill());
            newRaxRecord.setTop(ihfsRecord.getTop());
            newRaxRecord.setSurchg(ihfsRecord.getSurchg());
            newRaxRecord.setElev(ihfsRecord.getElev());
            newRaxRecord.setGates(ihfsRecord.getGates());
            newRaxRecord.setImpounded(ihfsRecord.getImpounded());
            newRaxRecord.setUses(ihfsRecord.getUses());
        }
        else if (type == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxReservoirRecord(raxRecord);            
        }

        oneDifferenceRecord = new RecordDifference(type, this, ihfsRecord, newRaxRecord, fieldDiffList);
        return oneDifferenceRecord;

    } // end of createDifferenceEntry method

    public void reportDifferences(RecordDifference oneRecordDifference)
    {
        FieldDifference oneFieldDifference = new FieldDifference();

        ReservoirRecord ihfsRecord = (ReservoirRecord) oneRecordDifference.getIhfsRecord();
        RaxReservoirRecord raxRecord = (RaxReservoirRecord) oneRecordDifference.getRaxRecord();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            logRecordDifferenceMessage("\nDifferences found in Table = Reservoir" + " for lPrimary Key = |"+ raxRecord.getLid() +
                                         "|" + displayDate(raxRecord.getSbd()) + "|");
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                logRecordDifferenceMessage("Column = " + oneFieldDifference.getName() + "   IHFS_value = "
                        + oneFieldDifference.getIhfsValue() + "   Rax_value = " + oneFieldDifference.getRaxValue());
            }
        }
        else
        {

            logNewRecordMessage("\nA new Reservoir record will need to be created in RAX db for Primary Key = |"+ raxRecord.getLid() +
                                "|" + displayDate(raxRecord.getSbd()) + "|" +
                                "\nlid = " + raxRecord.getLid() +
                                "\nsbd = " + displayDate(raxRecord.getSbd()) +
                                "\nsed = " + displayDate(raxRecord.getSed()) +
                                "\nname = " + raxRecord.getName() +
                                "\ntype = " + raxRecord.getType() +
                                "\nowner = " + raxRecord.getOwner() +
                                "\ndeadpool = " + displayDouble(raxRecord.getDeadpool()) +
                                "\nconserpool = " + displayDouble(raxRecord.getConserpool()) +
                                "\nfloodpool = " + displayDouble(raxRecord.getFloodpool()) +
                                "\nspillway = " + displayDouble(raxRecord.getSpillway()) +
                                "\nsill = " + displayDouble(raxRecord.getSill()) +
                                "\ntop = " + displayDouble(raxRecord.getTop()) +
                                "\nsurchg = " + displayDouble(raxRecord.getSurchg()) +
                                "\nelev = " + displayDouble(raxRecord.getElev()) +
                                "\ngates = " + displayInt(raxRecord.getGates()) +
                                "\nimpounded = " + displayDate(raxRecord.getImpounded()) +
                                "\nuses = " + raxRecord.getUses());

            logDebugMessage("\nA new Reservoir record will need to be created in RAX db for Primary Key = |"+ raxRecord.getLid() +
                            "|" + displayDate(raxRecord.getSbd()) + "|" +
                            "\nusing the following information from the IHFS db:" +
                            "\nlid = " + raxRecord.getLid() + "   IHFS_lid = " + ihfsRecord.getLid() +
                            "\nsbd = " + displayDate(raxRecord.getSbd()) +
                            "\nsed = " + displayDate(raxRecord.getSed()) +
                            "\nname = " + raxRecord.getName() + "   IHFS_name = " + ihfsRecord.getName() +
                            "\ntype = " + raxRecord.getType() + "   IHFS_type = " + ihfsRecord.getType() +
                            "\nowner = " + raxRecord.getOwner() + "   IHFS_Owner = " + ihfsRecord.getOwner() +
                            "\ndeadpool = " + displayDouble(raxRecord.getDeadpool()) + "   IHFS_deadpool = " + displayDouble(ihfsRecord.getDeadpool()) +
                            "\nconserpool = " + displayDouble(raxRecord.getConserpool()) + "   IHFS_conserpool = " + displayDouble(ihfsRecord.getConserpool()) +
                            "\nfloodpool = " + displayDouble(raxRecord.getFloodpool()) + "   IHFS_floodpool = " + displayDouble(ihfsRecord.getFloodpool()) +
                            "\nspillway = " + displayDouble(raxRecord.getSpillway()) + "   IHFS_spillway = " + displayDouble(ihfsRecord.getSpillway()) +
                            "\nsill = " + displayDouble(raxRecord.getSill()) + "   IHFS_sill = " + displayDouble(ihfsRecord.getSill()) +
                            "\ntop = " + displayDouble(raxRecord.getTop()) + "   IHFS_top = " + displayDouble(ihfsRecord.getTop()) +
                            "\nsurchg = " + displayDouble(raxRecord.getSurchg()) + "   IHFS_surchg = " + displayDouble(ihfsRecord.getSurchg()) +
                            "\nelev = " + displayDouble(raxRecord.getElev()) + "   IHFS_elev = " + displayDouble(ihfsRecord.getElev()) +
                            "\ngates = " + displayInt(raxRecord.getGates()) + "   IHFS_gates = " + displayInt(ihfsRecord.getGates()) +
                            "\nimpounded = " + displayDate(raxRecord.getImpounded()) + "   IHFS_impounded = " + displayDate(ihfsRecord.getImpounded()) +
                            "\nuses = " + raxRecord.getUses() + "   IHFS_uses = " + ihfsRecord.getUses());
        }

    } // end of reportDifferences method

    //----------------------------------------------------------------------------------------------
    public int processDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;
        RaxReservoirTable raxTable = null;
        RaxReservoirRecord newRaxRecord = null;
        
        ReservoirRecord ihfsRecord = (ReservoirRecord) oneRecordDifference.getIhfsRecord();
        RaxReservoirRecord raxRecord = (RaxReservoirRecord) oneRecordDifference.getRaxRecord();

        raxTable = _dm.getRaxReservoirTable();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxReservoirRecord(raxRecord);

            newRaxRecord.setSed(System.currentTimeMillis());
            
            numberOfFailedDbUpdates = processRaxInsertUpdate(UPDATE, "Reservoir", raxTable, raxRecord, newRaxRecord);
            
            newRaxRecord.setSbd(System.currentTimeMillis());
            newRaxRecord.setSed(DbTable.getNullDate());
            newRaxRecord.setName(makeMixedCase(ihfsRecord.getName()));
            newRaxRecord.setType(ihfsRecord.getType());
            newRaxRecord.setOwner(ihfsRecord.getOwner());
            newRaxRecord.setDeadpool(ihfsRecord.getDeadpool());
            newRaxRecord.setConserpool(ihfsRecord.getConserpool());
            newRaxRecord.setFloodpool(ihfsRecord.getFloodpool());
            newRaxRecord.setSpillway(ihfsRecord.getSpillway());
            newRaxRecord.setSill(ihfsRecord.getSill());
            newRaxRecord.setTop(ihfsRecord.getTop());
            newRaxRecord.setSurchg(ihfsRecord.getSurchg());
            newRaxRecord.setElev(ihfsRecord.getElev());
            newRaxRecord.setGates(ihfsRecord.getGates());
            newRaxRecord.setImpounded(ihfsRecord.getImpounded());
            newRaxRecord.setUses(ihfsRecord.getUses());
            
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "Reservoir", raxTable, newRaxRecord, newRaxRecord);
        }
        else
        {
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "Reservoir", raxTable, raxRecord, newRaxRecord);
        }

        return numberOfFailedDbUpdates;

    } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {

        RaxReservoirRecord raxRecord = (RaxReservoirRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getLid() + "|"
                + displayDate(raxRecord.getSbd()) + "|"
                + displayDate(raxRecord.getSed()) + "|"
                + raxRecord.getName() + "|"
                + raxRecord.getType() + "|"
                + raxRecord.getOwner() + "|"
                + displayDouble(raxRecord.getDeadpool()) + "|"
                + displayDouble(raxRecord.getConserpool()) + "|"
                + displayDouble(raxRecord.getFloodpool()) + "|"
                + displayDouble(raxRecord.getSpillway()) + "|"
                + displayDouble(raxRecord.getSill()) + "|"
                + displayDouble(raxRecord.getTop()) + "|"
                + displayDouble(raxRecord.getSurchg()) + "|"
                + displayDouble(raxRecord.getElev()) + "|"
                + displayInt(raxRecord.getGates()) + "|"
                + displayDate(raxRecord.getImpounded()) + "|"
                + raxRecord.getUses() + "|");

        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method

    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxReservoirTable table = (RaxReservoirTable) dbTable;
        RaxReservoirRecord record = (RaxReservoirRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxReservoirTable table = (RaxReservoirTable) dbTable;
        RaxReservoirRecord recordOld = (RaxReservoirRecord) dbRecordOld;
        RaxReservoirRecord recordNew = (RaxReservoirRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }

} // end of ReservoirDifferenceMgr class
