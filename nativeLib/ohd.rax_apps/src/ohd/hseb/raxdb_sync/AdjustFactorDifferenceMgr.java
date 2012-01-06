package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.AdjustFactorRecord;
import ohd.hseb.raxdb.generated.RaxAdjustFactorRecord;
import ohd.hseb.raxdb.generated.RaxAdjustFactorTable;

public class AdjustFactorDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    public AdjustFactorDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);
        
        setupOutputFiles("AdjustFactor");

    } // end of Constructor
    
    //----------------------------------------------------------------------------------------------
    public DiffSet findDifferences()
    {
        DiffSet tableDifferences = new DiffSet();

        List ihfsRecordList = null;
        List raxRecordList = null;
        
        RecordDifference oneDifference = null;

        logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing AdjustFactor Table records...");
        
        ihfsRecordList = _dm.getIhfsAdjustFactorRecordList();
        _numberOfIhfsRecords = _numberOfIhfsRecords + ihfsRecordList.size();
        raxRecordList = _dm.getRaxAdjustFactorRecordList();

        //add the RAX records to a Map
        Map raxPrimaryKeyMap = new HashMap();
        for (int i=0; i < raxRecordList.size(); i++)
        {
            RaxAdjustFactorRecord record = (RaxAdjustFactorRecord) raxRecordList.get(i);
            String raxMapKey = createKeyForMap(record);
            raxPrimaryKeyMap.put(raxMapKey, record);
        } // end of raxRecordList for loop
        
        // loop through IHFS record list and check if same key found in RAX
        // if not, this needs to be added as new to RAX
        // if same key found then check to see if they are "different"
        for (int i=0; i<ihfsRecordList.size(); i++)
        {
            List fieldDifferenceList = null;

            AdjustFactorRecord currentIhfsRecord = (AdjustFactorRecord) ihfsRecordList.get(i);
            String currentIhfsMapKey = createKeyForMap(currentIhfsRecord);
            RaxAdjustFactorRecord raxRecord = (RaxAdjustFactorRecord) raxPrimaryKeyMap.get(currentIhfsMapKey);

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
                
        displayEndOfFindDifferencesMessage("AdjustFactor");
        
        return tableDifferences;

    } // end of findDifferences method

    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(AdjustFactorRecord ihfsRecord, RaxAdjustFactorRecord raxRecord)
    {
        List fieldDifferenceList = new ArrayList();

        checkAndAddToFieldDifferenceList("divisor", ihfsRecord.getDivisor(), raxRecord.getDivisor(), 2, fieldDifferenceList);
        checkAndAddToFieldDifferenceList("base", ihfsRecord.getBase(), raxRecord.getBase(), 2, fieldDifferenceList);
        checkAndAddToFieldDifferenceList("multiplier", ihfsRecord.getMultiplier(), raxRecord.getMultiplier(), 2, fieldDifferenceList);
        checkAndAddToFieldDifferenceList("adder", ihfsRecord.getAdder(), raxRecord.getAdder(), 2, fieldDifferenceList);

        return fieldDifferenceList;

    } // end of getFieldDifferenceList method
    
    //----------------------------------------------------------------------------------------------
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, AdjustFactorRecord ihfsRecord,
            final RaxAdjustFactorRecord raxRecord, List fieldDiffList)
    {
        RecordDifference oneDifferenceRecord;
        RaxAdjustFactorRecord newRaxRecord = null;

        if (type == RecordDifferenceOriginType.NEW)
        {
            newRaxRecord = new RaxAdjustFactorRecord();

            newRaxRecord.setLid(ihfsRecord.getLid());
            newRaxRecord.setPe1(createRaxPeOrTsFromIhfs(ihfsRecord.getPe(), 1));
            newRaxRecord.setPe2(createRaxPeOrTsFromIhfs(ihfsRecord.getPe(), 2));
            newRaxRecord.setDur(setRaxDurFromIhfsDur(ihfsRecord.getDur()));
            newRaxRecord.setIdur(ihfsRecord.getDur());
            newRaxRecord.setT(createRaxPeOrTsFromIhfs(ihfsRecord.getTs(), 1));
            newRaxRecord.setS(createRaxPeOrTsFromIhfs(ihfsRecord.getTs(), 2));
            newRaxRecord.setE(ihfsRecord.getExtremum());
            newRaxRecord.setBegin_date(System.currentTimeMillis()); // set to Today's date
            newRaxRecord.setDivisor(ihfsRecord.getDivisor());
            newRaxRecord.setBase(ihfsRecord.getBase());
            newRaxRecord.setMultiplier(ihfsRecord.getMultiplier());
            newRaxRecord.setAdder(ihfsRecord.getAdder());
        }
        else if (type == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxAdjustFactorRecord(raxRecord);            
        }

        oneDifferenceRecord = new RecordDifference(type, this, ihfsRecord, newRaxRecord, fieldDiffList);
        return oneDifferenceRecord;

    } // end of createDifferenceEntry method

    //----------------------------------------------------------------------------------------------
    public void reportDifferences(RecordDifference oneRecordDifference)
    {
        FieldDifference oneFieldDifference = new FieldDifference();

        AdjustFactorRecord ihfsRecord = (AdjustFactorRecord) oneRecordDifference.getIhfsRecord();
        RaxAdjustFactorRecord raxRecord = (RaxAdjustFactorRecord) oneRecordDifference.getRaxRecord();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            logRecordDifferenceMessage("\nDifferences found in Table = AdjustFactor for key = |" + createKeyForMap(raxRecord) + displayDate(raxRecord.getBegin_date()) + "|");
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                logRecordDifferenceMessage("Column = " + oneFieldDifference.getName() + "   IHFS_value = "
                        + oneFieldDifference.getIhfsValue() + "   Rax_value = " + oneFieldDifference.getRaxValue());
            }
        }
        else
        {
            logNewRecordMessage("\nA new AdjustFactor record will need to be created in RAX db for key = |"+ createKeyForMap(raxRecord) + displayDate(raxRecord.getBegin_date()) + "|" +
                                "\nlid = " + raxRecord.getLid() +
                                "\npe1 = " + raxRecord.getPe1() + " pe2 = " + raxRecord.getPe2() +
                                "\ndur = " + raxRecord.getDur() +
                                "\nidur = " + displayInt(raxRecord.getIdur()) +
                                "\nt = " + raxRecord.getT() + " s = " + raxRecord.getS() +
                                "\ne = " + raxRecord.getE() +
                                "\nbegin_date = " + displayDate(raxRecord.getBegin_date()) +
                                "\ndivisor = " + displayDouble(raxRecord.getDivisor()) +
                                "\nbase = " + displayDouble(raxRecord.getBase()) +
                                "\nmultiplier = " + displayDouble(raxRecord.getMultiplier()) +
                                "\nadder = " + displayDouble(raxRecord.getAdder()));

            logDebugMessage("\nA new AdjustFactor record will need to be created in RAX db for key = |" + createKeyForMap(raxRecord) + displayDate(raxRecord.getBegin_date()) + "|" +
                            "\nusing the following information from the IHFS db:" +
                            "\nlid = " + raxRecord.getLid() + "   IHFS_lid = " + ihfsRecord.getLid() +
                            "\npe1 = " + raxRecord.getPe1() + " pe2 = " + raxRecord.getPe2() + "   IHFS_pe = " + ihfsRecord.getPe() +
                            "\ndur = " + raxRecord.getDur() + "   IHFS_dur = " + ihfsRecord.getDur() +
                            "\nidur = " + displayInt(raxRecord.getIdur()) + "   IHFS_dur = " + displayInt(ihfsRecord.getDur()) +
                            "\nt = " + raxRecord.getT() + " s = " + raxRecord.getS() + "   IHFS_ts = " + ihfsRecord.getTs() +
                            "\ne = " + raxRecord.getE() + "   IHFS_extremum = " + ihfsRecord.getExtremum() +
                            "\nbegin_date = " + displayDate(raxRecord.getBegin_date()) +
                            "\ndivisor = " + displayDouble(raxRecord.getDivisor()) + "   IHFS_divisor = " + displayDouble(ihfsRecord.getDivisor()) +
                            "\nbase = " + displayDouble(raxRecord.getBase()) + "   IHFS_base = " + displayDouble(ihfsRecord.getBase()) +
                            "\nmultiplier = " + displayDouble(raxRecord.getMultiplier()) + "   IHFS_multiplier = " + displayDouble(ihfsRecord.getMultiplier()) +
                            "\nadder = " + displayDouble(raxRecord.getAdder()) + "   IHFS_adder = " + displayDouble(ihfsRecord.getAdder()));
        }

    } // end of reportDifferences method

    //----------------------------------------------------------------------------------------------
    public int processDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;
        RaxAdjustFactorTable raxTable = null;
        RaxAdjustFactorRecord newRaxRecord = null;
        
        AdjustFactorRecord ihfsRecord = (AdjustFactorRecord) oneRecordDifference.getIhfsRecord();
        RaxAdjustFactorRecord raxRecord = (RaxAdjustFactorRecord) oneRecordDifference.getRaxRecord();

        raxTable = _dm.getRaxAdjustFactorTable();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxAdjustFactorRecord(raxRecord);

            newRaxRecord.setBegin_date(System.currentTimeMillis());
            newRaxRecord.setDivisor(ihfsRecord.getDivisor());
            newRaxRecord.setBase(ihfsRecord.getBase());
            newRaxRecord.setMultiplier(ihfsRecord.getMultiplier());
            newRaxRecord.setAdder(ihfsRecord.getAdder());
            
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "AdjustFactor", raxTable, newRaxRecord, null);
        }
        else
        {
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "AdjustFactor", raxTable, raxRecord, null);
        }

        return numberOfFailedDbUpdates;
        
    } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(RaxAdjustFactorRecord record)
    {
        String key = null;
        
        key = record.getLid() + "|" + record.getPe1() + record.getPe2() + "|" + record.getIdur() + "|" + record.getT() + record.getS() +"|" + record.getE() +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(AdjustFactorRecord record)
    {
        String key = null;
        
        key = record.getLid() + "|" + record.getPe() + "|" + record.getDur() + "|" + record.getTs() +"|" + record.getExtremum() +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {
        RaxAdjustFactorRecord raxRecord = (RaxAdjustFactorRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getLid() + "|"
                + raxRecord.getPe1() + "|"
                + raxRecord.getPe2() + "|"
                + raxRecord.getDur() + "|"
                + displayInt(raxRecord.getIdur()) + "|"
                + raxRecord.getT() + "|"
                + raxRecord.getS() + "|"
                + raxRecord.getE() + "|"
                + displayDate(raxRecord.getBegin_date()) + "|"
                + displayDouble(raxRecord.getDivisor()) + "|"
                + displayDouble(raxRecord.getBase()) + "|"
                + displayDouble(raxRecord.getMultiplier()) + "|"
                + displayDouble(raxRecord.getAdder()) + "|");

        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method

    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxAdjustFactorTable table = (RaxAdjustFactorTable) dbTable;
        RaxAdjustFactorRecord record = (RaxAdjustFactorRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxAdjustFactorTable table = (RaxAdjustFactorTable) dbTable;
        RaxAdjustFactorRecord recordOld = (RaxAdjustFactorRecord) dbRecordOld;
        RaxAdjustFactorRecord recordNew = (RaxAdjustFactorRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }

} // end of AdjustFactorDifferenceMgr class
