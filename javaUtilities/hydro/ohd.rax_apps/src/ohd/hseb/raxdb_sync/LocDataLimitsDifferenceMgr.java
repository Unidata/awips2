package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.LocDataLimitsRecord;
import ohd.hseb.raxdb.generated.RaxLocDataLimitsRecord;
import ohd.hseb.raxdb.generated.RaxLocDataLimitsTable;

public class LocDataLimitsDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    public LocDataLimitsDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);
        
        setupOutputFiles("LocDataLimits");

    } // end of Constructor
    
    //----------------------------------------------------------------------------------------------
    public DiffSet findDifferences()
    {
        DiffSet tableDifferences = new DiffSet();

        List ihfsRecordList = null;
        List raxRecordList = null;
        
        RecordDifference oneDifference = null;

        logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing LocDataLimits Table records...");
        
        ihfsRecordList = _dm.getIhfsLocDataLimitsRecordList();
        _numberOfIhfsRecords = _numberOfIhfsRecords + ihfsRecordList.size();
        raxRecordList = _dm.getRaxLocDataLimitsRecordList();

        //add the RAX records to a Map
        Map raxPrimaryKeyMap = new HashMap();
        for (int i=0; i < raxRecordList.size(); i++)
        {
            RaxLocDataLimitsRecord record = (RaxLocDataLimitsRecord) raxRecordList.get(i);
            String raxMapKey = createKeyForMap(record);
            raxPrimaryKeyMap.put(raxMapKey, record);
        } // end of raxRecordList for loop
        
        // loop through IHFS record list and check if same key found in RAX
        // if not, this needs to be added as new to RAX
        // if same key found then check to see if they are "different"
        for (int i=0; i<ihfsRecordList.size(); i++)
        {
            List fieldDifferenceList = null;

            LocDataLimitsRecord currentIhfsRecord = (LocDataLimitsRecord) ihfsRecordList.get(i);
            String currentIhfsMapKey = createKeyForMap(currentIhfsRecord);
            RaxLocDataLimitsRecord raxRecord = (RaxLocDataLimitsRecord) raxPrimaryKeyMap.get(currentIhfsMapKey);

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
                
        displayEndOfFindDifferencesMessage("LocDataLimits");
        
        return tableDifferences;

    } // end of findDifferences method

    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(LocDataLimitsRecord ihfsRecord, RaxLocDataLimitsRecord raxRecord)
    {
        List fieldDifferenceList = new ArrayList();

        checkAndAddToFieldDifferenceList("monthdayend", ihfsRecord.getMonthdayend(), raxRecord.getMonthdayend(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("gross_range_min", ihfsRecord.getGross_range_min(), raxRecord.getGross_range_min(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("gross_range_max", ihfsRecord.getGross_range_max(), raxRecord.getGross_range_max(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("reason_range_min", ihfsRecord.getReason_range_min(), raxRecord.getReason_range_min(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("reason_range_max", ihfsRecord.getReason_range_max(), raxRecord.getReason_range_max(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("roc_max", ihfsRecord.getRoc_max(), raxRecord.getRoc_max(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("alert_limit", ihfsRecord.getAlert_upper_limit(), raxRecord.getAlert_limit(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("alert_roc_limit", ihfsRecord.getAlert_roc_limit(), raxRecord.getAlert_roc_limit(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("alarm_limit", ihfsRecord.getAlarm_upper_limit(), raxRecord.getAlarm_limit(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("alarm_roc_limit", ihfsRecord.getAlarm_roc_limit(), raxRecord.getAlarm_roc_limit(), fieldDifferenceList);

        return fieldDifferenceList;

    } // end of getFieldDifferenceList method
    
    //----------------------------------------------------------------------------------------------
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, LocDataLimitsRecord ihfsRecord,
            final RaxLocDataLimitsRecord raxRecord, List fieldDiffList)
    {
        RecordDifference oneDifferenceRecord;
        RaxLocDataLimitsRecord newRaxRecord = null;

        if (type == RecordDifferenceOriginType.NEW)
        {
            newRaxRecord = new RaxLocDataLimitsRecord();

            newRaxRecord.setLid(ihfsRecord.getLid());
            newRaxRecord.setPe1(createRaxPeOrTsFromIhfs(ihfsRecord.getPe(), 1));
            newRaxRecord.setPe2(createRaxPeOrTsFromIhfs(ihfsRecord.getPe(), 2));
            newRaxRecord.setDur(setRaxDurFromIhfsDur(ihfsRecord.getDur()));
            newRaxRecord.setIdur(ihfsRecord.getDur());
            newRaxRecord.setMonthdaystart(ihfsRecord.getMonthdaystart());
            newRaxRecord.setMonthdayend(ihfsRecord.getMonthdayend());
            newRaxRecord.setGross_range_min(ihfsRecord.getGross_range_min());
            newRaxRecord.setGross_range_max(ihfsRecord.getGross_range_max());
            newRaxRecord.setReason_range_min(ihfsRecord.getReason_range_min());
            newRaxRecord.setReason_range_max(ihfsRecord.getReason_range_max());
            newRaxRecord.setRoc_max(ihfsRecord.getRoc_max());
            newRaxRecord.setAlert_limit(ihfsRecord.getAlert_upper_limit());
            newRaxRecord.setAlert_roc_limit(ihfsRecord.getAlert_roc_limit());
            newRaxRecord.setAlarm_limit(ihfsRecord.getAlarm_upper_limit());
            newRaxRecord.setAlarm_roc_limit(ihfsRecord.getAlarm_roc_limit());
        }
        else if (type == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxLocDataLimitsRecord(raxRecord);            
        }

        oneDifferenceRecord = new RecordDifference(type, this, ihfsRecord, newRaxRecord, fieldDiffList);
        return oneDifferenceRecord;

    } // end of createDifferenceEntry method

    //----------------------------------------------------------------------------------------------
    public void reportDifferences(RecordDifference oneRecordDifference)
    {
        FieldDifference oneFieldDifference = new FieldDifference();

        LocDataLimitsRecord ihfsRecord = (LocDataLimitsRecord) oneRecordDifference.getIhfsRecord();
        RaxLocDataLimitsRecord raxRecord = (RaxLocDataLimitsRecord) oneRecordDifference.getRaxRecord();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            logRecordDifferenceMessage("\nDifferences found in Table = LocDataLimits" + " for key = |" + createKeyForMap(raxRecord) + raxRecord.getMonthdayend() + "|");
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                logRecordDifferenceMessage("Column = " + oneFieldDifference.getName() + "  IHFS_value = "
                        + oneFieldDifference.getIhfsValue() + "  Rax_value = " + oneFieldDifference.getRaxValue());
            }
        }
        else
        {

            logNewRecordMessage("\nA new LocDataLimits record will need to be created in RAX db for key = |" + createKeyForMap(raxRecord) + raxRecord.getMonthdayend() + "|" +
                                "\nlid = " + raxRecord.getLid() +
                                "\npe1 = " + raxRecord.getPe1() + " pe2 = " + raxRecord.getPe2() +
                                "\ndur = " + raxRecord.getDur() +
                                "\nidur = " + displayInt(raxRecord.getIdur()) +
                                "\nmonthdaystart = " + raxRecord.getMonthdaystart() +
                                "\nmonthdayend = " + raxRecord.getMonthdayend() +
                                "\ngross_range_min = " + displayDouble(raxRecord.getGross_range_min()) +
                                "\ngross_range_max = " + displayDouble(raxRecord.getGross_range_max()) +
                                "\nreason_range_min = " + displayDouble(raxRecord.getReason_range_min()) +
                                "\nreason_range_max = " + displayDouble(raxRecord.getReason_range_max()) +
                                "\nroc_max = " + displayDouble(raxRecord.getRoc_max()) +
                                "\nalert_limit = " + displayDouble(raxRecord.getAlert_limit()) +
                                "\nalert_roc_limit = " + displayDouble(raxRecord.getAlert_roc_limit()) +
                                "\nalarm_limit = " + displayDouble(raxRecord.getAlarm_limit()) +
                                "\nalarm_roc_limit = " + displayDouble(raxRecord.getAlarm_roc_limit()));

            logDebugMessage("\nA new LocDataLimits record will need to be created in RAX db for key = |" + createKeyForMap(raxRecord) + raxRecord.getMonthdayend() + "|" +
                            "\nusing the following information from the IHFS db:" +
                            "\nlid = " + raxRecord.getLid() + "   IHFS_lid = " + ihfsRecord.getLid() +
                            "\npe1 = " + raxRecord.getPe1() + " pe2 = " + raxRecord.getPe2() + "   IHFS_pe = " + ihfsRecord.getPe() +
                            "\ndur = " + raxRecord.getDur() + "   IHFS_dur = " + ihfsRecord.getDur() +
                            "\nidur = " + displayInt(raxRecord.getIdur()) + "   IHFS_dur = " + ihfsRecord.getDur() +
                            "\nmonthdaystart = " + raxRecord.getMonthdaystart() + "   IHFS_monthdaystart = " + ihfsRecord.getMonthdaystart() +
                            "\nmonthdayend = " + raxRecord.getMonthdayend() + "   IHFS_monthdayend = " + ihfsRecord.getMonthdayend() +
                            "\ngross_range_min = " + displayDouble(raxRecord.getGross_range_min()) + "   IHFS_gross_range_min = " + displayDouble(ihfsRecord.getGross_range_min()) +
                            "\ngross_range_max = " + displayDouble(raxRecord.getGross_range_max()) + "   IHFS_gross_range_max = " + displayDouble(ihfsRecord.getGross_range_max()) +
                            "\nreason_range_min = " + displayDouble(raxRecord.getReason_range_min()) + "   IHFS_reason_range_min = " + displayDouble(ihfsRecord.getReason_range_min()) +
                            "\nreason_range_max = " + displayDouble(raxRecord.getReason_range_max()) + "   IHFS_reason_range_max = " + displayDouble(ihfsRecord.getReason_range_max()) +
                            "\nroc_max = " + displayDouble(raxRecord.getRoc_max()) + "   IHFS_roc_max = " + displayDouble(ihfsRecord.getRoc_max()) +
                            "\nalert_limit = " + displayDouble(raxRecord.getAlert_limit()) + "   IHFS_alert_upper_limit = " + displayDouble(ihfsRecord.getAlert_upper_limit()) +
                            "\nalert_roc_limit = " + displayDouble(raxRecord.getAlert_roc_limit()) + "   IHFS_alert_roc_limit = " + displayDouble(ihfsRecord.getAlert_roc_limit()) +
                            "\nalarm_limit = " + displayDouble(raxRecord.getAlarm_limit()) + "   IHFS_alarm_upper_limit = " + displayDouble(ihfsRecord.getAlarm_upper_limit()) +
                            "\nalarm_roc_limit = " + displayDouble(raxRecord.getAlarm_roc_limit()) + "   IHFS_alarm_roc_limit = " + displayDouble(ihfsRecord.getAlarm_roc_limit()));
        }

    } // end of reportDifferences method

    //----------------------------------------------------------------------------------------------
    public int processDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;
        RaxLocDataLimitsTable raxTable = null;
        RaxLocDataLimitsRecord newRaxRecord = null;
        int transactionType = UPDATE;
        
        LocDataLimitsRecord ihfsRecord = (LocDataLimitsRecord) oneRecordDifference.getIhfsRecord();
        RaxLocDataLimitsRecord raxRecord = (RaxLocDataLimitsRecord) oneRecordDifference.getRaxRecord();

        raxTable = _dm.getRaxLocDataLimitsTable();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxLocDataLimitsRecord(raxRecord);
            
            newRaxRecord.setMonthdayend(ihfsRecord.getMonthdayend());
            newRaxRecord.setGross_range_min(ihfsRecord.getGross_range_min());
            newRaxRecord.setGross_range_max(ihfsRecord.getGross_range_max());
            newRaxRecord.setReason_range_min(ihfsRecord.getReason_range_min());
            newRaxRecord.setReason_range_max(ihfsRecord.getReason_range_max());
            newRaxRecord.setRoc_max(ihfsRecord.getRoc_max());
            newRaxRecord.setAlert_limit(ihfsRecord.getAlert_upper_limit());
            newRaxRecord.setAlert_roc_limit(ihfsRecord.getAlert_roc_limit());
            newRaxRecord.setAlarm_limit(ihfsRecord.getAlarm_upper_limit());
            newRaxRecord.setAlarm_roc_limit(ihfsRecord.getAlarm_roc_limit());
        }
        else
            transactionType = INSERT;
        
        numberOfFailedDbUpdates = processRaxInsertUpdate(transactionType, "LocDataLimits", raxTable, raxRecord, newRaxRecord);

        return numberOfFailedDbUpdates;
        
    } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(RaxLocDataLimitsRecord record)
    {
        String key = null;
        
        key = record.getLid() + "|" + record.getPe1() + record.getPe2() + "|" + record.getIdur() + "|" + record.getMonthdaystart() +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(LocDataLimitsRecord record)
    {
        String key = null;
        
        key = record.getLid() + "|" + record.getPe() + "|" + record.getDur() + "|" + record.getMonthdaystart() +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {

        RaxLocDataLimitsRecord raxRecord = (RaxLocDataLimitsRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getLid() + "|"
                + raxRecord.getPe1() + "|"
                + raxRecord.getPe2() + "|"
                + raxRecord.getDur() + "|"
                + displayInt(raxRecord.getIdur()) + "|"
                + raxRecord.getMonthdaystart() + "|"
                + raxRecord.getMonthdayend() + "|"
                + displayDouble(raxRecord.getGross_range_min()) + "|"
                + displayDouble(raxRecord.getGross_range_max()) + "|"
                + displayDouble(raxRecord.getReason_range_min()) + "|"
                + displayDouble(raxRecord.getReason_range_max()) + "|"
                + displayDouble(raxRecord.getRoc_max()) + "|"
                + displayDouble(raxRecord.getAlert_limit()) + "|"
                + displayDouble(raxRecord.getAlert_roc_limit()) + "|"
                + displayDouble(raxRecord.getAlarm_limit()) + "|"
                + displayDouble(raxRecord.getAlarm_roc_limit()) + "|");

        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method

    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxLocDataLimitsTable table = (RaxLocDataLimitsTable) dbTable;
        RaxLocDataLimitsRecord record = (RaxLocDataLimitsRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxLocDataLimitsTable table = (RaxLocDataLimitsTable) dbTable;
        RaxLocDataLimitsRecord recordOld = (RaxLocDataLimitsRecord) dbRecordOld;
        RaxLocDataLimitsRecord recordNew = (RaxLocDataLimitsRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }

} // end of LocDataLimitsDifferenceMgr class
