package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.DataLimitsRecord;
import ohd.hseb.raxdb.generated.RaxDataLimitsRecord;
import ohd.hseb.raxdb.generated.RaxDataLimitsTable;

public class DataLimitsDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    public DataLimitsDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);
        
        setupOutputFiles("DataLimits");

    } // end of Constructor
    
    //----------------------------------------------------------------------------------------------
    public DiffSet findDifferences()
    {
        DiffSet tableDifferences = new DiffSet();

        List ihfsRecordList = null;
        List raxRecordList = null;
        
        RecordDifference oneDifference = null;

        logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing DataLimits Table records...");
        
        ihfsRecordList = _dm.getIhfsDataLimitsRecordList();
        _numberOfIhfsRecords = _numberOfIhfsRecords + ihfsRecordList.size();
        raxRecordList = _dm.getRaxDataLimitsRecordList();

        //add the RAX records to a Map
        Map raxPrimaryKeyMap = new HashMap();
        for (int i=0; i < raxRecordList.size(); i++)
        {
            RaxDataLimitsRecord record = (RaxDataLimitsRecord) raxRecordList.get(i);
            String raxMapKey = createKeyForMap(record);
            raxPrimaryKeyMap.put(raxMapKey, record);
        } // end of raxRecordList for loop
        
        // loop through IHFS record list and check if same key found in RAX
        // if not, this needs to be added as new to RAX
        // if same key found then check to see if they are "different"
        for (int i=0; i<ihfsRecordList.size(); i++)
        {
            List fieldDifferenceList = null;

            DataLimitsRecord currentIhfsRecord = (DataLimitsRecord) ihfsRecordList.get(i);
            String currentIhfsMapKey = createKeyForMap(currentIhfsRecord);
            RaxDataLimitsRecord raxRecord = (RaxDataLimitsRecord) raxPrimaryKeyMap.get(currentIhfsMapKey);

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
            
        displayEndOfFindDifferencesMessage("DataLimits");
        
        return tableDifferences;

    } // end of findDifferences method

    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(DataLimitsRecord ihfsRecord, RaxDataLimitsRecord raxRecord)
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
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, DataLimitsRecord ihfsRecord,
            final RaxDataLimitsRecord raxRecord, List fieldDiffList)
    {
        RecordDifference oneDifferenceRecord;
        RaxDataLimitsRecord newRaxRecord = null;

        if (type == RecordDifferenceOriginType.NEW)
        {
            newRaxRecord = new RaxDataLimitsRecord();

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
            newRaxRecord =  new RaxDataLimitsRecord(raxRecord);            
        }

        oneDifferenceRecord = new RecordDifference(type, this, ihfsRecord, newRaxRecord, fieldDiffList);
        return oneDifferenceRecord;

    } // end of createDifferenceEntry method

    //----------------------------------------------------------------------------------------------
    public void reportDifferences(RecordDifference oneRecordDifference)
    {
        FieldDifference oneFieldDifference = new FieldDifference();

        DataLimitsRecord ihfsRecord = (DataLimitsRecord) oneRecordDifference.getIhfsRecord();
        RaxDataLimitsRecord raxRecord = (RaxDataLimitsRecord) oneRecordDifference.getRaxRecord();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            logRecordDifferenceMessage("\nDifferences found in Table = DataLimits" + " for key = |" + createKeyForMap(raxRecord) + raxRecord.getMonthdayend() + "|");
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                logRecordDifferenceMessage("Column = " + oneFieldDifference.getName() + "   IHFS_value = "
                        + oneFieldDifference.getIhfsValue() + "   Rax_value = " + oneFieldDifference.getRaxValue());
            }
        }
        else
        {

            logNewRecordMessage("\nA new DataLimits record will need to be created in RAX db for key = |" + createKeyForMap(raxRecord) + raxRecord.getMonthdayend() + "|" +
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

            logDebugMessage("\nA new DataLimits record will need to be created in RAX db for key = |" + createKeyForMap(raxRecord) + raxRecord.getMonthdayend() + "|" +
                            "\nusing the following information from the IHFS db:" +
                            "\npe1 = " + raxRecord.getPe1() + " pe2 = " + raxRecord.getPe2() + "   IHFS_pe = " + ihfsRecord.getPe() +
                            "\ndur = " + raxRecord.getDur() + "   IHFS_dur = " + displayInt(ihfsRecord.getDur()) +
                            "\nidur = " + displayInt(raxRecord.getIdur()) + "   IHFS_dur = " + displayInt(ihfsRecord.getDur()) +
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
        RaxDataLimitsTable raxTable = null;
        RaxDataLimitsRecord newRaxRecord = null;
        int transactionType = UPDATE;
        
        DataLimitsRecord ihfsRecord = (DataLimitsRecord) oneRecordDifference.getIhfsRecord();
        RaxDataLimitsRecord raxRecord = (RaxDataLimitsRecord) oneRecordDifference.getRaxRecord();

        raxTable = _dm.getRaxDataLimitsTable();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxDataLimitsRecord(raxRecord);

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
        
        numberOfFailedDbUpdates = processRaxInsertUpdate(transactionType, "DataLimits", raxTable, raxRecord, newRaxRecord);

        return numberOfFailedDbUpdates;
        
    } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(RaxDataLimitsRecord record)
    {
        String key = null;
        
        key = record.getPe1() + record.getPe2() + "|" + record.getIdur() + "|" + record.getMonthdaystart() +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(DataLimitsRecord record)
    {
        String key = null;
        
        key = record.getPe() + "|" + record.getDur() + "|" + record.getMonthdaystart() +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {
        RaxDataLimitsRecord raxRecord = (RaxDataLimitsRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getPe1() + "|"
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
        RaxDataLimitsTable table = (RaxDataLimitsTable) dbTable;
        RaxDataLimitsRecord record = (RaxDataLimitsRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxDataLimitsTable table = (RaxDataLimitsTable) dbTable;
        RaxDataLimitsRecord recordOld = (RaxDataLimitsRecord) dbRecordOld;
        RaxDataLimitsRecord recordNew = (RaxDataLimitsRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }

} // end of DataLimitsDifferenceMgr class
