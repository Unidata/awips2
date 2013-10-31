package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.IngestFilterRecord;
import ohd.hseb.raxdb.generated.RaxIngestFilterRecord;
import ohd.hseb.raxdb.generated.RaxIngestFilterTable;


public class IngestFilterDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    private DiffSet _ingestFilterDifferences;

    public IngestFilterDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);
        
        setupOutputFiles("IngestFilter");

        _ingestFilterDifferences = new DiffSet();
    } // end of IngestFilterDifferenceFinder constructor

    //----------------------------------------------------------------------------------------------
    public DiffSet findDifferences()
    {
        
        logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing IngestFilter Table records...");
        String tokenName = "adb_sync_ihfs_ingest";
        logApplicationMessage("Apps_defaults Token " + tokenName + " set to: " + _dm.getRawTokenValue(tokenName) +
                              "\nSynchronize the ingest column of the IngestFilter tables: " + _dm.useIngestColumnForComparison());


        findSomeDifferences("0", "A");  // Zero up to A
        findSomeDifferences("A", "C");  // A & B
        findSomeDifferences("C", "E");  // C & D
        findSomeDifferences("E", "G");  // E & F
        findSomeDifferences("G", "I");  // G & H
        findSomeDifferences("I", "K");  // I & J
        findSomeDifferences("K", "M");  // K & L
        findSomeDifferences("M", "O");  // M & N
        findSomeDifferences("O", "Q");  // O & P
        findSomeDifferences("Q", "S");  // Q & R
        findSomeDifferences("S", "U");  // S & T
        findSomeDifferences("U", "W");  // U & V
        findSomeDifferences("W", "X");  // W & X
        findSomeDifferences("X", "ZZZZZZZZ");  // X - ZZZZZZZZ

        displayEndOfFindDifferencesMessage("IngestFilter");
        
        return _ingestFilterDifferences;

    } // end of findDifferences method
    
    private void findSomeDifferences(String first, String last)
    {
        List ihfsRecordList = null;
        List raxRecordList = null;
        
        RecordDifference oneDifference = null;

        ihfsRecordList = _dm.getIhfsIngestFilterRecordList(first, last);
        _numberOfIhfsRecords = _numberOfIhfsRecords + ihfsRecordList.size();
//        logApplicationMessage("\ngetIhfsIngestFilterRecordList for range of >= " + first + " and < " + last + " has " + ihfsRecordList.size() + " Records");
        System.gc();
        raxRecordList = _dm.getRaxIngestFilterRecordList(first, last);
//        logApplicationMessage("\ngetRaxIngestFilterRecordList for range of >= " + first + " and < " + last + " has " + raxRecordList.size() + " Records");

        //add the RAX records to a Map
        Map raxPrimaryKeyMap = new HashMap();
        for (int i=0; i < raxRecordList.size(); i++)
        {
            RaxIngestFilterRecord record = (RaxIngestFilterRecord) raxRecordList.get(i);
            String raxMapKey = createKeyForMap(record);
            raxPrimaryKeyMap.put(raxMapKey, record);
        } // end of raxRecordList for loop
        
        // loop through IHFS record list and check if same key found in RAX
        // if not, this needs to be added as new to RAX
        // if same key found then check to see if they are "different"
        for (int i=0; i<ihfsRecordList.size(); i++)
        {
            List fieldDifferenceList = null;

            IngestFilterRecord currentIhfsRecord = (IngestFilterRecord) ihfsRecordList.get(i);
            String currentIhfsMapKey = createKeyForMap(currentIhfsRecord);
            RaxIngestFilterRecord raxRecord = (RaxIngestFilterRecord) raxPrimaryKeyMap.get(currentIhfsMapKey);

            if (raxRecord == null)  //not in the rax database
            {
                oneDifference = createDifferenceEntry(RecordDifferenceOriginType.NEW, currentIhfsRecord, null, null);
                _ingestFilterDifferences.addDifference(oneDifference);
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
                    _ingestFilterDifferences.addDifference(oneDifference);
                    _numberOfModRecords++;
                } 
            }

        } // end of ihfsRecordList for loop
                
    } // end of findSomeDifferences method
    
    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(IngestFilterRecord ihfsRecord, RaxIngestFilterRecord raxRecord)
    {
        List fieldDifferenceList = new ArrayList();

        checkAndAddToFieldDifferenceList("ts_rank", ihfsRecord.getTs_rank(), raxRecord.getTs_rank(), fieldDifferenceList);

        if (_dm.useIngestColumnForComparison())
            checkAndAddToFieldDifferenceList("ingest", setRaxIngestFlag(ihfsRecord.getIngest()), raxRecord.getIngest(), fieldDifferenceList);

        return fieldDifferenceList;

    } // end of getFieldDifferenceList method

    //----------------------------------------------------------------------------------------------
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, IngestFilterRecord ihfsRecord,
                                            final RaxIngestFilterRecord raxRecord, List fieldDiffList)
    {
        RecordDifference oneDifferenceRecord;
        RaxIngestFilterRecord newRaxRecord = null;
        
        if (type == RecordDifferenceOriginType.NEW)
        {
            newRaxRecord = new RaxIngestFilterRecord();

            newRaxRecord.setLid(ihfsRecord.getLid());
            newRaxRecord.setPe1(createRaxPeOrTsFromIhfs(ihfsRecord.getPe(), 1));
            newRaxRecord.setPe2(createRaxPeOrTsFromIhfs(ihfsRecord.getPe(), 2));
            newRaxRecord.setDur(setRaxDurFromIhfsDur(ihfsRecord.getDur()));
            newRaxRecord.setIdur(ihfsRecord.getDur());
            newRaxRecord.setT(createRaxPeOrTsFromIhfs(ihfsRecord.getTs(), 1));
            newRaxRecord.setS(createRaxPeOrTsFromIhfs(ihfsRecord.getTs(), 2));
            newRaxRecord.setE(ihfsRecord.getExtremum());
            newRaxRecord.setTs_rank(ihfsRecord.getTs_rank());
            newRaxRecord.setDet(DbTable.getNullString());  // set to null as per adbinit rules
            if (_dm.useIngestColumnForComparison())
                newRaxRecord.setIngest(setRaxIngestFlag(ihfsRecord.getIngest()));
            else // don't use IHFS ingest value, just set it to True
                newRaxRecord.setIngest(setRaxIngestFlag("T"));
            newRaxRecord.setNew_report("N");
            newRaxRecord.setActive("Y");
            newRaxRecord.setOfs_input(setRaxInputFlag(ihfsRecord.getOfs_input()));
            newRaxRecord.setObstime(DbTable.getNullTime());  // set to null as per adbinit rules
            newRaxRecord.setOwnag(DbTable.getNullString());  // set to null as per adbinit rules
            newRaxRecord.setOwnloc(DbTable.getNullString());  // set to null as per adbinit rules
            newRaxRecord.setMpe_input(setRaxInputFlag(ihfsRecord.getStg2_input()));
        }
        else if (type == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxIngestFilterRecord(raxRecord);            
        }
        
        oneDifferenceRecord = new RecordDifference(type, this, ihfsRecord, newRaxRecord, fieldDiffList);
        return oneDifferenceRecord;
        
    } // end of createDifferenceEntry method

    //----------------------------------------------------------------------------------------------
    public void reportDifferences(RecordDifference oneRecordDifference)
    {

        FieldDifference oneFieldDifference = new FieldDifference();

        IngestFilterRecord ihfsIngestRecord = (IngestFilterRecord) oneRecordDifference.getIhfsRecord();
        RaxIngestFilterRecord raxIngestRecord = (RaxIngestFilterRecord) oneRecordDifference.getRaxRecord();
        
        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            logRecordDifferenceMessage("\nDifferences found in Table = IngestFilter for key = |" + createKeyForMap(raxIngestRecord));
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                logRecordDifferenceMessage("Column = " + oneFieldDifference.getName() + "   IHFS_value = "
                            + oneFieldDifference.getIhfsValue() + "   Rax_value = " + oneFieldDifference.getRaxValue());
             }
        }
        else
        {
            logNewRecordMessage("\nA New Ingest Filter record will need to be created in RAX db for key = |" + createKeyForMap(raxIngestRecord) +
                                "\nlid = " + raxIngestRecord.getLid() +
                                "\npe1 = " + raxIngestRecord.getPe1() + " pe2 = " + raxIngestRecord.getPe2() +
                                "\ndur = " + raxIngestRecord.getDur() +
                                "\nidur = " + displayInt(raxIngestRecord.getIdur()) +
                                "\nt = " + raxIngestRecord.getT() + " s = " + raxIngestRecord.getS() +
                                "\ne = " + raxIngestRecord.getE() +
                                "\nts_rank = " + displayInt(raxIngestRecord.getTs_rank()) +
                                "\ndet = " + raxIngestRecord.getDet() +
                                "\ningest = " + displayInt(raxIngestRecord.getIngest()) +
                                "\nnew_report = " + raxIngestRecord.getNew_report() +
                                "\nactive = " + raxIngestRecord.getActive() +
                                "\nofs_input = " + raxIngestRecord.getOfs_input() +
                                "\nobstime = " + displayTime(raxIngestRecord.getObstime()) +
                                "\nownag = " + raxIngestRecord.getOwnag() +
                                "\nownloc = " + raxIngestRecord.getOwnloc() +
                                "\nmpe_input = " + raxIngestRecord.getMpe_input());

            logDebugMessage("\nA New Ingest Filter record will need to be created in RAX db for key = |" + createKeyForMap(raxIngestRecord) +
            "\nusing the following information from the IHFS db:" +
            "\nlid = " + raxIngestRecord.getLid() + "   IHFS_lid = " + ihfsIngestRecord.getLid() +
            "\npe1 = " + raxIngestRecord.getPe1() + " pe2 = " + raxIngestRecord.getPe2() + "   IHFS_pe = " + ihfsIngestRecord.getPe() +
            "\ndur = " + raxIngestRecord.getDur() + "   IHFS_dur = " + displayInt(ihfsIngestRecord.getDur()) +
            "\nidur = " + displayInt(raxIngestRecord.getIdur()) + "   IHFS_dur = " + displayInt(ihfsIngestRecord.getDur()) +
            "\nt = " + raxIngestRecord.getT() + " s = " + raxIngestRecord.getS() + "   IHFS_ts = " + ihfsIngestRecord.getTs() +
            "\ne = " + raxIngestRecord.getE() + "   IHFS_extremum = " + ihfsIngestRecord.getExtremum() +
            "\nts_rank = " + displayInt(raxIngestRecord.getTs_rank()) + "   IHFS_ts_rank = " + displayInt(ihfsIngestRecord.getTs_rank()) +
            "\ndet = " + raxIngestRecord.getDet() +
            "\ningest = " + displayInt(raxIngestRecord.getIngest()) + "   IHFS_ingest = " + ihfsIngestRecord.getIngest() +
            "\nnew_report = " + raxIngestRecord.getNew_report() +
            "\nactive = " + raxIngestRecord.getActive() +
            "\nofs_input = " + raxIngestRecord.getOfs_input() + "   IHFS_ofs_input = " + ihfsIngestRecord.getOfs_input() +
            "\nobstime = " + displayTime(raxIngestRecord.getObstime()) +
            "\nownag = " + raxIngestRecord.getOwnag() +
            "\nownloc = " + raxIngestRecord.getOwnloc() +
            "\nmpe_input = " + raxIngestRecord.getMpe_input() + "   IHFS_stg2_input = " + ihfsIngestRecord.getStg2_input());
        }
        
    } // end of reportDifferences method

    //----------------------------------------------------------------------------------------------
    public int processDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;
        RaxIngestFilterTable raxTable = null;
        RaxIngestFilterRecord newRaxRecord = null;
        int transactionType = UPDATE;

        IngestFilterRecord ihfsRecord = (IngestFilterRecord) oneRecordDifference.getIhfsRecord();
        RaxIngestFilterRecord raxRecord = (RaxIngestFilterRecord) oneRecordDifference.getRaxRecord();

        raxTable = _dm.getRaxIngestFilterTable();
        
        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxIngestFilterRecord(raxRecord);

            newRaxRecord.setTs_rank(ihfsRecord.getTs_rank());
            if (_dm.useIngestColumnForComparison())
                newRaxRecord.setIngest(setRaxIngestFlag(ihfsRecord.getIngest()));
        }
        else
            transactionType = INSERT;
        
        numberOfFailedDbUpdates = processRaxInsertUpdate(transactionType, "IngestFilter", raxTable, raxRecord, newRaxRecord);

        return numberOfFailedDbUpdates;
        
    } // end of processDifferences method

    private String createKeyForMap(RaxIngestFilterRecord record)
    {
        String key = null;
        
        key = record.getLid() + "|" + record.getPe1() + record.getPe2() + "|" + record.getIdur() +
              "|" + record.getT() + record.getS() + "|" + record.getE() +"|";
        
        return key;
    }

    private String createKeyForMap(IngestFilterRecord record)
    {
        String key = null;
        
        key = record.getLid() + "|" + record.getPe() + "|" + record.getDur() +
              "|" + record.getTs() + "|" + record.getExtremum() +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------

    private String setRaxInputFlag(String ihfsFlag)
    {
        String raxInputFlag = "1";
        
        // set RAX ofs_input or mpe_input field to the character string "0" (zero)
        // if IHFS ofs_input or stg2_input field is "F" (false),
        // otherwise set it to character string "1" (one)
        if (ihfsFlag != null)
        {
            if (ihfsFlag.equalsIgnoreCase("F"))
                raxInputFlag = "0";
        }

        return raxInputFlag;
        
    }// end of setRaxInputFlag method

    private short setRaxIngestFlag(String ihfsFlag)
    {
        short raxIngestFlag = 1;
        
        // set RAX ingest field to the number 0 (zero) if IHFS ingest field is "F" (false),
        // otherwise set it to number 1 (one)
        if (ihfsFlag != null)
        {
            if (ihfsFlag.equalsIgnoreCase("F"))
                raxIngestFlag = 0;
        }

        return raxIngestFlag;
        
    }// end of setRaxIngestFlag method

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {
        RaxIngestFilterRecord raxRecord = (RaxIngestFilterRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getLid() + "|"
                + raxRecord.getPe1() + "|"
                + raxRecord.getPe2() + "|"
                + raxRecord.getDur() + "|"
                + displayInt(raxRecord.getIdur()) + "|"
                + raxRecord.getT() + "|"
                + raxRecord.getS() + "|"
                + raxRecord.getE() + "|"
                + displayInt(raxRecord.getTs_rank()) + "|"
                + raxRecord.getDet() + "|"
                + displayInt(raxRecord.getIngest()) + "|"
                + raxRecord.getNew_report() + "|"
                + raxRecord.getActive() + "|"
                + raxRecord.getOfs_input() + "|"
                + displayTime(raxRecord.getObstime()) + "|"
                + raxRecord.getOwnag() + "|"
                + raxRecord.getOwnloc() + "|"
                + raxRecord.getMpe_input() + "|");

        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method

    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxIngestFilterTable table = (RaxIngestFilterTable) dbTable;
        RaxIngestFilterRecord record = (RaxIngestFilterRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxIngestFilterTable table = (RaxIngestFilterTable) dbTable;
        RaxIngestFilterRecord recordOld = (RaxIngestFilterRecord) dbRecordOld;
        RaxIngestFilterRecord recordNew = (RaxIngestFilterRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }

} // end of IngestFilterDifferenceMgr class

