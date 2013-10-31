package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.CrestRecord;
import ohd.hseb.raxdb.generated.RaxCrestRecord;
import ohd.hseb.raxdb.generated.RaxCrestTable;

public class CrestDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{
    public CrestDifferenceMgr(RaxSyncDataMgr dm)
    {

        setDataMgr(dm);
        
        setupOutputFiles("Crest");

    } // end of Constructor
    
    public DiffSet findDifferences()
    {
       DiffSet tableDifferences = new DiffSet();

       List ihfsRecordList = null;
       List raxRecordList = null;
       
       RecordDifference oneDifference = null;
       
       String previousRecordsKey = "RussErb";

       logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing Crest Table records...");
       
       ihfsRecordList = _dm.getIhfsCrestRecordList();
       _numberOfIhfsRecords = _numberOfIhfsRecords + ihfsRecordList.size();
       raxRecordList = _dm.getRaxCrestRecordList();

       //add the RAX records to a Map
       Map raxPrimaryKeyMap = new HashMap();
       for (int i=0; i < raxRecordList.size(); i++)
       {
           RaxCrestRecord record = (RaxCrestRecord) raxRecordList.get(i);
           String raxMapKey = createKeyForMap(record);
           raxPrimaryKeyMap.put(raxMapKey, record);
       } // end of raxRecordList for loop
       
       // loop through IHFS record list and check if same key found in RAX
       // if not, this needs to be added as new to RAX
       // if same key found then check to see if they are "different"
       // If there are more than one IHFS record for the same Lid then only process the first one
       for (int i=0; i<ihfsRecordList.size(); i++)
       {
           List fieldDifferenceList = null;

           CrestRecord currentIhfsRecord = (CrestRecord) ihfsRecordList.get(i);
           String currentIhfsMapKey = createKeyForMap(currentIhfsRecord);

           if(!currentIhfsMapKey.equals(previousRecordsKey))
           {

               RaxCrestRecord raxRecord = (RaxCrestRecord) raxPrimaryKeyMap.get(currentIhfsMapKey);

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
               } // end of found in RAX dabase

           } // last record's key was the same as the current record's key
           else
           {
               _numberOfSkippedRecords++;
           }
           
           // set the previous lid to now be the current IHFS record's lid 
           previousRecordsKey = currentIhfsMapKey;

       } // end of ihfsRecordList for loop

       displayEndOfFindDifferencesMessage("Crest");
       
       return tableDifferences;

    } // end of findDifferences method

    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(CrestRecord ihfsRecord, RaxCrestRecord raxRecord)
    {
        List fieldDifferenceList = new ArrayList();

        checkAndAddToFieldDifferenceList("crstdatetime", ihfsRecord.getTimcrst(), raxRecord.getCrstdatetime(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("stage", ihfsRecord.getStage(), raxRecord.getStage(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("flow", makeRaxCrestFlowFromIhfsRecord(ihfsRecord), raxRecord.getFlow(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("hw", ihfsRecord.getHw(), raxRecord.getHw(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("jam", ihfsRecord.getJam(), raxRecord.getJam(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("olddatum", ihfsRecord.getOlddatum(), raxRecord.getOlddatum(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("prelim", ihfsRecord.getPrelim(), raxRecord.getPrelim(), fieldDifferenceList);

        return fieldDifferenceList;

    } // end of getFieldDifferenceList method
    
    //----------------------------------------------------------------------------------------------
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, CrestRecord ihfsRecord,
                                            final RaxCrestRecord raxRecord, List fieldDiffList)
    {
        RecordDifference oneDifferenceRecord;
        RaxCrestRecord newRaxRecord = null;
        
        if (type == RecordDifferenceOriginType.NEW)
        {
            newRaxRecord = new RaxCrestRecord();
            
            newRaxRecord.setLid(ihfsRecord.getLid());
            newRaxRecord.setDatecrst(ihfsRecord.getDatcrst());
            newRaxRecord.setCrstdatetime(ihfsRecord.getTimcrst());
            newRaxRecord.setStage(ihfsRecord.getStage());
            newRaxRecord.setStg_qual(DbTable.getNullString());
            newRaxRecord.setFlow(makeRaxCrestFlowFromIhfsRecord(ihfsRecord));
            newRaxRecord.setFlow_qual(DbTable.getNullString());
            newRaxRecord.setHw(ihfsRecord.getHw());
            newRaxRecord.setJam(ihfsRecord.getJam());
            newRaxRecord.setOlddatum(ihfsRecord.getOlddatum());
            newRaxRecord.setPrelim(ihfsRecord.getPrelim());
        }
        else if (type == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxCrestRecord(raxRecord);            
        }
        
        oneDifferenceRecord = new RecordDifference(type, this, ihfsRecord, newRaxRecord, fieldDiffList);
        return oneDifferenceRecord;
        
    } // end of createDifferenceEntry method
    
    //----------------------------------------------------------------------------------------------
     public void reportDifferences(RecordDifference oneRecordDifference)
     {

         FieldDifference oneFieldDifference = new FieldDifference();

         CrestRecord ihfsRecord = (CrestRecord) oneRecordDifference.getIhfsRecord();
         RaxCrestRecord raxRecord = (RaxCrestRecord) oneRecordDifference.getRaxRecord();
         
         if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
         {
             logRecordDifferenceMessage("\nDifferences found in Table = Crest for key = |" + createKeyForMap(ihfsRecord));
             for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
             {
                 oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                 logRecordDifferenceMessage("Column = " + oneFieldDifference.getName() + "   IHFS_value = "
                             + oneFieldDifference.getIhfsValue() + "   Rax_value = " + oneFieldDifference.getRaxValue());
              }
         }
         else
         {   
             logNewRecordMessage("\nA new Crest record will need to be created in RAX db for key = |" + createKeyForMap(ihfsRecord) +
                                 "\nlid = " + raxRecord.getLid() +
                                 "\ndatecrst = " + displayDate(raxRecord.getDatecrst()) +
                                 "\ncrstdatetime = " + raxRecord.getCrstdatetime() +
                                 "\nstage = " + displayDouble(raxRecord.getStage()) +
                                 "\nstg_qual = " + raxRecord.getStg_qual() +
                                 "\nflow = " + displayDouble(raxRecord.getFlow()) +
                                 "\nflow_qual = " + raxRecord.getFlow_qual() +
                                 "\nhw = " + raxRecord.getHw() +
                                 "\njam = " + raxRecord.getJam() +
                                 "\nolddatum = " + raxRecord.getOlddatum() +
                                 "\nprelim = " + raxRecord.getPrelim());

             logDebugMessage("\nA new Crest record will need to be created in RAX db for key = |" + createKeyForMap(ihfsRecord) +
                             "\nusing the following information from the IHFS db:" +
                             "\nlid = " + raxRecord.getLid() + "   IHFS_lid = " + ihfsRecord.getLid() +
                             "\ndatecrst = " + displayDate(raxRecord.getDatecrst()) +
                             "   IHFS_datcrst = " + displayDate(ihfsRecord.getDatcrst()) +
                             "\ncrstdatetime = " + raxRecord.getCrstdatetime() + "   IHFS_timcrst = " + ihfsRecord.getTimcrst() +
                             "\nstage = " + displayDouble(raxRecord.getStage()) + "   IHFS_stage = " + displayDouble(ihfsRecord.getStage()) +
                             "\nstg_qual = " + raxRecord.getStg_qual() +
                             "\nflow = " + displayDouble(raxRecord.getFlow()) + "   IHFS_q = " + displayInt(ihfsRecord.getQ()) +
                             "\nflow_qual = " + raxRecord.getFlow_qual() +
                             "\nhw = " + raxRecord.getHw() + "   IHFS_hw = " + ihfsRecord.getHw() +
                             "\njam = " + raxRecord.getJam() + "   IHFS_jam = " + ihfsRecord.getJam() +
                             "\nolddatum = " + raxRecord.getOlddatum() + "   IHFS_olddatum = " + ihfsRecord.getOlddatum() +
                             "\nprelim = " + raxRecord.getPrelim() + "   IHFS_prelimhw = " + ihfsRecord.getPrelim());
         }
         
     } // end of reportDifferences method

     //----------------------------------------------------------------------------------------------
     public int processDifferences(RecordDifference oneRecordDifference)
     {
         int numberOfFailedDbUpdates = 0;
         RaxCrestTable raxTable = null;
         RaxCrestRecord newRaxRecord = null;
         int transactionType = UPDATE;

         CrestRecord ihfsRecord = (CrestRecord) oneRecordDifference.getIhfsRecord();
         RaxCrestRecord raxRecord = (RaxCrestRecord) oneRecordDifference.getRaxRecord();

         raxTable = _dm.getRaxCrestTable();
         
         if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
         {
             newRaxRecord =  new RaxCrestRecord(raxRecord);

             newRaxRecord.setCrstdatetime(ihfsRecord.getTimcrst());
             newRaxRecord.setStage(ihfsRecord.getStage());
             newRaxRecord.setFlow(makeRaxCrestFlowFromIhfsRecord(ihfsRecord));
             newRaxRecord.setHw(ihfsRecord.getHw());
             newRaxRecord.setJam(ihfsRecord.getJam());
             newRaxRecord.setOlddatum(ihfsRecord.getOlddatum());
             newRaxRecord.setPrelim(ihfsRecord.getPrelim());
         }
         else
             transactionType = INSERT;
         
         numberOfFailedDbUpdates = processRaxInsertUpdate(transactionType, "Crest", raxTable, raxRecord, newRaxRecord);

         return numberOfFailedDbUpdates;
         
     } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(RaxCrestRecord record)
    {
        String key = null;
        
        key = record.getLid() + "|" + displayDate(record.getDatecrst()) +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(CrestRecord record)
    {
        String key = null;
        
        key = record.getLid() + "|" + displayDate(record.getDatcrst()) +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {
        RaxCrestRecord raxRecord = (RaxCrestRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getLid() + "|"
                + displayDate(raxRecord.getDatecrst()) + "|"
                + raxRecord.getCrstdatetime() + "|"
                + displayDouble(raxRecord.getStage()) + "|"
                + raxRecord.getStg_qual() + "|"
                + displayDouble(raxRecord.getFlow()) + "|"
                + raxRecord.getFlow_qual() + "|"
                + raxRecord.getHw() + "|"
                + raxRecord.getJam() + "|"
                + raxRecord.getOlddatum() + "|"
                + raxRecord.getPrelim() + "|");

        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method   

    //----------------------------------------------------------------------------------------------
    private double makeRaxCrestFlowFromIhfsRecord(CrestRecord ihfsRecord)
    {
        double raxCrestFlow = 0;
        
        // to create a RAX flow from an IHFS crest record
        // if IHFS q is null then set to null otherwise cast as a double
        if (DbTable.isNull(ihfsRecord.getQ()))
        {
            raxCrestFlow = DbTable.getNullDouble();
        }
        else
            raxCrestFlow = (double)ihfsRecord.getQ();

        return raxCrestFlow;
        
    }// end of makeRaxCrestFlowFromIhfsRecord method

    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxCrestTable table = (RaxCrestTable) dbTable;
        RaxCrestRecord record = (RaxCrestRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxCrestTable table = (RaxCrestTable) dbTable;
        RaxCrestRecord recordOld = (RaxCrestRecord) dbRecordOld;
        RaxCrestRecord recordNew = (RaxCrestRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }

} // end of CrestDifferenceMgr class
