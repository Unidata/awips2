package ohd.hseb.raxdb_sync;

import java.sql.SQLException;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.raxbase.model.RaxRatingShift;
import ohd.hseb.raxdb.generated.RaxRatingShiftRecord;
import ohd.hseb.raxdb.generated.RaxRatingShiftTable;

public class RatingShiftDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    public RatingShiftDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);

    } // end of Constructor
    
    //----------------------------------------------------------------------------------------------
    public DiffSet findDifferences()
    {
 
//        throw (new Error ("This object is not a standard Difference Mgr, and is only to be used to resolve differences, not find them."));
        
        DiffSet tableDifferences = new DiffSet();
        
        return tableDifferences;

    } // end of findDifferences method
    
    //----------------------------------------------------------------------------------------------
    public void reportDifferences(RecordDifference oneRecordDifference)
    {

        throw (new Error ("This object is not a standard Difference Mgr, and is only to be used to resolve differences, not report them."));

    } // end of reportDifferences method

    //----------------------------------------------------------------------------------------------
    public int processDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;

        RatingCurveHolder raxRatingCurveHolder = (RatingCurveHolder) oneRecordDifference.getRaxRecord();
        RaxRatingShift raxRatingShift = raxRatingCurveHolder.getRatingShift();
        
        RaxRatingShiftRecord raxRatingShiftRecord = RaxRatingShift.getRaxRatingShiftRecord(raxRatingShift);

        RaxRatingShiftTable raxRatingShiftTable = _dm.getRaxRatingShiftTable();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            RatingCurveHolder ihfsRatingCurveHolder = (RatingCurveHolder) oneRecordDifference.getIhfsRecord();
            RaxRatingShift ihfsRatingShift = ihfsRatingCurveHolder.getRatingShift();
            
            if (ihfsRatingShift != null)
                raxRatingShift.setShiftA(ihfsRatingShift.getShiftA());
            
            RaxRatingShiftRecord newRaxRatingShiftRecord = RaxRatingShift.getRaxRatingShiftRecord(raxRatingShift);
            newRaxRatingShiftRecord.setBegin_date(currentTimeMillisTrimmedToSeconds());

            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "RatingShift", raxRatingShiftTable, newRaxRatingShiftRecord, null);
        }
        else
        {
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "RatingShift", raxRatingShiftTable, raxRatingShiftRecord, null);
        }
        
        return numberOfFailedDbUpdates;

    } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {
        RaxRatingShiftRecord raxRecord = (RaxRatingShiftRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getLid() + "|"
                + raxRecord.getPe1() + "|"
                + raxRecord.getPe2() + "|"
                + displayDouble(raxRecord.getTbl_ver()) + "|"
                + displayDateTime(raxRecord.getBegin_date()) + "|"
                + raxRecord.getSrc() + "|"
                + displayDouble(raxRecord.getVal_a()) + "|"
                + displayDouble(raxRecord.getSh_a()) + "|"
                + displayDouble(raxRecord.getVal_b()) + "|"
                + displayDouble(raxRecord.getSh_b()) + "|"
                + displayDouble(raxRecord.getVal_c()) + "|"
                + displayDouble(raxRecord.getSh_c()) + "|"
                + displayDouble(raxRecord.getVal_d()) + "|"
                + displayDouble(raxRecord.getSh_d()) + "|"
                + displayDouble(raxRecord.getDatum_adj()) + "|");

        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method

    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxRatingShiftTable table = (RaxRatingShiftTable) dbTable;
        RaxRatingShiftRecord record = (RaxRatingShiftRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxRatingShiftTable table = (RaxRatingShiftTable) dbTable;
        RaxRatingShiftRecord recordOld = (RaxRatingShiftRecord) dbRecordOld;
        RaxRatingShiftRecord recordNew = (RaxRatingShiftRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }
    
}// end of RatingShiftDifferenceMgr class
