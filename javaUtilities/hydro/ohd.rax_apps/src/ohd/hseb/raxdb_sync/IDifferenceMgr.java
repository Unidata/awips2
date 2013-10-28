package ohd.hseb.raxdb_sync;


public interface IDifferenceMgr
{

    public DiffSet findDifferences();
    public void reportDifferences(RecordDifference oneRecordDifference);
    public int processDifferences(RecordDifference oneRecordDifference);

} // end of IDifferenceMgr interface
