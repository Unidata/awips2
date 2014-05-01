package ohd.hseb.raxdb_sync;

import java.util.List;

import ohd.hseb.db.DbRecord;

public class RecordDifference
{
    private RecordDifferenceOriginType _diffType = null;
    
    private IDifferenceMgr _differenceMgr;

    private DbRecord _ihfsRecord = null;
    private DbRecord _raxRecord = null;

    private List _fieldDifferenceList = null;

    public RecordDifference ()
    {

    } // end of default Difference constructor
    
    public RecordDifference (RecordDifferenceOriginType diffType, IDifferenceMgr differenceMgr, DbRecord ihfsRecord, DbRecord raxRecord,
                             List fieldDifferenceList)
    {
        setDiffType(diffType);
        setDifferenceMgr(differenceMgr);
        setIhfsRecord(ihfsRecord);
        setRaxRecord(raxRecord);
        setFieldDifferenceList(fieldDifferenceList);
    } // end of Difference constructor
    
    private void setDiffType(RecordDifferenceOriginType diffType)
    {
        _diffType = diffType;
    }
    
    public RecordDifferenceOriginType getDiffType()
    {
        return _diffType;
    }
    
    private void setDifferenceMgr(IDifferenceMgr differenceMgr)
    {
        _differenceMgr = differenceMgr;
    }

    public IDifferenceMgr getDifferenceMgr()
    {
        return _differenceMgr;
    }

    private void setIhfsRecord(DbRecord ihfsRecord)
    {
        _ihfsRecord = ihfsRecord;
    }

    public DbRecord getIhfsRecord()
    {
        return _ihfsRecord;
    }

    private void setRaxRecord(DbRecord raxRecord)
    {
        _raxRecord = raxRecord;
    }

    public DbRecord getRaxRecord()
    {
        return _raxRecord;
    }
        
    private void setFieldDifferenceList(List fieldDifferenceList)
    {
        _fieldDifferenceList = fieldDifferenceList;
    }

    public List getFieldDifferenceList()
    {
        return _fieldDifferenceList;
    }
        
} // end of Difference class
