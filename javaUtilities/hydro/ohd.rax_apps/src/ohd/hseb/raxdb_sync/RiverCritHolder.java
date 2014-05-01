package ohd.hseb.raxdb_sync;

import ohd.hseb.db.DbRecord;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.raxbase.model.RaxRiverCrit;

public class RiverCritHolder extends DbRecord
{

    private RaxRiverCrit _riverCrit = null;
    private RiverstatRecord _riverstatRecord = null;
    
    public RiverCritHolder()
    {
    }

    public RiverCritHolder(RaxRiverCrit riverCrit, RiverstatRecord riverstatRecord)
    {
        setRaxRiverCrit(riverCrit);
        setRiverstatRecord(riverstatRecord);
    }
 
    private void setRaxRiverCrit(RaxRiverCrit riverCrit)
    {
        _riverCrit = riverCrit;
    }
    
    public RaxRiverCrit getRaxRiverCrit()
    {
        return _riverCrit;
    }

    private void setRiverstatRecord(RiverstatRecord riverstatRecord)
    {
        _riverstatRecord = riverstatRecord;
    }
    
    public RiverstatRecord getRiverstatRecord()
    {
        return _riverstatRecord;
    }

    @Override
    public String toString()
    {
        // TODO Auto-generated method stub
        return null;
    }

}
