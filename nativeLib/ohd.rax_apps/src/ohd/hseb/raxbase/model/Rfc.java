package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.RfcRecord;

public class Rfc
{
    private String _rfc = null;
    
    public Rfc(){}
    
    public Rfc( Rfc rfc )
    {
        setRfc( rfc.getRfc() );
    }
    
    public Rfc( RfcRecord rfcRecord )
    {
        setRfc( rfcRecord.getRfc() );
    }
    
    public static Rfc getRfc( RfcRecord record )
    {
        return new Rfc( record );
    }

    public static RfcRecord getRfcRecord( Rfc rfc )
    {
        RfcRecord record = new RfcRecord();

        record.setRfc( rfc.getRfc() );
        
        return record;
    }
    
    public String toString()
    {
        return "Rfc = " + _rfc;
    }
    
    public String keyString()
    {
        return "Rfc = " + _rfc;
    }
    
    public void setRfc( String rfc )
    {
        this._rfc = rfc;
    }

    public String getRfc()
    {
        return _rfc;
    }
}