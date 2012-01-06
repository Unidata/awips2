package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.Wfo_hsaRecord;

public class WfoHsa
{
    private String _wfoHsa = null;
    
    public WfoHsa(){}
    
    public WfoHsa( WfoHsa wfoHsa )
    {
        setWfoHsa( wfoHsa.getWfoHsa() );
    }
    
    public WfoHsa( Wfo_hsaRecord wfoHsaRecord )
    {
        setWfoHsa( wfoHsaRecord.getWfo_hsa() );
    }
    
    public static WfoHsa getWfoHsa( WfoHsa record )
    {
        return new WfoHsa( record );
    }

    public static Wfo_hsaRecord getWfoHsaRecord( WfoHsa wfoHsa )
    {
        Wfo_hsaRecord record = new Wfo_hsaRecord();

        record.setWfo_hsa( wfoHsa.getWfoHsa() );
        
        return record;
    }
    
    public String toString()
    {
        return "WfoHsa = " + _wfoHsa;
    }
    
    public String keyString()
    {
        return "WfoHsa = " + _wfoHsa;
    }
    
    public void setWfoHsa( String wfoHsa )
    {
        this._wfoHsa = wfoHsa;
    }

    public String getWfoHsa()
    {
        return _wfoHsa;
    }
}