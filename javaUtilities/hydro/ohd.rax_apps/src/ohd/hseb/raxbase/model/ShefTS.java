package ohd.hseb.raxbase.model;

import ohd.hseb.raxbase.util.TSManager;
import ohd.hseb.raxdb.generated.SheftsRecord;

public class ShefTS
{
    private String _ts = null;
    private String _name = null;

    public ShefTS(){}
    
    public ShefTS( String name, String ts )
    {
        setTs( ts );
        setName( name );
    }
    
    public ShefTS( ShefTS shefTS )
    {
        this( shefTS.getName(), shefTS.getTs() );
    }
    
    public ShefTS( SheftsRecord shefTSRecord )
    {
        setTs( TSManager.getTSFromTandS( shefTSRecord.getT(), shefTSRecord.getS() ) );
        setName( shefTSRecord.getName() );
    }
    
    public static ShefTS getShefTS( SheftsRecord record )
    {
        return new ShefTS( record );
    }
    
    public static SheftsRecord getShefTSRecord( ShefTS shefTS )
    {
        SheftsRecord record = new SheftsRecord();
        
        record.setT( TSManager.getTFromTS( shefTS.getTs() ) );
        record.setS( TSManager.getSFromTS( shefTS.getTs() ) );
        record.setName( shefTS.getName() );
        
        return record;
    }
    
    public String toString()
    {
        return "TS = " + _ts + 
               " | Name = " + _name;
    }
    
    public String keyString()
    {
        return "TS = " + _ts;
    }
    public void setTs( String ts )
    {
        _ts = ts;
    }
    public String getTs()
    {
        return _ts;
    }
    public void setName( String name )
    {
        _name = name;
    }
    public String getName()
    {
        return _name;
    }
}
