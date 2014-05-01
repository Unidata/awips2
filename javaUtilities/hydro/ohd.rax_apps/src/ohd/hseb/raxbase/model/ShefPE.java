package ohd.hseb.raxbase.model;

import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxdb.generated.ShefpeRecord;

public class ShefPE
{
    private String _pe = null;
    private String _name = null;
    private String _engUnit = null;
    private String _metUnit = null;
    
    public ShefPE(){}
    
    public ShefPE( String name, String pe )
    {
        setName( name );
        setPe( pe );
    }
    
    public ShefPE( ShefPE shefPE )
    {
        setName( shefPE.getName() );
        setPe( shefPE.getPe() );
        setEngUnit( shefPE.getEngUnit() );
        setMetUnit( shefPE.getMetUnit() );
    }
    
    public ShefPE( ShefpeRecord shefPeRecord )
    {
        setName( shefPeRecord.getName() );
        setPe( PEManager.getPEFromPe1Pe2( shefPeRecord.getPe1(), shefPeRecord.getPe2() ) );
        setEngUnit( shefPeRecord.getEng_unit() );
        setMetUnit( shefPeRecord.getMet_unit() );
    }
    
    public static ShefPE getShefPE( ShefpeRecord shefPeRecord )
    {
        return new ShefPE( shefPeRecord );
    }
    
    public static ShefpeRecord getShefPeRecord( ShefPE shefPE )
    {
        ShefpeRecord record = new ShefpeRecord();
        
        record.setPe1( PEManager.getPe1FromPE( shefPE.getPe() ) );
        record.setPe2( PEManager.getPe2FromPE( shefPE.getPe() ) );
        record.setName( shefPE.getName() );
        record.setEng_unit( shefPE.getEngUnit() );
        record.setMet_unit( shefPE.getMetUnit() );
        
        return record;
    }
    
    public String toString()
    {
        return "PE = " + _pe +
               " | Name = " + _name +
               " | EngUnit = " + _engUnit +
               " | MetUnit = " + _metUnit;
    }
    
    public String keyString()
    {
        return "PE = " + _pe;
    }

    public void setPe( String pe )
    {
        _pe = pe;
    }

    public String getPe()
    {
        return _pe;
    }

    public void setName( String name )
    {
        _name = name;
    }

    public String getName()
    {
        return _name;
    }

    public void setEngUnit( String engUnit )
    {
        _engUnit = engUnit;
    }

    public String getEngUnit()
    {
        return _engUnit;
    }

    public void setMetUnit( String metUnit )
    {
        _metUnit = metUnit;
    }

    public String getMetUnit()
    {
        return _metUnit;
    }

}
