package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.ShefqcRecord;

public class ShefQC
{
    private static final short MISSING = -9999;
    
    private String _shefQualifierCode = null;
    private int _power = MISSING;
    private String _name = null;
    
    public ShefQC(){}

    public ShefQC( ShefQC shefQC )
    {
        setShefQualifierCode( shefQC.getShefQualifierCode() );
        setPower( shefQC.getPower() );
        setName( shefQC.getName() );
    }

    public ShefQC( ShefqcRecord shefQCRecord )
    {
        setShefQualifierCode( shefQCRecord.getShef_qualifier_code() );
        setPower( shefQCRecord.getPower() );
        setName( shefQCRecord.getName() );
    }
    
    public static ShefQC getShefProb( ShefqcRecord shefQCRecord )
    {
        return new ShefQC( shefQCRecord );
    }
    
    public static ShefqcRecord getShefQCRecord( ShefQC shefQC )
    {
        ShefqcRecord record = new ShefqcRecord();
        
        record.setShef_qualifier_code( shefQC.getShefQualifierCode() );
        record.setPower( shefQC.getPower() );
        record.setName( shefQC.getName() );
        
        return record;
    }
    
    public String toString()
    {
        return "ShefQualifierCode = " + _shefQualifierCode +
               " | Power = " + _power +
               " | Name = " + _name;
    }
    
    public String keyString()
    {
        return "Power = " + _power;
    }
    
    public void setName( String name )
    {
        _name = name;
    }

    public String getName()
    {
        return _name;
    }

    public void setShefQualifierCode( String shefQualifierCode )
    {
        _shefQualifierCode = shefQualifierCode;
    }

    public String getShefQualifierCode()
    {
        return _shefQualifierCode;
    }

    public void setPower( int power )
    {
        _power = power;
    }

    public int getPower()
    {
        return _power;
    }
}
