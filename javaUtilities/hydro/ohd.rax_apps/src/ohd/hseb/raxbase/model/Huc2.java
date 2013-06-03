package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.Huc2Record;

public class Huc2
{
    private String _code2 = null;
    private String _code12 = null;
    private String _desreg = null;
    
    public Huc2(){}
    
    public Huc2( Huc2 huc2 )
    {
        setCode2( huc2.getCode2() );
        setCode12( huc2.getCode12() );
        setDesreg( huc2.getDesreg() );
    }
    
    public Huc2( Huc2Record huc2Record )
    {
        setCode2( huc2Record.getCode2() );
        setCode12( huc2Record.getCode12() );
        setDesreg( huc2Record.getDesreg() );
    }
    
    public static Huc2 getHuc2( Huc2Record record )
    {
        return new Huc2( record );
    }

    public static Huc2Record getHuc2Record( Huc2 huc2 )
    {
        Huc2Record record = new Huc2Record();
        
        record.setCode2( huc2.getCode2() );
        record.setCode12( huc2.getCode12() );
        record.setDesreg( huc2.getDesreg() );
        
        return record;
    }
    
    public String toString()
    {
        return "Code12 = " + _code12 +
               " | Code2 = " + _code2 +
               " | Desreg = " + _desreg;
    }
    
    public String keyString()
    {
        return "Code12 = " + _code12;
    }
    
    public void setCode2( String code2 )
    {
        _code2 = code2;
    }
    public String getCode2()
    {
        return _code2;
    }
    public void setCode12( String code12 )
    {
        _code12 = code12;
    }
    public String getCode12()
    {
        return _code12;
    }
    public void setDesreg( String desreg )
    {
        _desreg = desreg;
    }
    public String getDesreg()
    {
        return _desreg;
    }
    
    
}
