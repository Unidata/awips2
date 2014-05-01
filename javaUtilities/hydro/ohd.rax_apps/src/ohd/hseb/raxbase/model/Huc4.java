package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.Huc4Record;

public class Huc4
{
    private String _code4 = null;
    private String _code12 = null;
    private String _code34 = null;
    private String _dessubreg = null;
    
    public Huc4(){}
    
    public Huc4( Huc4 huc4 )
    {
        setCode4( huc4.getCode4() );
        setCode12( huc4.getCode12() );
        setCode34( huc4.getCode34() );
        setDessubreg( huc4.getDessubreg() );
    }
    
    public Huc4( Huc4Record huc4Record )
    {
        setCode4( huc4Record.getCode4() );
        setCode12( huc4Record.getCode12() );
        setCode34( huc4Record.getCode34() );
        setDessubreg( huc4Record.getDessubreg() );
    }
    
    public static Huc4 getHuc4( Huc4Record record )
    {
        return new Huc4( record );
    }

    public static Huc4Record getHuc4Record( Huc4 huc4 )
    {
        Huc4Record record = new Huc4Record();
        
        record.setCode4( huc4.getCode4() );
        record.setCode12( huc4.getCode12() );
        record.setCode34( huc4.getCode34() );
        record.setDessubreg( huc4.getDessubreg() );
        
        return record;
    }
    
    public String toString()
    {
        return "Code12 = " + _code12 +
               " | Code4 = " + _code4 +
               " | Code34 = " + _code34 + 
               " | Desreg = " + _dessubreg;
    }
    
    public String keyString()
    {
        return "Code12 = " + _code12 + " | Code34 = " + _code34;
    }
    
    public void setCode4( String code4 )
    {
        _code4 = code4;
    }
    public String getCode4()
    {
        return _code4;
    }

    public void setCode12( String code12 )
    {
        _code12 = code12;
    }
    public String getCode12()
    {
        return _code12;
    }

    public void setCode34( String code34 )
    {
        _code34 = code34;
    }

    public String getCode34()
    {
        return _code34;
    }

    public void setDessubreg( String dessubreg )
    {
        _dessubreg = dessubreg;
    }

    public String getDessubreg()
    {
        return _dessubreg;
    }
}
