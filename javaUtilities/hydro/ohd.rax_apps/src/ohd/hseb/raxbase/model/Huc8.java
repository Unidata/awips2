package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.Huc8Record;

public class Huc8
{
    private String _code8 = null;
    private String _code12 = null;
    private String _code34 = null;
    private String _code56 = null;
    private String _code78 = null;
    private String _descat = null;
    
    public Huc8(){}
    
    public Huc8( Huc8 huc8 )
    {
        setCode8( huc8.getCode8() );
        setCode12( huc8.getCode12() );
        setCode34( huc8.getCode34() );
        setCode56( huc8.getCode56() );
        setCode78( huc8.getCode78() );
        setDescat( huc8.getDescat() );
    }
    
    public Huc8( Huc8Record huc8Record )
    {
        setCode8( huc8Record.getCode8() );
        setCode12( huc8Record.getCode12() );
        setCode34( huc8Record.getCode34() );
        setCode56( huc8Record.getCode56() );
        setCode78( huc8Record.getCode78() );
        setDescat( huc8Record.getDescat() );
    }
    
    public static Huc8 getHuc8( Huc8Record record )
    {
        return new Huc8( record );
    }

    public static Huc8Record getHuc8Record( Huc8 huc8 )
    {
        Huc8Record record = new Huc8Record();
        
        record.setCode8( huc8.getCode8() );
        record.setCode12( huc8.getCode12() );
        record.setCode34( huc8.getCode34() );
        record.setCode56( huc8.getCode56() );
        record.setCode78( huc8.getCode78() );
        record.setDescat( huc8.getDescat() );
        
        return record;
    }
    
    public String toString()
    {
        return "Code12 = " + _code12 +
               " | Code8 = " + _code8 +
               " | Code34 = " + _code34 +
               " | Code56 = " + _code56 +
               " | Code78 = " + _code78 +
               " | Descat = " + _descat;
    }
    
    public String keyString()
    {
        return "Code12 = " + _code12 + " | Code34 = " + _code34 + " | Code56 = " + _code56 + " | Code78 = " + _code78;
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

    public void setCode56( String code56 )
    {
        _code56 = code56;
    }

    public String getCode56()
    {
        return _code56;
    }

    public void setCode78( String code78 )
    {
        _code78 = code78;
    }

    public String getCode78()
    {
        return _code78;
    }

    public void setDescat( String descat )
    {
        _descat = descat;
    }

    public String getDescat()
    {
        return _descat;
    }

    public void setCode8( String code8 )
    {
        _code8 = code8;
    }

    public String getCode8()
    {
        return _code8;
    }
}
