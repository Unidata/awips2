package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.Huc6Record;

public class Huc6
{
    private String _code6 = null;
    private String _code12 = null;
    private String _code34 = null;
    private String _code56 = null;
    private String _desacct = null;
    
    public Huc6(){}
    
    public Huc6( Huc6 huc6 )
    {
        setCode6( huc6.getCode6() );
        setCode12( huc6.getCode12() );
        setCode34( huc6.getCode34() );
        setCode56( huc6.getCode56() );
        setDesacct( huc6.getDesacct() );
    }
    
    public Huc6( Huc6Record huc6Record )
    {
        setCode6( huc6Record.getCode6() );
        setCode12( huc6Record.getCode12() );
        setCode34( huc6Record.getCode34() );
        setCode56( huc6Record.getCode56() );
        setDesacct( huc6Record.getDesacct() );
    }
    
    public static Huc6 getHuc6( Huc6Record record )
    {
        return new Huc6( record );
    }

    public static Huc6Record getHuc6Record( Huc6 huc6 )
    {
        Huc6Record record = new Huc6Record();
        
        record.setCode6( huc6.getCode6() );
        record.setCode12( huc6.getCode12() );
        record.setCode34( huc6.getCode34() );
        record.setCode56( huc6.getCode56() );
        record.setDesacct( huc6.getDesacct() );
        
        return record;
    }
    
    public String toString()
    {
        return "Code12 = " + _code12 +
               " | Code6 = " + _code6 +
               " | Code34 = " + _code34 +
               " | Code56 = " + _code56 +
               " | Desreg = " + getDesacct();
    }
    
    public String keyString()
    {
        return "Code12 = " + _code12 + " | Code34 = " + _code34 + " | Code56 = " + _code56;
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

    public void setCode6( String code6 )
    {
        _code6 = code6;
    }

    public String getCode6()
    {
        return _code6;
    }

    public void setDesacct( String desacct )
    {
        this._desacct = desacct;
    }

    public String getDesacct()
    {
        return _desacct;
    }
}
