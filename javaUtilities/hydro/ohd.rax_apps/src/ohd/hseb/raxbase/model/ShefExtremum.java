package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.ShefexRecord;

public class ShefExtremum
{
    private String _extremum = null;
    private String _name = null;
    
    public ShefExtremum(){}
    
    public ShefExtremum( String name, String extremum )
    {
        setExtremum( extremum );
        setName( name );
    }
    
    public ShefExtremum( ShefExtremum shefExtremum )
    {
        this( shefExtremum.getName(), shefExtremum.getExtremum() );
    }
    
    public ShefExtremum ( ShefexRecord shefExRecord )
    {
        setExtremum( shefExRecord.getE() );
        setName( shefExRecord.getName() );
    }
    
    public static ShefExtremum getShefExtremum( ShefexRecord shefExRecord )
    {
        return new ShefExtremum( shefExRecord );
    }
    
    public static ShefexRecord getShefExRecord( ShefExtremum shefExtremum )
    {
        ShefexRecord record = new ShefexRecord();
        
        record.setE( shefExtremum.getExtremum() );
        record.setName( shefExtremum.getName() );
        
        return record;
    }
    
    public String toString()
    {
        return "Extremum = " + _extremum +
               " | Name = " + _name;
    }
    
    public String keyString()
    {
        return "Extremum = " + _extremum;
    }
    
    public void setExtremum( String extremum )
    {
        _extremum = extremum;
    }
    public String getExtremum()
    {
        return _extremum;
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
