package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.ShefdurRecord;

public class ShefDuration
{
    private short _iduration;
    private String _duration;
    private String _name;
    
    public ShefDuration()
    {
    }

    public ShefDuration( ShefDuration shefDur )
    {
        this( shefDur.getName(), shefDur.getDuration(), shefDur.getIduration() );
    }
    
    public ShefDuration( String name, String duration, short iduration )
    {
        setName( name );
        setDuration( duration );
        setIduration( iduration );
    }
    
    public ShefDuration( ShefdurRecord record )
    {
        setName( record.getName() );
        setDuration( record.getDur() );
        setIduration( record.getIdur() );
    }
    
    public static ShefDuration getShefDuration( ShefdurRecord record )
    {
        return new ShefDuration( record );
    }
    
    public static ShefdurRecord getShefDurRecord( ShefDuration shefDuration )
    {
        ShefdurRecord record = new ShefdurRecord();
        
        record.setDur( shefDuration.getDuration() );
        record.setIdur( shefDuration.getIduration() );
        record.setName( shefDuration.getName() );
        
        return record;
    }
    
    public String toString()
    {
        return "Duration = " + _duration +
               " | IDuration = " + _iduration +
               " | Name = " + _name;
    }
    
    public String keyString()
    {
        return "Duration = " + _duration;
    }
    
    public void setIduration( short iduration )
    {
        _iduration = iduration;
    }
    public short getIduration()
    {
        return _iduration;
    }
    public void setDuration( String duration )
    {
        _duration = duration;
    }
    public String getDuration()
    {
        return _duration;
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