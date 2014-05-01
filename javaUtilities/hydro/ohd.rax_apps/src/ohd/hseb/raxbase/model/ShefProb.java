package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.ShefprobRecord;

public class ShefProb
{
    private float _probability;
    private String _p;
    private String _name;
    
    public ShefProb(){}

    public ShefProb( ShefProb shefProb )
    {
        this( shefProb.getName(), shefProb.getP(), shefProb.getProbability() );
    }
    
    public ShefProb( String name, String p, float probability )
    {
        setName( name );
        setP( p );
        setProbability( probability );
    }
    
    public ShefProb( ShefprobRecord shefProbRecord )
    {
        setName( shefProbRecord.getName() );
        setP( shefProbRecord.getP() );
        setProbability( shefProbRecord.getProbability() );
    }
    
    public static ShefProb getShefProb( ShefprobRecord shefProbRecord )
    {
        return new ShefProb( shefProbRecord );
    }
    
    public static ShefprobRecord getShefProbRecord( ShefProb shefProb )
    {
        ShefprobRecord record = new ShefprobRecord();
        
        record.setName( shefProb.getName() );
        record.setP( shefProb.getP() );
        record.setProbability( shefProb.getProbability() );
        
        return record;
    }
    
    public String toString()
    {
        return "P = " + _p +
               " | Probability = " + _probability +
               " | Name = " + _name;
    }
    
    public String keyString()
    {
        return "P = " + _p;
    }
    
    public void setProbability( float probability )
    {
        _probability = probability;
    }

    public float getProbability()
    {
        return _probability;
    }

    public void setP( String p )
    {
        _p = p;
    }

    public String getP()
    {
        return _p;
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
