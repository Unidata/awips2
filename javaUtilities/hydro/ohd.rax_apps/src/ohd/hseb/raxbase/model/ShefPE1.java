package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.Shefpe1Record;

public class ShefPE1
{
    private String _pe1 = null;
    private String _name = null;
    
    public ShefPE1(){}
    
    public ShefPE1( ShefPE1 shefPE1 )
    {
        setName( shefPE1.getName() );
        setPe1( shefPE1.getPe1() );
    }
    
    public ShefPE1( Shefpe1Record shefPe1Record )
    {
        setName( shefPe1Record.getName() );
        setPe1( shefPe1Record.getPe1() );
    }
    
    public static ShefPE1 getShefPE1( Shefpe1Record shefPe1Record )
    {
        return new ShefPE1( shefPe1Record );
    }
    
    public static Shefpe1Record getShefPe1Record( ShefPE1 shefPE1 )
    {
        Shefpe1Record record = new Shefpe1Record();
        
        record.setPe1( shefPE1.getPe1() );
        record.setName( shefPE1.getName() );
        
        return record;
    }
    
    public String toString()
    {
        return "PE1 = " + _pe1 +
               " | Name = " + _name;
    }
    
    public String keyString()
    {
        return "PE1 = " + _pe1;
    }

    public void setPe1( String pe1 )
    {
        _pe1 = pe1;
    }

    public String getPe1()
    {
        return _pe1;
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
