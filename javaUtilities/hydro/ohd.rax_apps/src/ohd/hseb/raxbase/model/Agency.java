package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.AgencyRecord;

public class Agency
{
    private String _agCode = null;
    private String _agLoc = null;
    private String _des = null;
    
    public Agency(){}
    
    public Agency( Agency agency )
    {
        setAgCode( agency.getAgCode() );
        setAgLoc( agency.getAgLoc() );
        setDes( agency.getDes() );
    }

    public Agency( AgencyRecord record )
    {
        setAgCode( record.getAgcode() );
        setAgLoc( record.getAgloc() );
        setDes( record.getDes() );
    }
    
    public static Agency getAgency( AgencyRecord record )
    {
        return new Agency( record );
    }
    
    public static AgencyRecord getAgencyRecord( Agency agency )
    {
        AgencyRecord record = new AgencyRecord();
        
        record.setAgcode( agency.getAgCode() );
        record.setAgloc( agency.getAgLoc() );
        record.setDes( agency.getDes() );
        
        return record;
    }
    
    public String toString()
    {
        return "AgCode = " + _agCode +
               " | AgLoc = " + _agLoc + 
               " | Des = " + _des;
    }
    
    public String keyString()
    {
        return "AgCode = " + _agCode +
               " | AgLoc = " + _agLoc; 
    }
    
    public void setAgCode( String agCode )
    {
        _agCode = agCode;
    }

    public String getAgCode()
    {
        return _agCode;
    }

    public void setAgLoc( String agLoc )
    {
        _agLoc = agLoc;
    }

    public String getAgLoc()
    {
        return _agLoc;
    }

    public void setDes( String des )
    {
        _des = des;
    }

    public String getDes()
    {
        return _des;
    }
}
