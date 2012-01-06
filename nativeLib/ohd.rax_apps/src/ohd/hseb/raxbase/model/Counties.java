package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.CountiesRecord;

public class Counties
{
    private String _county = null;
    private String _state = null;
    private String _countryFips = null;
    private String _countyFips = null;
    private String _wfo = null;
    private String _zon = null;
    
    public Counties(){}
    
    public Counties( Counties counties )
    {
        setCounty( counties.getCounty() );
        setState( counties.getState() );
        setCountryFips( counties.getCountryFips() );
        setCountyFips( counties.getCountyFips() );
        setWfo( counties.getWfo() );
        setZon( counties.getZon() );
    }
    
    public Counties( CountiesRecord countiesRecord )
    {
        setCounty( countiesRecord.getCounty() );
        setState( countiesRecord.getState() );
        setCountryFips( countiesRecord.getCountryfips() );
        setCountyFips( countiesRecord.getCountyfips() );
        setWfo( countiesRecord.getWfo() );
        setZon( countiesRecord.getZon() );
    }
    
    public static Counties getCounties( CountiesRecord record )
    {
        return new Counties( record );
    }
    
    public static CountiesRecord getCountiesRecord( Counties counties )
    {
        CountiesRecord record = new CountiesRecord();
        
        record.setCounty( counties.getCounty() );
        record.setState( counties.getState() );
        record.setCountryfips( counties.getCountryFips() );
        record.setCountyfips( counties.getCountyFips() );
        record.setWfo( counties.getWfo() );
        record.setZon( counties.getZon() );
        
        return record;
    }
    
    public String toString()
    {
        return "County = " + _county +
               " | State = " + _state +
               " | CountryFips = " + _countryFips +
               " | CountyFips = " + _countyFips + 
               " | Wfo = " + _wfo +
               " | Zon = " + _zon;
    }
    
    public String keyString()
    {
        return "CountryFips = " + _countryFips + 
               " | State = " + _state + 
               " | CountyFips = " + _countyFips;
    }
    
    public String getCounty()
    {
        return _county;
    }

    public void setCounty(String county)
    {
        _county = county ;
    }

    public String getState()
    {
        return _state;
    }

    public void setState(String state)
    {
        _state = state ;
    }

    public String getCountryFips()
    {
        return _countryFips;
    }

    public void setCountryFips(String countryfips)
    {
        _countryFips = countryfips ;
    }

    public String getCountyFips()
    {
        return _countyFips;
    }

    public void setCountyFips(String countyfips)
    {
        _countyFips = countyfips ;
    }

    public String getWfo()
    {
        return _wfo;
    }

    public void setWfo(String wfo)
    {
        _wfo = wfo ;
    }

    public String getZon()
    {
        return _zon;
    }

    public void setZon(String zon)
    {
        _zon = zon ;
    }

}
