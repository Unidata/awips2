package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.CountryRecord;

public class Country
{
    private String _country = null;
    private String _countryFips = null;
    
    
    public Country(){}
    
    public Country( Country country )
    {
        setCountry( country.getCountry() );
        setCountryFips( country.getCountryFips() );
    }
    
    public Country( CountryRecord record )
    {
        setCountry( record.getCountry() );
        setCountryFips( record.getCountryfips() );
    }
    
    public static Country getCountry( CountryRecord record )
    {
        return ( new Country( record ) );
    }
    
    public static CountryRecord getCountryRecord( Country country )
    {
        CountryRecord record = new CountryRecord();
        
        record.setCountry( country.getCountry() );
        record.setCountryfips( country.getCountryFips() );
        
        return record;
    }

    public String keyString()
    {
        return "Country = " + _country;
    }
    
    public void setCountry( String country )
    {
        _country = country;
    }

    public String getCountry()
    {
        return _country;
    }

    public void setCountryFips( String countryFips )
    {
        _countryFips = countryFips;
    }

    public String getCountryFips()
    {
        return _countryFips;
    }
}
