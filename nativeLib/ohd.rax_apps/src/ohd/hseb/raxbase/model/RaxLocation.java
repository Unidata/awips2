package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.RaxLocationRecord;
import ohd.hseb.util.StringDataConverter;

public class RaxLocation 
{
    private static final int MISSING = -9999;
    
    private String _lid = null;
    private long _beginDate = MISSING;
    private long _endDate = MISSING;
    private double _latitude = MISSING;
    private double _longitude = MISSING;
    private String _name = null;
    private String _detailInfo = null;
    private String _hsa = null;
    private int _elevation = MISSING;
    private String _state = null;
    private String _countyFips = null;
    private String _wfo = null;
    private short _postCode = MISSING;
    private String _rfc = null;
    private String _goes = null;
    private String _huc = null;
    private String _zone = null;
    private String _dbsource = null;
    private String _countryfips = null;
    
    public RaxLocation(){}
    
    public RaxLocation( RaxLocation raxLocation )
    {
        setLid( raxLocation.getLid() );
        setBeginDate( raxLocation.getBeginDate() );
        setEndDate( raxLocation.getEndDate() );
        setLatitude( raxLocation.getLatitude() );
        setLongitude( raxLocation.getLongitude() );
        setName( raxLocation.getName() );
        setDetailInfo( raxLocation.getDetailInfo() );
        setHsa( raxLocation.getHsa() );
        setElevation( raxLocation.getElevation() );
        setState( raxLocation.getState() );
        setCountyFips( raxLocation.getCountyFips() );
        setWfo( raxLocation.getWfo() );
        setPostCode( raxLocation.getPostCode() );
        setRfc( raxLocation.getRfc() );
        setGoes( raxLocation.getGoes() );
        setHuc( raxLocation.getHuc() );
        setZone( raxLocation.getZone() );
        setDbsource( raxLocation.getDbsource() );
        setCountryfips( raxLocation.getCountryfips() );
    }

    public String toString()
    {
        StringDataConverter converter = new StringDataConverter();
        
        return "Lid = " + _lid + " | BeginDate = " + converter.getDateStringFromDateLong( _beginDate ) + " | EndDate = " + converter.getDateStringFromDateLong( _endDate ) + "\n" +
               " | Latitude = " + _latitude + " | Longitude = " + _longitude + " | Name = " + _name + "\n" +
               " | DetailInfo = " + _detailInfo + " | HSA = " + _hsa + " | Elevation = " + _elevation + "\n" +
               " | State = " + _state + " | CountyFips = " + _countyFips + " Wfo = " + _wfo + "\n" +
               " | PostCode = " + _postCode + " | RFC = " + _rfc + " | Goes = " + _goes + "\n" +
               " | Huc = " + _huc + " | Zone = " + _zone + " | DbSource = " + _dbsource + "\n" +
               " | CountryFips = " + _countryfips;
    }
    
    public RaxLocation( RaxLocationRecord raxLocationRecord )
    {
        setLid( raxLocationRecord.getLid() );
        setBeginDate( raxLocationRecord.getSbd() );
        setEndDate( raxLocationRecord.getSed() );
        setLatitude( raxLocationRecord.getLat() );
        setLongitude( raxLocationRecord.getLon() );
        setName( raxLocationRecord.getName() );
        setDetailInfo( raxLocationRecord.getDet() );
        setHsa( raxLocationRecord.getHsa() );
        setElevation( raxLocationRecord.getElev() );
        setState( raxLocationRecord.getState() );
        setCountyFips( raxLocationRecord.getCountyfips() );
        setWfo( raxLocationRecord.getWfo() );
        setPostCode( raxLocationRecord.getPost() );
        setRfc( raxLocationRecord.getRfc() );
        setGoes( raxLocationRecord.getGoes() );
        setHuc( raxLocationRecord.getHuc() );
        setZone( raxLocationRecord.getZon() );
        setDbsource( raxLocationRecord.getDbsource() );
        setCountryfips( raxLocationRecord.getCountryfips() );
    }
    
    public static RaxLocation getRaxLocation( RaxLocationRecord record )
    {
        return ( new RaxLocation( record ) );
    }
    
    public static RaxLocationRecord getRaxLocationRecord( RaxLocation raxLocation )
    {
        RaxLocationRecord raxLocationRecord = new RaxLocationRecord();
        
        raxLocationRecord.setLid( raxLocation.getLid() );
        raxLocationRecord.setSbd( raxLocation.getBeginDate() );
        raxLocationRecord.setSed( raxLocation.getEndDate() );
        raxLocationRecord.setLat( raxLocation.getLatitude() );
        raxLocationRecord.setLon( raxLocation.getLongitude() );
        raxLocationRecord.setName( raxLocation.getName() );
        raxLocationRecord.setDet( raxLocation.getDetailInfo() );
        raxLocationRecord.setHsa( raxLocation.getHsa() );
        raxLocationRecord.setElev( raxLocation.getElevation() );
        raxLocationRecord.setState( raxLocation.getState() );
        raxLocationRecord.setCountyfips( raxLocation.getCountyFips() );
        raxLocationRecord.setWfo( raxLocation.getWfo() );
        raxLocationRecord.setPost( raxLocation.getPostCode() );
        raxLocationRecord.setRfc( raxLocation.getRfc() );
        raxLocationRecord.setGoes( raxLocation.getGoes() );
        raxLocationRecord.setHuc( raxLocation.getHuc() );
        raxLocationRecord.setZon( raxLocation.getZone() );
        raxLocationRecord.setDbsource( raxLocation.getDbsource() );
        raxLocationRecord.setCountryfips( raxLocation.getCountryfips() );
        
        return raxLocationRecord;
    }

    public void setLid( String lid ) 
    {
        _lid = lid;
    }

    public String getLid() 
    {
        return _lid;
    }

    public void setBeginDate( long beginDate ) 
    {
        _beginDate = beginDate;
    }

    public long getBeginDate() 
    {
        return _beginDate;
    }

    public void setEndDate( long endDate )
    {
        _endDate = endDate;
    }

    public long getEndDate() 
    {
        return _endDate;
    }

    public void setLatitude( double latitude ) 
    {
        _latitude = latitude;
    }

    public double getLatitude() 
    {
        return _latitude;
    }

    public void setLongitude( double longitude )
    {
        _longitude = longitude;
    }

    public double getLongitude() 
    {
        return _longitude;
    }

    public void setName( String name )
    {
        _name = name;
    }

    public String getName() 
    {
        return _name;
    }

    public void setDetailInfo( String detailInfo ) 
    {
        _detailInfo = detailInfo;
    }

    public String getDetailInfo() 
    {
        return _detailInfo;
    }

    public void setHsa( String hsa ) 
    {
        _hsa = hsa;
    }

    public String getHsa() 
    {
        return _hsa;
    }

    public void setElevation( int elevation ) 
    {
        _elevation = elevation;
    }

    public int getElevation() 
    {
        return _elevation;
    }

    public void setState( String state ) 
    {
        _state = state;
    }

    public String getState() 
    {
        return _state;
    }

    public void setCountyFips( String countyFips )
    {
        _countyFips = countyFips;
    }

    public String getCountyFips() 
    {
        return _countyFips;
    }

    public void setWfo( String wfo ) 
    {
        _wfo = wfo;
    }

    public String getWfo() 
    {
        return _wfo;
    }

    public void setPostCode( short postCode ) 
    {
        _postCode = postCode;
    }

    public short getPostCode() 
    {
        return _postCode;
    }

    public void setRfc( String rfc ) 
    {
        _rfc = rfc;
    }

    public String getRfc() 
    {
        return _rfc;
    }

    public void setGoes( String goes )
    {
        _goes = goes;
    }

    public String getGoes() 
    {
        return _goes;
    }

    public void setHuc( String huc )
    {
        _huc = huc;
    }

    public String getHuc() 
    {
        return _huc;
    }

    public void setZone( String zone )
    {
        _zone = zone;
    }

    public String getZone() 
    {
        return _zone;
    }

    public void setDbsource( String dbsource )
    {
        _dbsource = dbsource;
    }

    public String getDbsource() 
    {
        return _dbsource;
    }

    public void setCountryfips( String countryfips )
    {
        _countryfips = countryfips;
    }

    public String getCountryfips() 
    {
        return _countryfips;
    }
}
