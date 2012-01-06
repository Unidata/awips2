package ohd.hseb.geomap.model;

public class LatLonBounds
{
    private double _northLat = 0;
    private double _southLat = 0;
    private double _westLon = 0;
    private double _eastLon = 0;
    
    // -----------------------------------------------------------------------------------------------------------
    
    public LatLonBounds(double northLat, double southLat, double westLon, double eastLon)
    {
        setNorthLat(northLat);
        setSouthLat(southLat);
        setWestLon(westLon);
        setEastLon(eastLon);
        
        return;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public LatLonBounds(LatLonPoint northWestLatLong, LatLonPoint southEastLatLong)
    {
        setNorthLat(northWestLatLong.getLat());
        setSouthLat(southEastLatLong.getLat());
        setWestLon(northWestLatLong.getLon());
        setEastLon(southEastLatLong.getLon());
    }
    // -----------------------------------------------------------------------------------------------------------
      
    public LatLonBounds(LatLonBounds origBounds)
    {
        setNorthLat(origBounds.getNorthLat());
        setSouthLat(origBounds.getSouthLat());
        setWestLon(origBounds.getWestLon());
        setEastLon(origBounds.getEastLon());
        
        return;
    }
    // -----------------------------------------------------------------------------------------------------------
      
    public void setNorthLat(double northLat)
    {
        _northLat = northLat;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public double getNorthLat()
    {
        return _northLat;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public void changeNorthLat(double changeAmount)
    {
        _northLat += changeAmount;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public void setSouthLat(double southLat)
    {
        _southLat = southLat;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public double getSouthLat()
    {
        return _southLat;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public void changeSouthLat(double changeAmount)
    {
        _southLat += changeAmount;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public void setWestLon(double westLon)
    {
        _westLon = westLon;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public double getWestLon()
    {
        return _westLon;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public void changeWestLon(double changeAmount)
    {
        _westLon += changeAmount;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public void setEastLon(double eastLon)
    {
        _eastLon = eastLon;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public double getEastLon()
    {
        return _eastLon;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public void changeEastLon(double changeAmount)
    {
        _eastLon += changeAmount;
    }
    // -----------------------------------------------------------------------------------------------------------
  
    public double getLatDifference()
    {
        return getNorthLat() - getSouthLat();
    }
    
    // -----------------------------------------------------------------------------------------------------------
    public double getLonDifference()
    {
        return getEastLon() - getWestLon();
    }
    
    // -----------------------------------------------------------------------------------------------------------
    
    
    public void updateLatLonBounds(float lat1, float lat2, 
                                    float lon1, float lon2)
    {
       updateLatitude(lat1);
       updateLatitude(lat2);
       
       updateLongitude(lon1);
       updateLongitude(lon2);
       
       return;      
    }
    // -----------------------------------------------------------------------------------------------------------
    
    private void updateLatitude(float lat)
    {
        if (lat > getNorthLat())
        {
            setNorthLat(lat);
        }
        if (lat < getSouthLat())
        {
            setSouthLat(lat);
        }
        
        return;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    private void updateLongitude(float lon)
    {
        if (lon < getWestLon())
        {
            setWestLon(lon);
        }
        if (lon > getEastLon())
        {
            setEastLon(lon);
        }
        
        return;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public boolean equals(Object object)
    {
        boolean result = false;
        
        LatLonBounds otherBounds = (LatLonBounds) object;
        if ( 
                (getNorthLat() == otherBounds.getNorthLat() ) &&
                (getSouthLat() == otherBounds.getSouthLat() ) && 
                (getEastLon() == otherBounds.getEastLon()) &&
                (getWestLon() == otherBounds.getWestLon() )
        )
        {
            result = true;
        }

        return result;
    }
    // -----------------------------------------------------------------------------------------------------------
    
    public String toString()
    {
        String string = getNorthLat() + "," + getWestLon() + " to " + getSouthLat() + ", "  + getEastLon();
        return string;
        
    }
    // -----------------------------------------------------------------------------------------------------------
       
} //end class LatLonBounds
