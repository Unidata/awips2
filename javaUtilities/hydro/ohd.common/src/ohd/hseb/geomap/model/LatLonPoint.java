package ohd.hseb.geomap.model;

public class LatLonPoint implements DrawableShape
{
    private double _lat;
    private double _lon;
    
    
    public LatLonPoint(double lat, double lon)
    {
        setLat(lat);
        setLon(lon);
    }
    
    public void setLat(double lat)
    {
        _lat = lat;
    }
    
    public double getLat()
    {
        return _lat;
    }
    
    public void changeLat(double changeAmount)
    {
        _lat += changeAmount;
   
    }
    
    public void setLon(double lon)
    {
        _lon = lon;
    }
    
    public double getLon()
    {
        return _lon;
    }
    
    public void changeLon(double changeAmount)
    {
        _lon += changeAmount;
    }
    
    public boolean equals(LatLonPoint point)
    {
        double tolerance = 0.000000001;
        
        boolean result = false;
        
        double latDiff =  Math.abs(getLat() - point.getLat());
        double lonDiff =  Math.abs(getLon() - point.getLon());
        
        if ( (latDiff < tolerance)  && (lonDiff < tolerance))
        {
            result = true;
        }
        
        return result;
    }
    
    public String toString()
    {
        
        return String.format("lat = %5.2f , lon =  %5.2f ", _lat, _lon);
    }
    

}
