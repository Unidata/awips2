package ohd.hseb.geomap.examples;

import ohd.hseb.geomap.model.LatLonPoint;

public class StationPoint
{
    private String _id;
    private String _name;
    private LatLonPoint _latLonPoint;
    
    // ----------------------------------------------------------------------------
    
    public StationPoint(String id, String name, LatLonPoint latLonPoint)
    {
        setId(id);
        setName(name);
        setLatLonPoint(latLonPoint);
        
        return;
    }
    
    // ----------------------------------------------------------------------------
    
    public void setId(String id)
    {
        _id = id;
    }
    
    //  ----------------------------------------------------------------------------
    
    public String getId()
    {
        return _id;
    }
    
    //  ----------------------------------------------------------------------------
    
    
    public void setName(String name)
    {
        _name = name;
    }
    
    //  ----------------------------------------------------------------------------
    
    public String getName()
    {
        return _name;
    }
    
    //  ----------------------------------------------------------------------------
    
    
    public void setLatLonPoint(LatLonPoint latLonPoint)
    {
        _latLonPoint = latLonPoint;
    }
    
    //  ----------------------------------------------------------------------------
    
    public LatLonPoint getLatLonPoint()
    {
        return _latLonPoint;
    }
    //  ----------------------------------------------------------------------------
    
    
}
