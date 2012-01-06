package ohd.hseb.geomap.model;

import java.util.ArrayList;
import java.util.List;

public class Polygon implements DrawableShape
{
    List<LatLonPoint> _pointList = new ArrayList<LatLonPoint>();
    
    public Polygon(List<LatLonPoint> pointList)
    {
        _pointList = pointList;   
    }
    
    public List<LatLonPoint> getPointList()
    {
        return _pointList;
    }
}
