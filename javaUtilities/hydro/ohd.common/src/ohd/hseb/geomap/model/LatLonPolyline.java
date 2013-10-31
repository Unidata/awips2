package ohd.hseb.geomap.model;

import java.util.ArrayList;
import java.util.List;


public class LatLonPolyline implements DrawableShape
{
    List<LatLonPoint> _pointList = null;

    
    public LatLonPolyline()
    {
        _pointList = new ArrayList<LatLonPoint>();  
    }
    
    public LatLonPolyline(List<LatLonPoint> pointList)
    {
        _pointList = pointList;   
    }
    
    public void addPoint(LatLonPoint point)
    {
        _pointList.add(point);
    }
    
    public void addPointList(List<LatLonPoint> pointList)
    {
        _pointList.addAll(pointList);
    }

    public List<LatLonPoint> getPointList()
    {
        return _pointList;
    }
    
    public boolean equals(LatLonPolyline line)
    {
        boolean result = false;
        
        if (_pointList.size() == line.getPointList().size())
        {
            result = true;
            for (int i = 0; i < _pointList.size(); i++)
            {
                LatLonPoint point1 = _pointList.get(i);
                LatLonPoint point2 = line.getPointList().get(i);
                if (! point1.equals(point2) )
                {
                    result = false;
                    break;
                }
            }
        }
        
        
        return result;
    }
    
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
        
        for (LatLonPoint point: _pointList)
        {
            buffer.append(point.toString());
        }
        
        return buffer.toString();
        
    }

}
