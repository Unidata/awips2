package ohd.hseb.geomap.projection;

import java.awt.Point;

import ohd.hseb.geomap.model.LatLonPoint;

// ------------------------------------------------------------------------------------------
public class FlatMapProjection extends BaseMapProjection
{
    
//  ------------------------------------------------------------------------------------------


    public FlatMapProjection (LatLonPoint centerLatLonPoint)
    {
        setCenterLatLon(centerLatLonPoint);
    }

    
    public LatLonPoint getLatLonPoint(int x, int y)
    {
        double lat = 0.0;
        double lon = 0.0;
 
        double mapCenterLon = getCenterLatLon().getLon();
        double mapCenterLat = getCenterLatLon().getLat();

        lon =  ( ( ( double ) (x - getOffsetX()) / getMapScaleFactor() )  +  mapCenterLon) ;
        lat = mapCenterLat - ( ( double ) ( y - getOffsetY() ) / getMapScaleFactor() ) ;
        
        LatLonPoint point = new LatLonPoint(lat, lon);
        
        return point;
    }

  
    //------------------------------------------------------------------------------------------------- 
    public LatLonPoint getLatLonPoint(Point screenPoint)
    {
        
        int x = screenPoint.x;
        int y = screenPoint.y;
   
        return getLatLonPoint(x, y);
    }
    //-------------------------------------------------------------------------------------------------
   
    public Point getScreenPoint(LatLonPoint latLonPoint)
    {
        
        double lat = latLonPoint.getLat();
        double lon = latLonPoint.getLon();
         
        return getScreenPoint(lat, lon);
    
    }

//  -------------------------------------------------------------------------------------------------
    
    public Point getScreenPoint(double lat, double lon)
    {
        
        double mapCenterLat = getCenterLatLon().getLat();       
        double mapCenterLon = getCenterLatLon().getLon();
      
        if (mapCenterLon > 0 && lon < 0)
            lon = lon + 360;

        if (mapCenterLon < 0 && lon > 0)
            lon = lon - 360;

        int  x = (int) ((lon - mapCenterLon) * getMapScaleFactor() ) + getOffsetX();
        int  y = (int) ( getOffsetY() - ((lat - mapCenterLat) * getMapScaleFactor() ) );
        
        
   
        return new Point(x,y);
    }

    //-------------------------------------------------------------------------------------------------
    
    
      
}
