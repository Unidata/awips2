package ohd.hseb.geomap.projection;

import java.awt.Point;
import java.util.List;

import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;

public interface MapProjection
{
     void setMapScaleFactor(double mapScaleFactor);
     double getMapScaleFactor();
     
     int getMapWidth();
     void setMapWidth(int mapWidth);
      
     int getMapHeight();
     void setMapHeight(int mapHeight);
    
     //configuration methods
     void setLatLonBounds(LatLonBounds latLonBounds);
     LatLonBounds getLatLonBounds();
     
     void panProportionally(double northSouthPanProportion, double eastWestPanProportion);
     
     void setCenterLatLon(LatLonPoint centerLatLong);
     LatLonPoint getCenterLatLon();
     
      //useful methods
     
     boolean isInRange(double lat, double lon);
     
     LatLonPoint getLatLonPoint(Point screenPoint);
     LatLonPoint getLatLonPoint(int x, int y);
     List<LatLonPoint> getLatLonPointList(List<Point> screenCoordList);
     
     Point getScreenPoint(LatLonPoint latlonPoint);
     Point getScreenPoint(double lat, double lon);
     
     List<Point> getScreenPointList(List<LatLonPoint> latLonPointList);
     
     
}
