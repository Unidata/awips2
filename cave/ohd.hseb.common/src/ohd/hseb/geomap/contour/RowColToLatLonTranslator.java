package ohd.hseb.geomap.contour;

import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.RowColumnPoint;

public interface RowColToLatLonTranslator
{

    LatLonPoint getLatLonPoint(RowColumnPoint rowColumnPoint);
    LatLonPoint getLatLonPoint(double row, double col);
  
//    RowColumnPoint getRowColumnPoint(double lat, double lon);
    RowColumnPoint getRowColumnPoint(LatLonPoint latLonPoint);
    
}
