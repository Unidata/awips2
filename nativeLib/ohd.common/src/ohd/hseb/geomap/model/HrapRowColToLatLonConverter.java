package ohd.hseb.geomap.model;

import java.util.HashMap;
import java.util.Map;

import ohd.hseb.geomap.contour.RowColToLatLonTranslator;

public class HrapRowColToLatLonConverter implements RowColToLatLonTranslator
{

    private static double  RADIANS_PER_DEGREE = 0.017453293;
    private static double  DEGREES_PER_RADIAN = 57.29577951;
    private static double  EARTH_RADIUS = 6371.2;
    private static double  MESH_LEN = 4.7625;
    private static double  STDLAT = 60;
    private static double  STDLON = 105;

   // public static int  LOCAL_HRAP_ROWS = 131;
   // public static int  LOCAL_HRAP_COLS = 131;
    
    private int _baseNationalRow = 0;
    private int _baseNationalColumn = 0;
    
    private Map<RowColumnPoint, LatLonPoint> _latLonCache = new HashMap<RowColumnPoint, LatLonPoint>();
    
    // -------------------------------------------------------------------------------------------------
    /**
     *  This class translates between local Hrap grid locations and Lat-Lon corrdinates.
     *  It uses the baseNationalRow and the baseNationalColumn for this
     */
    
    public HrapRowColToLatLonConverter(int baseNationalRow, int baseNationalColumn)
    {
        _baseNationalRow = baseNationalRow;
        _baseNationalColumn = baseNationalColumn;
    }
    // -------------------------------------------------------------------------------------------------
    
    public RowColumnPoint getRowColumnPoint(LatLonPoint latLonPoint)
    {
        return getRowColumnPoint(latLonPoint.getLat(), latLonPoint.getLon());
    }
    
    // -------------------------------------------------------------------------------------------------
              
    public RowColumnPoint getRowColumnPoint(double lat, double lon)
    {

        double  tlat, re;
        double  latrad, lonrad;
        double  r;
        double  x, y;

        tlat = STDLAT * RADIANS_PER_DEGREE;

        re = (EARTH_RADIUS * (1. + Math.sin(tlat))) / MESH_LEN;
        latrad = lat * RADIANS_PER_DEGREE;
        lonrad = (lon + 180. - STDLON) * RADIANS_PER_DEGREE;

        r = re * Math.cos(latrad) / (1. + Math.sin(latrad));
        x = r  * Math.sin(lonrad);
        y = r  * Math.cos(lonrad);

        double col = x +  401.;
        double row = y + 1601.;
        
        col -= getBaseNationalColumn();
        row -= getBaseNationalRow();
        
        return new RowColumnPoint(row, col);

    }
    // -------------------------------------------------------------------------------------------------
    public static RowColumnPoint getNationalRowColumnPoint(LatLonPoint latLonPoint)
    {
        return getNationalRowColumnPoint(latLonPoint.getLat(), latLonPoint.getLon());
    }
    
    // -------------------------------------------------------------------------------------------------
              
    public static RowColumnPoint getNationalRowColumnPoint(double lat, double lon)
    {

        double  tlat, re;
        double  latrad, lonrad;
        double  r;
        double  x, y;

        tlat = STDLAT * RADIANS_PER_DEGREE;

        re = (EARTH_RADIUS * (1. + Math.sin(tlat))) / MESH_LEN;
        latrad = lat * RADIANS_PER_DEGREE;
        lonrad = (lon + 180. - STDLON) * RADIANS_PER_DEGREE;

        r = re * Math.cos(latrad) / (1. + Math.sin(latrad));
        x = r  * Math.sin(lonrad);
        y = r  * Math.cos(lonrad);

        double col = x +  401.;
        double row = y + 1601.;
    
        return new RowColumnPoint(row, col);

    }
    // -------------------------------------------------------------------------------------------------
    public LatLonPoint getLatLonPoint(RowColumnPoint rowColumnPoint)
    {
        
        return getLatLonPoint(rowColumnPoint.getRow(), rowColumnPoint.getCol());
    }

    // -------------------------------------------------------------------------------------------------
    public LatLonPoint getLatLonPointOld(double row, double col)
    {
        
        RowColumnPoint rowColumnPoint = new RowColumnPoint(row, col);
        LatLonPoint latLonPoint = _latLonCache.get(rowColumnPoint);
        
        if (latLonPoint == null )
        {
            latLonPoint = calculateLatLonPoint(row, col);
            _latLonCache.put(rowColumnPoint, latLonPoint);
        }
        
        return  latLonPoint;
    }
  
    // -------------------------------------------------------------------------------------------------
    public LatLonPoint getLatLonPoint(double row, double col)
    {
               
        return  calculateLatLonPoint(row, col);
    }
    // -------------------------------------------------------------------------------------------------
 
    public LatLonPoint calculateLatLonPoint(double row, double col)
    {

        row = row + getBaseNationalRow();
        col = col + getBaseNationalColumn();
        
     
        double  x, y;
        double  rr, gi;
        double  ang, tlat;
        
        double lat = 0;
        double lon = 0;

        tlat = 60.0 / DEGREES_PER_RADIAN;

        x = col -  401.;
        y = row - 1601.;

        rr = (x * x) + (y * y);
        gi = ((EARTH_RADIUS * (1 + Math.sin(tlat)))/MESH_LEN);
        gi = gi * gi;

        lat = Math.asin((gi - rr) / (gi + rr)) * DEGREES_PER_RADIAN;

        ang = Math.atan2(y, x) * DEGREES_PER_RADIAN;

        if (ang < 0)
            ang=ang + 360.;

        lon = 270 + STDLON - ang;

        if (lon < 0)
            lon += 360.;

        if (lon > 360)
            lon -= 360.;

        lon *= -1.0;
        
         
        return  new LatLonPoint(lat, lon);
    }
    // -------------------------------------------------------------------------------------------------
    
    private void setBaseNationalRow(int baseNationalRow)
    {
        _baseNationalRow = baseNationalRow;
    }
    // -------------------------------------------------------------------------------------------------
    
    private int getBaseNationalRow()
    {
        return _baseNationalRow;
    }
    // -------------------------------------------------------------------------------------------------
    
    private void setBaseNationalColumn(int baseNationalColumn)
    {
        _baseNationalColumn = baseNationalColumn;
    }
    // -------------------------------------------------------------------------------------------------
    
    private int getBaseNationalColumn()
    {
        return _baseNationalColumn;
    }

    // -------------------------------------------------------------------------------------------------
    
    
}
