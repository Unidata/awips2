package ohd.hseb.geomap.test;

import ohd.hseb.geomap.contour.RowColToLatLonTranslator;
import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.RowColumnPoint;

public class MockRowColToLatLonTranslator implements RowColToLatLonTranslator
{
    private LatLonBounds _latLonBounds = null;
    private double  _latResInDegrees = 0;
    private double  _lonResInDegrees = 0;
    // ------------------------------------------------------------------------------
    
    public MockRowColToLatLonTranslator(LatLonBounds bounds, double latResInDegrees, double lonResInDegrees)
    {
        _latLonBounds = new LatLonBounds(bounds);
        _latResInDegrees = latResInDegrees;
    
    }
    // ------------------------------------------------------------------------------
    
    public RowColumnPoint getRowColumnPoint(LatLonPoint latLonPoint)
    {
        double row = getRow(latLonPoint.getLat());
        double col = getCol(latLonPoint.getLon());

        return  new RowColumnPoint(row, col);
    }
    // ------------------------------------------------------------------------------
    public LatLonPoint getLatLonPoint(double row, double col)
    {
        
        double lat = getLat((int)row);
        double lon = getLon((int)col);

        return  new LatLonPoint(lat, lon);
    }
    //  ------------------------------------------------------------------------------
    public LatLonPoint getLatLonPoint(RowColumnPoint rowColumnPoint)
    {
        return getLatLonPoint(rowColumnPoint.getRow(), rowColumnPoint.getCol());
    }
    //  ------------------------------------------------------------------------------
      
    public double getLat(int row)
    {
        double lat = row;
        
        return lat;
    }
    // ------------------------------------------------------------------------------
    public double getLon(int col)
    {
        double lon = col;
        
        return lon;
    }
    // ------------------------------------------------------------------------------
      
    public int getRow(double lat)
    {
        int row = -1;
         row = (int)  Math.ceil(lat);
        
        return row;    
    }
    // ------------------------------------------------------------------------------
    public int getCol(double lon)
    {
        int col = -1;
   
        col = (int)  Math.ceil(lon);
        
        return col;  
    }
    // ------------------------------------------------------------------------------
    
    public void setLatResInDegrees(double latResInDegrees)
    {
        _latResInDegrees = latResInDegrees;
    }
    // ------------------------------------------------------------------------------
    
    public double getLatResInDegrees()
    {
        return _latResInDegrees;
    }
    // ------------------------------------------------------------------------------
    
    public void setLonResInDegrees(double lonResInDegrees)
    {
        _lonResInDegrees = lonResInDegrees;
    }
    // ------------------------------------------------------------------------------
    
    public double getLonResInDegrees()
    {
        return _lonResInDegrees;
    }
  
    
}
