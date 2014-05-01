package ohd.hseb.geomap.model;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.geomap.contour.RowColToLatLonTranslator;

public class ElevationMap implements RowColToLatLonTranslator
{
    private LatLonBounds _latLonBounds = null;
    private double _latResInDegrees = 0;
    private double _lonResInDegrees = 0;
    
    private short[][] _valueArray = null;
    private int _rowCount = 0;
    private int _colCount = 0;
    // ----------------------------------------------------------------------------------------------------------------------
    public ElevationMap(LatLonBounds latLonBounds, double latResInDegrees, double lonResInDegrees)
    {
        _latLonBounds = new LatLonBounds(latLonBounds);
        setLatResInDegrees(latResInDegrees);
        setLonResInDegrees(lonResInDegrees);
        
        _rowCount = (int) Math.ceil(latLonBounds.getLatDifference() / latResInDegrees);
        _colCount = (int) Math.ceil(latLonBounds.getLonDifference() / lonResInDegrees);
        
        _valueArray = new short[getRowCount()][getColCount()];
    }    
    // ------------------------------------------------------------------------------
    public void setLatLonBounds(LatLonBounds latLonBounds)
    {
        _latLonBounds = latLonBounds;
    }
    // ------------------------------------------------------------------------------
    
    public LatLonBounds getLatLonBounds()
    {
        return _latLonBounds;
    }
    // ------------------------------------------------------------------------------

    public List<LatLonPoint> getLatLonPointList()
    {
        return new ArrayList<LatLonPoint>();
    }
    // ------------------------------------------------------------------------------
   
    public void setLatResInDegrees(double latResInDegrees)
    {
        _latResInDegrees = latResInDegrees;
    }

    public double getLatResInDegrees()
    {
        return _latResInDegrees;
    }

    public void setLonResInDegrees(double lonResInDegrees)
    {
        _lonResInDegrees = lonResInDegrees;
    }

    public double getLonResInDegrees()
    {
        return _lonResInDegrees;
    }
    // ------------------------------------------------------------------------------
    public RowColumnPoint getRowColumnPoint(LatLonPoint latLonPoint)
    {
        double row = getRow(latLonPoint.getLat());
        double col = getCol(latLonPoint.getLon());

        return  new RowColumnPoint(row, col);
    }
    //  ------------------------------------------------------------------------------
    public LatLonPoint getLatLonPoint(RowColumnPoint rowColumnPoint)
    {
        double lat = getLat((int) rowColumnPoint.getRow());
        double lon = getLon((int) rowColumnPoint.getCol());

        return  new LatLonPoint(lat, lon);
    }
    //  ------------------------------------------------------------------------------
    public LatLonPoint getLatLonPoint(double row, double col)
    {
        double lat = getLat((int) row);
        double lon = getLon((int) col);

        return  new LatLonPoint(lat, lon);
    }
    
    //  ------------------------------------------------------------------------------
       
    public int getRow(double lat)
    {
        int row = -1;
        
        double latDiff = _latLonBounds.getNorthLat() - lat;
        row = (int)  Math.ceil(latDiff / getLatResInDegrees());
        
        return row;    
    }
    // ------------------------------------------------------------------------------
    public int getCol(double lon)
    {
        int col = -1;
        
        double lonDiff =  lon - _latLonBounds.getWestLon();
        col = (int)  Math.ceil(lonDiff / getLonResInDegrees());
        
        return col;  
    }
    // ------------------------------------------------------------------------------
    public double getLat(int row)
    {
        double lat = _latLonBounds.getNorthLat()  - (row * getLatResInDegrees());
        
        return lat;
    }
    // ------------------------------------------------------------------------------
    public double getLon(int col)
    {
        double lon = _latLonBounds.getWestLon() + (col * getLonResInDegrees());
        
        return lon;
    }
    // ------------------------------------------------------------------------------
    
    
    public void setValueByLatLon(double lat, double lon, short value)
    {
          
         setValue(getRow(lat), getCol(lon), value);
    }
    
    // ------------------------------------------------------------------------------
    public void setValue(int row, int col, short value)
    {
          
         _valueArray[row][col] = value;
    }
    
    // ------------------------------------------------------------------------------
    public short[][] getValueArray()
    {
        return _valueArray;
    }
    // ------------------------------------------------------------------------------
    public short getValue(int row, int col)
    {      
        short value = 0;
        
        if (isInRange(row, col))
        {
            value = _valueArray[row][col];
        }
        return value;
    }
    
    private boolean isInRange(int row, int col)
    {
        boolean result = false;
        if  ((row >-1) && 
           (row < getRowCount())   &&
           (col >-1) &&
           (col < getColCount()) )
        {
            result = true;
        }
            
        return result;
    }
    
    public short getValueByLatLon(double lat, double lon)
    {
        int row = getRow(lat);
        int col = getCol(lon);
        
        return getValue(row, col);
    }
    
    // ------------------------------------------------------------------------------
    
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
        
        buffer.append(getLatLonBounds() + "lat res = " + getLatResInDegrees() + " lon res = " + getLonResInDegrees());
        
        int rowCount = getRowCount();
        int colCount = getColCount();
      
        for (int row = 0; row < rowCount; row++)
        {
            for (int col = 0; col < colCount; col++)
            {
                buffer.append(_valueArray[row][col] + " ");
            }
            buffer.append("\n");
        }
        
        return buffer.toString();
    }
    
    // ------------------------------------------------------------------------------
    
    
    public void setRowCount(int rowCount)
    {
        _rowCount = rowCount;
    }
    public int getRowCount()
    {
        return _rowCount;
    }
    public void setColCount(int colCount)
    {
        _colCount = colCount;
    }
    public int getColCount()
    {
        return _colCount;
    }
  
    
    // ------------------------------------------------------------------------------

}
