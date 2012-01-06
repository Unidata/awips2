package ohd.hseb.geomap.contour;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.Range;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;
import ohd.hseb.geomap.model.RowColumnPoint;

public class RowToLatLonContourer
{
    private double[][] _valueArray;
    private int _rowCount = 0;
    private int _colCount = 0;
    
    private RowColToLatLonTranslator _positionTranslator = null;

    // --------------------------------------------------------------------------------------------
    public RowToLatLonContourer(short [][] valueArray, RowColToLatLonTranslator translator )
    {
         this (getDoubleArrayFromShortArray(valueArray), translator);
    }
    // --------------------------------------------------------------------------------------------
    public RowToLatLonContourer(ContourGrid grid, RowColToLatLonTranslator translator)
    {
        this (grid.getValuesArray(), translator);
    }
    
    // --------------------------------------------------------------------------------------------
     
    
    public RowToLatLonContourer(double [][] valueArray, RowColToLatLonTranslator translator)
    {
        setValueArray(valueArray);
        
        _rowCount = valueArray.length;
        if (_rowCount > 0)
        {
            _colCount = valueArray[0].length;
        }
        
        _positionTranslator = translator;
        
     }
   
    
    // --------------------------------------------------------------------------------------------
    private static double[][] getDoubleArrayFromShortArray(short[][] shortArray)
    {
        int rowCount = shortArray.length;
        int colCount = 0;
        if (rowCount > 0)
        {
            colCount = shortArray[0].length;
        }
        
        double[][] doubleArray = new double[rowCount][colCount];
        for (int row = 0; row < rowCount; row ++)
        {
            for (int col = 0; col < colCount; col++)
            {
                doubleArray[row][col] = shortArray[row][col];
            }
        }
        
        return doubleArray;
    }
    // --------------------------------------------------------------------------------------------
    
    public void setValueArray(double[][] valueArray)
    {
        _valueArray = valueArray;
    }
    
    // --------------------------------------------------------------------------------------------

    public double[][] getValueArray()
    {
        return _valueArray;
    }
    
    //  --------------------------------------------------------------------------------------------
    
    public double getValue(int row, int col)
    {
      //  System.out.println("row = " + row + " col = " + col);
        return _valueArray[row][col];
    }
    
    //  --------------------------------------------------------------------------------------------
    public List< List<LatLonPolyline> > contour(double[] levelArray)
    {
        List< List<LatLonPolyline> > listOfLatLonPolylineLists = new ArrayList<List<LatLonPolyline> >();
        
        for (int i = 0; i < levelArray.length; i++)
        {
            List<LatLonPolyline> polylineList = contourOneLevel(levelArray[i]);
            listOfLatLonPolylineLists.add(polylineList);    
        }
        
        return listOfLatLonPolylineLists;
    }
    
    //  --------------------------------------------------------------------------------------------
  
    public List<LatLonPolyline> contourOneLevel(double level)
    {
        List<LatLonPolyline> polylineList = new ArrayList<LatLonPolyline>();
       
        LatLonPoint previousPoint = null;

        for (int row = 0; row < _rowCount; row++)
        {
            for (int col = 0; col < _colCount; col++)
            {

                List<LatLonPoint> pointList = getPointsFromFourCells(level, row, col); //eliminate duplicate points
                if (pointList.size() > 0)
                {
                    LatLonPolyline polyline = new LatLonPolyline(pointList);
                    polylineList.add(polyline);
                }
            }
        }
        
        return polylineList;
    }
    //  --------------------------------------------------------------------------------------------
    private List<LatLonPoint> getPointsFromFourCells(double level, int row, int col)
    {
        String header = "RowToLatLonContourer.getPointsFromFourCells(): ";
        List latLonPointList = new ArrayList<LatLonPoint> ();
        LatLonPoint point = null;
        
        if (col < _colCount - 1)
        {
            point = getCrossOverLatLonPoint(level, row, col, row, col+1);
            addPointToListIfNotNull(latLonPointList, point);
            
            if (row < _rowCount - 1)
            {
                point = getCrossOverLatLonPoint(level, row, col+1, row+1, col+1);
                addPointToListIfNotNull(latLonPointList, point);
                
                
                point = getCrossOverLatLonPoint(level, row+1, col+1, row+1, col);
                addPointToListIfNotNull(latLonPointList, point);
                
            }
            
        }
        if (row < _rowCount - 1)
        {
            point = getCrossOverLatLonPoint(level, row+1, col, row, col);
            addPointToListIfNotNull(latLonPointList, point);
            
        }
        if (latLonPointList.size() == 1)
        {
            latLonPointList.clear();
            System.out.println(header + "clearing list of its single point");
        }
        return latLonPointList;
    }
    //  --------------------------------------------------------------------------------------------
    private LatLonPoint getCrossOverLatLonPoint(double level,
                                                int row1, int col1,
                                                int row2, int col2)
    {
        LatLonPoint point = null;
        
        double value1 = getValue(row1, col1);
        double value2 =  getValue(row2, col2);
        Range range = new Range(value1, value2);
        
        if (range.isBetweenInclusive(level))
        {
            if (value1 == value2) //ignore it, this is counterintuitive, but needed
            {
                //do nothing
            }
            else //there is a distinct boundary and value1 != value2
            {
                LatLonPoint latLonPoint1  = getLatLonPoint(row1, col1);
                LatLonPoint latLonPoint2  = getLatLonPoint(row2, col2);
                 
                LatLonPoint interpolatedPoint = interpolate(level, latLonPoint1, value1, latLonPoint2, value2);
                point = interpolatedPoint;
            }
                
        }  
        return point;  
    }
    
    //  --------------------------------------------------------------------------------------------
    private LatLonPoint interpolate(double targetValue,
                                    LatLonPoint point1, double value1, 
                                    LatLonPoint point2, double value2)
    {  
       // String header = "RowToLatLonContourer.interpolate(): ";
        
        Interpolator interpolator = new Interpolator();
        
        double lat1 = point1.getLat();
        double lat2 = point2.getLat();

        double lon1 = point1.getLon();
        double lon2 = point2.getLon();


//      these are switched from the usual order, since we are seeking lat and lon instead of value
        double  interpolatedLat = interpolator.interpolate(targetValue, value1, lat1, value2, lat2); 
        double  interpolatedLon = interpolator.interpolate(targetValue, value1, lon1, value2, lon2); 

        LatLonPoint  newPoint = new LatLonPoint(interpolatedLat, interpolatedLon);
        
        RowColumnPoint rcPoint = getRowColumnPoint(newPoint);
     //   System.out.println(header + rcPoint );
     
        return newPoint;
    }
    
    //  --------------------------------------------------------------------------------------------
    
    private LatLonPoint getLatLonPoint(int row, int col)
    {
           
        LatLonPoint point = _positionTranslator.getLatLonPoint(new RowColumnPoint(row, col));
         
        return point;
    }
    
    //  --------------------------------------------------------------------------------------------
    private RowColumnPoint getRowColumnPoint(LatLonPoint latLonPoint)
    {
        RowColumnPoint point = _positionTranslator.getRowColumnPoint(latLonPoint);
         
        return point;
    }
    
    //  --------------------------------------------------------------------------------------------
    private void addPointToListIfNotNull(List<LatLonPoint> pointList, LatLonPoint point)
    {
        if (point != null)
        {
            pointList.add(point);
        }
        
        return;
    }
    //  --------------------------------------------------------------------------------------------

}
