/*
 * Created on Jul 14, 2004
 *
 * 
 */
package ohd.hseb.measurement;


import java.util.Comparator;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Chip Gobs
 *
 * This class contains a collection of MeasurementPoint objects and can
 * operate on the collection.
 */
public class MeasurementPointSeries
{
    // ------------------------------------------------------------------------
   
    private List _measurementPointList = null;
   
    private MeasuringUnit _xMeasuringUnit = null;
    private MeasuringUnit _yMeasuringUnit = null;
    
    private Comparator _comparator;
   
    // ------------------------------------------------------------------------
   
    public MeasurementPointSeries(MeasuringUnit xMeasuringUnit, MeasuringUnit yMeasuringUnit, Comparator comparator)
    {
        _xMeasuringUnit = xMeasuringUnit;
        _yMeasuringUnit = yMeasuringUnit;

        _comparator = comparator;
        
        _measurementPointList = new ArrayList();
    }
    
    // ------------------------------------------------------------------------
  
	public void setComparator(Comparator comparator)
	{
		_comparator = comparator;
	}
    
    // ------------------------------------------------------------------------
  
	public Comparator getComparator()
	{
		return _comparator;
	} 
    
    //  ------------------------------------------------------------------------  
    
    // ------------------------------------------------------------------------
    public void addPoint(MeasurementPoint newPoint)
    {
        newPoint = new MeasurementPoint(newPoint, _xMeasuringUnit, _yMeasuringUnit);
    
        boolean added = false;
        for (int i = 0; i < _measurementPointList.size() && !added; i++)
        {
            MeasurementPoint existingPoint = (MeasurementPoint) _measurementPointList.get(i);
            
            int result = _comparator.compare(newPoint, existingPoint); 
            
            if ((result == -1) || (result == 0))
            {
                _measurementPointList.add(i, newPoint);
                added = true;
            }
        }
        
        if (! added)
        {
            _measurementPointList.add(newPoint);
        }
     
        return;
    }
    // ------------------------------------------------------------------------
    public void removePoint(MeasurementPoint point)
    {
           _measurementPointList.remove(point);
    }
       
    // ------------------------------------------------------------------------
    public void clear()
    {
        _measurementPointList.clear();    
    }
    
    // ------------------------------------------------------------------------
 
    public int size()
    {
        int result = 0;
        
        if (_measurementPointList != null)
        {
           result = _measurementPointList.size();     
        }
        
        return  result;
    }
    // ------------------------------------------------------------------------
   
    public MeasurementPoint getPoint(int index)
    {
        return (MeasurementPoint) _measurementPointList.get(index);
    }
    //  ------------------------------------------------------------------------
   
    public Measurement getMaxXMeasurement()
    {
        return getMaxMeasurement(true);
    }
    
    //  ------------------------------------------------------------------------
   
    public Measurement getMinXMeasurement()
    {
        return getMinMeasurement(true);
    }
    
    //  ------------------------------------------------------------------------
   
   
    public Measurement getMaxYMeasurement()
    {
        return getMaxMeasurement(false);
        
    }
    
    //  ------------------------------------------------------------------------
   
    public Measurement getMinYMeasurement()
    {
        return getMinMeasurement(false);
    }
    
    //  ------------------------------------------------------------------------
    
    public Measurement getMaxMeasurement(boolean isXOrdinate)
    {
        Measurement maxMeasurement = null;
        Measurement pointMeasurement = null;
        
        MeasuringUnit unit = null;
        
        // determine which units we are dealing with
        if (isXOrdinate)
        {
            unit = _xMeasuringUnit;   
        }
        else
        {
            unit = _yMeasuringUnit;     
        }
        
        
        
        // look at each measurement point to determine the max
        for (int i = 0; i < size(); i++)
        {
            MeasurementPoint point = (MeasurementPoint) _measurementPointList.get(i);
           
            if (isXOrdinate)
            {
                pointMeasurement = point.getXMeasurement();
            }  
            else
            {
                pointMeasurement = point.getYMeasurement();   
            }
            
            if (maxMeasurement == null) 
            {
                maxMeasurement = pointMeasurement;    
            }
            else //maxMeasurement != null
            {
                if (pointMeasurement != null)
                {
                    if (pointMeasurement.getValue(unit) > maxMeasurement.getValue(unit))
                    {
                        maxMeasurement = pointMeasurement;    
                    }    
                }
            }    
              
        }
        
        return maxMeasurement;
        
    }
    
      
   
    //  ------------------------------------------------------------------------
    public Measurement getMinMeasurement(boolean isXOrdinate)
       {
           Measurement minMeasurement = null;
           Measurement pointMeasurement = null;
        
           MeasuringUnit unit = null;
        
           // determine which units we are dealing with
           if (isXOrdinate)
           {
               unit = _xMeasuringUnit;   
           }
           else
           {
               unit = _yMeasuringUnit;     
           }
        
        
        
           // look at each measurement point to determine the max
           for (int i = 0; i < size(); i++)
           {
               MeasurementPoint point = (MeasurementPoint) _measurementPointList.get(i);
            
            
            
               if (isXOrdinate)
               {
                   pointMeasurement = point.getXMeasurement();
               }  
               else
               {
                   pointMeasurement = point.getYMeasurement();   
               }
            
               if (minMeasurement == null) 
               {
                   minMeasurement = pointMeasurement;    
               }
               else //minMeasurement != null
               {
                   if (pointMeasurement != null)
                   {
                       if (pointMeasurement.getValue(unit) < minMeasurement.getValue(unit))
                       {
                           minMeasurement = pointMeasurement;    
                       }    
                   }
               }    
              
           }
        
           return minMeasurement;
        
       }
   //  ------------------------------------------------------------------------ 
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();

        for (int i = 0 ; i < size(); i++)
        {
            MeasurementPoint point = getPoint(i);
           
            Measurement xMeasurement = point.getXMeasurement();
            Measurement yMeasurement = point.getYMeasurement();
           
            buffer.append("x = " + xMeasurement + " y = " + yMeasurement + "\n");
                 
        }   
       
        return buffer.toString();
    
    }   
   
   //  ------------------------------------------------------------------------
   public static void main(String[] argStringArray)
   {
       MeasuringUnit xUnit = MeasuringUnit.hours;
       MeasuringUnit yUnit = MeasuringUnit.cfs;

       boolean compareXValues = true;
       boolean ascendingOrder = true;
       
       Comparator comparator = new MeasurementPointComparator(compareXValues, ascendingOrder);
       
       MeasurementPointSeries measurementPointSeries = new MeasurementPointSeries(xUnit, yUnit, comparator);
          
       int measurementCount = 10;

       for (int i = 0 ; i < measurementCount; i++)
       {
            Measurement m1 = new Measurement(measurementCount - i - 1, xUnit);
            Measurement m2 = new Measurement(2*i, yUnit);

            MeasurementPoint point = new MeasurementPoint(m1, m2);

            measurementPointSeries.addPoint(point);
       }
       
       
          
       for (int i = 0 ; i < measurementPointSeries.size(); i++)
       {
           MeasurementPoint point = measurementPointSeries.getPoint(i);
           
           Measurement xMeasurement = point.getXMeasurement();
           Measurement yMeasurement = point.getYMeasurement();
           
           System.out.println("x = " + xMeasurement + " y = " + yMeasurement);       
       }
       
       Measurement minXMeasurement = measurementPointSeries.getMinXMeasurement();   
       Measurement minYMeasurement = measurementPointSeries.getMinYMeasurement();
 
       Measurement maxXMeasurement = measurementPointSeries.getMaxXMeasurement(); 
       Measurement maxYMeasurement = measurementPointSeries.getMaxYMeasurement(); 

       System.out.println("minX = " + minXMeasurement + " minY = " + minYMeasurement); 
       System.out.println("maxX = " + maxXMeasurement + " maxY = " + maxYMeasurement); 
   } 
   //  ------------------------------------------------------------------------
   
}  //end MeasurementPointSeries
