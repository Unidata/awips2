/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.timeserieslite.gui.drawing;

import ohd.hseb.measurement.*;
import ohd.hseb.util.*;
import ohd.hseb.util.gui.drawing.*;

import java.awt.Graphics;
import java.awt.*;

/**
 * @author GobsC
 *
 *  This class extends TsCanvasPainter and encapsulates the
 *  drawing of an irregular time series on a TsPaintableCanvas
 */
public class TsDataPointPainter implements TslCanvasPainter
{

	private static final int MILLIS_PER_HOUR = 1000 * 60 * 60;
    
    private Color _pointColor = null;
    
    private IrregularTimeSeriesHolder _timeSeriesHolder = null;
    private RegularTimeSeriesHolder _regularTimeSeriesHolder = null;
    
    private TsPaintableCanvas _canvas = null;
  
    private boolean _isRegular = false;  // distinguishes between the kind of time series
  
    private boolean _drawLinesBetweenPoints = false;
    private boolean _shouldPaint = true;
    
    // ---------------------------------------------------------
    public TsDataPointPainter(Color pointColor, 
                        RegularTimeSeriesHolder regularTimeSeriesHolder,
                        boolean drawLinesBetweenPoints)
    {
        
        _isRegular = true;
        
        _regularTimeSeriesHolder = regularTimeSeriesHolder;
        
        _pointColor = pointColor;
        
        _drawLinesBetweenPoints = drawLinesBetweenPoints;
        
    }                       
    // ---------------------------------------------------------
    
    public TsDataPointPainter(Color pointColor, 
    					IrregularTimeSeriesHolder timeSeriesHolder)
    {
    	
        _isRegular = false;
        
    	_timeSeriesHolder = timeSeriesHolder;
    	
         _drawLinesBetweenPoints = false;
        
    	_pointColor = pointColor;
    }
    
    // ---------------------------------------------------------
    public TsDataPointPainter(Color pointColor, 
                           RegularTimeSeriesHolder regularTimeSeriesHolder)
    {
        this (pointColor, regularTimeSeriesHolder, false);
       
    }
//  ---------------------------------------------------------
    public void setShouldPaint(boolean shouldPaint)
    {
        _shouldPaint = shouldPaint;    
    }

//	---------------------------------------------------------
    public void paint(Graphics g)
    {	
        //String header = "StagePainter.paint(): ";
        
        if (_shouldPaint)
        {
		    drawDataPoints(g);
        }
    }

// ----------------------------------------------------------
    private void drawDataPoints( Graphics g ) 
    {
        if (_isRegular)
        {
            drawRegularDataPoints(g);
        }
        else
        {
            drawIrregularDataPoints(g);
        }
    }
// ----------------------------------------------------------
    private void drawRegularDataPoints( Graphics g ) 
    { 
        String header = "TsDataPointPainter.drawRegularDataPoints():";
         
    
        if (getRegularTimeSeries() == null )
        {
            //System.out.println(header + " time series is null");
            return;
        }
        
        long startTime = getRegularTimeSeries().getStartTime();
        long endTime = getRegularTimeSeries().getEndTime();

             
        g.setColor(_pointColor);
        

        long hoursPerInterval = 1;
        long millisPerInterval = MILLIS_PER_HOUR * hoursPerInterval;
         
        DataPoint previousDataPoint = null; 
         
        //draw each data point
        for ( long time = startTime; time <= endTime; time += millisPerInterval)
        { 
            
            Measurement measurement = getRegularTimeSeries().getMeasurementByTime(time);   
            
            measurement = measurement.getCopy(getDisplayedMeasuringUnit());
            
                
            double value = measurement.getValue();
              
            DataPoint dp1 = new DataPoint(time, value);
                
            drawDataPoint(dp1, g);
            
            
            if (_drawLinesBetweenPoints)
            {
                if  (previousDataPoint != null)
                {
                    drawSegment(previousDataPoint, dp1, g);    
                }
            
                previousDataPoint = dp1;
            }
                
        } // for loop
     
    } // end of  drawRegularDataPoints() method


// ----------------------------------------------------------

	private void drawIrregularDataPoints( Graphics g ) 
	{ 
        
        DataPoint previousDataPoint = null; 
        
		String header = "StagePainter.drawIrregularDataPoints():";
		//System.out.println(header + " at beginning");
		
		if (getTimeSeries() == null)
		{
			//System.out.println(header + " time series is null");
		}
			 
		g.setColor(_pointColor);
		
		//System.out.println(header + "time series = " + getTimeSeries());
		
	 	//draw each data point
		for ( int i = 0; (getTimeSeries() != null) && 
		                  i < getTimeSeries().getMeasurementCount(); i++)
		{ 
		   	
			AbsTimeMeasurement measurement = (AbsTimeMeasurement) 
								getTimeSeries().getAbsTimeMeasurementByIndex(i);	
			
			
			
			AbsTimeMeasurement convertedMeasurement = 
			                   AbsTimeMeasurement.getConvertedCopy(measurement, 
                                                       getDisplayedMeasuringUnit());
				
			double convertedValue = convertedMeasurement.getValue();
			long time = measurement.getTime();	 
			 
			DataPoint dp1 = new DataPoint(time, convertedValue);
				
			drawDataPoint(dp1, g);
            
            
			if (_drawLinesBetweenPoints)
			{
			    if  (previousDataPoint != null)
			    {
			        drawSegment(previousDataPoint, dp1, g);    
			    }
			    
			    previousDataPoint = dp1;
			}
			
            	
		
		} // for loop
	 
	} // end of  drawIrregularDataPoints() method

//  ---------------------------------------------------------
    private void drawSegment(DataPoint dp1, DataPoint dp2, Graphics g)
    {
        
        Viewport viewport = getViewport(); 
        
//      if it is viewable, then draw it           
        if   ( (getViewport().isViewable(dp1)) && (getViewport().isViewable(dp2)) )                 
        {                        
             Point p1 = getViewport().getScreenPoint(dp1);
             Point p2 = getViewport().getScreenPoint(dp2);
          
             g.drawLine(p1.x, p1.y, p2.x, p2.y);
             
         }
        else
        {
                 //System.out.println("StagePainter.drawSegment(): DataPoint " +
                 //                    dp1 + " is not Viewable"); 
        }          
        
    }
  
//  ---------------------------------------------------------
  
    private void drawDataPoint(DataPoint dp, Graphics g)
    {
        // if it is viewable, then draw it           
        if   (getViewport().isViewable(dp))                 
        {                        
            Point p1 = getViewport().getScreenPoint(dp);

            int diameter = 4;
            int radius = diameter/2;
            g.drawOval(p1.x-radius, p1.y-radius, diameter, diameter);   
        
            //System.out.println(header + " x = " + p1.x + " y = " + p1.y);  
        }
        else
        {
            //System.out.println("StagePainter.drawDataPoints(): DataPoint " +
            //                    dp1 + " is not Viewable"); 
        }        
        
    }    
//	 --------------------------------------------------------
    private IrregularTimeSeries getTimeSeries()
	{
        IrregularTimeSeries ts = null;  
           
	    if (_timeSeriesHolder == null)
		{
		    //System.out.println("StagePainter(). _timeSeriesHolder is null");	
		}
        else
        {
            ts = _timeSeriesHolder.getTimeSeries();    
        }
		return ts;
	}

//   ---------------------------------------------------------

    private RegularTimeSeries getRegularTimeSeries()
    {
        RegularTimeSeries ts = null;
        
        
        if (_regularTimeSeriesHolder == null)
        {
            //System.out.println("StagePainter(). _timeSeriesHolder is null"); 
        }
        else
        {
            ts =  _regularTimeSeriesHolder.getTimeSeries();      
        }
        
        return ts;
    }
//	   ---------------------------------------------------------
   
	   private MeasuringUnit getDisplayedMeasuringUnit()
	   {
		   return _canvas.getDisplayedMeasuringUnit();
	   }
//	   ---------------------------------------------------------
	   
	   private Viewport getViewport()
	   {
		   return _canvas.getViewport();
	   }
//	   ---------------------------------------------------------

	   public Measurement getMinMeasurement()
	   {
           String header = "StagePainter.getMinMeasurement()";
        
		   Measurement minMeasurement = null;
           
           //extremes don't count unless displayed
           if (_shouldPaint) 
           {
           
               long startTime = _canvas.getStartTime();
               long endTime = _canvas.getEndTime();
    		
    		   if ((_isRegular) && (getRegularTimeSeries() != null))
    		   {		
    			   minMeasurement = getRegularTimeSeries().getMinMeasurement(startTime, endTime);
    		   }
               else if (getTimeSeries() != null)
               {
                   minMeasurement = getTimeSeries().getMinMeasurement(startTime, endTime);  
               }
    		   else
    		   {
    			   minMeasurement = new Measurement(0.0, _canvas.getDisplayedMeasuringUnit());
    		   }
           }
          // System.out.println(header + "minMeasurement = " + minMeasurement);
    
        
		   return minMeasurement;				 
	   }
       
//  ----------------------------------------------------
   
//	   ----------------------------------------------------
    public Measurement getMaxMeasurement()
    {
          String header = "StagePainter.getMaxMeasurement()";
        
          Measurement maxMeasurement = null;
          
          if (_shouldPaint)  //extremes don't count unless displayed
          {
          
              long startTime = _canvas.getStartTime();
              long endTime = _canvas.getEndTime();
            
        
              if ((_isRegular) && (getRegularTimeSeries() != null))
              {     
                  maxMeasurement = getRegularTimeSeries().getMaxMeasurement(startTime, endTime);
              }
              else if (getTimeSeries() != null)
              {
                  maxMeasurement = getTimeSeries().getMaxMeasurement(startTime, endTime);  
              }
              else
              {
                  //System.out.println(header + "No time series found!!!!");
                  maxMeasurement = new Measurement(1.0, _canvas.getDisplayedMeasuringUnit());
              }
          
          }
          
          return maxMeasurement;                 
    }
    /**
     * @param canvas The canvas to set.
     */
    public void setCanvas(TsPaintableCanvas canvas)
    {
        _canvas = canvas;
    }
    /**
     * @return Returns the canvas.
     */
    public TsPaintableCanvas getCanvas()
    {
        return _canvas;
    }
    /**
     * @param drawLinesBetweenPoints The drawLinesBetweenPoints to set.
     */
    public void setDrawLinesBetweenPoints(boolean drawLinesBetweenPoints)
    {
        _drawLinesBetweenPoints = drawLinesBetweenPoints;
    }
    /**
     * @return Returns the drawLinesBetweenPoints.
     */
    public boolean getDrawLinesBetweenPoints()
    {
        return _drawLinesBetweenPoints;
    }
     
			
//----------------------------------------------------


} //end of PrecipPainter
