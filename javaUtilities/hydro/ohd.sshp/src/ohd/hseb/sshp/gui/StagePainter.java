/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.sshp.gui;

import ohd.hseb.measurement.*;
import ohd.hseb.util.*;
import ohd.hseb.util.gui.FontHelper;
import ohd.hseb.util.gui.drawing.*;

import java.awt.*;


/**
 * @author GobsC
 *
 *  This class extends TsCanvasPainter and encapsulates the
 *  drawing of precipitation data on a TsPaintableCanvas
 */
public class StagePainter implements TsCanvasPainter
{

    private static double MISSING_VALUE = -9999.0;
	private static final int MILLIS_PER_HOUR = 1000 * 60 * 60;
    
    private Color _pointColor = null;
    
    private IrregularTimeSeriesHolder _timeSeriesHolder = null;
    private RegularTimeSeriesHolder _regularTimeSeriesHolder = null;
    
    private TsPaintableCanvas _canvas = null;
  
    private boolean _isRegular = false;  // distinguishes between the kind of time series
  
    private boolean _drawLinesBetweenPoints = false;
    private boolean _shouldPaint = true;
    
    private String _pointString = null;
    private Point  _shiftAmount = new Point(0,0);
 
    private Font _pointLabelFont = new Font("Helvetica",Font.PLAIN,10);
    private FontHelper _pointLabelFontHelper = new FontHelper(_pointLabelFont);
    
    // ---------------------------------------------------------
    public StagePainter(Color pointColor, 
                        RegularTimeSeriesHolder regularTimeSeriesHolder,
                        TsPaintableCanvas canvas,
                        boolean drawLinesBetweenPoints)
    {
        
        _isRegular = true;
        
        _regularTimeSeriesHolder = regularTimeSeriesHolder;
        _canvas = canvas;
        
        _pointColor = pointColor;
        
        _drawLinesBetweenPoints = drawLinesBetweenPoints;
        
    }                       
    // ---------------------------------------------------------
    public StagePainter(Color pointColor, 
            IrregularTimeSeriesHolder timeSeriesHolder,
            TsPaintableCanvas canvas,
            boolean drawLinesBetweenPoints
            )
    {

        _isRegular = false;

        _timeSeriesHolder = timeSeriesHolder;
        _canvas = canvas;

        _pointColor = pointColor;
        
        _drawLinesBetweenPoints = drawLinesBetweenPoints;
    }

// ---------------------------------------------------------
    public StagePainter(Color pointColor, 
    					IrregularTimeSeriesHolder timeSeriesHolder,
    		 			TsPaintableCanvas canvas)
    {
    	
        _isRegular = false;
        
    	_timeSeriesHolder = timeSeriesHolder;
    	_canvas = canvas;
    	
    	_pointColor = pointColor;
    }
    
    // ---------------------------------------------------------
    public StagePainter(Color pointColor, 
                           RegularTimeSeriesHolder regularTimeSeriesHolder,
                           TsPaintableCanvas canvas)
    {
        this (pointColor, regularTimeSeriesHolder, canvas, false);
       
    }
//  ---------------------------------------------------------
    public void setShouldPaint(boolean shouldPaint)
    {
        _shouldPaint = shouldPaint;    
    }

//	---------------------------------------------------------
    public void paint(Graphics g)
    {	
       // String header = "StagePainter.paint(): ";     
       
        if (_shouldPaint)
        {
            
            Graphics2D g2d = (Graphics2D) g;
            
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON);
        
		    drawDataPoints(g);
            
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                        RenderingHints.VALUE_ANTIALIAS_OFF);
           
        }
    }

// ----------------------------------------------------------
    private void drawDataPoints( Graphics g ) 
    {
        String header = "StagePainter.drawDataPoints(): ";     
        g.setFont(_pointLabelFont);
        
        if (_isRegular)
        {
            //System.out.println(header + " painter's regular time series = " + _regularTimeSeriesHolder.getTimeSeries());

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
        String header = "StagePainter.drawRegularDataPoints():";
         
    
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
         
        final int pointsBetweenLabels = 6; 
        int pointCount = 0;
        
        //draw each data point
        for ( long time = startTime; time <= endTime; time += millisPerInterval)
        { 
            
            Measurement measurement = getRegularTimeSeries().getMeasurementByTime(time);   
            
            measurement = measurement.getCopy(getDisplayedMeasuringUnit());
            
                
            double value = measurement.getValue();
              
            if (value != MISSING_VALUE)
            {      
                DataPoint dp1 = new DataPoint(time, value);
                
                drawDataPoint(dp1, g, pointsBetweenLabels, pointCount);
                pointCount++;
                
                if (_drawLinesBetweenPoints)
                {
                    if  (previousDataPoint != null)
                    {
                        drawSegment(previousDataPoint, dp1, g);    
                    }
                    
                    previousDataPoint = dp1;
                }       
            }
                
        } // for loop
     
    } // end of  drawRegularDataPoints() method


// ----------------------------------------------------------

	private void drawIrregularDataPoints( Graphics g ) 
	{ 
		String header = "StagePainter.drawIrregularDataPoints():";
		//System.out.println(header + " at beginning");
		
		if (getTimeSeries() == null)
		{
			//System.out.println(header + " time series is null");
		}
			 
		g.setColor(_pointColor);
		
		//System.out.println(header + "time series = " + getTimeSeries());
		
        DataPoint previousDataPoint = null; 
           
        
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
    private void drawDataPoint(DataPoint dp, Graphics g, int pointsBetweenLabels, int pointCount)
    {
        
        // if it is viewable, then draw it           
        if   (getViewport().isViewable(dp))                 
        {                        
            Point p1 = getViewport().getScreenPoint(dp);

            int diameter = 4;
            int radius = diameter/2;
            
            if (_pointString == null)
            {
                g.drawOval(p1.x-radius, p1.y-radius, diameter, diameter);   
            }
            else if (_pointString.equalsIgnoreCase("x"))
            {      
                  drawX(g, p1, radius);            
            }
            else
            {
                String string = getPointString();
                
                int xShift = - _pointLabelFontHelper.getWidth(g, string) /2 ;
                
                int x = p1.x + xShift;
                int y =  p1.y + _shiftAmount.y;
                
                
                drawX(g, p1, radius);
                
              //  g.drawOval(p1.x-radius, p1.y-radius, diameter, diameter);   
             
                //occasionally draw the special label for the time series
                if (pointCount % pointsBetweenLabels == 0)
                {
                    g.drawString(getPointString(), x, y );
                }
               
            }
         
            //System.out.println(header + " x = " + p1.x + " y = " + p1.y);  
        }
        else
        {
            //System.out.println("StagePainter.drawDataPoints(): DataPoint " +
            //                    dp1 + " is not Viewable"); 
        }        
    }
    
    // --------------------------------------------------------------------------------
    
    private void drawX(Graphics g, Point point, int radius)
    {
        int x1 = point.x - radius;
        int y1 = point.y + radius;
        int x2 = point.x + radius;
        int y2 = point.y - radius;
            
        
        g.drawLine(x1, y1, x2, y2);
        g.drawLine(x1, y2, x2, y1);
        
        return;
    }
    
    // --------------------------------------------------------------------------------
    
    private void drawDataPoint(DataPoint dp, Graphics g)
    {
        
      
        // if it is viewable, then draw it           
        if   (getViewport().isViewable(dp))                 
        {                        
            Point p1 = getViewport().getScreenPoint(dp);

            int diameter = 4;
            int radius = diameter/2;
            
            if (_pointString == null)
            {
                g.drawOval(p1.x-radius, p1.y-radius, diameter, diameter);   
            }
            else if (_pointString.equalsIgnoreCase("x"))
            {      
                int x1 = p1.x - radius;
                int y1 = p1.y + radius;
                int x2 = p1.x + radius;
                int y2 = p1.y - radius;
                    
                g.drawLine(x1, y1, x2, y2);
                g.drawLine(x1, y2, x2, y1);         
            }
            else
            {
                String string = getPointString();
                      
                int x = p1.x + _shiftAmount.x;
                int y =  p1.y + _shiftAmount.y;
                g.drawOval(p1.x-radius, p1.y-radius, diameter, diameter);   
             // g.drawString(getPointString(), x, y );
               
            }
         
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
    public void setPointString(String pointString)
    {
        _pointString = pointString;
    }
    public String getPointString()
    {
        return _pointString;
    }
    public void setShiftAmount(Point shiftAmount)
    {
        _shiftAmount = shiftAmount;
    }
    public Point getShiftAmount()
    {
        return _shiftAmount;
    }
     
			
//----------------------------------------------------


} //end of PrecipPainter
