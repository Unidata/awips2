/*
 * Created on Jul 12, 2004
 *
 */
package ohd.hseb.util.gui.drawing;

import java.awt.Graphics;
import java.awt.Color;
import java.awt.Point;
//import java.util.List;

import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.MeasurementPoint;
import ohd.hseb.measurement.MeasurementPointSeries;

import ohd.hseb.util.DataPoint;


/**
 * @author GobsC
 *
 */

public class DataCurvePainter implements DataPointCanvasPainter
{
    
    private MeasurementPointSeries _measurementPointSeries = null;
    private DataPointCanvas _canvas = null;
    private boolean _shouldPaint = true;
    private Color _curveColor = null;
//  ----------------------------------------------------------------------------------------    

    public DataCurvePainter (MeasurementPointSeries series,
                             DataPointCanvas canvas,
                             Color curveColor)
    {
        _measurementPointSeries = series;     
        
        _canvas = canvas;
        
        _curveColor = curveColor;
    }
    
 
//  ----------------------------------------------------------------------------------------    

	public Measurement getMaxYMeasurement()
	{
		return _measurementPointSeries.getMaxYMeasurement();
		
	}

//  ----------------------------------------------------------------------------------------    

	public Measurement getMinYMeasurement()
	{
        return _measurementPointSeries.getMinYMeasurement();
	
	}

//  ----------------------------------------------------------------------------------------    
    public Measurement getMaxXMeasurement()
    {
        return _measurementPointSeries.getMaxXMeasurement();
    }

//  ----------------------------------------------------------------------------------------    

    public Measurement getMinXMeasurement()
    {
        return _measurementPointSeries.getMinXMeasurement();     
    }

//  ----------------------------------------------------------------------------------------    
	public void setShouldPaint(boolean shouldPaint)
	{
		_shouldPaint = shouldPaint;		
	}

// ----------------------------------------------------------------------------------------	
	public void paint(Graphics g)
	{
              
		if (_shouldPaint)
        {  
            drawDataPoints(g);            
        }
		
	} //end paint()
    
//  --------------------------------------------------------------------------
    private void drawDataPoints(Graphics g)
    {
        int count = _measurementPointSeries.size();
            
        g.setColor(_curveColor);    
            
        MeasuringUnit yUnit = _canvas.getDisplayedYMeasuringUnit();
        MeasuringUnit xUnit = _canvas.getDisplayedXMeasuringUnit();
  
        Measurement xMeasurement = null;
        Measurement yMeasurement = null;

        DataPoint oldDataPoint = null;

        for (int i = 0; i < count; i++)
        {
            MeasurementPoint point = _measurementPointSeries.getPoint(i);
    
            yMeasurement = point.getYMeasurement();
            double discharge = yMeasurement.getValue(yUnit);
                   
            xMeasurement = point.getXMeasurement();
            double hour = xMeasurement.getValue(xUnit);
   
            DataPoint dp = new DataPoint(hour, discharge);
    
            drawDataPoint(dp, g);
            
            if (oldDataPoint != null)
            {
                drawSegment(oldDataPoint, dp, g);    
            }
            
            oldDataPoint = dp;
        }      
    }
//  --------------------------------------------------------------------------

    private void drawSegment(DataPoint dp1, DataPoint dp2, Graphics g)
    {  
        Viewport viewport = getViewport(); 
    
//          if it is viewable, then draw it           
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
  
//   --------------------------------------------------------------------------
  
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
//  ----------------------------------------------------------------------------------------    
    private Viewport getViewport()
    {
        return _canvas.getViewport();
    }
//  ----------------------------------------------------------------------------------------    

    public void setMeasurementPointSeries(MeasurementPointSeries measurementPointSeries)
    {
        _measurementPointSeries = measurementPointSeries;
    }
//  ----------------------------------------------------------------------------------------    

    public MeasurementPointSeries getMeasurementPointSeries()
    {
        return _measurementPointSeries;
    }
//  ----------------------------------------------------------------------------------------    

} //end class DataCurvePainter
