/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.timeserieslite.gui.drawing;

import ohd.hseb.measurement.*;
import ohd.hseb.util.*;
import ohd.hseb.util.gui.drawing.Viewport;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;

import java.awt.*;

/**
 * @author GobsC
 *
 *  This class extends TsCanvasPainter and encapsulates the
 *  drawing of a significant data lines on a TsPaintableCanvas
 */
public class SignificantLinePainter implements TslCanvasPainter
{
  
    private Color _lineColor = null;
	
    private TsPaintableCanvas _canvas = null;
    
    private Measurement _measurement = null;
    
    private boolean _shouldPaint = false;
//  ---------------------------------------------------------
    public void setShouldPaint(boolean shouldPaint)
    {
        _shouldPaint = shouldPaint;    
    }


    // ---------------------------------------------------------
    public SignificantLinePainter(Color lineColor, 
    						   Measurement measurement,
                               MeasuringUnit displayedMeasuringUnit)
    {
    	
    	_lineColor = lineColor; 
    	
		_measurement = Measurement.getConvertedCopy(measurement,
                                                    displayedMeasuringUnit);
                                            
	
    }
 
//	---------------------------------------------------------
    public void paint(Graphics g)
    {	
		drawDataLine(g);
    }

// ----------------------------------------------------
   private Viewport getViewport()
   {
   		return _canvas.getViewport();
   }
    
// ---------------------------------------------------------
    
//	*************************************************************
//	 drawDataLine()
//	  scaling data & display data
//	*************************************************************
	private void drawDataLine( Graphics g ) 
	{ 
        MeasuringUnit unit = _canvas.getDisplayedMeasuringUnit();
		// convert again just in case the _canvas has had its units changed,
        // even though this feature is not available at this time
        if (_measurement.getUnit() != unit)
        {
		    _measurement = 
                 Measurement.getConvertedCopy(_measurement, unit);
            
        }			
		double value = _measurement.getValue();
		long minTime = (long) _canvas.getViewport().getMinDataPoint().getX();	
		long maxTime = (long) _canvas.getViewport().getMaxDataPoint().getX();		 
		
		DataPoint dp1 = new DataPoint(minTime, value);
		DataPoint dp2 = new DataPoint(maxTime, value);		
		
		if (getViewport().isViewable(dp1))
		{
			Point p1 = getViewport().getScreenPoint(dp1);
		    Point p2 = getViewport().getScreenPoint(dp2);
										  
		    g.setColor(_lineColor);
		    g.drawLine(p1.x, p1.y, p2.x, p2.y);
		}	
	 
	}

    //---------------------------------------------------------------
   
    
    //---------------------------------------------------------------

    
    public Measurement getMaxMeasurement()
    {
     
        Measurement measurement = null;
        
        if (_shouldPaint)
        {
           measurement = _measurement;
        } 
       
        return measurement;
    }

    
   public Measurement getMinMeasurement()
   {
     
       Measurement measurement = null;
        
       if (_shouldPaint)
       {
          measurement = _measurement;
       } 
       
       return measurement;
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
			
//----------------------------------------------------


} //end of  SignficantLinePainter
