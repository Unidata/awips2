/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.sshp.gui;

import ohd.hseb.measurement.*;
import ohd.hseb.util.*;
import ohd.hseb.util.gui.drawing.Viewport;
import ohd.hseb.util.gui.drawing.TsCanvasPainter;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;

import java.awt.Graphics;
import java.awt.*;

/**
 * @author GobsC
 *
 *  This class extends TsCanvasPainter and encapsulates the
 *  drawing of significant stage line data on a TsPaintableCanvas
 */
public class SigStageLinePainter implements TsCanvasPainter
{
  
    private Color _lineColor = null;
	
    private TsPaintableCanvas _canvas = null;
    
    private Measurement _sigStageMeasurement = null;
    
    private boolean _shouldPaint = false;
//  ---------------------------------------------------------
    public void setShouldPaint(boolean shouldPaint)
    {
        _shouldPaint = shouldPaint;    
    }


    // ---------------------------------------------------------
    public SigStageLinePainter(Color lineColor, 
    						   Measurement sigStageMeasurement,
    		 				   TsPaintableCanvas canvas)
    {
    	
    	_lineColor = lineColor; 
    	_canvas = canvas;
		
		_sigStageMeasurement = Measurement.getConvertedCopy(sigStageMeasurement,
                                            _canvas.getDisplayedMeasuringUnit());
                                            
	
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
        if (_sigStageMeasurement.getUnit() != unit)
        {
		    _sigStageMeasurement = 
                 Measurement.getConvertedCopy(_sigStageMeasurement, unit);
            
        }			
		double value = _sigStageMeasurement.getValue();
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
           measurement = _sigStageMeasurement;
        } 
       
        return measurement;
    }

    
   public Measurement getMinMeasurement()
   {
     
       Measurement measurement = null;
        
       if (_shouldPaint)
       {
          measurement = _sigStageMeasurement;
       } 
       
       return measurement;
   }
			
//----------------------------------------------------


} //end of  SigStageLinePainter
