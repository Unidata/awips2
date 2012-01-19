/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.sshp.gui;


import ohd.hseb.util.*;
import ohd.hseb.util.gui.drawing.*;

import java.awt.Graphics;
import java.awt.*;

/**
 * @author GobsC
 *
 *  This class extends TsCanvasPainter and encapsulates the
 *  drawing of the vertical time line indicator on a TsPaintableCanvas
 */
public class TimeLinePainter implements CanvasPainter
{
  
    private Color _lineColor = null;
	
    private TsPaintableCanvas _canvas = null;
    
    private TimeHolder _timeHolder = null;
  
    // ---------------------------------------------------------
    public TimeLinePainter(Color lineColor, 
    					   TimeHolder timeHolder,
    		 			   TsPaintableCanvas canvas)
    {
    	
    	_lineColor = lineColor; 
    	_canvas = canvas;
		
		_timeHolder = timeHolder;
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
        
        double dataX = _timeHolder.getTime();
        double maxDataY =  getViewport().getMaxDataPoint().getY();
        double minDataY =  getViewport().getMinDataPoint().getY();
        
        DataPoint dp1 = new DataPoint(dataX, maxDataY);
        DataPoint dp2 = new DataPoint(dataX, minDataY);
        
        if (getViewport().isViewable(dp1))
        {
        
    	    Point p1 = _canvas.getViewport().getScreenPoint(dp1);
            Point p2 = _canvas.getViewport().getScreenPoint(dp2);
     			
            g.setColor(_lineColor);        
            g.drawLine(p1.x, p1.y, p2.x, p2.y);	
		
        }
	 
	} // end of  drawDataLine()	 	
			
//----------------------------------------------------


} //end of  TimeLinePainter
