/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.sshp.gui;

//import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.util.*;
import ohd.hseb.util.gui.FontHelper;
import ohd.hseb.util.gui.drawing.*;

import java.awt.Graphics;
import java.awt.*;
import java.text.*;

/**
 * @author Chip Gobs
 *
 *  This class implements CanvasPainter and encapsulates the
 *  drawing of the X and Y axes and all the tick marks
 */
public class TsBackgroundPainter implements CanvasPainter
{
  
  
    private TsPaintableCanvas _canvas = null;
    
    private ValueMapper _rightLabelValueMapper = null;
    
    	
//	graphing hours
	private int _hoursPerMajorXTick = 6;
	private int _hoursPerMinorXTick = 1;

	//time constant
    private static final int MILLIS_PER_HOUR = 1000 * 60 * 60;	

	//number formatter
	private NumberFormat _numberFormat = NumberFormat.getInstance();

	// various colors	
	private static final Color _backgroundColor = Color.black;
	
	
	// NOTE: future changes:
	// will want to have multiple time series, so there needs
	// to be a way to have different colors for timeseries
	
	//private Color _lineColor = null;	
    private static final Color _gridColor = Color.GRAY;	
    private Color _outlineColor = Color.WHITE;
    private static final Color _labelColor = Color.WHITE;	
    private static final Color _currentTimeColor = Color.YELLOW;
   
    //unit labels 
    private String _leftUnitLabelString = null;
    private String _rightUnitLabelString = null;


//	fonts
	private Font _titleFont = null;
	private Font _labelFont = null;
   
	//FontHelper - custom class
	private FontHelper _titleFontHelper = null;
    private FontHelper _labelFontHelper = null;
   
	// margins and shifts
    private final static int _titleYShift = - 20;
	private int _leftLabelShift = 30;
	private int _rightLabelShift = 10;
	
	private double _smallRoundingError = 0.000000001;
    
    
    private boolean _showMinorTicks = true;
  
    // ---------------------------------------------------------
    public TsBackgroundPainter(Color outlineColor, 
    		 				   TsPaintableCanvas canvas,
                               String leftUnitLabelString,
                               String rightUnitLabelString)
    {   	
    	_outlineColor = outlineColor; 
    	_canvas = canvas;
    		
        _leftUnitLabelString = leftUnitLabelString;
        _rightUnitLabelString = rightUnitLabelString;
        
        _numberFormat.setMaximumFractionDigits(2);
	
		_titleFont = new Font("TimesRoman",Font.BOLD,14); 
		_labelFont = new Font("Helvetica",Font.PLAIN,9);
		
		_titleFontHelper = new FontHelper(_titleFont);
		_labelFontHelper = new FontHelper(_labelFont);
        
        _showMinorTicks = true;
		
    } //tsBackgroundPainter()
 
//	---------------------------------------------------------
    public void setShowMinorTicks(boolean showMinorTicks)
    {
        _showMinorTicks = showMinorTicks;    
    }
//  ---------------------------------------------------------
    public void paint(Graphics g)
    {	
		drawBackground(g);
    }

// ----------------------------------------------------
   private Viewport getViewport()
   {
   		return _canvas.getViewport();
   }
// ----------------------------------------------------

   public void setRightLabelValueMapper(ValueMapper rightLabelValueMapper)
   {
       _rightLabelValueMapper = rightLabelValueMapper;
   }
// ----------------------------------------------------

   private ValueMapper getRightLabelValueMapper()
   {
       return _rightLabelValueMapper;
   }
    
// ---------------------------------------------------------

//	*************************************************************
//	 drawDataLine()
//	  scaling data & display data
//	*************************************************************
	public void drawBackground(Graphics g)
	{
	
		Rectangle r = _canvas.getBounds(); 
	
		//set the normal font to use
		g.setFont( _labelFont ); 
	
		//clear the image
		g.setColor(_backgroundColor); 
		g.fillRect(0, 0, r.width, r.height); 
	   
		//draw the axis labels, ticks, and dotted lines	
		drawHorizontalLines( g ); 
		drawVerticalLines( g );
	
		//draw the Title
		drawTitle(g);
        
		int x = 0;
		int y = 0;
		
        x = 10;
        y = _canvas.getTopMargin() + _titleYShift;
        drawUnitLabel(_leftUnitLabelString, g, x, y);
        
        x = _canvas.getViewport().getMaxScreenPoint().x + 20;
        y = _canvas.getTopMargin() + _titleYShift;
        drawUnitLabel(_rightUnitLabelString, g, x, y);
        

	}
			
//----------------------------------------------------
	   private void drawTitle(Graphics g)
		{
	        String titleString = _canvas.getTitle();
	        Rectangle boundingRectangle = _canvas.getBounds();
	        
	        int middleX = (int) (boundingRectangle.width * .5);
			int totalLeftShift = _titleFontHelper.getWidth(g, titleString) / 2;
			
			int x = middleX - totalLeftShift;
			int y = (_canvas.getTopMargin() + _titleYShift);
			
		    g.setFont(_titleFont);
			g.setColor(_labelColor);
			g.drawString(titleString, x, y);
		 
		    return;
		} //drawTitle
	
    private void drawUnitLabel(String unitLabel, Graphics g, int x, int y)
    {
           g.setFont(_titleFont);
           g.setColor(_labelColor);
           g.drawString(unitLabel, x, y);
     
           return;
    } //drawTitle

//     --------------------------------------------------------------------------   

    private void drawVerticalLines(Graphics g)
	{
		//  determine the min and max data values to chart
    
        DataPoint minDataPoint = getViewport().getMinDataPoint();
		DataPoint maxDataPoint = getViewport().getMaxDataPoint();
		Point point1 = null;
		Point point2 = null;
           
		double minYValue = minDataPoint.getY();
		double maxYValue = maxDataPoint.getY();
	   
		double minXValue = minDataPoint.getX();
		double maxXValue = maxDataPoint.getX();

		// draw each tick line    
		double tickValue = minXValue;
    
		int minorTickHours = _hoursPerMinorXTick;
		int majorTickHours = _hoursPerMajorXTick;

		for (; tickValue <= maxXValue; tickValue += minorTickHours * MILLIS_PER_HOUR)
		{
		   point1 = getViewport().getScreenPoint(new DataPoint(tickValue, minYValue));
		   point2 = getViewport().getScreenPoint(new DataPoint(tickValue, maxYValue));
		
		
		   //if on a majorTick
            if (isOnATick((long)tickValue, majorTickHours))
		    {
		           	
			    if ((tickValue == minXValue) || (tickValue == maxXValue)) 
			    {
			       //on graph boundary
				    g.setColor(_outlineColor);
				    g.drawLine(point1.x, point1.y, point2.x, point2.y);
			    }	
			    else //not on a graph boundary
			    {
			 	    g.setColor(_gridColor);
				    CanvasHelper.drawDashedLine(g, point1.x, point1.y, point2.x, point2.y); 
			    }
		
		 	    //draw the major tick extension
			    g.setColor(_outlineColor);
			    g.drawLine(point1.x, point1.y+6, point2.x, point1.y); 

			    //draw a major tick
			    //build the labels
			    String hourLabelString = CanvasHelper.getHourString((long)tickValue) + "Z";
			    String dateLabelString = CanvasHelper.getDateString((long)tickValue);

			    //	draw the labels
			    g.setColor(_labelColor);
			    g.drawString(hourLabelString, point1.x-5, point1.y + 15);
		
			    //only draw the date string at 00Z
			    if (hourLabelString.equals("00Z"))
			    {
			        g.drawString(dateLabelString, point1.x-5, point1.y + 30);
			    }
			}
			else //on a minor tick
			{
                
                //draw a minor tick	
				//draw the minor tick extension
			
		        if ((tickValue == minXValue) || (tickValue == maxXValue)) 
				{
				    //on boundary
					g.setColor(_outlineColor);
				    g.drawLine(point1.x, point1.y, point2.x, point2.y);
				}	
			
				g.setColor(_outlineColor);
				g.drawLine(point1.x, point1.y+2, point2.x, point1.y); 
             
			}
		} //end for i
    
    
		// if applicable, draw current time line
		long currentTime = System.currentTimeMillis();
		if ( (currentTime >= minXValue) && (currentTime <=maxXValue))
		{
		    point1 = getViewport().getScreenPoint(new DataPoint(currentTime, minYValue));	
			point2 = getViewport().getScreenPoint(new DataPoint(currentTime, maxYValue));
		
			g.setColor(_currentTimeColor);
			g.drawLine(point1.x, point1.y, point2.x, point2.y); 
		}
    
	 } //end drawXAxis

//		  --------------------------------------------------------------------------
	  private boolean isOnATick(long timeValue, int hoursPerTick)
	  {
		  boolean result = false;
	
		  long roundedTime = TimeHelper.truncateTimeInMillisToNearestHour((long)timeValue, hoursPerTick);	
          
        //  String timeValueString = DbTimeHelper.getDateTimeStringFromLongTime(timeValue);
       //   String roundedTimeString = DbTimeHelper.getDateTimeStringFromLongTime(roundedTime);
          	
		  if (  ( timeValue == roundedTime ) || 
				( timeValue == roundedTime + (hoursPerTick*MILLIS_PER_HOUR))
			 )	
		  {
			  result = true;
		  }
		  return result;
	  } //end isOnATick
	
	  
// -------------------------------------------------------------------------------------------    
      
    private void drawHorizontalLines(Graphics g)
    {
         //determine the min and max data values to chart
    
         DataPoint minDataPoint = getViewport().getMinDataPoint();
         DataPoint maxDataPoint = getViewport().getMaxDataPoint();
        
         //String header = "TsBackgroundPainter.drawHorizontalLines(): ";
  
         //System.out.println(header + "minDataPoint = " + minDataPoint);
         //System.out.println(header + "maxDataPoint = " + maxDataPoint);

    
         double minYValue = minDataPoint.getY();
         double maxYValue = maxDataPoint.getY();
   
         double minXValue = minDataPoint.getX();
         double maxXValue = maxDataPoint.getX();
     
        
         //determine scaling
        
         ScalingHelper scalingHelper = new ScalingHelper(minYValue, maxYValue);
  
         int minorTickCountMax = scalingHelper.getMinorTickCount();
         int majorTickCountMax = scalingHelper.getMajorTickCount();
    
        // int totalTickCount = majorTickCountMax * minorTickCount;
    
         double majorTickIncrement = scalingHelper.getMajorTickIncrement();
         double majorTickValue = minYValue;
         
         double minorTickIncrement = scalingHelper.getMinorTickIncrement();
    
         Point point1 = null;
         Point point2 = null;
    
    
         g.setFont(_labelFont);
    
         int iterationCount = 0;
        
         double maxTickValueBound = (maxYValue + _smallRoundingError);
           
         for (int majorTickCount = 0;   majorTickCount < majorTickCountMax;  majorTickCount++)
         {
              
             majorTickValue = (majorTickCount * (majorTickIncrement)) + minYValue;
             
             //get the points that are pointed to.
             point1 = getViewport().getScreenPoint(new DataPoint(minXValue, majorTickValue));    
             point2 = getViewport().getScreenPoint(new DataPoint(maxXValue, majorTickValue));

             // set color as appropriate for the tick lines
             // Note: Checking for tickValue == maxYValue or minYValue may
             // not working due to rounding issues, so add in a fudge factor
        
             if  ( ( majorTickValue == minYValue) ||
                    ((majorTickValue + majorTickIncrement) > maxTickValueBound)
                 ) //at edge of chart
             {
                 //draw the outside boundary line
                 g.setColor(_outlineColor);
                 g.drawLine(point1.x, point1.y, point2.x, point2.y); 
             }
             else // in middle of chart
             {
                 //draw the inside the boundary line
                 g.setColor(_gridColor);
                 //CanvasHelper.drawDashedLine(g, point1.x, point1.y, point2.x, point2.y); 
                 g.drawLine(point1.x, point1.y, point2.x, point2.y);
                 
             }
    
    
             //draw the left side tick extension and label
         
             // draw the tick extension
             g.setColor(_outlineColor);
             g.drawLine(point1.x-5, point1.y, point1.x, point2.y); 
    
             // draw the label
             String numberLabelString = _numberFormat.format(majorTickValue);
             g.setColor(_labelColor);
             g.drawString(numberLabelString, point1.x - _leftLabelShift , point1.y + 3);
        
            
        
        
            //draw the right side tick extension and label
         
            // draw the tick extension
            g.setColor(_outlineColor);
            g.drawLine(point2.x+5, point2.y, point2.x, point2.y); 
    
            // draw the label
            ValueMapper valueMapper =  getRightLabelValueMapper();
         
            double newValue = majorTickValue;
            if (valueMapper != null)
            {
               Measurement m = new Measurement(majorTickValue, valueMapper.getInputMeasuringUnit());
               newValue = valueMapper.getResultMeasurement(m).getValue();
            }
         
            numberLabelString = _numberFormat.format(newValue);
            g.setColor(_labelColor);
            g.drawString(numberLabelString, point2.x + _rightLabelShift , point2.y + 3);
         
            //draw all of the minor ticks
            // minor ticks are always inside and do not get labels
            
            if (_showMinorTicks)
            {
                if (majorTickCount < majorTickCountMax - 1)
                {
                    for (int minorTickCount = 1; minorTickCount <= minorTickCountMax; minorTickCount++)
                    {
                        double minorTickValue = majorTickValue + (minorTickCount * minorTickIncrement);
                        
                        Point minorPoint1 = getViewport().getScreenPoint(new DataPoint(minXValue, minorTickValue));    
                        Point minorPoint2  = getViewport().getScreenPoint(new DataPoint(maxXValue, minorTickValue));
                                 
                                 
                        //draw the inside the boundary line
                        g.setColor(_gridColor);
                        CanvasHelper.drawDashedLine(g, minorPoint1.x, minorPoint1.y, minorPoint2.x, minorPoint2.y); 
        
                    }
                }
            
            } //end if _showMinorTicks
         
         }

     } //end drawYAxis

//  ------------------------------------------------------------------------------

   
  

} //end of  SigStageLinePainter
