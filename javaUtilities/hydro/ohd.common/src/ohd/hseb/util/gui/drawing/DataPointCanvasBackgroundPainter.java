/*
 * Created on July 14, 2004
 *
 * 
 */
package ohd.hseb.util.gui.drawing;

import ohd.hseb.util.*;
import ohd.hseb.util.gui.FontHelper;

import java.awt.Graphics;
import java.awt.*;
import java.text.*;

/**
 * @author Chip Gobs
 *
 *  This class implements CanvasPainter and encapsulates the
 *  drawing of the X and Y axes and all the tick marks
 */
public class DataPointCanvasBackgroundPainter implements CanvasPainter
{
    private String _className = "DataPointCanvasBackgroundPainter";
  	
    private DataPointCanvas _canvas = null;
    
	
	//number formatter
	private NumberFormat _numberFormat = NumberFormat.getInstance();

	// various colors	
	private static final Color _backgroundColor = Color.black;
	
	
	// NOTE: future changes:
	// will want to have multiple time series, so there needs
	// to be a way to have different colors for timeseries
	
    private static final Color _gridColor = Color.GRAY;	
    private Color _outlineColor = Color.WHITE;
    private static final Color _labelColor = Color.WHITE;
    private static final Color _numberColor = Color.WHITE;

   
    //unit labels 
    private String _xUnitLabelString = null;
    private String _yUnitLabelString = null;


//	fonts
	private Font _titleFont = null;
	private Font _labelFont = null;
	private Font _numberFont = null;

	// margins and shifts
    private final static int _titleYShift = - 20;
	private int _leftUnitLabelShift = 10;
	private final static int _constantYAxisNumberLabelSpacing = 0;
    
    private int _lowerNumberLabelShift = 15;   
    
    //FontHelper - custom class
    private FontHelper _labelFontHelper = null;
    private FontHelper _titleFontHelper = null;
	
	private double _smallRoundingError = 0.000000001;
    
    
    private boolean _showMinorVerticalTicks = true;
    private boolean _showMinorVerticalLabels = true;
    private boolean _showMinorHorizontalTicks = true;
     
    private ScalingHelper _verticalScalingHelper = null;
    private ScalingHelper _horizontalScalingHelper = null;
    // ---------------------------------------------------------
    public DataPointCanvasBackgroundPainter(Color lineColor, 
    		 				   DataPointCanvas canvas,
                               String xUnitLabelString,
                               String yUnitLabelString,
                               ScalingHelper horizontalScalingHelper,
                               ScalingHelper verticalScalingHelper)
    {   	
    	_outlineColor = lineColor; 
    	_canvas = canvas;
    	
    	
        _xUnitLabelString = xUnitLabelString;
        _yUnitLabelString = yUnitLabelString;
        
        _numberFormat.setMaximumFractionDigits(2);
	
		_titleFont = new Font("TimesRoman",Font.BOLD,20); 
		_labelFont= new Font("TimesRoman",Font.BOLD,14); 
		_numberFont = new Font("Helvetica",Font.PLAIN,10);
		
		_labelFontHelper = new FontHelper(_labelFont);
		_titleFontHelper = new FontHelper(_titleFont);
        
		_showMinorVerticalTicks = false;
		_showMinorHorizontalTicks = false;
    
        _horizontalScalingHelper = horizontalScalingHelper;
        _verticalScalingHelper = verticalScalingHelper;
		
    } //tsBackgroundPainter()
 
//	---------------------------------------------------------
    public void setShowMinorVerticalTicks(boolean showMinorVerticalTicks)
    {
        _showMinorVerticalTicks = showMinorVerticalTicks;    
    }
//  ---------------------------------------------------------
    public void setShowMinorHorizontalTicks(boolean showMinorHorizontalTicks)
    {
        _showMinorHorizontalTicks = showMinorHorizontalTicks;    
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


// ---------------------------------------------------------

//	*************************************************************
//	 drawDataLine()
//	  scaling data & display data
//	*************************************************************
	public void drawBackground(Graphics g)
	{
	
		Rectangle r = _canvas.getBounds(); 
	
		//clear the image
		g.setColor(_backgroundColor); 
		g.fillRect(0, 0, r.width, r.height); 
	   
		//draw the axis labels, ticks, and dotted lines	
		drawHorizontalLines( g ); 
		drawVerticalLines( g );
	
		// draw the title of the graph
		
		drawTitle(g);
        
        int x = 0;
        int y = 0;
        
        //draw x axis label
        int labelWidth = _labelFontHelper.getWidth(g, _xUnitLabelString);
        x = (int) ((r.width * .5) - (.5 * labelWidth));
        
        int labelShift = (int) (-.4 * _canvas.getBottomMargin());
  
        y = (r.height) + labelShift;
        drawLabel(_xUnitLabelString, g, x, y);
        
        
        //draw y axis label
        x = _leftUnitLabelShift;
        y = (r.height * 4/9);
        int verticalSpacing = _labelFontHelper.getHeight(g);
        drawVerticalLabel(_yUnitLabelString, g, x, y, verticalSpacing);
        

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

//	--------------------------------------------------------------------------	
    private void drawLabel(String labelString, Graphics g, int x, int y)
    {
          g.setFont(_labelFont);
          g.setColor(_labelColor);
          g.drawString(labelString, x, y);
     
          return;
    } //drawUnitLabel
//	--------------------------------------------------------------------------	
    private void drawVerticalLabel(String labelString, Graphics g, int x, int y, int verticalSpacing)
    {
          Graphics2D g2 = (Graphics2D) g;
        
          g2.setFont(_labelFont);
          g2.setColor(_labelColor);
          
          StringBuffer buffer = new StringBuffer();
          for (int i = 0; i < labelString.length(); i++)
          {
               char c = labelString.charAt(i);
               String newString = Character.toString(c);
               g2.drawString(newString, x, y);
               y += verticalSpacing;
          }
          
      
          return;
    } //drawVerticalUnitLabel   
// --------------------------------------------------------------------------------------------------
    private void drawXAxisNumberLabel(Graphics g, int x, int y, double value)
    {
        String header = _className + ".drawXAxisNumberLabel(): ";
        String numberLabelString =_numberFormat.format(value);
        
        g.setColor(_numberColor);
        g.setFont(_numberFont);
        
        int totalLeftShift = _labelFontHelper.getWidth(g, numberLabelString) / 2;
        g.drawString(numberLabelString, x - totalLeftShift, y + _lowerNumberLabelShift);
        
        
    }
//  --------------------------------------------------------------------------------------------------
    private void drawYAxisNumberLabel(Graphics g, int x, int y, double value)
    {
        String header = _className + "drawYAxisNumberLabel(): ";
        String numberLabelString =_numberFormat.format(value);
        
        g.setColor(_numberColor);
        g.setFont(_numberFont);
        
        int totalYShift =  _labelFontHelper.getHeight(g) / 4;
        int stringWidth =  _labelFontHelper.getWidth(g, numberLabelString);
        int totalLeftShift = -(stringWidth + _constantYAxisNumberLabelSpacing);
     //   System.out.println(header + "_labelFontHelper says that stringWidth of  = " +
     //           numberLabelString + " = " + stringWidth);        
       
        int newX = x + totalLeftShift;
        if (newX < 0)
        {
            newX  = 0;
        }
        
        int newY = y + totalYShift;
        
        g.drawString(numberLabelString, newX, newY);
        
        
    }
//	--------------------------------------------------------------------------	

    private void drawVerticalLines(Graphics g)
    {
         String header = _className + ".drawVerticalLines(): ";
         //determine the min and max data values to chart
    
         DataPoint minDataPoint = getViewport().getMinDataPoint();
         DataPoint maxDataPoint = getViewport().getMaxDataPoint();
          
         double minYValue = minDataPoint.getY();
         double maxYValue = maxDataPoint.getY();
   
         double minXValue = minDataPoint.getX();
         double maxXValue = maxDataPoint.getX();
     
        
         //determine scaling
         
         // this is correct.  To draw a series of vertical lines, I need to know 
         // how far left and right things are, so I need to scale horizontally.
         ScalingHelper scalingHelper = _horizontalScalingHelper;
        
         scalingHelper.setMinDataValue(minXValue);
         scalingHelper.setMaxDataValue(maxXValue);
  
         int minorTickCountMax = scalingHelper.getMinorTickCount();
         int majorTickCountMax = scalingHelper.getMajorTickCount();
     
         double majorTickIncrement = scalingHelper.getMajorTickIncrement();
         double majorTickValue = minXValue;
         
         double minorTickIncrement = scalingHelper.getMinorTickIncrement();
    
         Point point1 = null;
         Point point2 = null;
   
    
         g.setFont(_numberFont);
    
         int iterationCount = 0;
        
         double maxTickValueBound = (maxXValue + _smallRoundingError);
         
          
         for (int majorTickCount = 0;   majorTickCount < majorTickCountMax;  majorTickCount++)
         {
              
             majorTickValue = (majorTickCount * (majorTickIncrement)) + minXValue;
             
             //get the points that are pointed to.
             point1 = getViewport().getScreenPoint(new DataPoint(majorTickValue, minYValue));    
             point2 = getViewport().getScreenPoint(new DataPoint(majorTickValue, maxYValue));

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
                 g.drawLine(point1.x, point1.y, point2.x, point2.y);      
             }
    
    
             //draw the lower tick extension and label
         
             // draw the tick extension
             g.setColor(_outlineColor);
             g.drawLine(point1.x, point1.y+5, point1.x, point2.y); 
    
             // draw the label
             drawXAxisNumberLabel(g, point1.x, point1.y, majorTickValue);
             

            //draw all of the minor ticks
            // minor ticks are always inside and do not get labels
            
            if (_showMinorVerticalTicks)
            {
                if (majorTickCount < majorTickCountMax - 1)
                {
                    for (int minorTickCount = 1; minorTickCount <= minorTickCountMax; minorTickCount++)
                    {
                        double minorTickValue = majorTickValue + (minorTickCount * minorTickIncrement);
                        
                        Point minorPoint1 = getViewport().getScreenPoint(new DataPoint(minorTickValue, minYValue));    
                        Point minorPoint2  = getViewport().getScreenPoint(new DataPoint(minorTickValue, maxYValue));
                                 
                      
                        //draw the inside the boundary line
                        g.setColor(_gridColor);
                        CanvasHelper.drawDashedLine(g,
                                                   minorPoint1.x, minorPoint1.y,
                                                   minorPoint2.x, minorPoint2.y);
                                                   
                        // draw the label
                        if (_showMinorVerticalLabels)
                        {
                             drawXAxisNumberLabel(g, minorPoint1.x, minorPoint1.y, minorTickValue);
                        }
                                      
                    }
                }
            
            } //end if _showMinorTicks
         
         } //end for

     } //end drawYAxis
//		  --------------------------------------------------------------------------

    /*
    private boolean isOnATick(long timeValue, int hoursPerTick)
	  {
		  boolean result = false;
	
		  if ((timeValue % hoursPerTick) == 0 )
		  {
			  result = true;
		  }
		  return result;
	  } //end isOnATick
	*/
	  
// -------------------------------------------------------------------------------------------    
      
    private void drawHorizontalLines(Graphics g)
    {
         //determine the min and max data values to chart
    
         String header = "DataBackgroundPainter.drawHorizontalLines(): ";

         DataPoint minDataPoint = getViewport().getMinDataPoint();
         DataPoint maxDataPoint = getViewport().getMaxDataPoint();
        
         double minYValue = minDataPoint.getY();
         double maxYValue = maxDataPoint.getY();
   
         double minXValue = minDataPoint.getX();
         double maxXValue = maxDataPoint.getX();
     
        
         //determine scaling
        
      //   ScalingHelper scalingHelper = new ScalingHelper(minYValue, maxYValue);
  
         // this is correct.  To draw a series of horizontal lines, I need to know 
         // how far up and down things are, so I need to scale vertically
         ScalingHelper scalingHelper = _verticalScalingHelper;
         scalingHelper.setMinDataValue(minYValue);
         scalingHelper.setMaxDataValue(maxYValue);
     
         //scalingHelper = new ScalingHelper(minYValue, maxYValue);
        
         int minorTickCountMax = scalingHelper.getMinorTickCount();
         int majorTickCountMax = scalingHelper.getMajorTickCount();
     
         double majorTickIncrement = scalingHelper.getMajorTickIncrement();
         double majorTickValue = minYValue;
         
         double minorTickIncrement = scalingHelper.getMinorTickIncrement();
    
         Point point1 = null;
         Point point2 = null;
    
    
         g.setFont(_numberFont);
    
         int iterationCount = 0;
        
         double maxTickValueBound = (maxYValue + _smallRoundingError);
           
         for (int majorTickCount = 0;  majorTickCount < majorTickCountMax;  majorTickCount++)
         {
              
             majorTickValue = (majorTickCount * (majorTickIncrement)) + minYValue;
             
             //get the points that are pointed to.
             point1 = getViewport().getScreenPoint(new DataPoint(minXValue, majorTickValue));    
             point2 = getViewport().getScreenPoint(new DataPoint(maxXValue, majorTickValue));

             // set color as appropriate for the tick lines
             // Note: Checking for tickValue == maxYValue or minYValue may
             // not work due to rounding issues, so add in a fudge factor
        
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
             drawYAxisNumberLabel(g, point1.x, point1.y, majorTickValue);
           
          
            //draw all of the minor ticks
            // minor ticks are always inside and do not get labels
            
            if (_showMinorHorizontalTicks)
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
                        CanvasHelper.drawDashedLine(g, minorPoint1.x, 
                                                       minorPoint1.y,
                                                       minorPoint2.x,
                                                       minorPoint2.y); 
        
                    }
                }
            
            } //end if _showMinorTicks
         
         }

     } //end drawYAxis

  
    public void setShowMinorVerticalLabels(boolean showMinorVerticalLabels)
    {
        _showMinorVerticalLabels = showMinorVerticalLabels;
    }

   
    public boolean showMinorVerticalLabels()
    {
        return _showMinorVerticalLabels;
    }

//  ------------------------------------------------------------------------------


} //end of  DataPointCanvasBackgroundPainter
