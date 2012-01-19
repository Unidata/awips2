/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.sshp.gui;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.*;
import ohd.hseb.util.*;
import ohd.hseb.util.gui.FontHelper;
import ohd.hseb.util.gui.drawing.*;

import java.awt.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Map;

/**
 * @author GobsC
 *
 *  This class extends CanvasPainter and encapsulates the
 *  drawing of precipitation total textual data on a PaintableCanvas
 */
public class PrecipTotalTextPainter implements CanvasPainter
{

	private static final int MILLIS_PER_HOUR = 1000 * 60 * 60;
    private static final Font _labelFont = new Font("Helvetica",Font.PLAIN,12);
    private static final int Y_OFFSET = 2;
    private static final double VERY_SMALL_NUMBER = 0.0000000000000001;
    
    private RegularTimeSeriesHolder _precipTimeSeriesHolder = null;
    private TsPaintableCanvas _canvas = null;
        
    private boolean _shouldPaint = true;
    private boolean _showPrecipAmounts = false;
    
    private NumberFormat _numberFormat = null;
    private Color _textColor = null;
    
    private RegularTimeSeries _sixHourTimeSeries = null;
    private RegularTimeSeries _twelveHourTimeSeries = null;
    private RegularTimeSeries _twentyFourHourTimeSeries = null;
    
    private Map _timeToPositionedTextMap = new HashMap();
    // ---------------------------------------------------------
    public PrecipTotalTextPainter(Color textColor,
    		 			 RegularTimeSeriesHolder timeSeriesHolder,
                         boolean shouldPaint,
    		 			 TsPaintableCanvas canvas)
    {
    	
    	_precipTimeSeriesHolder = timeSeriesHolder;
    	_canvas = canvas;
    	       
        _textColor = textColor;
        setShouldPaint(shouldPaint);
    	
        _numberFormat = new DecimalFormat("#.0#");
    }
    
//  ---------------------------------------------------------
    public void setShouldPaint(boolean shouldPaint)
    {
        _shouldPaint = shouldPaint;    
    }
    
//	---------------------------------------------------------
    public void paint(Graphics g)
    {    	
    	//System.out.println("PrecipTotalTextPainter.paint(): precip ts = " + getTimeSeries());
    	
        if (_shouldPaint)
    	{    
            _timeToPositionedTextMap.clear();
            
            
     	     positionText(g, 6, 0, true);
            // positionText(g, 12, 0, false);
             
             positionText(g, 24, 0, false);
             positionText(g, 24, 6, false);
             positionText(g, 24, 12, false);
             positionText(g, 24, 18, false);
            
            drawText(g);
    	}
       
    }
//  ----------------------------------------------------
    public void drawText(Graphics g)
    {
        FontHelper fontHelper = new FontHelper(_labelFont);
        
        RegularTimeSeries timeSeries = _precipTimeSeriesHolder.getTimeSeries();
        long startTime = timeSeries.getStartTime();
        long endTime = timeSeries.getEndTime();
        
        for ( long t = startTime; (timeSeries != null) &&
            ( t <= endTime ); t+= MILLIS_PER_HOUR)
        { 
            PositionedText positionedText =
                            getPositionedText(t);
            
            if (positionedText != null)
            {
                String text = positionedText.getStringBuffer().toString();
                int x = positionedText.getPosition().x;
                int y = positionedText.getPosition().y;
                
                
                int width = fontHelper.getWidth(g, text);
                
                x -= width/2;
                
                g.drawString(text, x, y );
                
            }
        }    
    }
    
//  ----------------------------------------------------
    
     private void positionText( Graphics g,
                            int intervalInHours,
                            int shiftInHours,
                            boolean isFirst) 
    { 
        String header = "PrecipTotalTextPainter.positionText(): ";
	
 	    int y = _canvas.getHeight() - Y_OFFSET;
	
        g.setFont( _labelFont ); 
        
        RegularTimeSeries precipTimeSeries = _precipTimeSeriesHolder.getTimeSeries();
        
        RegularTimeSeries timeSeries =
               TimeSeriesTotaller.getRegularTimeSeries(intervalInHours, 
                                                       shiftInHours,
                                                       precipTimeSeries);  
        
        
      //  System.out.println(header + "--------------------\n" + timeSeries.toString());
      //  System.out.println(header + "-------end of time series-------------n" );
        
        StringBuffer stringBuffer = new StringBuffer();
	    //draw each data bar
	    for ( int i = 0; (timeSeries != null) &&
	                     ( i < timeSeries.getMeasurementCount()); i++)
	    { 
	   	
		    AbsTimeMeasurement measurement = (AbsTimeMeasurement) 
											timeSeries.getAbsTimeMeasurementByIndex(i);	
		
            AbsTimeMeasurement convertedMeasurement =  AbsTimeMeasurement.getConvertedCopy(measurement,
                                                             getDisplayedMeasuringUnit());
		    
		    double value = convertedMeasurement.getValue();
		    
		    long endTime = convertedMeasurement.getTime();
            
          //  System.out.println(header + "value = " + value + " endTime = " + getTimeString(endTime));
		    	    
		    DataPoint dp1 = new DataPoint(endTime, 0);	    
		    Point p1 = getViewport().getScreenPoint(dp1);
		    
		    g.setColor(_textColor);    
            
            PositionedText positionedText = getPositionedText(endTime, true);
            if (isFirst)
            {
                positionedText.clear();
            }
            else
            {
                positionedText.getStringBuffer().append(" / ");    
            }
            positionedText.getStringBuffer().append(_numberFormat.format(value));
            positionedText.setPosition(new Point(p1.x, y));
            	        
	   } // for loop
	   
	
	} // end of  drawDataBars method

     // --------------------------------------------------------------------------------
     
     private static String getTimeString(long time)
     {
             return DbTimeHelper.getDateTimeStringFromLongTime(time);
     }
 
     // --------------------------------------------------------------------------------
 
    private PositionedText getPositionedText(long time, boolean createNewOne)
    {
    //    String header = "PrecipTotalTextPainter.getPositionedText(): ";
        
        PositionedText text = null;
        text = (PositionedText) _timeToPositionedTextMap.get(new Long(time));
        if ((text == null) && (createNewOne))
        {
            text = new PositionedText();
            _timeToPositionedTextMap.put(new Long(time), text);
     //       System.out.println(header +  getTimeString(time));
        }
        
        return text;
    }
    
//  ----------------------------------------------------
    
    private PositionedText getPositionedText(long time)
    {
        return getPositionedText(time, false);
    }
//  ----------------------------------------------------
  
	private MeasuringUnit getDisplayedMeasuringUnit()
	{
		return _canvas.getDisplayedMeasuringUnit();
	}
//	---------------------------------------------------------
	   
	private Viewport getViewport()
	{
		return _canvas.getViewport();
	}
//	---------------------------------------------------------
	private  RegularTimeSeries getTimeSeries()
	{
		return _precipTimeSeriesHolder.getTimeSeries();	
	}

    public void setShowPrecipAmounts(boolean showPrecipAmounts)
    {
        _showPrecipAmounts = showPrecipAmounts;
    }

    public boolean isShowPrecipAmounts()
    {
        return _showPrecipAmounts;
    }

//---------------------------------------------------------

} //end of PrecipPainter
