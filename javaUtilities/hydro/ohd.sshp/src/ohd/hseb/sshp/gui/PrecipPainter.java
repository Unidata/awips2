/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.sshp.gui;

import ohd.hseb.measurement.*;
import ohd.hseb.util.*;
import ohd.hseb.util.gui.drawing.*;

import java.awt.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;

/**
 * @author GobsC
 *
 *  This class extends TsCanvasPainter and encapsulates the
 *  drawing of precipitation data on a PaintableCanvas
 */
public class PrecipPainter implements TsCanvasPainter
{

	private static final int MILLIS_PER_HOUR = 1000 * 60 * 60;
    
    private Color _barColor = null;
    private Color _outlineColor = null;
    
    private RegularTimeSeriesHolder _precipTimeSeriesHolder = null;
    private TsPaintableCanvas _canvas = null;
    
    private boolean _shouldDrawBars = true;
    
    private boolean _shouldPaint = true;
    
    private boolean _showPrecipAmounts = false;
    
    private static final Font _labelFont = new Font("Helvetica",Font.PLAIN,9);
      
    private static final double VERY_SMALL_NUMBER = 0.0000000000000001;
    
    private NumberFormat _numberFormat = null;
  
    // ---------------------------------------------------------
    public PrecipPainter(Color barColor, 
    					 Color outlineColor,
    		 			 RegularTimeSeriesHolder timeSeriesHolder,
                         boolean showPrecipAmounts,
    		 			 TsPaintableCanvas canvas)
    {
    	
    	_precipTimeSeriesHolder = timeSeriesHolder;
    	_canvas = canvas;
    	
    	_barColor = barColor;
    	_outlineColor = outlineColor;
        
        setShowPrecipAmounts(showPrecipAmounts);
    	
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
    	
    	//System.out.println("PrecipPainter.paint(): precip ts = " + getTimeSeries());
    	if (_shouldPaint)
    	{    
		    drawDataBars(g);
    	}
    }

// ----------------------------------------------------
    private void drawDataBars( Graphics g ) 
    { 
        String header = "PrecipPainter.drawDataBars(): ";
	   // System.out.println(header + "-------------------------");
        int x1;
	    int y1;
	    int x2;
	    int y2;
	    g.setColor(_barColor); 
	    
        if (_showPrecipAmounts)
        {    
           g.setFont( _labelFont ); 
        }
	  
	    //draw each data bar
	    for ( int i = 0; (getTimeSeries() != null) &&
	                     ( i < getTimeSeries().getMeasurementCount()); i++)
	    { 
	   	
		    Measurement measurement = (Measurement) 
											getTimeSeries().getMeasurementByIndex(i);	
		
		    Measurement convertedMeasurement =  Measurement.getConvertedCopy(measurement,
                                                             getDisplayedMeasuringUnit());
		    
		    double value = convertedMeasurement.getValue();
            
            if (value >= 0.0)
            
            {
	   
    		    long endTime = getTimeSeries().getMeasurementTimeByIndex(i);
    		    long startTime = endTime - MILLIS_PER_HOUR;
                
                /*
                if (!_shouldDrawBars)
                {
                    String timeString = CanvasHelper.getDateTimeStringFromLongTime(endTime);
                    System.out.println("Time = " + timeString  + " value = " + value); 
                }
                */
    		 
    		    DataPoint dp1 = 	 new DataPoint(startTime, 0);
    		    DataPoint dp2 = 	 new DataPoint(endTime, value);
    	   
    
    		    if  ( (getViewport().isViewable(dp1)) &&
    				 (getViewport().isViewable(dp2))
    			   )
    		    {
    	          
    			    Point p1 = getViewport().getScreenPoint(dp1);
    			    Point p2 = getViewport().getScreenPoint(dp2);
    
                    
    			    int barHeight =  Math.abs(p1.y - p2.y);
    			    int barWidth =   Math.abs(p1.x - p2.x);
    	
                    if (_shouldDrawBars)
                    {
    			        g.setColor(_barColor); 
    			        g.fillRect(p1.x, p2.y, barWidth, barHeight); 
    	    
    		 	        g.setColor(_outlineColor);
    			        g.drawRect(p1.x, p2.y, barWidth, barHeight);
                        
                        if ((_showPrecipAmounts) && (value > 0.0) )
                        {
                            g.setColor(_outlineColor);
                            
                            g.drawString(_numberFormat.format(value), p1.x, p2.y -2);
                        }
                    }
                    else //draw Lines
                    {
                        g.setColor(_barColor); 
                        g.drawLine(p1.x, p2.y, p1.x + barWidth, p2.y); 
                        
                    }
    		    }
            
            }//end if value > 0.0

	   } // for loop
	   
	
	} // end of  drawDataBars method
//	----------------------------------------------------
   
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

//---------------------------------------------------------
  public Measurement getMinMeasurement()
  {
      //force this method to return 0.0 always, since 
      //I always want 0 shown, regardless of the actual measurements
      Measurement minMeasurement = new Measurement(0.0, _canvas.getDisplayedMeasuringUnit());
       /*
      
		Measurement minMeasurement = null;
	
        long startTime = _canvas.getStartTime();
        long endTime = _canvas.getEndTime();
    
		if (getTimeSeries() != null)
		{
			 //make copies of the max and min measurements
			 minMeasurement = getTimeSeries().getMinMeasurement(startTime, endTime);
			 if (minMeasurement != null)
			 {
				 minMeasurement = new Measurement(minMeasurement);
			 }
		}
		else
		{
			minMeasurement = new Measurement(0.0, _canvas.getDisplayedMeasuringUnit());
		}
       
	   */
		return minMeasurement;				 
	}
//----------------------------------------------------
	
		public Measurement getMaxMeasurement()
		{

            Measurement minMeasurement = new Measurement(0.0, _canvas.getDisplayedMeasuringUnit());
           
            String header = "PrecipPainter.getMaxMeasurement()";
    
			Measurement maxMeasurement = null;
            long startTime = _canvas.getStartTime();
            long endTime = _canvas.getEndTime();
		
			if (getTimeSeries() != null)
			{
				 //make copies of the max and min measurements
				 maxMeasurement = getTimeSeries().getMaxMeasurement(startTime, endTime);
				 if (maxMeasurement != null)
				 {
					 maxMeasurement = new Measurement(maxMeasurement);
                     
                     if ( maxMeasurement.getValue(_canvas.getDisplayedMeasuringUnit()) < 1.0)
                     {
                         maxMeasurement = new Measurement(1.0, _canvas.getDisplayedMeasuringUnit());   
                     }
				 }
                 else
                 {
                     maxMeasurement = new Measurement(1.0, _canvas.getDisplayedMeasuringUnit());   
                 }	       	
			}
			else
			{
                 //System.out.println(header + " time series was null, setting max measurement to 1.0 ");
				 maxMeasurement = new Measurement(1.0, _canvas.getDisplayedMeasuringUnit());
			}
            
           
		
            //System.out.println(header + " max measurement is " + maxMeasurement);

			return maxMeasurement;				 
		}

//  ----------------------------------------------------

    public void setShouldDrawBars(boolean shouldDrawBars)
    {
        _shouldDrawBars = shouldDrawBars;
    }

//  ----------------------------------------------------


    public boolean shouldDrawBars()
    {
        return _shouldDrawBars;
    }

    public void setShowPrecipAmounts(boolean showPrecipAmounts)
    {
        _showPrecipAmounts = showPrecipAmounts;
    }

    public boolean isShowPrecipAmounts()
    {
        return _showPrecipAmounts;
    }
	 			
//----------------------------------------------------


} //end of PrecipPainter
