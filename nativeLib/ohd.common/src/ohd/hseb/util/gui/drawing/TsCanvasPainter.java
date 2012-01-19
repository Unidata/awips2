/*
 * Created on Oct 10, 2003
 *
 * 
 */
package ohd.hseb.util.gui.drawing;

//import ohd.hseb.util.gui.*;
import ohd.hseb.measurement.*;

/**
 * @author GobsC
 *
 *  This interface is an extension of CanvasPainter
 *  and specializes the CanvasPainter in that it deals with
 *  the maximum and minimum measurements, which 
 *  usually come from TimeSeries
 *  
 **/
public interface TsCanvasPainter extends CanvasPainter
{
    	
	public Measurement getMaxMeasurement();
	public Measurement getMinMeasurement();
    public void setShouldPaint(boolean shouldPaint);


} //end of TsCanvasPainter
