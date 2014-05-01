/*
 * Created on Oct 7, 2003
 *
 * This class is for events related to clicking on 
 * a TimeSeriesCanvas
 */
 
package ohd.hseb.util.gui.drawing;


import ohd.hseb.measurement.*;

/**
 * @author Chip Gobs
 *
 * 
 */
public class TsCanvasEvent
{

	private AbsTimeMeasurement _measurement = null;
	
// -----------------------------------------------------------------------
	public TsCanvasEvent(TsCanvasEvent event)
	{
		this(event._measurement);

		return;
	}
//	-----------------------------------------------------------------------
	
	public TsCanvasEvent(AbsTimeMeasurement measurement)
	{
		_measurement = new AbsTimeMeasurement(measurement);
		
		return;
	}

//-----------------------------------------------------------------------

    public AbsTimeMeasurement getMeasurement()
    {
        return _measurement;
    }
//	-----------------------------------------------------------------------
	
	

}
