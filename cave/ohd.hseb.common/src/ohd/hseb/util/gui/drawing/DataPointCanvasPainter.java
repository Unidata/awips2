/*
 * Created on Jul 15, 2004
 *
 * 
 */
package ohd.hseb.util.gui.drawing;

import ohd.hseb.measurement.Measurement;

/**
 * @author GobsC
 *
 * 
 */
public interface DataPointCanvasPainter extends CanvasPainter
{
    public Measurement getMaxXMeasurement();
    public Measurement getMinXMeasurement();
    
    public Measurement getMaxYMeasurement();
    public Measurement getMinYMeasurement();
    
    public void setShouldPaint(boolean shouldPaint);
}
