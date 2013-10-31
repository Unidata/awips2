/*
 * Created on Oct 22, 2003
 *
 * 
 */
package ohd.hseb.util;

import ohd.hseb.measurement.*;

/**
 * @author GobsC
 *
 * This interface is meant to be used by the TsBackgroundPainter in
 * order to determine how to label the right label in the
 * TsPaintableCanvas.  For example, an implementor of this interface
 * could provide the information needed to map between inches to mm
 * or from stage to discharge.
 * 
 */
public interface ValueMapper
{
    public Measurement getResultMeasurement(Measurement measurement);
    public MeasuringUnit getInputMeasuringUnit();
    public MeasuringUnit getOutputMeasuringUnit();
}
