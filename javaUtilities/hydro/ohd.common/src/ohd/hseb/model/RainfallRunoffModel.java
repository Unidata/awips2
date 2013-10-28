/*
 * Created on Nov 4, 2003
 *
 * 
 */
package ohd.hseb.model;

import ohd.hseb.measurement.*;

/**
 * @author Chip Gobs
 *
 *  This interface is made to be implemented by various
 *  specific rainfall-runoff models
 *
 **/
public interface RainfallRunoffModel
{
    public RegularTimeSeries calculateRunoff(long startTime,
                                             long endTime,
                                             RegularTimeSeries precipTimeSeries
                                             ); 
   
    public RainfallRunoffModelType getModelType();
}
