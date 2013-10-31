/*
 * Created on Apr 23, 2004
 *
 * 
 */
package ohd.hseb.measurement;

/**
 * @author GobsC
 *
 * Listener for TimeSeriesEvent objects
 */
public interface TimeSeriesListener
{

    void handleTimeSeriesEvent(TimeSeriesEvent event);
    Object getReceiver();
}
