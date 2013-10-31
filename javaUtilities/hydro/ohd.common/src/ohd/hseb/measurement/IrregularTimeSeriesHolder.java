/*
 * Created on Oct 14, 2003
 *
 * 
 * */
package ohd.hseb.measurement;

/**
 * @author Chip Gobs
 *
 * This is a simple Holder class for an IrregularTimeSeries object
 */
public class IrregularTimeSeriesHolder 
{
    private IrregularTimeSeries _timeSeries;

    public void setTimeSeries(IrregularTimeSeries timeSeries)
    {
        _timeSeries = timeSeries;
    }

    public IrregularTimeSeries getTimeSeries()
    {
        return _timeSeries;
    }
}
