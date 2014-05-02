package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

/**
 * Class to encapsulate the concept of Azimuth and Range in MPE. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * August 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 * 
 */
public class AzimuthAndRangePoint
{
    

    private double _azimuth;
    private double _range;
    
    public AzimuthAndRangePoint()
    {
    }
    
    public double getAzimuth()
    {
        return _azimuth;
    }
    public void setAzimuth(double azimuth)
    {
        _azimuth = azimuth;
    }

    public double getRange()
    {
        return _range;
    }

    public void setRange(double range)
    {
        _range = range;
    }

}
