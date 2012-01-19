/*
 * Created on Aug 27, 2004
 *
 */
package ohd.hseb.model;

/**
 * @author Chip Gobs
 *
 *	This class is holds the blending parameters for ForecastAdjuster
 */

public class ForecastAdjusterParams  
{
    
	private boolean _shouldDoAdjustment = false;
	private int _pairingTimeMinutes = 0;
	private int _interpolationHours = 0;
	private int _blendingHours = 0;
	private int _maxBlendingHours = 500;
	private ForecastInterpolationMethod _blendingMethod = ForecastInterpolationMethod.DIFFERENCE;
	
	
	// -------------------------------------------------------------------------------
    /**
     * @param useBlending The useBlending to set.
     */
    public void setShouldDoAdjustment(boolean shouldDoAdjustment)
    {
       // String header = "ForecastAdjusterParams.setShouldDoAdjustment():";
       // System.out.println(header + "shouldDoAdjustment now = " + shouldDoAdjustment);
        this._shouldDoAdjustment = shouldDoAdjustment;
    }
//  -------------------------------------------------------------------------------
    
    /**
     * @return Returns whether blending is on or off.
     */
    public boolean shouldDoAdjustment()
    {
        return _shouldDoAdjustment;
    }
//  -------------------------------------------------------------------------------
    
    /**
     * @param blendingHours The blendingHours to set.
     */
    public void setBlendingHours(int blendingHours)
    {
        _blendingHours = blendingHours;
    }
    
//  -------------------------------------------------------------------------------
    
    /**
     * @return Returns the blendingHours.
     */
    public int getBlendingHours()
    {
        return _blendingHours;
    }
    
//  -------------------------------------------------------------------------------
    
    /**
     * @param blendMethod The blendMethod to set.
     */
    public void setBlendingMethod(ForecastInterpolationMethod blendMethod)
    {
        _blendingMethod = blendMethod;
    }
//  -------------------------------------------------------------------------------
    
    /**
     * @return Returns the blendMethod.
     */
    public ForecastInterpolationMethod getBlendingMethod()
    {
        return _blendingMethod;
    }
//  -------------------------------------------------------------------------------
    /**
     * @param interpolationHours The interpolationHours to set.
     */
    public void setInterpolationHours(int interpolationHours)
    {
        _interpolationHours = interpolationHours;
    }
    /**
     * @return Returns the interpolationHours.
     */
    public int getInterpolationHours()
    {
        return _interpolationHours;
    }
    /**
     * @param pairingTimeMinutes The pairingTimeMinutes to set.
     */
    public void setPairingTimeMinutes(int pairingTimeMinutes)
    {
        _pairingTimeMinutes = pairingTimeMinutes;
    }
    /**
     * @return Returns the pairingTimeMinutes.
     */
    public int getPairingTimeMinutes()
    {
        return _pairingTimeMinutes;
    }
  
    
}
