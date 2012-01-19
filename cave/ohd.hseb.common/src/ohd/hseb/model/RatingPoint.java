/*
 * Created on Jul 7, 2003
 *
 * 
 */
package ohd.hseb.model;

/**
 * @author GobsC
 *
 * 
 */
public class RatingPoint
{
    private double _shiftAmount;
	private double _unshiftedStage;
	private double _discharge;
    
// ------------------------------------------------------------------------------------------
	public void setUnshiftedStage(double stage)
	{
		this._unshiftedStage = stage;
	}
//  ------------------------------------------------------------------------------------------

	public double getUnshiftedStage()
	{
		return _unshiftedStage;
	}
//  ------------------------------------------------------------------------------------------

    public void setShiftAmount(double shiftAmount)
    {
        _shiftAmount = shiftAmount;
    }
//  ------------------------------------------------------------------------------------------

    public double getShiftedStage()
    {
        return _unshiftedStage + _shiftAmount;
    }
//  ------------------------------------------------------------------------------------------
	public void setDischarge(double discharge)
	{
		this._discharge = discharge;
	}
//  ------------------------------------------------------------------------------------------

	public double getDischarge()
	{
		return _discharge;
	}
//  ------------------------------------------------------------------------------------------
    public boolean equals(RatingPoint point2)
    {
         boolean result = false;
         
         if ( 
              (this._unshiftedStage == point2._unshiftedStage) &&
              (this._shiftAmount == point2._shiftAmount) &&
              (this._discharge == point2._discharge )
            )
         {
             result = true;          
         }
         
         return result;
        
    }
//  ------------------------------------------------------------------------------------------
	
	public String toString()
	{
	    String outString = " stage = " + getUnshiftedStage() + 
                           " discharge = " + getDischarge();
	    	
	    return outString;
	}
//  ------------------------------------------------------------------------------------------

}
