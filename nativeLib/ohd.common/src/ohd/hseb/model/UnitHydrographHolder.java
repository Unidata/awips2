/*
 * Created on Jul 13, 2004
 *
 * 
 */
package ohd.hseb.model;

import ohd.hseb.model.UnitHydrograph;

/**
 * @author GobsC
 *
 * 
 */
public class UnitHydrographHolder
{
    private UnitHydrograph _unitHydrograph = null;
   
    // ---------------------------------------------------------------------------------
    
    public UnitHydrographHolder(UnitHydrograph unitHydrograph)
    {
        _unitHydrograph = unitHydrograph;    
    }

    // ---------------------------------------------------------------------------------

	public void setUnitHydrograph(UnitHydrograph unitHydrograph)
	{
		_unitHydrograph = unitHydrograph;
	}

    // ---------------------------------------------------------------------------------


	public UnitHydrograph getUnitHydrograph()
	{
		return _unitHydrograph;
	}
    
    // ---------------------------------------------------------------------------------
    
    
    
}
