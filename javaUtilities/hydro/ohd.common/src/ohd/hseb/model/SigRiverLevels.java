/*
 * Created on Oct 3, 2003
 *
 * 
 */
package ohd.hseb.model;

import ohd.hseb.db.DbTable;

/**
 * @author Chip Gobs
 *
 * 
 */
public class SigRiverLevels
{
	private String _locationId = null;
	
	private int MISSING_VALUE = -999;
	
	private boolean _useStage = true;
	private String _primaryPe = null;
	
    //stages
	private double _floodStage = MISSING_VALUE;
	private double _actionStage = MISSING_VALUE;
    private double _minorFloodStage = MISSING_VALUE;
    private double _moderateFloodStage = MISSING_VALUE;
    private double _majorFloodStage = MISSING_VALUE;

	//flows
	private double _floodFlow = MISSING_VALUE;
	private double _actionFlow = MISSING_VALUE;
    private double _minorFloodFlow = MISSING_VALUE;
    private double _moderateFloodFlow = MISSING_VALUE;
    private double _majorFloodFlow = MISSING_VALUE;

	//-------------------------------------------------------
	
	public SigRiverLevels()
	{
		
	}

	//--------------------------------------------------------------------------------

	public SigRiverLevels(String locationId, String primaryPe,
	                      double floodStage, double actionStage,
	                      double floodFlow, double actionFlow)
	{
		_locationId = locationId;
	
		_primaryPe = primaryPe;
		if (_primaryPe.substring(0,1).equalsIgnoreCase("Q"))
		{
		    _useStage = false;
		}
		  
		_floodStage = checkMissing(floodStage);
        _actionStage = checkMissing(actionStage);

        _floodFlow = checkMissing(floodFlow);
        _actionFlow = checkMissing(actionFlow);
        
 		return;
	}
	
	//	------------------------------------------------------------------------------
	private double checkMissing(double value)
	{
	    double resultingValue = value;
	    
	    if (value == 0.0)
	    {
	        resultingValue = MISSING_VALUE;
	    }
	    
	    return resultingValue;
	}

	//--------------------------------------------------------------------------------
	

	public void setLocationId(String locationId)
	{
        _locationId = locationId;
	}

	//--------------------------------------------------------------------------------


	public String getLocationId()
	{
		   return _locationId;
	}

	//--------------------------------------------------------------------------------

	
    public void setFloodStage(double floodStage)
    {
        _floodStage = floodStage;
    }

	//--------------------------------------------------------------------------------


    public double getFloodStage()
    {
        return _floodStage;
    }
    
//  --------------------------------------------------------------------------------

    private boolean hasValue(double value)
    {
		boolean result = true;
    	
		if ((value == MISSING_VALUE) || (DbTable.isNull(value)))
		{
			result = false;
		}
    	
		return result;
    }
    
//  --------------------------------------------------------------------------------

    
    public boolean hasFloodStage()
    {
    	return hasValue(_floodStage);
    }

//  --------------------------------------------------------------------------------

	public boolean hasActionStage()
	{
		return hasValue(_actionStage);
	}

//	--------------------------------------------------------------------------------
    public boolean hasMinorFloodStage()
    {
        return hasValue(_minorFloodStage);
    }
//    --------------------------------------------------------------------------------
	public boolean hasModerateFloodStage()
	{
	    return hasValue(_moderateFloodStage);
	}
//    --------------------------------------------------------------------------------
	public boolean hasMajorFloodStage()
	{
	    return hasValue(_majorFloodStage);
	}

//      --------------------------------------------------------------------------------
  
    public boolean hasFloodFlow()
    {
    	return hasValue(_floodFlow);
    }

//  --------------------------------------------------------------------------------

	public boolean hasActionFlow()
	{
		return hasValue(_actionFlow);
	}

//	--------------------------------------------------------------------------------
	public boolean hasMinorFloodFlow()
	{
	    return hasValue(_minorFloodFlow);
	}
//	--------------------------------------------------------------------------------
	public boolean hasModerateFloodFlow()
	{
	    return hasValue(_moderateFloodFlow);
	}
//	--------------------------------------------------------------------------------
	public boolean hasMajorFloodFlow()
	{
	    return hasValue(_majorFloodFlow);
	}

//  --------------------------------------------------------------------------------

    public void setActionStage(double actionStage)
    {
        _actionStage = actionStage;
    }

//  --------------------------------------------------------------------------------

    public double getActionStage()
    {
        return _actionStage;
    }

//  --------------------------------------------------------------------------------

	public String toString()
	{
		StringBuffer  buffer = new StringBuffer();
		
		buffer.append(_locationId + "with primary pe = "  + _primaryPe);
		
		if (hasFloodStage())
		{
			buffer.append("Flood Stage = " + _floodStage + " ");	
		}
		else
		{
			buffer.append("No flood stage set.");	
		}
		
		
		if (hasActionStage())
		{
			buffer.append("Action Stage = " + _actionStage + "\n");
			
		}
		else
		{
			buffer.append("No action stage set.");	
		}
		
		
		
		if (hasFloodFlow())
		{
			buffer.append("Flood Flow = " + _floodFlow + " ");	
		}
		else
		{
			buffer.append("No flood flow set.");	
		}
		
		
		
		if (hasActionFlow())
		{
			buffer.append("Action Flow = " + _actionFlow + "\n");
			
		}
		else
		{
			buffer.append("No action flow set.");	
		}
		
		
		String outString = buffer.toString();
		
		return outString;
		
	}
//	--------------------------------------------------------------------------------


    public void setPrimaryPe(String primaryPe)
    {
        _primaryPe = primaryPe;
    }

//  --------------------------------------------------------------------------------

    public String getPrimaryPe()
    {
        return _primaryPe;
    }
//  --------------------------------------------------------------------------------

    public void setFloodFlow(double floodFlow)
    {
        _floodFlow = floodFlow;
    }
//  --------------------------------------------------------------------------------

    public double getFloodFlow()
    {
        return _floodFlow;
    }
//  --------------------------------------------------------------------------------

   
    public void setActionFlow(double actionFlow)
    {
        _actionFlow = actionFlow;
    }
    
//  --------------------------------------------------------------------------------

    public double getActionFlow()
    {
        return _actionFlow;
    }
    
//  --------------------------------------------------------------------------------

    public void setUseStage(boolean useStage)
    {
        _useStage = useStage;
    }
    
//  --------------------------------------------------------------------------------

  
    public boolean useStage()
    {
        return _useStage;
    }

    public void setMinorFloodStage(double minorFloodStage)
    {
        _minorFloodStage = minorFloodStage;
    }

    public double getMinorFloodStage()
    {
        return _minorFloodStage;
    }

    /**
     * @param moderateFloodStage The moderateFloodStage to set.
     */
    public void setModerateFloodStage(double moderateFloodStage)
    {
        _moderateFloodStage = moderateFloodStage;
    }

    /**
     * @return Returns the moderateFloodStage.
     */
    public double getModerateFloodStage()
    {
        return _moderateFloodStage;
    }

    /**
     * @param majorFloodStage The majorFloodStage to set.
     */
    public void setMajorFloodStage(double majorFloodStage)
    {
        _majorFloodStage = majorFloodStage;
    }

    /**
     * @return Returns the majorFloodStage.
     */
    public double getMajorFloodStage()
    {
        return _majorFloodStage;
    }

    public void setMinorFloodFlow(double minorFloodFlow)
    {
        _minorFloodFlow = minorFloodFlow;
    }

    public double getMinorFloodFlow()
    {
        return _minorFloodFlow;
    }

    public void setModerateFloodFlow(double moderateFloodFlow)
    {
        _moderateFloodFlow = moderateFloodFlow;
    }

    public double getModerateFloodFlow()
    {
        return _moderateFloodFlow;
    }

    public void setMajorFloodFlow(double majorFloodFlow)
    {
        _majorFloodFlow = majorFloodFlow;
    }

    public double getMajorFloodFlow()
    {
        return _majorFloodFlow;
    }

//  --------------------------------------------------------------------------------
  
}
