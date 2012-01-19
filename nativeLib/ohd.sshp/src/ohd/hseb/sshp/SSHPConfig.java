/*
 * Created on Feb 4, 2004
 *
 * Filename : SshpConfig.java
 * Author   : Gautam Sood
 * Last Revision Date : Feb 4, 2004
 *  
 */
//package ohd.hseb.model;
package ohd.hseb.sshp;

import ohd.hseb.model.ForecastInterpolationMethod;

public class SSHPConfig
{
	private String _lid = null;
	private String _basinId = null;
	private long _postingTime = 0;
	private String _modelPref = null;
	private boolean _autoProcess = false;
	private String _sourcePref = null;
	private boolean _useStaticEvap = false;
	private boolean _useBlend = false;
	private ForecastInterpolationMethod _blendMethod = null;
	private int _blendHours = 0; 
	
	public SSHPConfig()
	{
	}
	
	public SSHPConfig( SSHPConfig sshpConfig )
	{


		_lid = sshpConfig.getLid();
		_basinId = sshpConfig.getBasinId();
		_postingTime = sshpConfig.getPostingTime();
		_modelPref = sshpConfig.getModelPref();
		_autoProcess = sshpConfig.isAutoProcess();
		_sourcePref = sshpConfig.getSourcePref();
		_useStaticEvap = sshpConfig.useStaticEvap();
		
		_blendMethod = sshpConfig.getBlendMethod(); 
	
	}

	public void setLid( String lid )
	{
		_lid = lid;
	}

	public String getLid()
	{
		return _lid;
	}

	public void setBasinId( String basinId )
	{
		_basinId = basinId;
	}

	public String getBasinId()
	{
		return _basinId;
	}

	public void setPostingTime( long postingTime )
	{
		_postingTime = postingTime;
	}

	public long getPostingTime()
	{
		return _postingTime;
	}

	public void setModelPref( String modelPref )
	{
		_modelPref = modelPref;
	}

	public String getModelPref()
	{
		return _modelPref;
	}

	public void setAutoProcess( boolean autoProcess )
	{
		_autoProcess = autoProcess;
	}

	public boolean isAutoProcess()
	{
		return _autoProcess;
	}

	public void setSourcePref( String sourcePref )
	{
		_sourcePref = sourcePref;
	}

	public String getSourcePref()
	{
		return _sourcePref;
	}

	public void setUseStaticEvap( boolean useStaticEvap )
	{
		_useStaticEvap = useStaticEvap;
	}

	public boolean useStaticEvap()
	{
		return _useStaticEvap;
	}

	
	public String toString()
	{
		return "Lid = " + _lid +
		       "Basin id = " + _basinId +
		       "Posting Time = " + _postingTime +
		       "Model Pref = " + _modelPref + 
		       "Auto Process = " + _autoProcess +
		       "Source Pref = " + _sourcePref + 
		       "Use Static Evap = " + _useStaticEvap;
	}

    /**
     * @param useBlend The useBlend to set.
     */
    public void setUseBlend(boolean useBlend)
    {
        _useBlend = useBlend;
    }

    /**
     * @return Returns the useBlend.
     */
    public boolean useBlend()
    {
        return _useBlend;
    }

    /**
     * @param blendMethod The blendMethod to set.
     */
    public void setBlendMethod(ForecastInterpolationMethod blendMethod)
    {
        _blendMethod = blendMethod;
    }

    /**
     * @return Returns the blendMethod.
     */
    public ForecastInterpolationMethod getBlendMethod()
    {
        return _blendMethod;
    }

    /**
     * @param blendHours The blendHours to set.
     */
    public void setBlendHours(int blendHours)
    {
        _blendHours = blendHours;
    }

    /**
     * @return Returns the blendHours.
     */
    public int getBlendHours()
    {
        return _blendHours;
    }
}
