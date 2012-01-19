package ohd.hseb.pdc_pp;

import ohd.hseb.model.ParamCode;
import ohd.hseb.util.TimeHelper;

public class RegularObsTimeSeriesDescriptor
{
    private String _lid;
    private String _shefParamCodeString;
    private ParamCode _paramCode;
    private String _pe;
    private short _dur;
    private String _extremum;
    private String _ts;
    private long _endTime = 0;
    private long _startTime = 0;
    private String _outputString = null;
    private String _identityString = null;
    private int _hashCode = 0;
    private boolean _missing = false;
    
    private boolean _identityChanged = true;  // is used to determine whether to recalculate the tostring
    
    public RegularObsTimeSeriesDescriptor() {}
        
    public RegularObsTimeSeriesDescriptor( RegularObsTimeSeriesDescriptor descriptor )
    {
        _lid = descriptor.getLid();
        _pe = descriptor.getPe();
        _dur = descriptor.getDur();
        _extremum = descriptor.getExtremum();
        _ts = descriptor.getTs();
        _endTime = descriptor.getEndTime();
        _startTime = descriptor.getStartTime();
        
        _missing = descriptor._missing;
        
        
         getIdentity();

       // _identityChanged = descriptor.hasIdentityChanged();
        
       // _outputString = descriptor.getOutputString();
       // _hashCode = descriptor._hashCode;
        
      
    }
    
    
    private void getIdentity()
    {
        if ( _identityChanged )
        {
//            computeToString();
//            computeHashCode();

            _outputString = "lid = " + _lid +
            " pe = " + _pe +
            " dur = " + _dur + 
            " extremum = " + _extremum +
            " ts = " + _ts;

            _hashCode = _outputString.hashCode();
            
            _shefParamCodeString =  getPe() + ParamCode.getShefDurationCode(getDur()) + 
                         getTs() + getExtremum();    
            
            _identityChanged = false;
        }
    }
    
    private void getIdentityString()
    {
        _identityString = _lid + "|" + _pe + "|" + _shefParamCodeString + "|" + _dur + "|" + _extremum + "|" + _ts; 
    }
    
    public String toString()
    {
        getIdentity();
       
        return _outputString;
    }

    public int hashCode()
    {
        getIdentity();
        
        return _hashCode;
    }

/*
    private void computeToString()
    {
        _outputString = "lid = " + _lid +
        " pe = " + _pe +
        " shef_param_code = " + _shef_param_code +
        " dur = " + _dur + 
        " extremum = " + _extremum +
        " ts = " + _ts;
    }

    private void computeHashCode()
    {
        _hashCode = _outputString.hashCode();
    }
    
*/
    public boolean equals( Object object )
    {
        RegularObsTimeSeriesDescriptor descriptor = (RegularObsTimeSeriesDescriptor) object;
        boolean returnValue = false;
        
      //  if ( this.toString().equals( descriptor.toString() ) )
        if ( _hashCode == descriptor.hashCode())
        {
            returnValue = true;
        }
        else
        {
            returnValue = false;
        }
        
        return returnValue;
    }
    
    
    public String getLid()
    {
        return _lid;
    }
    
    public String getPe()
    {
        return _pe;
    }
    
    public short getDur()
    {
        return _dur;
    }
    
    public String getTs()
    {
        return _ts;
    }
    
    public String getExtremum()
    {
        return _extremum;
    }
    
    public String getShef_param_code()
    {   
        getIdentity();
        
        return _shefParamCodeString;
    }
    
    public void setLid( String lid )
    {
        _lid = lid;
        setIdentityChanged( true );
    }
    
    public void setPe( String pe )
    {
        _pe = pe;
        setIdentityChanged( true );
    }
    
    public void setShef_param_code( String shef_param_code )
    {
        _shefParamCodeString = shef_param_code;
        setIdentityChanged( true );
    }
    
    public void setDur( short  dur )
    {
        _dur = dur;
        setIdentityChanged( true );
    }
    
    public void setExtremum( String extremum )
    {
        _extremum = extremum;
        setIdentityChanged( true );
    }
    
    public void setTs( String ts )
    {
        _ts = ts;
        setIdentityChanged( true );
    }

    public void setIdentityChanged( boolean valueChanged )
    {
        _identityChanged = valueChanged;
    }

    public boolean hasIdentityChanged()
    {
        return _identityChanged;
    }

    public void setOutputString( String outputString )
    {
        _outputString = outputString;
        setIdentityChanged( true );
    }

    public String getOutputString()
    {
        return _outputString;
    }

    public void setEndTime( long endTime )
    {
        _endTime = TimeHelper.roundTimeInMillisToNearestHour( endTime );
    }

    public long getEndTime()
    {
        return _endTime;
    }

    public void setStartTime( long startTime )
    {
        _startTime = TimeHelper.roundTimeInMillisToNearestHour( startTime );
    }

    public long getStartTime()
    {
        return _startTime;
    }

    /**
     * @param missing The missing to set.
     */
    public void setMissing(boolean missing)
    {
        _missing = missing;
    }

    /**
     * @return Returns the missing.
     */
    public boolean isMissing()
    {
        return _missing;
    }
}

