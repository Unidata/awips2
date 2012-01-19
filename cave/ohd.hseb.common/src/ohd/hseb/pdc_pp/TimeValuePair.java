package ohd.hseb.pdc_pp;

import ohd.hseb.db.DbTimeHelper;

public class TimeValuePair
{
    private long _dateTime = 0;
    private double _value = 0.0;
    private boolean _isMissing = true;
    
    public TimeValuePair( long dateTime, double value )
    {
        _dateTime = dateTime;
        _value = value;
        _isMissing = false;
    }
    
    public TimeValuePair( long dateTime )
    {
        _dateTime = dateTime;
        _isMissing = true;
    }
    
    public TimeValuePair( TimeValuePair timeValuePair )
    {
        this(timeValuePair.getDateTime(), timeValuePair.getValue()  );
      //  _dateTime = timeValuePair.getDateTime();
      //  _value = timeValuePair.getValue();
    }
    
    public String toString()
    {
        String string = DbTimeHelper.getDateTimeStringFromLongTime( _dateTime ) + "," + _value;
//        String string = _value + " ";
        
        return string;
    }
    
    public boolean isMissingValue()
    {
        boolean returnValue = false;
        
        if ( _value == PDCPreprocessorDataMgr.MISSING )
        {
            returnValue = true;
        }
        
        return returnValue;
    }

    public void setDateTime( long dateTime )
    {
        _dateTime = dateTime;
    }

    public long getDateTime()
    {
        return _dateTime;
    }

    public void setValue( double value )
    {
        _value = value;
    }

    public double getValue()
    {
        return _value;
    }
}
