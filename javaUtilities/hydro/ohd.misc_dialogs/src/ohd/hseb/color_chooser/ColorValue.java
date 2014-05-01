package ohd.hseb.color_chooser;

public class ColorValue 
{
    private String _userID;
    private String _applicationName;
    private String _colorUseName;
    private double _duration;
    private double _thresholdValue;
    private String _thresholdUnit;
    private String _colorName;
    
    public void setUserID( String userID ) 
    {
        _userID = userID;
    }
    
    public String getUserID() 
    {
        return _userID;
    }
    
    public void setApplicationName( String applicationName ) 
    {
        _applicationName = applicationName;
    }
    
    public String getApplicationName() 
    {
        return _applicationName;
    }
    
    public void setColorUseName( String colorUseName ) 
    {
        _colorUseName = colorUseName;
    }
    
    public String getColorUseName() 
    {
        return _colorUseName;
    }
    
    public void setDuration( double duration ) 
    {
        _duration = duration;
    }
    
    public double getDuration() 
    {
        return _duration;
    }
    
    public void setThresholdValue( double thresholdValue )
    {
        _thresholdValue = thresholdValue;
    }
    
    public double getThresholdValue() 
    {
        return _thresholdValue;
    }
    
    public void setThresholdUnit( String thresholdUnit ) 
    {
        _thresholdUnit = thresholdUnit;
    }
    
    public String getThresholdUnit() 
    {
        return _thresholdUnit;
    }
    
    public void setColorName( String colorName ) 
    {
        _colorName = colorName;
    }
    
    public String getColorName() 
    {
        return _colorName;
    }
    
    public String toString()
    {
        return _userID + " " + _colorUseName + " " + _duration + " " + _thresholdValue + " " + _colorName;
    }
}
