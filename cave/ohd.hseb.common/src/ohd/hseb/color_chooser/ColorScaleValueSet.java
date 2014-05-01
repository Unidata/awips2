package ohd.hseb.color_chooser;

public class ColorScaleValueSet 
{
    private ColorSet _colorSet = null;
    private ScaleValueSet _scaleValueSet = null;
    private String _colorUseString = null;
    private double _duration = -99999;
    private String _userID = null;
    

    public ColorScaleValueSet()
    {
    }

    public ColorScaleValueSet( ColorScaleValueSet colorScaleValueSet )
    {
        setColorSet( new ColorSet( colorScaleValueSet.getColorSet() ) );
        setScaleValueSet( new ScaleValueSet( colorScaleValueSet.getScaleValueSet() ) );
        setColorUseString( new String( colorScaleValueSet.getColorUseString() ) );
        setUserID( new String( colorScaleValueSet.getUserID() ) );
        setDuration( colorScaleValueSet.getDuration() );
/*        
        setColorSet( colorScaleValueSet.getColorSet() );
        setScaleValueSet( colorScaleValueSet.getScaleValueSet() );
        setColorUseString( colorScaleValueSet.getColorUseString() );
        setUserID( colorScaleValueSet.getUserID() );
        setDuration( colorScaleValueSet.getDuration() );
  */      
    }
    
    public static ColorScaleValueSet getCopyOfColorScaleValueSet( ColorScaleValueSet colorScaleValueSet )
    {
        ColorScaleValueSet returnColorScaleValueSet = new ColorScaleValueSet();
        
        returnColorScaleValueSet.setColorSet( new ColorSet( colorScaleValueSet.getColorSet() ) );
        returnColorScaleValueSet.setScaleValueSet( new ScaleValueSet( colorScaleValueSet.getScaleValueSet() ) );
        returnColorScaleValueSet.setColorUseString( new String( colorScaleValueSet.getColorUseString() ) );
        returnColorScaleValueSet.setUserID( new String( colorScaleValueSet.getUserID() ) );
        returnColorScaleValueSet.setDuration( colorScaleValueSet.getDuration() );
        
        return returnColorScaleValueSet;
    }
    
    public String toString()
    {
        return _userID + " " + _colorUseString + " " + _duration + "\n" + _colorSet;
    }

    public void setColorSet( ColorSet colorSet ) 
    {
        _colorSet = colorSet;
    }

    public ColorSet getColorSet() 
    {
        return _colorSet;
    }

    public void setScaleValueSet(ScaleValueSet scaleValueSet) 
    {
        _scaleValueSet = scaleValueSet;
    }

    public ScaleValueSet getScaleValueSet()
    {
        return _scaleValueSet;
    }

    public void setColorUseString(String colorUseString) 
    {
        _colorUseString = colorUseString;
    }

    public String getColorUseString() 
    {
        return _colorUseString;
    }

    public void setDuration( double duration ) 
    {
        _duration = duration;
    }

    public double getDuration() 
    {
        return _duration;
    }

    public void setUserID( String userID ) 
    {
        _userID = userID;
    }

    public String getUserID() 
    {
        return _userID;
    }
    
}
