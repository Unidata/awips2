package ohd.hseb.raxbase.model;

public class RaxRatingPoint
{
    private static final short MISSING = -9999;
    
    private double _stage = MISSING;
    private double _discharge = MISSING;

//  ------------------------------------------------------------------------------------------

    public RaxRatingPoint()
    {
        this( MISSING, MISSING );
    }
    
//  ------------------------------------------------------------------------------------------

    public RaxRatingPoint( double stage, double discharge )
    {
        setStage( stage );
        setDischarge( discharge );
    }
//  ------------------------------------------------------------------------------------------

    public void setStage(double stage)
    {
        _stage = stage;
    }
//  ------------------------------------------------------------------------------------------

    public double getStage()
    {
        return _stage;
    }
//  ------------------------------------------------------------------------------------------
    public void setDischarge(double discharge)
    {
        _discharge = discharge;
    }
//  ------------------------------------------------------------------------------------------

    public double getDischarge()
    {
        return _discharge;
    }
//  ------------------------------------------------------------------------------------------
    public boolean equals( RaxRatingPoint point )
    {
        boolean result = false;

        if ( ( _stage == point.getStage() ) &&
             ( _discharge == point.getDischarge() ) )
        {
            result = true;          
        }

        return result;

    }
//  ------------------------------------------------------------------------------------------

    public String toString()
    {
        String outString = " Stage = " + getStage() + 
        " Discharge = " + getDischarge();

        return outString;
    }
//  ------------------------------------------------------------------------------------------

}
