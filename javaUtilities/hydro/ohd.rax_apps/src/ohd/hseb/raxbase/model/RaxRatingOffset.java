package ohd.hseb.raxbase.model;

public class RaxRatingOffset
{
    private static final short MISSING = -9999;
    
    private double _stage = MISSING;
    private double _offset = MISSING;

//  ------------------------------------------------------------------------------------------

    public RaxRatingOffset()
    {
        this( MISSING, MISSING );
    }
    
//  ------------------------------------------------------------------------------------------

    public RaxRatingOffset( double stage, double offset )
    {
        setStage( stage );
        setOffset( offset );
    }
//  ------------------------------------------------------------------------------------------

    public void setStage( double stage )
    {
        _stage = stage;
    }
//  ------------------------------------------------------------------------------------------

    public double getStage()
    {
        return _stage;
    }
//  ------------------------------------------------------------------------------------------
    public void setOffset( double offset )
    {
        _offset = offset;
    }
//  ------------------------------------------------------------------------------------------

    public double getOffset()
    {
        return _offset;
    }
//  ------------------------------------------------------------------------------------------
    public boolean equals( RaxRatingOffset raxRatingOffset )
    {
        boolean result = false;

        if ( ( _stage == raxRatingOffset.getStage() ) &&
             ( _offset == raxRatingOffset.getOffset() ) )
        {
            result = true;          
        }

        return result;

    }
//  ------------------------------------------------------------------------------------------

    public String toString()
    {
        String outString = " Stage = " + getStage() + 
        " Offset = " + getOffset();

        return outString;
    }
//  ------------------------------------------------------------------------------------------

}
