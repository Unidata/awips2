package ohd.hseb.util;

public class Range
{
    private double _min = 0;
    private double _max = 0;
    // ---------------------------------------------------------------------------
    
    public Range(double value1, double value2)
    {
        double min = Math.min(value1, value2);
        double max = Math.max(value1, value2);
        
        setMin(min);
        setMax(max);
    }
    // ---------------------------------------------------------------------------
    public boolean isBetweenInclusive(double value)
    {
        boolean result = false;
        
        if ((value >= getMin()) && (value <= getMax() ))
        {
            result = true;
        }
        
        return result;
    }

    // ---------------------------------------------------------------------------
    
    public void setMin(double min)
    {
        _min = min;
    }

    // ---------------------------------------------------------------------------
    
    public double getMin()
    {
        return _min;
    }

    // ---------------------------------------------------------------------------
    
    public void setMax(double max)
    {
        _max = max;
    }

    // ---------------------------------------------------------------------------
    
    public double getMax()
    {
        return _max;
    }
    
    // ---------------------------------------------------------------------------
    
}
