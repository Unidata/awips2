package ohd.hseb.bias_trans;

public class BiasDynSet
{
    private short  _memSpanIndex; /* usually  0 - 9. */
    private double _gageRadarPairCount;
    private float _sumOfGageValues;
    private float _sumOfRadarValues;
    private float _meanFieldBias;
    
    public void setMemSpanIndex(short memSpanIndex)
    {
        _memSpanIndex = memSpanIndex;
    }
    
    public short getMemSpanIndex()
    {
        return _memSpanIndex;
    }
    
    public void setGageRadarPairCount(double numPairs)
    {
        _gageRadarPairCount = numPairs;
    }
    
    public double getGageRadarPairCount()
    {
        return _gageRadarPairCount;
    }
    
    public void setSumOfGageValues(float sumGag)
    {
        _sumOfGageValues = sumGag;
    }
    
    public float getSumOfGageValues()
    {
        return _sumOfGageValues;
    }
    
    public void setSumOfRadarValues(float sumRad)
    {
        _sumOfRadarValues = sumRad;
    }
    
    public float getSumOfRadarValues()
    {
        return _sumOfRadarValues;
    }
    
    public void setMeanFieldBias(float bias)
    {
        _meanFieldBias = bias;
    }
    
    public float getMeanFieldBias()
    {
        return _meanFieldBias;
    }
    
    public String toString ()
    {
        String output = "_memspanInd = " + _memSpanIndex + " _numPairs = " + _gageRadarPairCount + " " +
                        " _sumGag = " + _sumOfGageValues + " _sumRad = " + _sumOfRadarValues + " _bias = " + _meanFieldBias + "\n";
        return output;
    }
    
}
