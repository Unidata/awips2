/*
 * Created on Apr 7, 2004
 *
 * 
 */
package ohd.hseb.util.gui.drawing;

/**
 * @author GobsC
 *
 * 
 */
public class ScalingHelper
{
    
    //includes the first and last ticks
   // private final static int _minMajorTickCount = 4;
   private final static int _maxMajorTickCount = 6;
    
    
    private final static double _niceLowerNumberFactor = 5.0;
     
    private double[] _niceMajorIncrementArray  =  {1.0, 2.0, 5.0};
    private double[] _niceMinorIncrementArray = {.2, .5, 1};
    private double _baseFactorStartingPoint = .01;
// -------------------------------------------------------------------------    
    private double _minDataValue;
    private double _maxDataValue;
    
    private double _minScaleValue;
    private double _maxScaleValue;
    
    private int _majorTickCount;
    private int _minorTickCount;
    
    private double _majorTickIncrement;
    private double _minorTickIncrement;
    
    
 
// -------------------------------------------------------------------------    
   
    public ScalingHelper(double minDataValue, double maxDataValue)
    {
        _minDataValue = minDataValue;
        _maxDataValue = maxDataValue;
        
        rescale();    
    }

// --------------------------------------------------------------------

    public void setMaxDataValue(double maxDataValue)
    {
        _maxDataValue = maxDataValue;
        
        rescale();
    }

//  --------------------------------------------------------------------

    public double getMaxDataValue()
    {
        return _maxDataValue;
        
    }

//  --------------------------------------------------------------------

    public void setMinDataValue(double minDataValue)
    {
        _minDataValue = minDataValue;
        rescale();
    }

//  --------------------------------------------------------------------

    public double getMinDataValue()
    {
        return _minDataValue;
    }

//  --------------------------------------------------------------------
    public int getMajorTickCount()
    {
        return _majorTickCount;
    }

    public double getMajorTickIncrement()
    {
        return _majorTickIncrement;
    }

//  --------------------------------------------------------------------

    public double getMaxScaleValue()
    {
        return _maxScaleValue;
    }
    
//  --------------------------------------------------------------------
    
    
    public int getMinorTickCount()
    {
        return _minorTickCount;
    }
    
 //  --------------------------------------------------------------------
   
    public double getMinorTickIncrement()
    {
        return _minorTickIncrement;
    }
//  --------------------------------------------------------------------
    
    public double getMinScaleValue()
    {
        return _minScaleValue;
    }

//  --------------------------------------------------------------------
    
    public void setNiceMajorIncrementArray(double[] niceMajorIncrementArray)
    {
        _niceMajorIncrementArray = niceMajorIncrementArray;
        rescale(); 
    }

//  --------------------------------------------------------------------
 
    public double[] getNiceMajorIncrementArray()
    {
        return _niceMajorIncrementArray;
    }

//  --------------------------------------------------------------------
 
    public void setNiceMinorIncrementArray(double[] niceMinorIncrementArray)
    {
        _niceMinorIncrementArray = niceMinorIncrementArray;
        rescale();
    }

//  --------------------------------------------------------------------
 
    public double[] getNiceMinorIncrementArray()
    {
        return _niceMinorIncrementArray;
    }

//  --------------------------------------------------------------------
    private void rescale()
    {
        
        
        int multipleCount = (int) Math.floor(_minDataValue/ _niceLowerNumberFactor);
        _minScaleValue = multipleCount * _niceLowerNumberFactor;

    
        double baseFactor = _baseFactorStartingPoint;

        // find the right nice increment
    
        boolean done = false;
        int i = 0;


        double range = _maxDataValue - _minScaleValue;
        
        
        //ensure that range is a sane value
        
        if (range < 1.0)
        {
            range = 1.0;    
        }

        
        while (!done )
        {
            double testIncrement = _niceMajorIncrementArray[i]  * ( baseFactor);
          
            int testTickCount = (int) Math.ceil(range/testIncrement + 1);


            //if there are a reasonable number of tickCounts, then stop
           // if  ((testTickCount >= _minMajorTickCount) && 
           //      (testTickCount <= _maxMajorTickCount))
            if (testTickCount <= _maxMajorTickCount)
            {

                _majorTickCount = testTickCount;

                _majorTickIncrement = testIncrement;

                _minorTickIncrement =  _niceMinorIncrementArray[i] * baseFactor;
                _minorTickCount = (int) (_niceMajorIncrementArray[i]/ _niceMinorIncrementArray[i]) - 1;

                //first tick counts as a tick, so subtract 1
                _maxScaleValue = _minScaleValue  + ( (_majorTickCount-1) * _majorTickIncrement); 

                done = true;
            }    
            
            i++;
            
            if ((i >= _niceMajorIncrementArray.length) ||
                (i >= _niceMinorIncrementArray.length))
            {
                i = 0;
                baseFactor *= 10.0;
            }
              
        } //end while !done



        return;
    }
    
//  --------------------------------------------------------------------
    public String toString()
    {
        String outString = " minDataValue = " + getMinDataValue() + 
                           " maxDataValue = " + getMaxDataValue() + "\n" +
                           " minScaleValue = " + getMinScaleValue() + 
                           " maxScaleValue = " + getMaxScaleValue() + "\n" +
                           " majorTickCount = " + getMajorTickCount() +  
                           " minorTickCount = " + getMinorTickCount() +  "\n" +
                           " majorTickIncrement = " + getMajorTickIncrement() +  
                           " minorTickIncrement = " + getMinorTickIncrement(); 

        return outString;
    }

  
//  --------------------------------------------------------------------
    public static void main(String[] argArray)
    {
        double minValue = 87.6;
        double maxValue = 123.5;
        ScalingHelper scaler = new ScalingHelper(minValue, maxValue);

        System.out.println(scaler.toString());

    }

    /**
     * @param baseFactorStartingPoint The baseFactorStartingPoint to set.
     */
    public void setBaseFactorStartingPoint(double baseFactorStartingPoint)
    {
        _baseFactorStartingPoint = baseFactorStartingPoint;
    }

    /**
     * @return Returns the baseFactorStartingPoint.
     */
    public double getBaseFactorStartingPoint()
    {
        return _baseFactorStartingPoint;
    }

   
//  --------------------------------------------------------------------



}
