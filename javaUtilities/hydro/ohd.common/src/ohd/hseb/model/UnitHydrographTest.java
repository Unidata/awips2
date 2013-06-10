package ohd.hseb.model;

import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RelTimeMeasurement;
import junit.framework.TestCase;

public class UnitHydrographTest extends TestCase
{

    
    public void testScalingAll()
    {
        
        UnitHydrograph origUhg = getTestUnitHydrograph();
    
        testScaling(origUhg.getTotalDischarge());
        testScaling(origUhg.getTotalDischarge() * 2);
        testScaling(origUhg.getTotalDischarge()/2);
        testScaling(100);
        
     
    }
    
    public void testScaling(double desiredTotalDischarge)
    {
        UnitHydrograph origUhg = getTestUnitHydrograph();
        
        UnitHydrograph scaledUhg = origUhg.getScaledUnitHydrograph(desiredTotalDischarge);
        assertEquals(desiredTotalDischarge, scaledUhg.getTotalDischarge(), 0.0001 );  
    }
    
    public static UnitHydrograph getTestUnitHydrograph()
    {
        double[] valueArray = { 100.0, 200.0, 350.0, 500.0, 
                                300.0, 250.0, 200.0, 150.0, 
                                100.0, 50.0, 25.0, 10}; 
    
        UnitHydrograph uhg = new UnitHydrograph(MeasuringUnit.cfs, 1);
        
        
        for (int i = 0; i < valueArray.length; i++)
        {
            RelTimeMeasurement measurement = new RelTimeMeasurement(valueArray[i], i, MeasuringUnit.cfs);
            uhg.addMeasurement(measurement); 
        }
        
        return uhg;
        
    }
    
    
}
