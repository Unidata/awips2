package ohd.hseb.pdc_pp;

import java.util.Comparator;

public class RegularObsTimeSeriesComparator implements Comparator
{

    public int compare(Object object1, Object object2)
    {
        RegularObsTimeSeries ts1 = (RegularObsTimeSeries) object1;
        RegularObsTimeSeries ts2 = (RegularObsTimeSeries) object2;
        
        RegularObsTimeSeriesDescriptor desc1 = ts1.getDescriptor();
        RegularObsTimeSeriesDescriptor desc2 = ts2.getDescriptor();
        
        int returnValue = desc1.toString().compareTo(desc2.toString());
        
        return returnValue;
    }

    
    
}
