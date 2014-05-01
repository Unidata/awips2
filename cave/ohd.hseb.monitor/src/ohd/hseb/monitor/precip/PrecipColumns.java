package ohd.hseb.monitor.precip;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.monitor.LocationInfoColumns;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
/**
 * 
 * @author rajaramv
 * 
 * The purpose of this class is to provide a consistent set of column names and to provide the
 * Descriptor list of Precipitation-related required by the OHD Java Table Framework for the RiverMonitor and PrecipMonitor
 * applications. (GobsC)
 */
public class PrecipColumns
{
    public static final String LATEST_30MIN = "Latest 30min Precip";
    public static final String LATEST_1HR = "Latest 1hr Precip";
    public static final String LATEST_3HR = "Latest 3hr Precip";
    public static final String LATEST_6HR = "Latest 6hr Precip";
    public static final String LATEST_12HR = "Latest 12hr Precip";
    public static final String LATEST_18HR = "Latest 18hr Precip";
    public static final String LATEST_24HR = "Latest 24hr Precip";

    public static final String TOH_PRECIP_1HR = "1hr Precip";
  
    public static final String FFG_1HR = "FFG 1hr";
    public static final String FFG_3HR = "FFG 3hr";
    public static final String FFG_6HR = "FFG 6hr";

    public static final String DIFF_1HR = "Diff 1hr";
    public static final String DIFF_3HR = "Diff 3hr";
    public static final String DIFF_6HR = "Diff 6hr";

    public static final String RATIO_1HR = "Ratio 1hr %";
    public static final String RATIO_3HR = "Ratio 3hr %";
    public static final String RATIO_6HR = "Ratio 6hr %";

    public static final String PRECIP_THREAT = "Precip Threat";
    
    public static final String LATEST_PRECIP_PARAM_CODE = "Latest Precip Param Code";
    
    public static final String TOH_PRECIP_1HR_PARAM_CODE = "1hr Precip Param Code";
   
    /**
     * Creates a list of precip columns each of type JTableColumnDescriptor object.
     * If the parameter passed is null then the new list is created and the location info columns & precip data columns are added to it, 
     * if not the precip data column's list is added to the list that is passed as argument.
     * The created new list is returned.
     * @param precipMonitorColumnDescriptorList
     * @return
     */
    public List<JTableColumnDescriptor> getPrecipColumnsList(List<JTableColumnDescriptor> precipMonitorColumnDescriptorList)
    {
        String textAlignment = "center";
        String numberAlignment = "right";
       
        if(precipMonitorColumnDescriptorList == null)
        {
            precipMonitorColumnDescriptorList = new ArrayList<JTableColumnDescriptor>();
            precipMonitorColumnDescriptorList = new LocationInfoColumns().getLocationInfoColumnsList(precipMonitorColumnDescriptorList);
        }
       
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.LATEST_PRECIP_PARAM_CODE, textAlignment, "Latest Precip Param Code"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.TOH_PRECIP_1HR_PARAM_CODE, textAlignment, "Top-of-Hour 1hr Precip Param Code"));
        
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.LATEST_30MIN, numberAlignment, "Latest 30min Precip"));  
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.LATEST_1HR, numberAlignment, "Latest 1hr Precip"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.LATEST_3HR, numberAlignment, "Latest 3hr Precip"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.LATEST_6HR, numberAlignment, "Latest 6hr Precip"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.LATEST_12HR, numberAlignment, "Latest 12hr Precip"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.LATEST_18HR, numberAlignment, "Latest 18hr Precip"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.LATEST_24HR, numberAlignment, "Latest 24hr Precip"));

        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.TOH_PRECIP_1HR, numberAlignment, "Top-of-Hour 1hr Precip"));
 
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.FFG_1HR, numberAlignment, "FFG 1hr"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.FFG_3HR, numberAlignment, "FFG 3hr"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.FFG_6HR, numberAlignment, "FFG 6hr"));

        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.DIFF_1HR, numberAlignment, "Latest Precip 1hr - FFG 1hr"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.DIFF_3HR, numberAlignment, "Latest Precip 3hr - FFG 3hr"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.DIFF_6HR, numberAlignment, "Latest Precip 6hr - FFG 6hr"));

        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.RATIO_1HR, numberAlignment, "(Latest Precip 1hr/FFG 1hr) * 100"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.RATIO_3HR, numberAlignment, "(Latest Precip 3hr/FFG 3hr) * 100"));
        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.RATIO_6HR, numberAlignment, "(Latest Precip 6hr/FFG 6hr) * 100"));

        precipMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PrecipColumns.PRECIP_THREAT, textAlignment,"Precip Threat"));
        
        return precipMonitorColumnDescriptorList;
    }
}
