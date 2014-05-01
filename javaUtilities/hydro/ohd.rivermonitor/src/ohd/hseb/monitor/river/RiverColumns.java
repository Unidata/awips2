package ohd.hseb.monitor.river;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.monitor.LocationInfoColumns;
import ohd.hseb.monitor.precip.PrecipColumns;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;

public class RiverColumns
{
    public static final String GROUP_ID = "Group Id";
    public static final String GROUP_NAME = "Group Name";
    public static final String LOCATION_ID = "Location Id";
    public static final String LOCATION_NAME = "Location Name";
    public static final String HSA = "HSA";
    public static final String GROUP_ORDINAL = "Group Ordinal";
    public static final String LOCATION_ORDINAL = "Location Ordinal";
    public static final String COUNTY = "County";
    public static final String STATE = "State";
    public static final String STREAM = "Stream";
    public static final String RIVER_BASIN = "River Basin";
    public static final String BANK_FULL = "Bank Full";
    
    public static final String PRIMARY_RIVER_PE = "Primary River PE";
    
    public static final String FLD_STG = "Flood Stage";
    public static final String ACT_STG = "Action Stage";
    public static final String FLD_FLOW = "Flood Flow";
    public static final String ACT_FLOW = "Action Flow";
    public static final String MIN_STG = "Minor Stage";
    public static final String MAJ_STG = "Major Stage";
    public static final String MOD_STG = "Moderate Stage";
    public static final String LAT_OBS_VALUE = "LatestObs Value";
    public static final String LAT_OBS_TIME = "LatestObs Time";
    public static final String MAX_FCST_VALUE = "MaxFcst Value";
    public static final String MAX_FCST_VALID_TIME = "MaxFcst ValidTime";
    public static final String MAX_FCST_BASIS_TIME = "MaxFcst BasisTime";
    public static final String OBSFCST_MAX = "ObsFcstMax";
    public static final String LAT_STG_VALUE = "Latest StgValue";
    public static final String LAT_STG_TIME = "Latest StgTime";;
    public static final String LAT_FLOW_VALUE = "Latest FlowValue";
    public static final String LAT_FLOW_TIME = "Latest FlowTime";
    public static final String FLD_STG_DEP = "FldStg Departure";
    public static final String ACT_STG_DEP = "ActStg Departure";
    public static final String THREAT = "Threat";
    public static final String ALERT_ALARM = "AlertAlarm";
    public static final String VTEC_SUMMARY = "VTEC Summary";
    public static final String EVENT_END_TIME = "Event EndTime";
    public static final String UGC_EXPIRE_TIME = "UGCExpire Time";
    public static final String SSHP_MAX_FCST_VALUE = "SSHP MaxFcstValue";
    public static final String SSHP_MAX_FCST_VALID_TIME = "SSHP MaxFcstValidTime";
    public static final String SSHP_FCST_BASIS_TIME = "SSHP FcstBasisTime";
    public static final String SSHP_FCST_FLD_TIME = "SSHP FcstFloodTime";

    
    public List<JTableColumnDescriptor> getRiverColumnsList(List<JTableColumnDescriptor> riverMonitorColumnDescriptorList)
    {
        String textAlignment = "center";
        String numberAlignment = "right";
        String timeAlignment = "center";
       
        if(riverMonitorColumnDescriptorList == null)
        {
            riverMonitorColumnDescriptorList = new ArrayList<JTableColumnDescriptor>();
            riverMonitorColumnDescriptorList = new LocationInfoColumns().getLocationInfoColumnsList(riverMonitorColumnDescriptorList);
            riverMonitorColumnDescriptorList = new PrecipColumns().getPrecipColumnsList(riverMonitorColumnDescriptorList);
        }
       
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(PRIMARY_RIVER_PE, textAlignment, "Primary River PE"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(STREAM, textAlignment, "Stream Name"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(BANK_FULL, numberAlignment, "Bank Full Stage"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(FLD_STG, numberAlignment, "Flood Stage"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(ACT_STG, numberAlignment, "Warning Stage"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(FLD_FLOW, numberAlignment, "Flood Flow"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(ACT_FLOW, numberAlignment, "Action Flow"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(MIN_STG, numberAlignment, "Minor Stage"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(MAJ_STG, numberAlignment, "Major Stage"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(MOD_STG, numberAlignment, "Moderate Stage"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(LAT_OBS_VALUE, numberAlignment, "Latest Observed Value"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(LAT_OBS_TIME, timeAlignment, "Latest Observed Time"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(MAX_FCST_VALUE, numberAlignment, "MaxFcst Value"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(MAX_FCST_VALID_TIME, timeAlignment, "MaxFcst ValidTime"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(MAX_FCST_BASIS_TIME, timeAlignment, "MaxFcst BasisTime"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(OBSFCST_MAX, numberAlignment, "ObsFcstMax"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(LAT_STG_VALUE, numberAlignment, "Latest StgValue"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(LAT_STG_TIME, timeAlignment, "Latest StgTime"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(LAT_FLOW_VALUE, numberAlignment, "Latest FlowValue"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(LAT_FLOW_TIME, timeAlignment, "Latest FlowTime"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(FLD_STG_DEP, numberAlignment, "FldStg Departure"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(ACT_STG_DEP, numberAlignment, "ActStg Departure"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(THREAT, textAlignment,"Threat"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(ALERT_ALARM, textAlignment, "AlertAlarm"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(VTEC_SUMMARY, textAlignment, "VTEC Summary"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(EVENT_END_TIME, timeAlignment, "Event EndTime"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(UGC_EXPIRE_TIME, timeAlignment, "UGCExpire Time"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(SSHP_MAX_FCST_VALUE, numberAlignment, "SSHP MaxFcstValue"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(SSHP_MAX_FCST_VALID_TIME, timeAlignment, "SSHP MaxFcstValidTime"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(SSHP_FCST_BASIS_TIME, timeAlignment, "SSHP FcstBasisTime"));
        riverMonitorColumnDescriptorList.add(new JTableColumnDescriptor(SSHP_FCST_FLD_TIME, timeAlignment, "SSHP FcstFloodTime"));

        return riverMonitorColumnDescriptorList;
    }
}
