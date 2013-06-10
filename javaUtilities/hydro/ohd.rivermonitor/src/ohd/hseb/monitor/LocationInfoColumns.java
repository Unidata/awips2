package ohd.hseb.monitor;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;

public class LocationInfoColumns
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
    public static final String RIVER_BASIN = "River Basin";


    /**
     * Creates a list of location info columns each of type JTableColumnDescriptor object.
     * If the parameter passed is null then the new list is created and the location info columns are added to it, 
     * if not the location info columns are added to the list that is passed as argument.
     * The created new list is returned.
     * @param locationInfoColumnDescriptorList
     * @return
     */
    public List<JTableColumnDescriptor> getLocationInfoColumnsList(List<JTableColumnDescriptor> locationInfoColumnDescriptorList)
    {
        String textAlignment = "center";
        String numberAlignment = "right";
     
        if( locationInfoColumnDescriptorList == null)
        {
            locationInfoColumnDescriptorList = new ArrayList<JTableColumnDescriptor>();
        }

        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(GROUP_ID, textAlignment, "Group Identification"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(GROUP_NAME, textAlignment, "Group Name"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(LOCATION_ID, textAlignment, "Location Identification"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(LOCATION_NAME, textAlignment, "Location Name"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(HSA, textAlignment, "Hydrologic Service Area"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(GROUP_ORDINAL, numberAlignment, "Group Ordinal"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(LOCATION_ORDINAL, numberAlignment, "Location Ordinal"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(COUNTY, textAlignment, "County Name"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(STATE, textAlignment, "State Name"));
        locationInfoColumnDescriptorList.add(new JTableColumnDescriptor(RIVER_BASIN, textAlignment, "River Basin"));


        return locationInfoColumnDescriptorList;
    }

}
