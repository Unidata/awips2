package ohd.hseb.rivermonlocgroup;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;

public class RiverMonLocationColumns
{
    public static final String GROUP_ID = "Group Id";
    public static final String LOCATION_ID = "Location";
    public static final String LOCATION_ORDINAL = "Location Ordinal";
  
    /**
     * Creates a list of rivermon location columns each of type JTableColumnDescriptor object.
     * If the parameter passed is null then the new list is created and the rivermon location columns are added to it, 
     * if not the location info columns are added to the list that is passed as argument.
     * The created new list is returned.
     * @param riverMonLocationColumnDescriptorList
     * @return
     */
    public List<JTableColumnDescriptor> getRiverMonLocationColumnsList(List<JTableColumnDescriptor> riverMonLocationColumnDescriptorList)
    {
        String textAlignment = "center";
        String numberAlignment = "right";
     
        if( riverMonLocationColumnDescriptorList == null)
        {
            riverMonLocationColumnDescriptorList = new ArrayList<JTableColumnDescriptor>();
        }

        riverMonLocationColumnDescriptorList.add(new JTableColumnDescriptor(GROUP_ID, true, 95, textAlignment, "Group Identification"));
        riverMonLocationColumnDescriptorList.add(new JTableColumnDescriptor(LOCATION_ID, true, 95, textAlignment, "Location Identification"));
        riverMonLocationColumnDescriptorList.add(new JTableColumnDescriptor(LOCATION_ORDINAL, true, 105, numberAlignment, "Location Ordinal"));
  
        return riverMonLocationColumnDescriptorList;
    }

}
