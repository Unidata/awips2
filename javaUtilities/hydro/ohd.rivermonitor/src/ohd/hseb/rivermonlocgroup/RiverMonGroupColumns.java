package ohd.hseb.rivermonlocgroup;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;

public class RiverMonGroupColumns
{
    public static final String GROUP_ID = "Group Id";
    public static final String GROUP_NAME = "Group Name";
    public static final String GROUP_ORDINAL = "Group Ordinal";
    public static final String HSA = "HSA";
  
    /**
     * Creates a list of rivermon group columns each of type JTableColumnDescriptor object.
     * If the parameter passed is null then the new list is created and the rivermon group columns are added to it, 
     * if not the location info columns are added to the list that is passed as argument.
     * The created new list is returned.
     * @param riverMonGroupColumnDescriptorList
     * @return
     */
    public List<JTableColumnDescriptor> getRiverMonGroupColumnsList(List<JTableColumnDescriptor> riverMonGroupColumnDescriptorList)
    {
        String textAlignment = "center";
        String numberAlignment = "right";
     
        if( riverMonGroupColumnDescriptorList == null)
        {
            riverMonGroupColumnDescriptorList = new ArrayList<JTableColumnDescriptor>();
        }

        riverMonGroupColumnDescriptorList.add(new JTableColumnDescriptor(GROUP_ID, true, 95, textAlignment));
        riverMonGroupColumnDescriptorList.add(new JTableColumnDescriptor(GROUP_NAME, true, 335, textAlignment));
        riverMonGroupColumnDescriptorList.add(new JTableColumnDescriptor(GROUP_ORDINAL, true, 85, numberAlignment));
        riverMonGroupColumnDescriptorList.add(new JTableColumnDescriptor(HSA, true, 75, textAlignment));
        
        return riverMonGroupColumnDescriptorList;
    }

}
