package ohd.hseb.officenotes;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;

public class OfficeNotesColumns
{
    public static final String TOPIC = "Topic";
    public static final String ID = "Id";
    public static final String POSTING_TIME = "PostingTime";
    public static final String NOTE = "Note";
    public static final String EXPIRE_TIME = "ExpireTime";
    public static final String DATA_TIME = "DataTime";
    public static final String UPDATE_TIME = "UpdateTime";
    
    /**
     * Creates a list of office notes columns each of type JTableColumnDescriptor object.
     * If the parameter passed is null then the new list is created and the office notes columns are added to it, 
     * if not the office notes columns are added to the list that is passed as argument.
     * The created new list is returned.
     * @param officeNotesColumnDescriptorList
     * @return
     */
    public List<JTableColumnDescriptor> getOfficeNotesColumnsList(List<JTableColumnDescriptor> officeNotesColumnDescriptorList)
    {
        if( officeNotesColumnDescriptorList == null)
        {
            officeNotesColumnDescriptorList = new ArrayList<JTableColumnDescriptor>();
        }

        officeNotesColumnDescriptorList.add(new JTableColumnDescriptor(TOPIC, true, 75, "center"));
        officeNotesColumnDescriptorList.add(new JTableColumnDescriptor(ID, true, 75, "center"));
        officeNotesColumnDescriptorList.add(new JTableColumnDescriptor(POSTING_TIME, true, 145, "center"));
        officeNotesColumnDescriptorList.add(new JTableColumnDescriptor(NOTE, true, 640, "left"));
        officeNotesColumnDescriptorList.add(new JTableColumnDescriptor(EXPIRE_TIME, true, 145, "center"));
        
        return officeNotesColumnDescriptorList;
    }

}
