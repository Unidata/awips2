package ohd.hseb.vtecevent;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;

public class VtecEventColumns
{
    public static String OFFICE_ID ="OffId";
    public static String LID = "LocId";
    public static String PRODUCT_MODE = "Mode";
    public static String ACTION = "Act";
    public static String PHENOMENON ="Ph";
    public static String SIGNIFICANCE ="Sig";
    public static String EVENT_TRACKING_NUMBER ="ETN";
    public static String BEGIN_TIME ="BeginTime";
    public static String END_TIME ="EndTime";
    public static String ACTIVE ="Active";
    public static String SEVERITY ="Sev";
    public static String IMMEDIATE_CAUSE ="ImC";
    public static String RISE_TIME ="RiseTime";
    public static String CREST_TIME ="CrestTime";
    public static String FALL_TIME ="FallTime";
    public static String PRODUCT_ID ="ProdId";
    public static String PRODUCT_TIME ="ProductTime";
    public static String UGC_EXPIRE_TIME ="UGCExpTime";
    public static String RECORD ="Rec";
    public static String RISE_TS ="TSr";
    public static String CREST_TS ="TSc";
    public static String FALL_TS ="TSf";
    public static String CREST_VALUE ="CrestVal";
    
    /**
     * Creates a list of vtecevent columns each of type JTableColumnDescriptor object.
     * If the parameter passed is null then the new list is created and the vtecevent columns are added to it, 
     * if not the vtecevent columns are added to the list that is passed as argument.
     * The created new list is returned.
     * @param officeNotesColumnDescriptorList
     * @return
     */
    public List<JTableColumnDescriptor> getVtecEventColumnsList(List<JTableColumnDescriptor> vtecEventColumnDescriptorList)
    {
        if( vtecEventColumnDescriptorList == null)
        {
            vtecEventColumnDescriptorList = new ArrayList<JTableColumnDescriptor>();
        }

        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(OFFICE_ID, true, 30, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(LID, true, 50, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(PRODUCT_MODE, true, 35, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(ACTION, true, 35, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(PHENOMENON, true, 25, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(SIGNIFICANCE, true, 25, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(EVENT_TRACKING_NUMBER, true, 35, "right"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(BEGIN_TIME, true, 90, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(END_TIME, true, 90, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(ACTIVE, true, 40, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(SEVERITY, true, 25, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(IMMEDIATE_CAUSE, true, 35, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(RISE_TIME, true, 90, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(CREST_TIME, true, 90, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(FALL_TIME, true, 90, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(PRODUCT_ID, true, 100, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(PRODUCT_TIME, true, 90, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(UGC_EXPIRE_TIME, true, 90, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(RECORD, true, 25, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(RISE_TS, true, 25, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(CREST_TS, true, 25, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(FALL_TS, true, 25, "center"));
        vtecEventColumnDescriptorList.add(new JTableColumnDescriptor(CREST_VALUE, true, 60, "right"));
        
        return vtecEventColumnDescriptorList;
    }
}
