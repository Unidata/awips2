package ohd.hseb.alertalarm;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;

public class AlertAlarmColumns
{
    public static  String LID = "LocId";
    public static  String PE = "PE";
    public static  String DUR= "Dur";
    public static  String TS = "TS";
    public static  String EXT = "Ext";
    public static  String PROB = "Prob";
    public static  String VALID_TIME = "ValidTime";
    public static  String BASIS_TIME = "BasisTime";
    public static  String VALUE = "Value";
    public static  String SUP_VALUE = "Sup Val";
    public static  String SHEF_QUAL_CODE = "SQC";
    public static  String QUAL_CODE = "QC";
    public static  String REVISION = "Rev";
    public static  String PRODUCT_ID = "ProdId";
    public static  String PROD_TIME = "ProductTime";
    public static  String POSTING_TIME = "PostingTime";
    public static  String ACTION_TIME = "ActionTime";
    public static  String DESC = "Desc";
    public static  String THREAT = "Threat";
    
    
    /**
     * Creates a list of alertalarm columns each of type JTableColumnDescriptor object.
     * If the parameter passed is null then the new list is created and the alertalarm columns are added to it, 
     * if not the alertalarm columns are added to the list that is passed as argument.
     * The created new list is returned.
     * @param officeNotesColumnDescriptorList
     * @return
     */
    public List<JTableColumnDescriptor> getAlertAlarmColumnsList(List<JTableColumnDescriptor> alertAlarmColumnDescriptorList)
    {
        if( alertAlarmColumnDescriptorList == null)
        {
            alertAlarmColumnDescriptorList = new ArrayList<JTableColumnDescriptor>();
        }

        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(LID, true, 50, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(PE, true, 25, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(DUR, true, 25, "right"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(TS, true, 25, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(EXT, true, 25, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(PROB, true, 35, "right"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(VALID_TIME, true, 90, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(BASIS_TIME, true, 90, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(VALUE, true, 50, "right"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(SUP_VALUE, true, 50, "right"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(SHEF_QUAL_CODE, true, 35, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(QUAL_CODE, true, 40, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(REVISION, true, 25, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(PRODUCT_ID, true, 100, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(PROD_TIME, true, 90, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(POSTING_TIME, true, 90, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(ACTION_TIME, true, 70, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(DESC, true, 45, "center"));
        alertAlarmColumnDescriptorList.add(new JTableColumnDescriptor(THREAT, true, 45, "center"));
        
        return alertAlarmColumnDescriptorList;
    }
}
