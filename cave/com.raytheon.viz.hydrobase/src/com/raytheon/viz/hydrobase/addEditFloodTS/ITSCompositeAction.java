package com.raytheon.viz.hydrobase.addEditFloodTS;

/**
 * Action interface in AddEditFloodEventDlg
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 11/09/2015   DCS15095    wkwock      Initial creation
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */
public interface ITSCompositeAction {
    public void removeTSComp (TSComposite tsComp);
    
    public void addTSComp(TSComposite tsComp);
}
