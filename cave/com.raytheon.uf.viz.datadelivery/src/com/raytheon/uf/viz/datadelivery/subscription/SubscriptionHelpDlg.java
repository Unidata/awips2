package com.raytheon.uf.viz.datadelivery.subscription;


import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.datadelivery.help.DataDeliveryHelp;

/**
 * Help dialog for the Subscription Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012   645      jpiatt     Initial creation.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */

public class SubscriptionHelpDlg extends DataDeliveryHelp {
    
    /**
     * Constructor.
     * @param parentShell 
     *          Parent shell.         
     */

    public SubscriptionHelpDlg(Shell parentShell) {

       this.parentShell = parentShell;
       this.helpText = getHelpText();
       
   }
 
    /**
     * Get the Help Text.
     * 
     * @return String
     *            Help Text.  
     */
    private String getHelpText() {
    	
        String helpText = "<HTML><HEAD><TITLE>Subscription Table Help</TITLE></HEAD><BODY>" +
        "The Subscription Manager Table allows a user to add, edit, copy, view, and delete " +
        "data delivery subscriptions.<br><br>" +
//        "<a href='#file'><u><b>File</b></u><br></a>" +
//        "<a href='#edit'><u><b>Edit</b></u><br></a>" +
//        "<a href='#settings'><u><b>Settings</b></u><br></a><br>" +
        "<dl>" +
        "   <dt>" +
        "      <a name='file'><u><b>File</b></u><br>"+
        "        <dd>" +
        "        <b>New Subscription</b><br>" +
        "        The New Subscription menu option opens the Dataset Discovery Browser. " +
        "        The Dataset Discovery Browser allows a user to find Datasets using " +
        "        filtering options.<br>First, select an Area for the Areal Coverage. Optionally, use" +
        "        the Set Area to select an Area. Next, select an available Data Type. Move the data type to " +
        "        the selected column using the arrows.  Next, select from the filtering options.  Filters " +
        "        include Data Provider, Data Set, Level or Parameter. Click the arrows to the left of the " +
        "        Filter titles to expand the filter selection sections. After Filters are selected, click " +
        "        <b>Update Results</b> to load the list of Datasets in the table. Select a Dataset row(s) and click the " +
        "        <b>Subset...</b> button. Optionally, select Subset information. Enter a Subset Name. Click " +
        "        the <b>Subscribe</b> button to subscribe to the subset. Click the <b>Query</b> button " +
        "        to perform an adhoc retrieval using the currently selected subset parameters.<br>" +
        "        <b>Set As Default</b><br>" +
        "        Set the current configuration settings to be the default " +
        "        settings for the table. When the table is opened the " +
        "        saved default settings are applied.<br>" +
        "        <b>Load Configuration...</b><br>" +
        "        Load a previously saved configuration file. Select from the Available " +
        "        Configuration Files list and click the <b>Load</b> button to load a configuration " +
        "        file. Use the <b>Preview</b> button to view the XML associated with the " +
        "        configuration file.<br>" +
        "        <b>Save Configuration</b><br>" +
        "        Save the current configuration settings to the most recently " +
        "        saved file name. The first time Save Configuration is accessed, " +
        "        select to save either at the User or Site level and enter a file name. " +
        "        Click the <b>Save</b> button to save the file. Use the Load Configuration " +
        "        menu item to load a saved configuration.<br>" +
        "        <b>Save Configuration As...</b><br>" +
        "        Save the current configuration settings to a named file " +
        "        that may be loaded in the future. In the Save Configuration dialog " +
        "        select to save either at the User or Site level and enter a file name. " +
        "        Click the <b>Save</b> button to save the file. Use the Load Configuration " +
        "        menu item to load a saved configuration.<br>" +
        "        <b>Refresh Table</b><br>" +
        "        Refresh the table data.<br>" +
        "        <b>Exit</b><br>" +
        "        Exit the table.<br><br>" +
        "   <dt>" +
        "       <a name='edit'><u><b>Edit</u></b><br>" +
        "       <dd>" +
        "        <b>Edit Subscription...</b><br>" +
        "        Highlight a row in the table to edit. Modify the Subscription Delivery Options, " +
        "        the Subscription Duration, the Subscription Active Period, the Subscription " +
        "        Priority or the additional Subscription Information. <br>Subscription Duration sets " +
        "        the start and expiration times for subscription delivery. Use the No Expiration " +
        "        radio button to continue to receive the subscription. <br>The Subscription Active " +
        "        Period may be used to select a specified time range for the subscription to be " +
        "        active. (Note: The active period does not include the year.) The Subscription " +
        "        Priority sets the order of subscription fullfillment and may be set to High (1), " +
        "        Default (2), or Low (3).<br>The Subscription Information allows the Description " +
        "        to be modified.  The Subscription Name may not be modified.<br> Click <b>OK</b> " +
        "        to perform the edit.<br>" +
        "        <b>Copy Subscription...</b><br>" +
        "        Highlight a row in the table to copy. Enter a New Subscription Name. Click " +
        "        <b>OK</b> to perform the copy.<br>" +
        "        <b>Delete Subscription...</b><br>" +
        "        Highlight a row(s) in the table to delete. Click <b>Yes</b> to perform the " +
        "        deletion.<br><br>" +
        "   <dt>" +
        "      <a name='settings'><u><b>Settings</u></b><br>" +
        "      <dd>" +
        "        <b>Configure Table...</b><br>" +
        "        The Table Column Configuration Settings allow a user to hide and make visible table " +
        "        columns. The list of Hidden Columns is located on the left and the Visible Columns list is " +
        "        located on the right. If columns are moved to the Visible Columns list, the corresponding " +
        "        columns will be displayed in the Notification Table. Use the right and left arrows to " +
        "        move column names back and forth from the Hidden Columns list to the Visible Columns list. " +
        "        Use the up and down arrows to change the order of the visible columns as seen in the table. " +
        "        (Note: Column names are not duplicated and appear only once either in the Hidden or Visible " +
        "        lists.) After making selections, click the <b>OK</b> button to configure the table. (Note: " +
        "        Unless the configuration is set to the default or saved, changes will be lost when the " +
        "        table is closed.)<br>" +
        "</dl>" +
        "</BODY></HTML>";
        
        return helpText;

     }

}
