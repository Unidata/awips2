package com.raytheon.uf.viz.datadelivery.notification;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.datadelivery.help.DataDeliveryHelp;

/**
 * Help dialog for the Notification Table.
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

public class NotificationHelpDlg extends DataDeliveryHelp {

    
    /**
     * Constructor.
     * @param parentShell 
     *          Parent shell.         
     */

    public NotificationHelpDlg(Shell parentShell) {

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
    	
        String helpText = "<HTML><HEAD><TITLE>Notification Table Help</TITLE></HEAD><BODY>" +
        "The Notification Center Table allows a user to view and delete " +
        "data delivery notifications.<br><br>" +
//        "<a href='#file'><u><b>File</b></u><br></a>" +
//        "<a href='#edit'><u><b>Edit</b></u><br></a>" +
//        "<a href='#settings'><u><b>Settings</b></u><br></a><br>" +
        "<dl>" +
        "   <dt>" +
        "      <a name='file'><u><b>File</b></u><br>"+
        "      <dd>" +
        "         <b>Set As Default</b><br>" +
        "         Set the current configuration settings to be the default " +
        "         settings for the table. When the table is opened the " +
        "         saved default settings are applied.<br>" +
        "         <b>Load Configuration...</b><br>" +
        "         Load a previously saved configuration file. Select from the Available " +
        "         Configuration Files list and click the <b>Load</b> button to load a configuration " +
        "         file. Use the <b>Preview</b> button to view the XML associated with the " +
        "         configuration file.<br>" +
        "         <b>Save Configuration</b><br>" +
        "         Save the current configuration settings to the most recently " +
        "         saved file name. The first time Save Configuration is accessed, " +
        "         select to save either at the User or Site level and enter a file name. " +
        "         Click the <b>Save</b> button to save the file. Use the Load Configuration " +
        "         menu item to load a saved configuration.<br>" +
        "         <b>Save Configuration As...</b><br>" +
        "         Save the current configuration settings to a named file " +
        "         that may be loaded in the future. In the Save Configuration dialog " +
        "         select to save either at the User or Site level and enter a file name. " +
        "         Click the <b>Save</b> button to save the file. Use the Load Configuration " +
        "         menu item to load a saved configuration.<br>" +
        "         <b>Delete Configuration...</b><br>" +
        "         Delete a saved configuration file.<br>" +
        "         <b>Exit</b><br>" +
        "         Exit the table.<br><br>" +
        "   <dt>" +
        "       <a name='edit'><u><b>Edit</u></b><br>" +
        "       <dd>" +
        "         <b>Find...</b><br>" +
        "         Find and highlight items in the table using the Find dialog. The " +
        "         Case Sensitive option allows a user to search text that is capitalization " +
        "         sensitive. The Exclude option allows a user to search the table for " +
        "         items that exclude the entered text. The Case Sensitive and Exclude options " +
        "         may be used in conjunction with another. The Column Selection grouping allows " +
        "         users to search a specific column in the table. After the search criteria " +
        "         has been selected, click the <b>Find Next</b> button to perform the search. The " +
        "         <b>Hightlight All</b> button highlights all the rows matching the find " +
        "         criteria. Close the Find dialog using the <b>Close</b> button.<br>" +
        "          <b>Delete by Priority</b><br>" +
        "         Delete all rows in the table having a specific priority. (Note: Notifications " +
        "         only deleted from the user view and not permanently deleted from the database.)<br>" +
        "          <b>Delete Older Than Selected </b><br>" +
        "         Delete all rows in the table having a date in the Time column that is older " +
        "         than the currently highlighted row. (Note: Notifications <br>" +
        "         only deleted from the user view and not permanently deleted from the database.)<br>" +
        "         <b>Delete Notification(s)</b><br>" +
        "         Delete the currently highlighted row(s) in the table. (Note: Notifications " +
        "         only deleted from the user view and not permanently deleted from the database.)<br><br>" +
        "   <dt>" +
        "      <a name='settings'><u><b>Settings</u></b><br>" +
        "      <dd>" +
        "         <b>Configure Table...</b><br>" +
        "         The Initial Startup Configuration items are set when the table is opened. " +
        "         Check the <b>Load All Messages</b> check box to display all messages in the table that " +
        "         are contained in the database. Enter a number of messages or hours amount in the " +
        "         <b>Load Last</b> spinner to select the amount of messages or number of hours of messages " +
        "         to display in the table. Set the <b>Initial Sort Column</b> and either <b>Sort Ascending</b> " +
        "         or <b>Sort Descending</b> to sort the table by a specific colum upon opening. " +
        "         <br>The Display Configuration items refresh the table upon clicking <b>OK</b>. Select the " +
        "         number of <b>Rows Per Page</b> using the selection drop down. Select the image setting " +
        "         for the Priority table column using the radio buttons. Select to make columns " +
        "         hidden or visible using the arrows provided. After making selections, click the <b>OK</b>" +
        "         button to configure the table. (Note: Unless the configuration is set to the default " +
        "         or saved, changes will be lost when the table is closed.)<br>" +
        "         <b>Filter Table...</b><br>" +
        "         A user may filter the table by user, by subscription or by priority.<br>In the Filter by " +
        "         User section, the list of available user names are in the Available Users column. If users are moved " +
        "         to the Selected Users list, the corresponding columns containing those user names will be " +
        "         displayed in the Notification Table. Use the arrows to move users back and forth from the " +
        "         Available Users list to the Selected Users list. (Note: User names are not duplicated and " +
        "         appear only once either in the Available or Selected lists.) The Always include my notifications " +
        "         check box may be used to keep the currently logged in user in the Selected Users column. " +
        "         <br>In the Filter by Subscription section, the list of available subscription names are in the " +
        "         Available Subscriptions column. If subscriptions are moved to the Selected Subscriptions list " +
        "         the corresponding columns containing those subscription names will be displayed in the " +
        "         Notification Table. Use the arrows to move subscriptions back and forth from the " +
        "         Available Subscriptions list to the Selected Subscriptions list. (Note: Subscription " +
        "         names are not duplicated and appear only once either in the Available or Selected lists.)" +
        "         <br>In the Filter by Priority section, select which columns containing specific priorities to " + 
        "         display using the checkboxes." +
        "</dl>" +
        "</BODY></HTML>";
        
        return helpText;

     }

}

