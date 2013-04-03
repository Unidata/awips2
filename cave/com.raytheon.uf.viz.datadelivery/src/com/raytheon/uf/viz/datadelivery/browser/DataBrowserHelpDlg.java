package com.raytheon.uf.viz.datadelivery.browser;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.datadelivery.help.DataDeliveryHelp;

/**
 * Help dialog for the Data Discover Browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2012   645      jpiatt     Initial creation.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */

public class DataBrowserHelpDlg extends DataDeliveryHelp {

    /**
     * Constructor.
     * @param parentShell 
     *          Parent shell.         
     */
    public DataBrowserHelpDlg(Shell parentShell) {

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
    	
        String helpText = "<HTML><HEAD><TITLE>Dataset Discovery Browser Help</TITLE></HEAD><BODY>" +
        "The Dataset Discovery Browser allows a user to find Datasets using " +
        "filtering options.<br>First, select an Area for the Areal Coverage. Optionally, use" +
        "the Set Area to select an Area. Next, select an available Data Type. Move the data type to " +
        "the selected column using the arrows.  Next, select from the filtering options.  Filters " +
        "include Data Provider, Data Set, Level or Parameter. Click the arrows to the left of the " +
        "Filter titles to expand the filter selection sections. After Filters are selected, click " +
        "<b>Update Results</b> to load the list of Datasets in the table. Select a Dataset and click the " +
        "<b>Subset...</b> button. Optionally, select Subset information. Enter a Subset Name. Click " +
        "the <b>Subscribe</b> button to subscribe to the subset. Click the <b>Query</b> button " +
        "to find details about the subset." +
        "<dl>" +
        "   <dt>" +
        "      <br><u><b>File</b></u><br>" +
        "      <dd><b>New Configuration</b><br>" +
        "        Clear the current configuration settings.<br>" +
        "      <b>Load Configuration...</b><br>" +
        "        Load a previously saved configuration file. Select from the Available " +
        "        Configuration Files list and click the <b>Load</b> button to load a configuration " +
        "        file. Use the <b>Preview</b> button to view the XML associated with the " +
        "        configuration file.<br>" +
        "      <b>Save Configuration</b><br>" +
        "        Save the current configuration settings to the most recently " +
        "        saved file name. The first time Save Configuration is accessed, " +
        "        select to save either at the User or Site level and enter a file name. " +
        "        Click the <b>Save</b> button to save the file. Use the Load Configuration " +
        "        menu item to load a saved configuration.<br>" +
        "     <b>Save Configuration As...</b><br>" +
        "        Save the current configuration settings to a named file " +
        "        that may be loaded in the future. In the Save Configuration dialog " +
        "        select to save either at the User or Site level and enter a file name. " +
        "        Click the <b>Save</b> button to save the file. Use the Load Configuration " +
        "        menu item to load a saved configuration.<br>" +
        "      <b>Delete Configuration...</b><br>" +
        "        Delete a saved configuration file.<br>" +
        "      <b>Exit</b><br>" +
        "        Exit the table.<br><br>" +
        "</dl>" +
        "</BODY></HTML>";
        
        return helpText;

     }
}
