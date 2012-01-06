/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * May 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.wipe;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Shell;

/**
 * The dialog class for unloading all but overlay data;
 * this class may be not necessary since we can directly
 * use jface.dialogs.MessageDialog in the Action class.
 * 
 * The reason of using it is in the future, more complicated 
 * clear action may be required; then a true dialog can be
 * easily implemented.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/10		#265		G. Zhang   	Initial Creation.
 *
 * </pre>
 * 
 * @author	G. Zhang
 */
public class WipeDialog extends Dialog{
	
	private Shell parent = null;
	
	private static final String TITLE = "Clear Map Data Confirmation";
	private static final String MESSAGE = "Do You Really Want To Remove All Data On The Map ?";
	
	/**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public WipeDialog(Shell parent){
        super(parent);
        this.parent = parent;
    }
    
    /**
     * Open method used to display the confirmation message.
     * 
     * @return Return object (can be null).
     */
    public Object open() {

    	boolean f = MessageDialog.openConfirm(parent, TITLE, MESSAGE);
    	
    	return f;
    }

}
