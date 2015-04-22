package gov.noaa.nws.ncep.viz.tools.frame;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The dialog class for marking bad frames;
 * this class may be not necessary since we can directly
 * use jface.dialogs.MessageDialog in the Action class.
 * 
 * However, in the future, more complicated actions may be required; 
 * then a true dialog can be easily implemented.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/10		#309		G. Zhang   	Initial Creation.
 *
 * </pre>
 * 
 * @author	G. Zhang
 */

public class BadFrameMngDlg  extends Dialog{
	
	private Shell parent = null;
	
	private static final String TITLE = "Bad Frame Confirmation";
	private static final String MESSAGE = "Do you really want to tag this as a bad frame ?";
	
	/**
     * Constructor.
     * 
     * @param parent: Parent shell.
     */
    public BadFrameMngDlg(Shell parent){
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
