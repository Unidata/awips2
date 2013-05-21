package gov.noaa.nws.ncep.viz.resourceManager.ui.loadRbd;

import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.CreateRbdControl;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneLayout;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 *  Main Dialog to manage and load RBDs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/16/11      #408       Greg Hull    Created
 * 02/22/2013    #972       Greg Hull    work with AbstractRBD
 *                                       
 * </pre>
 * 
 * @author ghull 
 * @version 1
 */

public class EditRbdDialog extends Dialog {

    private Shell shell;
    private boolean isOpen = false;

    private RscBundleDisplayMngr rbdMngr = null;
    
    private CreateRbdControl createRbdCntr = null;
    
	public EditRbdDialog( Shell parShell, AbstractRBD<?> seldRbd ) throws VizException {
    	super(parShell);

    	rbdMngr = new RscBundleDisplayMngr( new NcPaneLayout(6,6), seldRbd.getDisplayType() );

		shell = new Shell( parShell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL );
		shell.setText( "Edit RBD "+seldRbd.getRbdName() );

		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 1;
		mainLayout.marginWidth = 1;
		shell.setLayout( mainLayout );
		
		shell.setLocation( parShell.getLocation().x+20, parShell.getLocation().y + 20 );
				
		rbdMngr.initFromRbdBundle( seldRbd );
		
		createRbdCntr = new CreateRbdControl( shell, rbdMngr );

		createRbdCntr.configureForEditRbd();

		GridData gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		
		createRbdCntr.setLayoutData(gd);
				
    	shell.setMinimumSize(400,320);
    	
    	shell.pack();

    	createRbdCntr.updateDialog();	
	}
	
	
   	public boolean isOpen() {
        return isOpen; // shell != null && !shell.isDisposed();
    }

    public AbstractRBD<?> open() {
    	Shell parent = getParent();
    	Display display = parent.getDisplay();

    	shell.open();

    	isOpen = true;

    	// Don't let the user do anything else while the Edit Rbd Dialog is up.
    	parent.setEnabled(false);
    	
    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}
    	parent.setEnabled(true);

    	isOpen = false;
    	
    	return createRbdCntr.getEditedRbd();    	
    }
}
