package com.raytheon.viz.warnings.ui;

import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.warnings.rsc.AbstractWWAResource;
import com.raytheon.viz.warnings.rsc.WarningsResource;
import com.raytheon.viz.warnings.rsc.WatchesResource;

/**
 * This action shows an option in the resource menu that opens a new
 * dialog which allows the users to specify display properties for 
 * the WWA resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer          Description
 * ------------ ---------- ----------------  --------------------------
 * Mar 15, 2022            srcarter@ucar     Initial creation
 * 
 * </pre>
 * 
 * @author srcarter
 */

public class WWADrawingPropertiesAction extends AbstractRightClickAction {

	@Override
	public String getText() {
		return "Open Drawing Properties...";
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.action.Action#run()
	 * 
	 * Create and assign a new DrawingPropertiesDialog to the associated
	 * resource if it does not already have a dialog.  If it has a dialog
	 * proceed with the existing one and show the dialog.
	 */
	@Override
	public void run() {
	 	AbstractWWAResource rsc = (AbstractWWAResource)getSelectedRsc();
	 	
	 	DrawingPropertiesDialog dialog = rsc.getDrawingDialog();
	 	
	 	if(dialog == null){
	 		dialog = new DrawingPropertiesDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), rsc);
	 		rsc.setDrawingDialog(dialog);
	 	}
	 	dialog.open();
	}
	
	/* (non-Javadoc)
	 * @see com.raytheon.viz.ui.cmenu.AbstractRightClickAction#isHidden()
	 * 
	 * Only display the drawing properties dialog or WarningsResource and
	 * WatchesResource.  Will not display for CWASPS resources.
	 */
	@Override
	public boolean isHidden(){
	 	AbstractVizResource rsc = getSelectedRsc();
	 	if(rsc instanceof WarningsResource || rsc instanceof WatchesResource){
	 		return false;
	 	}
	 	return true;
	}
	
}
