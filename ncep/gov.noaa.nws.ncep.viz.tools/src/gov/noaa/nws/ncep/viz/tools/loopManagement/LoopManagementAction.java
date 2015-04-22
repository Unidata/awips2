package gov.noaa.nws.ncep.viz.tools.loopManagement;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * 
 * Opens Loop Management dialog
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date       	    Ticket#		Engineer	Description
 *  ------------	----------	-----------	--------------------------
 *  Sept 28, 2010   317          X. Guo     Initial Creation.
 *  
 * </pre>
 * 
 * @author xguo
 * @version 1
 */
public class LoopManagementAction extends AbstractTool {

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
	 */
	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException {
		super.execute(arg0);

		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
				.getShell();
		
		LoopManagementDialog lpd = new LoopManagementDialog(shell);
		lpd.open();
		
		return null;
	}

}
