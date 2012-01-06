package gov.noaa.nws.ncep.viz.localization.command.handler;

//import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.IHandlerListener;
//import org.eclipse.core.commands.IHandler;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.tools.AbstractTool;

import gov.noaa.nws.ncep.viz.localization.ui.dialog.SyncFileToolDialog;

//public class SyncFileToolCommandHandler extends AbstractHandler implements IHandler {
public class SyncFileToolCommandHandler extends AbstractTool implements IHandler {

	private SyncFileToolDialog syncFileToolDialog; 
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

    	/*
    	 * The following is the section for prototyping authentication login dialog
    	 */
    	if(syncFileToolDialog == null) {
    		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    		syncFileToolDialog = new SyncFileToolDialog(window.getShell()); 
    	}
    	if(!syncFileToolDialog.isDialogOpen()) {
    		syncFileToolDialog.open(); 
    		syncFileToolDialog = null; 
//    		deactivate(); 
//    		syncFileToolDialog.setDialogOpen(true); 
    	}
    	
		return null;
	}

//    public void deactivate() {    
//    	if(syncFileToolDialog != null) {
//    		syncFileToolDialog.close(); 
//    		syncFileToolDialog = null; 
//    	}
//    }

}

