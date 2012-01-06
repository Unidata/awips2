package gov.noaa.nws.ncep.viz.resourceManager.ui;


import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.editor.AbstractEditor;

import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.resourceManager.Activator;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;


/**
 * Dialog to define resource bundles
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 11/11/08		  24		Greg Hull		Created		
 * 03/23/09       85        Greg Hull      Use Shell from Current Editor to make modal
 * 06/22/09       115       Greg Hull      Use SWT version of dialog
 * 
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class ResourceManagerAction extends AbstractHandler {

	static  RscBundleDisplayMngr rbdMngr = new RscBundleDisplayMngr();
	static  ResourceManagerDialog id = null;
	
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {  
        String mode = arg0.getParameter( "mode" );

    	if( id != null ) {
    		// if the Dialog is already up, then bring to the front but don't change the tab.
    		//
    		// TODO: best solution is to only do this if this is the hotKey. If this is
    		// a command from the main menu, then the user probably wants the tab that they
    		// selected.
    		// 
    		id.setActiveTab( "" );
    		return null;
    	}
        try {
            id = new ResourceManagerDialog( NmapUiUtils.getCaveShell(),
            		                        "Resource Manager", rbdMngr, mode );    
            id.open();
            id = null;

        } catch (VizException e) {
            Status status = new Status(Status.ERROR, Activator.PLUGIN_ID, 0,
                    "Resource Mngr Error.", e);
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "ERROR", "Resource Mngr Dlg Error",
                    status);
            throw new ExecutionException(
                    "Resource Mngr Dlg Error", e);
        }
        finally {
        	id = null;
        }
        return null;
    }
}
