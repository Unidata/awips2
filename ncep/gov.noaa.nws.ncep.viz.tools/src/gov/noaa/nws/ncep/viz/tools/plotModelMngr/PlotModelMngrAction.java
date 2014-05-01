package gov.noaa.nws.ncep.viz.tools.plotModelMngr;

import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;


/**
 * Popup Point data display manager dialog in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2009  	        	M. Li    	Initial creation. 
 * Dec. 2009                Greg Hull   change name to plot model editor. Don't refresh mapEditor
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */
public class PlotModelMngrAction extends AbstractHandler {

	private static PlotModelMngrDialog plotModelMngrDialog;
		
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	
    	try {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		if( plotModelMngrDialog == null ) 
			plotModelMngrDialog = new PlotModelMngrDialog(shell);
		
		if( !plotModelMngrDialog.isOpen() ) 
			plotModelMngrDialog.open();
    	}
    	finally {
            plotModelMngrDialog = null;    		
    	}
		
        return null;
    }
}
