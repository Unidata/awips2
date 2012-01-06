package gov.noaa.nws.ncep.viz.tools.logos;

import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;


/**
 * Popup Logos display controls dialog in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 2009  	105        M. Li    	Initial creation. 
 * Nov  2009               G. Hull      Migrate to to11d6
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */
public class LogosDisplayAction extends AbstractHandler {

	protected LogosDisplayControlDialog id;
		
	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	
    	NCMapEditor mapEditor = NmapUiUtils.getActiveNatlCntrsEditor();

        /*
         * Pop up Seek result window
         */
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		if (id == null) id = new LogosDisplayControlDialog(shell);
		if (!(id.isOpen())) id.open();
        mapEditor.refresh();
		
        return null;
    }
    
}
