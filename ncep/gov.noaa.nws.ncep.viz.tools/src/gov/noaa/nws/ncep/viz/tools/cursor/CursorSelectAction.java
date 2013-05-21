package gov.noaa.nws.ncep.viz.tools.cursor;

import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.editor.AbstractEditor;


/**
 * Popup Logos display controls dialog in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 2009  	105        M. Li    	Initial creation. 
 * 09/09        #169       G. Hull      NCMapEditor
 * 02/11/13      #972        G. Hull     AbstractEditor instead of NCMapEditor
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */
public class CursorSelectAction extends AbstractHandler {

	protected CursorSelectDialog id;
		
	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	
    	AbstractEditor mapEditor = NcDisplayMngr.getActiveNatlCntrsEditor();

        /*
         * Pop up Seek result window
         */
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		if (id == null) id = new CursorSelectDialog(shell);
		if (!(id.isOpen())) id.open();
        mapEditor.refresh();
		
        return null;
    }
    
}
