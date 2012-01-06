package gov.noaa.nws.ncep.viz.tools.locatorOptions;


import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;


/**
 * Popup the Default Locator Options dialog in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  07/09/09    138        Greg Hull            Initial creation. 
 * 11/24/09                Greg Hull    migrate to to11d6
 *
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 * 
 */
public class LocatorOptionsAction extends AbstractHandler {
        protected LocatorOptionsDialog id;
        public Object execute(ExecutionEvent arg0) throws ExecutionException {
        	NCMapEditor mapEditor = NmapUiUtils.getActiveNatlCntrsEditor();

        	Shell shell = NmapUiUtils.getCaveShell();

        	if( id == null ) 
        		id = new LocatorOptionsDialog(shell);
        	
        	if( !id.isOpen() ) 
        		id.open();
        	
        	mapEditor.refresh();

        	return null;
    }
}
