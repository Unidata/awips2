package gov.noaa.nws.ncep.viz.tools.conditionalfilter;

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
 * April 2012   #615        S. Gurung       Initial Creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 * 
 */
public class ConditionalFilterMngrAction extends AbstractHandler {

	private static ConditionalFilterMngrDialog conditionalFilterMngr;
		
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	
    	try {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		if( conditionalFilterMngr == null ) 
			conditionalFilterMngr = new ConditionalFilterMngrDialog(shell);
		
		if( !conditionalFilterMngr.isOpen() ) 
			conditionalFilterMngr.open();
    	}
    	finally {
    		conditionalFilterMngr = null;    		
    	}
		
        return null;
    }
}
