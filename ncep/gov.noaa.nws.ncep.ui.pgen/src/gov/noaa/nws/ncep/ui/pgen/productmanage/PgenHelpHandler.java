/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.PgenHelpHandler
 * 
 * October 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * Pops up the PGEN help dialog in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/09  		#151        J. Wu    	Initial creation. 
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */
public class PgenHelpHandler extends AbstractHandler {
		
	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    			
    	Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  		   	   	    
    	PgenHelpDialog dlg = PgenHelpDialog.getInstance( shell );
				
		if ( !(dlg.isOpen() ) ) {
			dlg.open();
		}        
    	
        return null;
    
    }
    
}




