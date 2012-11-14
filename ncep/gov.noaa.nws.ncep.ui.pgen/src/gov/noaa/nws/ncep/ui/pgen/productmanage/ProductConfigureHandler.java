/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.ProductConfigureHandler
 * 
 * August 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * Pops up PGEN product type configuration dialog in National Centers perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09  		#151        J. Wu    	Initial creation. 
 * 07/12  		#822        J. Wu    	Ensure PGEN palette is activated. 
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */
public class ProductConfigureHandler extends AbstractHandler {
		
	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    			
    	if ( PgenSession.getInstance().getPgenPalette() == null ) {
        	MessageDialog confirmDlg = new MessageDialog( 
                		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
                		"PGEN Palette Needed", null, 
                		"Please Activate the PGEN Palette First",
                		MessageDialog.WARNING, new String[]{"OK"}, 0 );
                
            confirmDlg.open();
            	
            return null;
        }
   	
    	Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  		   	   	    
    	ProductConfigureDialog dlg = ProductConfigureDialog.getInstance( shell );
				
		if ( !(dlg.isOpen() ) ) {
			dlg.open();
		}        
    	
        return null;
    
    }
    
}



