/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenRestoreHandler
 * 
 * 31 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.controls.PgenRestoreDialog;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Define a handler for PGEN restore controls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/09		#158		s. gilbert  Initial Creation.
 *
 * </pre>
 * 
 * @author	s. gilbert
 * @version	0.0.1
 */
public class PgenRestoreHandler extends AbstractTool {
	
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

    	super.execute(event);
        
    	Shell shell = new Shell( SWT.SHELL_TRIM | SWT.PRIMARY_MODAL); 
        
    	//  Set "active" icon for the palette button corresponding to this tool
        String btnName = event.getParameter("name");
    	PgenSession.getInstance().getPgenPalette().setActiveIcon(btnName);
        
        PgenRestoreDialog restoreDlg = null;
        
        if ( restoreDlg == null ) {
       	    try {	
       	        restoreDlg = new PgenRestoreDialog( shell );        		
       	    }
            catch (VizException e) {
                e.printStackTrace();
            }  
        }
        
        if ( restoreDlg != null )  restoreDlg.open();
        
    	//  Reset the original icon for the palette button corresponding to this tool
    	PgenSession.getInstance().getPgenPalette().resetIcon(btnName);
      
    	return null;
    }

}