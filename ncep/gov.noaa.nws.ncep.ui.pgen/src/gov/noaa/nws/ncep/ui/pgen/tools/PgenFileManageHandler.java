/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenFileManageHandler
 * 
 * 11 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.controls.PgenFileManageDialog;
import gov.noaa.nws.ncep.ui.pgen.controls.PgenFileManageDialog1;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Define a handler for PGEN file open/save controls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/09		#63			J. Wu   	Initial Creation.
 * 04/09		#103		B. Yin		Extends from AbstractPgenTool
 * 08/09		#335		J. Wu		Redefined "Save"/"Save As"/"Save All".
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 */
public class PgenFileManageHandler extends AbstractTool {
	
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        
    	Shell shell = new Shell( SWT.SHELL_TRIM ); /*RESIZE | SWT.PRIMARY_MODAL);*/
        String btnClicked = (String)event.getApplicationContext();
        
    	//Set "active" icon for the palette button corresponding to this tool
        String btnName = event.getParameter( "name" );
    	PgenSession.getInstance().getPgenPalette().setActiveIcon( btnName );
    	
    	String curFile = PgenSession.getInstance().getPgenResource().getActiveProduct().getOutputFile();    	
    	
    	if ( curFile != null && btnClicked.equalsIgnoreCase( "Save" ) ) {
    		PgenSession.getInstance().getPgenResource().saveCurrentProduct( curFile );
    	}
    	else if ( curFile != null && btnClicked.equalsIgnoreCase( "Save All" ) ) {   			  
    	    if ( PgenSession.getInstance().getPgenResource().getProducts().size() > 1 ) {
    		    PgenSession.getInstance().getPgenResource().saveAllProducts();
    	    }
    	    else {
    	    	PgenSession.getInstance().getPgenResource().saveCurrentProduct( curFile );
    	    }
    	}
    	else {    // "Save As"
//    		PgenFileManageDialog file_dlg = null;
    		PgenFileManageDialog1 file_dlg = null;

    		if ( file_dlg == null ) {
    			try {	
    				file_dlg = new PgenFileManageDialog1( shell, btnClicked );   
    				file_dlg.setBlockOnOpen(true);
    			}
    			catch (VizException e) {
    				e.printStackTrace();
    			}  
    		}

    		if ( file_dlg != null )  file_dlg.open();       
    	}
    	
    	//Reset the original icon for the palette button corresponding to this tool        
    	if ( PgenSession.getInstance().getPgenPalette() != null ) {
            PgenSession.getInstance().getPgenPalette().resetIcon(btnName);
    	}
    
    	return null;
    }

}