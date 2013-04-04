package gov.noaa.nws.ncep.viz.tools.newEditors;

import gov.noaa.nws.ncep.viz.common.ui.UserEntryDialog;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.VizApp;

/**
 * from the main menu, create a new display/editor and load 
 * it with the default RBD.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/20/11                Greg Hull   change name to plot model editor. Don't refresh mapEditor
 * 08/12/11       #450     Greg Hull   use the RBD name instead of the filename. 
 * 02/15/2012     #627     Archana    Updated the call to addRbd() to accept 
 *                                    a NCMapEditor object as one of the arguments   
 * 08/16/2012     #655     B. Hebbard  (TTR 382) Run rbdLoader sync instead of async,
 *                                     to ensure descriptor has valid xforms before paint
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 * 
 */

public class NewDisplayAction extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
        String promptForName = event.getParameter( "promptForName" );

		// create a unique display name.
		//
		File rbdFile = NcPathManager.getInstance().getStaticFile( 
				         NcPathConstants.DFLT_RBD );
		
		String newDisplayName = null; 
		
		if( promptForName.equalsIgnoreCase( "true" ) ) {
			
			// pop up a dialog to prompt for the new name
			UserEntryDialog entryDlg = new UserEntryDialog( NmapUiUtils.getCaveShell(),
					"Display Name", 
					"Enter the name of the new Display:", 
					newDisplayName );

			newDisplayName = entryDlg.open();

			if( newDisplayName == null || // cancel pressed
					newDisplayName.isEmpty() ) {
				return null;
			}
		}

		createNewDefaultDisplay( newDisplayName );
		
		return null;
	}

	public static void createNewDefaultDisplay( String newDisplayName ) {
    	try {
    		RbdBundle rbd = RbdBundle.getDefaultRBD();
    		
    		rbd.resolveLatestCycleTimes(); // shouldn't be needed  but just in case
    		
    		if( newDisplayName == null || newDisplayName.isEmpty() ) {
    			newDisplayName = rbd.getRbdName();
    		}
    		
    		NCMapEditor editor = NmapUiUtils.createNatlCntrsEditor( newDisplayName );
   // 		rbd.setNcEditor( editor );
    		
    		ResourceBndlLoader rbdLoader = new ResourceBndlLoader( "Loading RBD: "+rbd.getRbdName() );
    		
    		rbdLoader.addRBD( rbd, editor );
    		//TODO -- is there a way we still run async, w/o reintroducing TTR 382?
    		// VizApp.runAsync( rbdLoader );
    		VizApp.runSync( rbdLoader );
    	}
    	catch ( Exception ve ) {
    		System.out.println("Could not load rbd: " + ve.getMessage());
    		ve.printStackTrace();
    	}   
		return;		
	}
}
