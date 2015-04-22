package gov.noaa.nws.ncep.viz.tools.newEditors;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.common.ui.UserEntryDialog;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.NcMapRBD;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
 * 02/15/2012     #627     Archana     Updated the call to addRbd() to accept 
 *                                     a NCMapEditor object as one of the arguments
 * 08/16/2012     #655     B. Hebbard  (TTR 382) Run rbdLoader sync instead of async,
 *                                     to ensure descriptor has valid xforms before paint
 * 02/13/13       #972     G. Hull     AbstractRBD
 * 03/14/13                G. Hull     changed to return the new editor.
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
		String newDisplayName="Error getting default RBD Name";
		try {
			newDisplayName = NcMapRBD.getDefaultRBD().getRbdName();
		} catch (VizException e) {
		} 
		
		if( promptForName == null || promptForName.equalsIgnoreCase( "true" ) ) {
			
			// pop up a dialog to prompt for the new name
			UserEntryDialog entryDlg = new UserEntryDialog( NcDisplayMngr.getCaveShell(),
					"Display Name", 
					"Enter the name of the new Display:", 
					newDisplayName );

			newDisplayName = entryDlg.open();

			if( newDisplayName == null || // cancel pressed
					newDisplayName.isEmpty() ) {
				return null;
			}
		}

		AbstractEditor ed = createNewDefaultDisplay( newDisplayName );
		
		return (Object)ed;
	}

	public static AbstractEditor createNewDefaultDisplay( String newDisplayName ) {
		try {
    		if( newDisplayName == null || newDisplayName.isEmpty() ) {
    			newDisplayName = NcMapRBD.getDefaultRBD().getRbdName();
    		}
    		
    		AbstractEditor editor = NcDisplayMngr.createNatlCntrsEditor( 
    				NcDisplayType.NMAP_DISPLAY, newDisplayName  );
    		
    		ResourceBndlLoader rbdLoader = new ResourceBndlLoader( "Loading RBD: "+ newDisplayName );
    		
			rbdLoader.addDefaultRBD( NcDisplayType.NMAP_DISPLAY, editor );
			
    		//TODO -- is there a way we still run async, w/o reintroducing TTR 382?
    		// VizApp.runAsync( rbdLoader );
    		VizApp.runSync( rbdLoader );
    		
    		// Note that the new editor will be returned before
    		// the rbd is loaded into it. 
    		return editor;
    	}
    	catch ( Exception ve ) {
    		System.out.println("Could not load rbd: " + ve.getMessage());
    		ve.printStackTrace();
    		return null;
    	}       	
	}
}
