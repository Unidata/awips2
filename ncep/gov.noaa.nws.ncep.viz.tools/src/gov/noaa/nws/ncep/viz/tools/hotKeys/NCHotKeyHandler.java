/*
 * NCHotKeyHandler
 * 
 *Date Created (01 October 2010)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.tools.hotKeys;

import java.util.List;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

 

/**
 * Hot Key handler to switch between tabs in the National Centers Perspective
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 *     Date        Ticket#	    Engineer	    Description
 * -------------------------------------------------------------------
 * 01-Oct-2010   289        Archana     Initial Creation
 * 02/01/2011    399        Chin        Modified to be generic hot
 *                                      key handler for all editors
 * 03/20/2013    972        Greg H.     use new NcDisplayName and NcDisplayMngr methods instead                                     
 *                                      of parsing the tab title.
 * 
 *</pre> 
 * @author archana
 *  @version 1.0
 *
 */
  public class NCHotKeyHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		if(event.getCommand() == null){
			return null;
		}
		
		Object appCntxt = event.getApplicationContext();
			
		if( appCntxt != null ) {
			
		}
		
		String tabKeyValue = event.getParameter("keyNum");

		if(tabKeyValue == null || tabKeyValue.isEmpty() ) {
			return null;
		}

		AbstractEditor ncDisp = NcDisplayMngr.findDisplayByID( 
				new NcDisplayName( Integer.parseInt(tabKeyValue), "N/A" ) );

		if( ncDisp != null ) {
			NcDisplayMngr.bringToTop( ncDisp );
		}

		return null;	
	}

 }