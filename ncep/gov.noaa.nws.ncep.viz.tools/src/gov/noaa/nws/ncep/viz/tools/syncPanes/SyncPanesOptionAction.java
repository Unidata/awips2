
package gov.noaa.nws.ncep.viz.tools.syncPanes;

import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import java.io.File;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

/**
 * Option to set the behaviour for multi-paned displays
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/25/09      #169        Greg Hull    created
 * 10/27/09      #180        Greg Hull    Moved out of perspectives project
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class SyncPanesOptionAction extends AbstractHandler 
                      implements IElementUpdater {

	private static boolean syncPanes = false;
	
	static boolean arePanesSynced() {
		return syncPanes;
	}
	
    public SyncPanesOptionAction() {
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();
        if( currEditor.getNumberofPanes() > 1 )	{
        	setSynced( !syncPanes );            
        	currEditor.setGeoSyncPanesEnabled( syncPanes );
        }
        else {
        	System.out.println("Can not Geo-Syncronize the active display "+
    						   "because it is not a multi-pane display." );
        }

        return null;
    }

    public void setSynced( boolean s ) {
        syncPanes = s;
    }

    @Override
    public void updateElement(UIElement element, Map parameters) {
    	NCMapEditor activeDisplay = NmapUiUtils.getActiveNatlCntrsEditor();
    	
    	if( activeDisplay != null ) {
    		syncPanes = activeDisplay.arePanesGeoSynced();	
    		element.setChecked( syncPanes );	
    	}
    }
}
