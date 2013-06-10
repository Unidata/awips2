
package gov.noaa.nws.ncep.viz.tools.syncPanes;

import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import java.io.File;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Option to set the behaviour for multi-paned displays
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/25/09      #169        Greg Hull    created
 * 10/27/09      #180        Greg Hull    Moved out of perspectives project
 * 02/11/13      #972        G. Hull       AbstractEditor instead of NCMapEditor
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
    	AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
        if( currEditor.getDisplayPanes().length > 1 )	{
        	setSynced( !syncPanes );            
        	NcEditorUtil.setGeoSyncPanesEnabled( currEditor, syncPanes );
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
    	AbstractEditor activeDisplay = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	if( activeDisplay != null ) {
    		syncPanes = NcEditorUtil.arePanesGeoSynced( activeDisplay );	
    		element.setChecked( syncPanes );	
    	}
    }
}
