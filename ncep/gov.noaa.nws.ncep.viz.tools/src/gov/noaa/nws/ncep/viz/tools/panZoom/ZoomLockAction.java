package gov.noaa.nws.ncep.viz.tools.panZoom;

import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.viz.ui.editor.AbstractEditor;

// NOT IMPLEMENTED
public class ZoomLockAction extends AbstractHandler implements IElementUpdater {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
        
        return null;
	}

	@Override
	public void updateElement(UIElement element, Map parameters) {
		AbstractEditor activeDisplay = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
	}

}
