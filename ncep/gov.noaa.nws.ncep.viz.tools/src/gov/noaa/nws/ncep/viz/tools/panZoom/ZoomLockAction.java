package gov.noaa.nws.ncep.viz.tools.panZoom;

import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

// NOT IMPLEMENTED
public class ZoomLockAction extends AbstractHandler implements IElementUpdater {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
        NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();
        
        return null;
	}

	@Override
	public void updateElement(UIElement element, Map parameters) {
    	NCMapEditor activeDisplay = NmapUiUtils.getActiveNatlCntrsEditor();
    	
	}

}
