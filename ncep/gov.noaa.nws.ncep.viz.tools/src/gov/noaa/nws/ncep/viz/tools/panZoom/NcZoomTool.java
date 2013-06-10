package gov.noaa.nws.ncep.viz.tools.panZoom;

import java.util.HashMap;
import java.util.Map;

import gov.noaa.nws.ncep.viz.ui.display.AbstractNcModalTool;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
//import com.raytheon.viz.ui.tools.nav.ZoomHandler;

/**
 * 
 * <pre>
 *  
 *   SOFTWARE HISTORY
 *  
 * Date         Ticket#         Engineer        Description
 * ------------ ----------      -----------     --------------------------
 * 03/16/11      migration       Greg Hull         Initial Creation
 *   
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcZoomTool extends AbstractNcModalTool {
    /** The mouse handler */
    private NcZoomHandler currentHandler;

    @Override
    protected void activateTool() {
        currentHandler = NcZoomHandler.getInstance(editor);
        editor.registerMouseHandler(currentHandler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
        if (editor != null) {
            editor.unregisterMouseHandler(currentHandler);
        }
    }

}
