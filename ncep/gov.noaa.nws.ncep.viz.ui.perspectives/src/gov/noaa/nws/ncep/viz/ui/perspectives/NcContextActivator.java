package gov.noaa.nws.ncep.viz.ui.perspectives;

import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.perspectives.AbstractWorkbenchPartContextActivator;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *               #972       G. Hull    Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

public class NcContextActivator extends AbstractWorkbenchPartContextActivator {

    NcContextActivator(IWorkbenchPage page) {
        super(page);
    }

    @Override
    protected boolean isPerspectivePart(IWorkbenchPartReference partRef) {
        if (partRef != null) {
            IWorkbenchPart part = partRef.getPart(false);
            if (part instanceof IDisplayPaneContainer) {
                for (IDisplayPane pane : ((IDisplayPaneContainer) part)
                        .getDisplayPanes()) {

                	if (pane.getRenderableDisplay() instanceof NCMapRenderableDisplay) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}
