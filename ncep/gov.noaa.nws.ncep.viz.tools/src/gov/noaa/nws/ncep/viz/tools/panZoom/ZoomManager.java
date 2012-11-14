package gov.noaa.nws.ncep.viz.tools.panZoom;

import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

public class ZoomManager implements ISelectedPanesChangedListener {

    @Override
    public void selectedPanesChanged(String id, IDisplayPane[] panes) {

        IDisplayPaneContainer container = getContainer(panes[0]);

        for (IDisplayPane pane : panes) {
            IDescriptor desc = pane.getDescriptor();
            if (desc instanceof NCMapDescriptor) {
                if (((NCMapDescriptor) desc).getSuspendZoom()) {
                    ZoomUtil.disableZoom(container);
                    return;
                }
            }
        }

        ZoomUtil.enableZoom(container);

    }

    private IDisplayPaneContainer getContainer(IDisplayPane pane) {
        IDisplayPaneContainer container = null;

        AbstractEditor[] editors = UiUtil.getEditors(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow(), VizPerspectiveListener
                .getCurrentPerspectiveManager().getPerspectiveId());

        for (AbstractEditor ed : editors) {
            for (IDisplayPane aPane : ed.getDisplayPanes()) {
                if (aPane == pane) {
                    container = ed;
                    return container;
                }
            }
        }

        return null;
    }
}
