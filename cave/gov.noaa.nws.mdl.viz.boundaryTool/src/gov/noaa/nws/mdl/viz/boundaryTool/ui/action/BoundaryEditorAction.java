package gov.noaa.nws.mdl.viz.boundaryTool.ui.action;

import gov.noaa.nws.mdl.viz.boundaryTool.ui.layer.BoundaryEditorLayer;

import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;

/**
 * @author Mamoudou Ba
 * @version 1.0
 * 
 *          April 2011: Substantially modified from A2 "TimeOfArrivalAction"
 *          class
 */

public class BoundaryEditorAction extends
        AbstractGenericToolAction<BoundaryEditorLayer> {

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<BoundaryEditorLayer> getResourceData() {
        return new GenericToolsResourceData<BoundaryEditorLayer>(
                BoundaryEditorLayer.NAME, BoundaryEditorLayer.class);

    }

    // November 25, 2013

    @Override
    protected BoundaryEditorLayer getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        BoundaryEditorLayer layer = getExistingResource();
        if (layer == null)
            return super.getResource(loadProperties, descriptor);

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                BoundaryEditorLayer layer = getExistingResource();
                if (layer != null) {
                    layer.makeEditableAndReopenDialog();
                }
            }
        });
        return layer;

    }

    private BoundaryEditorLayer getExistingResource() {
        IDisplayPane[] panes = getSelectedPanes();
        if (panes != null && panes.length > 0) {
            List<BoundaryEditorLayer> layers = null;
            layers = panes[0].getDescriptor().getResourceList()
                    .getResourcesByTypeAsType(BoundaryEditorLayer.class);
            if (!layers.isEmpty()) {
                return layers.get(0);
            }
        }
        return null;
    }

}
