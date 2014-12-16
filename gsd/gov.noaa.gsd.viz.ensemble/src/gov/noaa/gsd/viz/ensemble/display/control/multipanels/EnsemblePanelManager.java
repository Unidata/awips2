package gov.noaa.gsd.viz.ensemble.display.control.multipanels;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Manage ensemble display Panes. Support the multiple panel display in the
 * ensemble. For example show up the plan view and time series windows at same
 * time, select a location and update the point in the time series.
 * 
 * 
 * @author jing
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb, 2014      5056       jing     Initial creation
 * 
 * </pre>
 */
public class EnsemblePanelManager {
    AbstractEditor theEditor;

    public void load(IDisplayPane pane, AbstractVizResource<?, ?> rsc) {
        // TODO
    }

    public void unload(IDisplayPane pane, AbstractVizResource<?, ?> rsc) {
        // TODO
    }

    public void unloadAll(IDisplayPane pane, AbstractVizResource<?, ?> rsc) {
        // TODO
    }

    public void cleanPanes(IDisplayPane pane, AbstractVizResource<?, ?> rsc) {
        // TODO
    }

}
