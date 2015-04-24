/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.drawing;

import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.drawing.polygon.filter.LatLonToScreenFilter;
import com.raytheon.uf.viz.drawing.polygon.filter.ScreenToLatLonFilter;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * An input adapter for a resource/layer that is displayed on a map. Provides
 * convenience methods to assist with custom input handling.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2015  3974      njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class RscInputAdapter<R extends AbstractVizResource<?, MapDescriptor>>
        extends InputAdapter {

    protected R rsc;

    protected LatLonToScreenFilter latlonToScreen;

    protected ScreenToLatLonFilter screenToLatLon;

    protected RscInputAdapter(R rsc) {
        this.rsc = rsc;
        latlonToScreen = new LatLonToScreenFilter(rsc);
        screenToLatLon = new ScreenToLatLonFilter(rsc);
    }

    protected Coordinate screenToLatLon(int screenX, int screenY) {
        return getContainer().translateClick(screenX, screenY);
    }

    /**
     * Update the cursor with the specified cursor val
     * 
     * @param cursor
     *            the cursor value as specified by SWT.CURSOR_
     */
    protected void updateCursor(int cursor) {
        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                .getCurrentWindow();
        window.getShell().setCursor(
                window.getShell().getDisplay().getSystemCursor(cursor));
    }

    protected IDisplayPaneContainer getContainer() {
        return rsc.getResourceContainer();
    }

    protected boolean withinDisplayBounds(int x, int y) {
        // Checks if the x/y location is within the display bounds
        IRenderableDisplay display = rsc.getDescriptor().getRenderableDisplay();
        return display.getBounds().contains(x, y);
    }

    protected boolean availableForInput() {
        boolean available = rsc.getProperties().isVisible();
        if (rsc.hasCapability(EditableCapability.class)) {
            available = (available && rsc.getCapability(
                    EditableCapability.class).isEditable());
        }
        return available;
    }
}
