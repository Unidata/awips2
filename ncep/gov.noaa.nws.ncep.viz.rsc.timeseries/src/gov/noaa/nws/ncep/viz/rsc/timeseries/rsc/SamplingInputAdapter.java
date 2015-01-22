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
package gov.noaa.nws.ncep.viz.rsc.timeseries.rsc;

import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Default input handler for timeseries/geomag sampling
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/16/2014   R4079      qzhou       Initial creation
 * 07/28/2014   R4078      sgurung     Added null check
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class SamplingInputAdapter<T extends GeoMagResource> extends
        InputAdapter {

    private T resource;

    public SamplingInputAdapter(T resource) {
        this.resource = resource;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {

        IDisplayPaneContainer container = resource.getResourceContainer();

        if (container != null) {
            Coordinate c = container.translateClick(x, y);

            boolean isActiveResource = false;

            AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
            IDisplayPane activePane = editor.getActiveDisplayPane();

            ResourceList acResources = activePane.getDescriptor()
                    .getResourceList();
            int acRscSize = acResources.size();

            for (int i = acRscSize - 1; i >= 0; i--) {
                ResourcePair rp = acResources.get(i);
                AbstractVizResource<?, ?> activeRsc = rp.getResource();

                if (activeRsc != null
                        && activeRsc instanceof GeoMagResource
                        && rp.getProperties().isVisible()
                        && !((GeoMagResource) activeRsc).getLegendStr().equals(
                                "No Data")) {

                    if (activeRsc.equals(resource)) {
                        isActiveResource = true;
                    }
                    break;
                }
            }

            if (resource.getResourceContainer().getDisplayPanes().length > 1) {
                // Coordinate latLonCoord = ((GeoMagResource) resource)
                // .getLatLonFromPixel(c);

                for (IDisplayPane pane : resource.getResourceContainer()
                        .getDisplayPanes()) {

                    if (!pane.equals(activePane) && isActiveResource) {

                        ResourceList resources = pane.getDescriptor()
                                .getResourceList();
                        int size = resources.size();

                        for (int i = 0; i < size && size > 1; i++) {
                            ResourcePair rp = resources.get(i);
                            AbstractVizResource<?, ?> rsc = rp.getResource();

                            if (rsc != null && rsc instanceof GeoMagResource
                                    && rp.getProperties().isVisible()) {
                                // ((GeoMagResource) rsc)
                                // .setVirtualCursor(latLonCoord);

                                ((GeoMagResource) rsc).issueRefresh();

                            }
                        }
                    }
                }
            }

            if (isActiveResource) {
                if (c != null) {
                    resource.sampleCoord = new ReferencedCoordinate(c);
                } else {
                    resource.sampleCoord = null;
                }
            }

            // The sampling is always true for geomag
            resource.issueRefresh();
        }

        return false;
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        return handleMouseMove(x, y);
    }

    public boolean handleMouseExit(Event event) {
        resource.sampleCoord = null;
        // resource.virtualCursor = null;

        resource.issueRefresh();

        return false;
    }

    public boolean handleMouseEnter(Event event) {
        return handleMouseMove(event.x, event.y);
    }
}
