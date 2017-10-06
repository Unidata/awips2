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
package gov.noaa.gsd.viz.ensemble.display.rsc.histogram;

import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource.DisplayMode;

import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Ensemble default input handler for sampling. Is a modification of the
 * SamplingInputAdapter.
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
 * July, 2014      5056     jing       Initial creation
 * January 12, 2016 12301   jing       Added  click-move-click sampling
 * </pre>
 * @param <T>
 */
public class EnsSamplingInputAdapter<T extends EnsSamplingResource> extends
        InputAdapter {

    private T resource;

    /**
     * The flag implement 'click-move-click' sampling for distribution view. If
     * it is true, do distribution sampling. mouse-left click , sets the flag
     * isGraphicsSample to true (starts sampling), move the mouse to continue
     * sampling, and mouse-left click again, to stop sampling.
     * 
     */
    private boolean isGraphicsSample = false;

    public EnsSamplingInputAdapter(T resource) {
        this.resource = resource;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        /** Do nothing in graphics histogram case if isGraphicsSample is false */
        if (((HistogramResource<?>) resource).getMode() == DisplayMode.GRAPHIC_HISTGRAM
                && !isGraphicsSample) {
            return false;
        }

        /** Sample for all other case */
        IDisplayPaneContainer container = resource.getResourceContainer();
        Coordinate c = container.translateClick(x, y);
        if (c != null) {
            resource.sampleCoord = new ReferencedCoordinate(c);
        } else {
            resource.sampleCoord = null;
        }
        if (resource.isSampling()) {
            resource.issueRefresh();
        }
        return false;
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        return handleMouseMove(x, y);
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {

        /** Only allow sampling if the graphics histogram is turned off. */
        if (((HistogramResource<?>) resource).getMode() != DisplayMode.GRAPHIC_HISTGRAM) {
            return handleMouseMove(x, y);
        }
        IDisplayPaneContainer container = resource.getResourceContainer();
        Coordinate c = container.translateClick(x, y);
        if (c != null) {
            resource.sampleCoord = new ReferencedCoordinate(c);
        } else {
            resource.sampleCoord = null;
        }
        if (resource.isSampling()) {
            resource.issueRefresh();
        }

        /** toggle the flag value for graphics histogram mode */
        isGraphicsSample = !isGraphicsSample;
        return false;

    }

    public boolean handleMouseExit(Event event) {
        resource.sampleCoord = null;
        if (resource.isSampling()) {
            resource.issueRefresh();
        }
        return false;
    }

    public boolean handleMouseEnter(Event event) {
        return handleMouseMove(event.x, event.y);
    }
}
